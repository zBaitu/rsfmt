use std::cmp::Ordering;
use std::collections::HashMap;

use rustc_ap_rustc_parse::{self as parse};
use rustc_ap_rustc_session::parse::ParseSess;

use crate::ast;
use crate::ir::*;

const MAX_BLANK_LINES: u8 = 1;

fn trans_comments(cmnts: Vec<ast::Comment>) -> Vec<Comment> {
    let mut pre_blank_line_pos = 0;
    let mut blank_lines = 0;

    cmnts.into_iter().fold(Vec::new(), |mut cmnts, cmnt| {
        if cmnt.style == ast::CommentStyle::BlankLine {
            let cur_pos = cmnt.pos.0;

            if cur_pos != pre_blank_line_pos + 1 {
                blank_lines = 1;
                cmnts.push(trans_comment(cmnt));
            } else {
                blank_lines += 1;
                if blank_lines <= MAX_BLANK_LINES {
                    cmnts.push(trans_comment(cmnt));
                }
            }

            pre_blank_line_pos = cur_pos;
        } else {
            blank_lines = 0;
            cmnts.push(trans_comment(cmnt));
        }

        cmnts
    })
}

fn trans_comment(cmnt: ast::Comment) -> Comment {
    let kind = match cmnt.style {
        ast::CommentStyle::Trailing => CommentKind::Trailing,
        _ => CommentKind::Leading,
    };

    Comment {
        pos: cmnt.pos.0,
        kind,
        lines: cmnt.lines,
    }
}

fn span(s: u32, e: u32) -> ast::Span {
    const ROOT: ast::SyntaxContext = ast::SyntaxContext::root();
    ast::Span::new(ast::BytePos(s), ast::BytePos(e), ROOT)
}

fn ident_to_string(ident: &ast::Ident) -> String {
    if (*ident).name != ast::kw::PathRoot {
        ident.name.to_string()
    } else {
        "".to_string()
    }
}

fn path_to_string(path: &ast::Path) -> String {
    let mut first = true;
    path.segments.iter().fold(String::new(), |mut s, e| {
        if !first {
            s.push_str("::");
        }
        first = false;
        s.push_str(&ident_to_string(&e.ident));
        s
    })
}

fn ext_to_string(ext: &ast::Extern) -> Option<String> {
    match *ext {
        ast::Extern::None => None,
        ast::Extern::Implicit => Some("extern".to_string()),
        ast::Extern::Explicit(ref str_lit) => Some(format!("extern \"{}\"", str_lit.symbol_unescaped)),
    }
}

fn abi_to_string(abi: &Option<ast::StrLit>) -> Option<String> {
    abi.as_ref().map(|abi| format!("\"{}\"", abi.symbol_unescaped))
}

fn uop_to_string(op: &ast::UnOp) -> &'static str {
    ast::UnOp::to_string(*op)
}

fn macro_style(style: &ast::DelimToken) -> MacroStyle {
    match *style {
        ast::DelimToken::Paren => MacroStyle::Paren,
        ast::DelimToken::Bracket => MacroStyle::Bracket,
        ast::DelimToken::Brace => MacroStyle::Brace,
        _ => unreachable!("{:#?}", style),
    }
}

fn delim_to_string(delim: &ast::DelimToken, open: bool) -> String {
    let s = match delim {
        ast::DelimToken::Paren => if open { "(" } else { ")" },
        ast::DelimToken::Bracket => if open { "[" } else { "]" },
        ast::DelimToken::Brace => if open { "{" } else { "}" },
        _ => unreachable!("{:#?}", delim),
    };
    s.to_string()
}

fn is_inner(style: &ast::AttrStyle) -> bool {
    *style == ast::AttrStyle::Inner
}

fn is_block(kind: &ast::CommentKind) -> bool {
    *kind == ast::CommentKind::Block
}

fn is_unsafe(unsafety: &ast::Unsafe) -> bool {
    match *unsafety {
        ast::Unsafe::Yes(..) => true,
        ast::Unsafe::No => false,
    }
}

fn is_default(defaultness: &ast::Defaultness) -> bool {
    match *defaultness {
        ast::Defaultness::Default(..) => true,
        ast::Defaultness::Final => false,
    }
}

fn is_sized(modifier: &ast::TraitBoundModifier) -> bool {
    *modifier == ast::TraitBoundModifier::Maybe
}

fn is_mut(mutability: &ast::Mutability) -> bool {
    *mutability == ast::Mutability::Mut
}

fn is_raw(borrow: &ast::BorrowKind) -> bool {
    *borrow == ast::BorrowKind::Raw
}

fn is_dyn(syntax: &ast::TraitObjectSyntax) -> bool {
    *syntax == ast::TraitObjectSyntax::Dyn
}

fn is_async(asyncness: &ast::Async) -> bool {
    match *asyncness {
        ast::Async::Yes {..} => true,
        ast::Async::No => false,
    }
}

fn is_const(constness: &ast::Const) -> bool {
    match *constness {
        ast::Const::Yes(..) => true,
        ast::Const::No => false,
    }
}

fn is_auto(autoness: &ast::IsAuto) -> bool {
    *autoness == ast::IsAuto::Yes
}

fn has_patten(param: &ast::Param, pattern: &Pattern) -> bool {
    if let Some(sf) = param.to_self() {
        match sf.node {
            ast::SelfKind::Value(..) | ast::SelfKind::Region(..) => return false,
            _ => (),
        }
    }

    match pattern.pattern {
        PattenKind::Ident(ref ident) => !ident.name.is_empty(),
        _ => true,
    }
}

fn is_neg(polarity: &ast::ImplPolarity) -> bool {
    match *polarity {
        ast::ImplPolarity::Negative(..) => true,
        ast::ImplPolarity::Positive => false,
    }
}

fn is_block_unsafe(rules: &ast::BlockCheckMode) -> bool {
    match *rules {
        ast::BlockCheckMode::Unsafe(source) => source == ast::UnsafeSource::UserProvided,
        _ => false,
    }
}

fn is_inclusive(limit: &ast::RangeLimits) -> bool {
    *limit == ast::RangeLimits::Closed
}

fn is_patten_inclusive(range_end: &ast::RangeEnd) -> bool {
    matches!(*range_end, ast::RangeEnd::Included(..))
}

fn is_static(movability: &ast::Movability) -> bool {
    *movability == ast::Movability::Static
}

fn is_move(capture: &ast::CaptureBy) -> bool {
    *capture == ast::CaptureBy::Value
}

fn is_ref_mut(binding: &ast::BindingMode) -> (bool, bool) {
    match *binding {
        ast::BindingMode::ByRef(ref mutability) => (true, is_mut(mutability)),
        ast::BindingMode::ByValue(ref mutability) => (false, is_mut(mutability)),
    }
}

fn is_macro_semi(style: &ast::MacStmtStyle) -> bool {
    matches!(style, ast::MacStmtStyle::Semicolon)
}

fn map_trans_label(label: &Option<ast::Label>) -> Option<String> {
    label.as_ref().map(|label| ident_to_string(&label.ident))
}

macro_rules! trans_list {
    ($sf: ident, $list: ident, $trans_single: ident) => ({
        $list.iter().map(|ref e| $sf.$trans_single(e)).collect()
    });
}

macro_rules! map_trans {
    ($sf: ident, $opt: expr, $trans: ident) => ({
        $opt.as_ref().map(|item| $sf.$trans(item))
    });
}

pub struct TrResult {
    pub krate: Crate,
    pub leading_cmnts: HashMap<Pos, Vec<String>>,
    pub trailing_cmnts: HashMap<Pos, String>,
}

pub fn translate(src: String, sess: ParseSess, krate: ast::Crate, cmnts: Vec<ast::Comment>) -> TrResult {
    Translator::new(src, sess, trans_comments(cmnts)).trans_crate(krate)
}

struct Translator {
    src: String,
    sess: ParseSess,
    cmnts: Vec<Comment>,

    cmnt_idx: usize,
    last_loc: Loc,
    leading_cmnts: HashMap<Pos, Vec<String>>,
    trailing_cmnts: HashMap<Pos, String>,
}

impl Translator {
    fn new(src: String, sess: ParseSess, cmnts: Vec<Comment>) -> Translator {
        Translator {
            src,
            sess,
            cmnts,

            cmnt_idx: 0,
            last_loc: Default::default(),
            leading_cmnts: HashMap::new(),
            trailing_cmnts: HashMap::new(),
        }
    }

    fn trans_crate(mut self, krate: ast::Crate) -> TrResult {
        self.last_loc.start = krate.span.lo().0;

        let loc = self.loc(&krate.span);
        let attrs = self.trans_attrs(&krate.attrs);
        let items = self.trans_items(&krate.items);
        let crate_file_end = self.crate_file_end();
        self.trans_comments(crate_file_end);

        TrResult {
            krate: Crate {
                loc,
                attrs,
                items,
            },
            leading_cmnts: self.leading_cmnts,
            trailing_cmnts: self.trailing_cmnts,
        }
    }

    fn trans_comments(&mut self, pos: Pos) {
        let cmnts = self.trans_trailing_comments(pos);
        self.trans_leading_comments(pos, cmnts);
    }

    fn trans_trailing_comments(&mut self, pos: Pos) -> Vec<String> {
        let mut cmnts = Vec::new();

        if self.cmnt_idx >= self.cmnts.len() {
            return cmnts;
        }
        let cmnt = &self.cmnts[self.cmnt_idx];
        if cmnt.pos >= pos || cmnt.kind != CommentKind::Trailing {
            return cmnts;
        }
        self.cmnt_idx += 1;

        self.trailing_cmnts.insert(self.last_loc.end, cmnt.lines[0].clone());
        cmnts.extend_from_slice(&cmnt.lines[1..]);
        cmnts
    }

    fn trans_leading_comments(&mut self, pos: Pos, mut cmnts: Vec<String>) {
        while self.cmnt_idx < self.cmnts.len() {
            let cmnt = &self.cmnts[self.cmnt_idx];
            if cmnt.pos >= pos {
                break;
            }

            if cmnt.lines.is_empty() {
                cmnts.push(String::new());
            } else {
                cmnts.extend_from_slice(&cmnt.lines);
            }

            self.cmnt_idx += 1;
        }

        if !cmnts.is_empty() {
            self.leading_cmnts.insert(pos, cmnts);
        }
    }

    fn trans_attrs(&mut self, attrs: &[ast::Attribute]) -> Vec<Attr> {
        trans_list!(self, attrs, trans_attr)
    }

    fn trans_thin_attrs(&mut self, attrs: &ast::AttrVec) -> Vec<Attr> {
        trans_list!(self, attrs, trans_attr)
    }

    fn trans_attr(&mut self, attr: &ast::Attribute) -> Attr {
        let is_inner = is_inner(&attr.style);
        let (loc, attr) = match attr.kind {
            ast::AttrKind::DocComment(ref cmnt_kind, ref sym) => {
                (self.leaf_loc(&attr.span), AttrKind::Doc(self.trans_doc_attr(cmnt_kind, sym)))
            },
            ast::AttrKind::Normal(..) => {
                let loc = self.loc(&attr.span);
                if let Some(meta_item) = attr.meta().as_ref() {
                    let span_forward = if is_inner { 2 } else { 1 };
                    let attr = AttrKind::Attr(self.trans_meta_attr(meta_item, span_forward));
                    (loc, attr)
                } else {
                    let attr = AttrKind::Raw(LocStr::new(self.span_to_snippet(&attr.span)));
                    (loc, attr)
                }
            },
        };

        Attr {
            loc,
            is_inner,
            attr,
        }
    }

    fn trans_doc_attr(&mut self, cmnt_kind: &ast::CommentKind, sym: &ast::Symbol) -> DocAttr {
        DocAttr {
            is_block: is_block(cmnt_kind),
            doc: LocStr::new(sym.to_string()),
        }
    }

    fn trans_meta_attr(&mut self, meta_item: &ast::MetaItem, span_forward: u32) -> MetaAttr {
        let name = LocStr::new(path_to_string(&meta_item.path));
        let span = span(meta_item.span.lo().0 + span_forward, meta_item.span.hi().0);
        match meta_item.kind {
            ast::MetaItemKind::Word => {
                MetaAttr {
                    loc: self.leaf_loc(&span),
                    name,
                    metas: None,
                }
            },
            ast::MetaItemKind::NameValue(ref lit) => {
                let s = LocStr::new(format!("{} = {}", name, self.literal_to_string(lit)));
                MetaAttr {
                    loc: self.leaf_loc(&span),
                    name: s,
                    metas: None,
                }
            },
            ast::MetaItemKind::List(ref nested_meta_items) => {
                let loc = self.loc(&span);
                let metas = self.trans_nested_metas(nested_meta_items);
                self.set_loc(&loc);

                MetaAttr {
                    loc,
                    name,
                    metas: Some(metas),
                }
            },
        }
    }

    fn trans_nested_metas(&mut self, nested_meta_items: &[ast::NestedMetaItem]) -> Vec<MetaAttr> {
        let mut metas: Vec<MetaAttr> = trans_list!(self, nested_meta_items, trans_nested_meta);
        metas.sort_by(|a, b| a.name.s.cmp(&b.name.s));
        metas
    }

    fn trans_nested_meta(&mut self, nested_meta_item: &ast::NestedMetaItem) -> MetaAttr {
        match nested_meta_item {
            ast::NestedMetaItem::Literal(ref lit) => {
                MetaAttr {
                    loc: self.leaf_loc(&nested_meta_item.span()),
                    name: LocStr::new(self.literal_to_string(lit)),
                    metas: None,
                }
            },
            ast::NestedMetaItem::MetaItem(ref meta_iten) => {
                self.trans_meta_attr(meta_iten, 0)
            },
        }
    }

    fn trans_items(&mut self, items: &[ast::P<ast::Item>]) -> Vec<Item> {
        trans_list!(self, items, trans_item)
    }

    fn trans_item(&mut self, item: &ast::Item) -> Item {
        let loc = self.loc(&item.span);
        let attrs = self.trans_attrs(&item.attrs);
        let vis = self.trans_vis(&item.vis);
        let ident = ident_to_string(&item.ident);
        let item = match item.kind {
            ast::ItemKind::ExternCrate(ref rename) => ItemKind::ExternCrate(self.trans_extren_crate(ident, rename)),
            ast::ItemKind::Use(ref tree) => ItemKind::Use(self.trans_use(tree)),
            ast::ItemKind::Mod(ref unsafety, ref module) => {
                let is_unsafe = is_unsafe(unsafety);
                match module {
                    ast::ModKind::Unloaded => ItemKind::ModDecl(self.trans_mod_decl(is_unsafe, ident)),
                    ast::ModKind::Loaded(ref items, ..) => ItemKind::Mod(self.trans_mod(is_unsafe, ident, items)),
                }
            },
            ast::ItemKind::TyAlias(ref type_alias) => ItemKind::TypeAlias(self.trans_type_alias(ident, type_alias)),
            ast::ItemKind::TraitAlias(ref generics, ref bounds) => {
                ItemKind::TraitAlias(self.trans_trait_alias(ident, generics, bounds))
            },
            ast::ItemKind::Const(ref defaultness, ref ty, ref expr) => {
                ItemKind::Const(self.trans_const(defaultness, ident, ty, expr))
            },
            ast::ItemKind::Static(ref ty, ref mutability, ref expr) => {
                ItemKind::Static(self.trans_static(mutability, ident, ty, expr))
            },
            ast::ItemKind::Struct(ref var, ref generics) => ItemKind::Struct(self.trans_struct(ident, generics, var)),
            ast::ItemKind::Union(ref var, ref generics) => ItemKind::Union(self.trans_union(ident, generics, var)),
            ast::ItemKind::Enum(ref enum_def, ref generics) => {
                ItemKind::Enum(self.trans_enum(ident, generics, enum_def))
            },
            ast::ItemKind::Fn(ref fn_kind) => ItemKind::Fn(self.trans_fn(ident, fn_kind)),
            ast::ItemKind::ForeignMod(ref module) => ItemKind::ForeignMod(self.trans_foreign_mod(module)),
            ast::ItemKind::Trait(ref trait_kind) => ItemKind::Trait(self.trans_trait(ident, trait_kind)),
            ast::ItemKind::Impl(ref impl_kind) => ItemKind::Impl(self.trans_impl(impl_kind)),
            ast::ItemKind::MacroDef(ref macro_def) => ItemKind::MacroDef(self.trans_macro_def(ident, macro_def)),
            ast::ItemKind::MacCall(ref macro_call) => ItemKind::MacroCall(self.trans_macro_call(macro_call)),
            ast::ItemKind::GlobalAsm(..) => unreachable!("{:#?}", item.kind),
        };

        self.set_loc(&loc);
        Item {
            loc,
            attrs,
            vis,
            item,
        }
    }

    fn trans_vis(&mut self, vis: &ast::Visibility) -> Vis {
        match vis.kind {
            ast::VisibilityKind::Public => "pub".to_string(),
            ast::VisibilityKind::Crate(sugar) => match sugar {
                ast::CrateSugar::PubCrate => "pub(crate)".to_string(),
                ast::CrateSugar::JustCrate => "crate".to_string(),
            },
            ast::VisibilityKind::Restricted { ref path, .. } => {
                let path = path_to_string(path);
                if path == "self" || path == "super" {
                    format!("pub({})", path)
                } else {
                    format!("pub(in {})", path)
                }
            },
            ast::VisibilityKind::Inherited => "".to_string(),
        }
    }

    fn trans_extren_crate(&mut self, ident: String, rename: &Option<ast::Symbol>) -> ExternCrate {
        let name = match *rename {
            Some(ref rename) => format!("{} as {}", rename, ident),
            None => ident,
        };

        ExternCrate {
            name,
        }
    }

    fn trans_use(&mut self, tree: &ast::UseTree) -> Use {
        let use_tree = self.trans_use_tree(tree);
        Use {
            path: use_tree.path,
            trees: use_tree.trees,
        }
    }

    fn trans_use_tree(&mut self, tree: &ast::UseTree) -> UseTree {
        let (loc, path, trees) = match tree.kind {
            ast::UseTreeKind::Simple(rename, ..) => {
                let loc = self.leaf_loc(&tree.span);
                let mut path = path_to_string(&tree.prefix);
                if let Some(ref rename) = rename {
                    path = format!("{} as {}", path, ident_to_string(rename));
                }
                (loc, path, None)
            },
            ast::UseTreeKind::Glob => {
                let loc = self.leaf_loc(&tree.span);
                let mut path = String::new();
                if !tree.prefix.segments.is_empty() {
                    path.push_str(&path_to_string(&tree.prefix));
                    path.push_str("::");
                }
                path.push('*');
                (loc, path, None)
            },
            ast::UseTreeKind::Nested(ref trees) => {
                let loc = self.loc(&tree.span);
                let path = path_to_string(&tree.prefix);
                let trees = self.trans_use_trees(trees);
                self.set_loc(&loc);
                (loc, path, Some(trees))
            },
        };

        UseTree {
            loc,
            path,
            trees,
        }
    }

    fn trans_use_trees(&mut self, trees: &[(ast::UseTree, ast::NodeId)]) -> Vec<UseTree> {
        let mut trees: Vec<UseTree> = trees.iter().map(|e| self.trans_use_tree(&e.0)).collect();
        trees.sort_by(|a, b| {
            if a.path.starts_with("self") {
                Ordering::Less
            } else if b.path.starts_with("self") {
                Ordering::Greater
            } else {
                a.path.cmp(&b.path)
            }
        });
        trees
    }

    fn trans_mod_decl(&mut self, is_unsafe: bool, ident: String) -> ModDecl {
        ModDecl {
            is_unsafe,
            name: ident,
        }
    }

    fn trans_mod(&mut self, is_unsafe: bool, ident: String, items: &[ast::P<ast::Item>]) -> Mod {
        Mod {
            is_unsafe,
            name: ident,
            items: self.trans_items(items),
        }
    }

    fn trans_type_alias(&mut self, ident: String, type_alias: &ast::TyAliasKind) -> TypeAlias {
        TypeAlias {
            is_default: is_default(&type_alias.0),
            name: ident,
            generics: self.trans_generics(&type_alias.1),
            bounds: self.trans_type_param_bounds(&type_alias.2),
            ty: map_trans!(self, &type_alias.3, trans_type),
        }
    }

    fn trans_trait_alias(&mut self, ident: String, generics: &ast::Generics, bounds: &ast::GenericBounds)
    -> TraitAlias {
        TraitAlias {
            name: ident,
            generics: self.trans_generics(generics),
            bounds: self.trans_type_param_bounds(bounds),
        }
    }

    fn trans_generics(&mut self, generics: &ast::Generics) -> Generics {
        Generics {
            lifetime_defs: self.trans_lifetime_defs(&generics.params),
            type_params: self.trans_type_params(&generics.params),
            const_params: self.trans_const_params(&generics.params),
            wh: self.trans_where(&generics.where_clause.predicates),
        }
    }

    fn trans_lifetime_defs(&mut self, params: &[ast::GenericParam]) -> Vec<LifetimeDef> {
        params.iter().fold(Vec::new(), |mut lifetime_defs, param| {
            if let ast::GenericParamKind::Lifetime = param.kind {
                lifetime_defs.push(self.trans_lifetime_def(param));
            }
            lifetime_defs
        })
    }

    fn trans_lifetime_def(&mut self, param: &ast::GenericParam) -> LifetimeDef {
        let lifetime = self.trans_lifetime(&param.ident);
        LifetimeDef {
            loc: lifetime.loc,
            lifetime,
            bounds: self.trans_lifetimes(&param.bounds),
        }
    }

    fn trans_lifetime(&mut self, ident: &ast::Ident) -> Lifetime {
        Lifetime {
            loc: self.leaf_loc(&ident.span),
            s: ident.name.to_string(),
        }
    }

    fn trans_lifetimes(&mut self, bounds: &ast::GenericBounds) -> Vec<Lifetime> {
        bounds.iter().fold(Vec::new(), |mut lifetimes, bound| {
            if let ast::GenericBound::Outlives(ref lifetime) = bound {
                lifetimes.push(self.trans_lifetime(&lifetime.ident));
            }
            lifetimes
        })
    }

    fn trans_type_params(&mut self, params: &[ast::GenericParam]) -> Vec<TypeParam> {
        params.iter().fold(Vec::new(), |mut type_params, param| {
            if let ast::GenericParamKind::Type {..} = param.kind {
                type_params.push(self.trans_type_param(param));
            }
            type_params
        })
    }

    fn trans_type_param(&mut self, param: &ast::GenericParam) -> TypeParam {
        let loc = self.loc(&param.ident.span);
        let name = ident_to_string(&param.ident);
        let bounds = self.trans_type_param_bounds(&param.bounds);
        let default = match param.kind {
            ast::GenericParamKind::Type { ref default, } => map_trans!(self, default, trans_type),
            _ => None,
        };
        self.set_loc(&loc);

        TypeParam {
            loc,
            name,
            bounds,
            default,
        }
    }

    fn trans_type_param_bounds(&mut self, bounds: &ast::GenericBounds) -> TypeParamBounds {
        TypeParamBounds(trans_list!(self, bounds, trans_type_param_bound))
    }

    fn trans_type_param_bound(&mut self, bound: &ast::GenericBound) -> TypeParamBound {
        match *bound {
            ast::GenericBound::Outlives(ref lifetime) => {
                TypeParamBound::Lifetime(self.trans_lifetime(&lifetime.ident))
            },
            ast::GenericBound::Trait(ref poly_trait_ref, ref modifier) => {
                TypeParamBound::PolyTraitRef(self.trans_poly_trait_ref(poly_trait_ref, modifier))
            },
        }
    }

    fn trans_poly_trait_ref(&mut self, poly_trait_ref: &ast::PolyTraitRef, modifier: &ast::TraitBoundModifier)
    -> PolyTraitRef {
        if is_sized(modifier) {
            return PolyTraitRef::new_sized(self.leaf_loc(&poly_trait_ref.span));
        }

        let loc = self.loc(&poly_trait_ref.span);
        let lifetime_defs = self.trans_lifetime_defs(&poly_trait_ref.bound_generic_params);
        let trait_ref = self.trans_trait_ref(&poly_trait_ref.trait_ref);
        self.set_loc(&loc);

        PolyTraitRef {
            loc,
            lifetime_defs,
            trait_ref,
        }
    }

    fn trans_trait_ref(&mut self, trait_ref: &ast::TraitRef) -> TraitRef {
        self.trans_path(&trait_ref.path)
    }

    fn trans_const_params(&mut self, params: &[ast::GenericParam]) -> Vec<ConstParam> {
        params.iter().fold(Vec::new(), |mut const_params, param| {
            if let ast::GenericParamKind::Const { ref ty, ref kw_span, ref default, } = param.kind {
                const_params.push(self.trans_const_param(kw_span, &param.ident, ty, default));
            }
            const_params
        })
    }

    fn trans_const_param(&mut self, span: &ast::Span, ident: &ast::Ident,
                         ty: &ast::Ty, default: &Option<ast::AnonConst>) -> ConstParam {
        let loc = self.loc(span);
        let name = ident_to_string(ident);
        let ty = self.trans_type(ty);
        let default = default.as_ref().map(|ac| self.trans_expr(&ac.value));
        self.set_loc(&loc);

        ConstParam {
            loc,
            name,
            ty,
            default,
        }
    }

    fn trans_path(&mut self, path: &ast::Path) -> Path {
        let loc = self.loc(&path.span);
        let segments = self.trans_path_segments(&path.segments);
        self.set_loc(&loc);

        Path {
            loc,
            segments,
        }
    }

    fn trans_path_segments(&mut self, segments: &[ast::PathSegment]) -> Vec<PathSegment> {
        trans_list!(self, segments, trans_path_segment)
    }

    fn trans_path_segment(&mut self, seg: &ast::PathSegment) -> PathSegment {
        PathSegment {
            loc: self.loc(&seg.ident.span),
            name: ident_to_string(&seg.ident),
            param: self.trans_generics_args_or_none(&seg.args),
        }
    }

    fn trans_generics_args_or_none(&mut self, args: &Option<ast::P<ast::GenericArgs>>) -> PathParam {
        match *args {
            Some(ref args) => self.trans_generic_args(args),
            None => PathParam::Angle(Default::default()),
        }
    }

    fn trans_generic_args(&mut self, args: &ast::GenericArgs) -> PathParam {
        match *args {
            ast::AngleBracketed(ref param) => PathParam::Angle(self.trans_angle_param(param)),
            ast::Parenthesized(ref param) => PathParam::Paren(self.trans_paren_param(param)),
        }
    }

    fn trans_angle_param(&mut self, param: &ast::AngleBracketedArgs) -> AngleParam {
        AngleParam {
            lifetimes: self.trans_generic_args_to_lifetime(&param.args),
            types: self.trans_generic_args_to_types(&param.args),
            consts: self.trans_generic_args_to_exprs(&param.args),
            bindings: self.trans_type_constraints(&param.args),
        }
    }

    fn trans_generic_args_to_lifetime(&mut self, args: &[ast::AngleBracketedArg]) -> Vec<Lifetime> {
        args.iter().fold(Vec::new(), |mut lifetimes, arg| {
            if let ast::AngleBracketedArg::Arg(ast::GenericArg::Lifetime(ref lifetime)) = arg {
                lifetimes.push(self.trans_lifetime(&lifetime.ident));
            }
            lifetimes
        })
    }

    fn trans_generic_args_to_types(&mut self, args: &[ast::AngleBracketedArg]) -> Vec<Type> {
        args.iter().fold(Vec::new(), |mut types, arg| {
            if let ast::AngleBracketedArg::Arg(ast::GenericArg::Type(ref ty)) = arg {
                types.push(self.trans_type(ty));
            }
            types
        })
    }

    fn trans_generic_args_to_exprs(&mut self, args: &[ast::AngleBracketedArg]) -> Vec<Expr> {
        args.iter().fold(Vec::new(), |mut exprs, arg| {
            if let ast::AngleBracketedArg::Arg(ast::GenericArg::Const(ref ac)) = arg {
                exprs.push(self.trans_expr(&ac.value));
            }
            exprs
        })
    }

    fn trans_type_constraints(&mut self, args: &[ast::AngleBracketedArg]) -> Vec<TypeBinding> {
        args.iter().fold(Vec::new(), |mut constraints, arg| {
            if let ast::AngleBracketedArg::Constraint(ref constraint) = arg {
                constraints.push(self.trans_type_constraint(constraint));
            }
            constraints
        })
    }

    fn trans_type_constraint(&mut self, binding: &ast::AssocTyConstraint) -> TypeBinding {
        let loc = self.loc(&binding.span);
        let name = ident_to_string(&binding.ident);
        let binding = match binding.kind {
            ast::AssocTyConstraintKind::Equality { ref ty, } => TypeBindingKind::Eq(self.trans_type(ty)),
            ast::AssocTyConstraintKind::Bound { ref bounds, } => {
                TypeBindingKind::Bound(self.trans_type_param_bounds(bounds))
            },
        };
        self.set_loc(&loc);

        TypeBinding {
            loc,
            name,
            binding,
        }
    }

    fn trans_paren_param(&mut self, args: &ast::ParenthesizedArgs) -> ParenParam {
        let loc = self.loc(&args.span);
        let inputs = self.trans_types(&args.inputs);
        let output = match args.output {
            ast::FnRetTy::Ty(ref ty) => Some(self.trans_type(ty)),
            ast::FnRetTy::Default(..) => None,
        };
        self.set_loc(&loc);

        ParenParam {
            loc,
            inputs,
            output,
        }
    }

    fn trans_qself(&mut self, qself: &ast::QSelf) -> QSelf {
        QSelf {
            ty: self.trans_type(&qself.ty),
            pos: qself.position,
        }
    }

    fn trans_where(&mut self, predicates: &[ast::WherePredicate]) -> Where {
        Where {
            clauses: self.trans_where_clauses(predicates),
        }
    }

    fn trans_where_clauses(&mut self, predicates: &[ast::WherePredicate]) -> Vec<WhereClause> {
        trans_list!(self, predicates, trans_where_clause)
    }

    fn trans_where_clause(&mut self, predicate: &ast::WherePredicate) -> WhereClause {
        match *predicate {
            ast::WherePredicate::RegionPredicate(ref region) => self.trans_where_lifetime(region),
            ast::WherePredicate::BoundPredicate(ref bound) => self.trans_where_bound(bound),
            ast::WherePredicate::EqPredicate(..) => unreachable!("{:#?}", predicate),
        }
    }

    fn trans_where_lifetime(&mut self, region: &ast::WhereRegionPredicate) -> WhereClause {
        let loc = self.loc(&region.span);
        let lifetime = self.trans_lifetime(&region.lifetime.ident);
        let bounds = self.trans_lifetimes(&region.bounds);
        self.set_loc(&loc);

        WhereClause {
            loc,
            clause: WhereKind::LifetimeDef(LifetimeDef {
                loc: lifetime.loc,
                lifetime,
                bounds,
            }),
        }
    }

    fn trans_where_bound(&mut self, bound: &ast::WhereBoundPredicate) -> WhereClause {
        let loc = self.loc(&bound.span);
        let lifetime_defs = self.trans_lifetime_defs(&bound.bound_generic_params);
        let ty = self.trans_type(&bound.bounded_ty);
        let bounds = self.trans_type_param_bounds(&bound.bounds);
        self.set_loc(&loc);

        WhereClause {
            loc,
            clause: WhereKind::Bound(WhereBound {
                lifetime_defs,
                ty,
                bounds,
            }),
        }
    }

    fn trans_types(&mut self, types: &[ast::P<ast::Ty>]) -> Vec<Type> {
        trans_list!(self, types, trans_type)
    }

    fn trans_type(&mut self, ty: &ast::Ty) -> Type {
        let loc = self.loc(&ty.span);

        let ty = match ty.kind {
            ast::TyKind::Infer => TypeKind::Symbol("_"),
            ast::TyKind::Never => TypeKind::Symbol("!"),
            ast::TyKind::CVarArgs => TypeKind::Symbol("..."),
            ast::TyKind::ImplicitSelf => TypeKind::Symbol("self"),
            ast::TyKind::Path(ref qself, ref path) => TypeKind::Path(Box::new(self.trans_path_type(qself, path))),
            ast::TyKind::Ptr(ref mut_type) => TypeKind::Ptr(Box::new(self.trans_ptr_type(mut_type))),
            ast::TyKind::Rptr(ref lifetime, ref mut_type) => {
                TypeKind::Ref(Box::new(self.trans_ref_type(lifetime, mut_type)))
            },
            ast::TyKind::Tup(ref types) => TypeKind::Tuple(Box::new(self.trans_tuple_type(types))),
            ast::TyKind::Paren(ref ty) => TypeKind::Tuple(Box::new(self.trans_tuple_type(&[ty.clone()]))),
            ast::TyKind::Slice(ref ty) => TypeKind::Slice(Box::new(self.trans_slice_type(ty))),
            ast::TyKind::Array(ref ty, ref ac) => TypeKind::Array(Box::new(self.trans_array_type(ty, &ac.value))),
            ast::TyKind::AnonymousStruct(ref fields, _) => TypeKind::Struct(self.trans_struct_type(fields)),
            ast::TyKind::AnonymousUnion(ref fields, _) => TypeKind::Union(self.trans_union_type(fields)),
            ast::TyKind::TraitObject(ref bounds, ref syntax) => {
                TypeKind::Trait(Box::new(self.trans_trait_type(is_dyn(syntax), false, bounds)))
            },
            ast::TyKind::ImplTrait(_, ref bounds) => {
                TypeKind::Trait(Box::new(self.trans_trait_type(false, true, bounds)))
            },
            ast::TyKind::BareFn(ref bare_fn) => {
                TypeKind::BareFn(Box::new(self.trans_bare_fn_type(bare_fn)))
            },
            ast::TyKind::MacCall(ref mac_call) => TypeKind::MacroCall(self.trans_macro_call(mac_call)),
            ast::TyKind::Typeof(..) | ast::TyKind::Err => unreachable!("{:#?}", ty.kind),
        };

        self.set_loc(&loc);
        Type {
            loc,
            ty,
        }
    }

    fn trans_path_type(&mut self, qself: &Option<ast::QSelf>, path: &ast::Path) -> PathType {
        PathType {
            qself: map_trans!(self, qself, trans_qself),
            path: self.trans_path(path),
        }
    }

    fn trans_ptr_type(&mut self, mut_type: &ast::MutTy) -> PtrType {
        PtrType {
            is_mut: is_mut(&mut_type.mutbl),
            ty: self.trans_type(&mut_type.ty),
        }
    }

    fn trans_ref_type(&mut self, lifetime: &Option<ast::Lifetime>, mut_type: &ast::MutTy) -> RefType {
        RefType {
            lifetime: lifetime.as_ref().map(|lifetime| self.trans_lifetime(&lifetime.ident)),
            is_mut: is_mut(&mut_type.mutbl),
            ty: self.trans_type(&mut_type.ty),
        }
    }

    fn trans_tuple_type(&mut self, types: &[ast::P<ast::Ty>]) -> TupleType {
        TupleType {
            types: self.trans_types(types),
        }
    }

    fn trans_slice_type(&mut self, ty: &ast::Ty) -> SliceType {
        SliceType {
            ty: self.trans_type(ty),
        }
    }

    fn trans_array_type(&mut self, ty: &ast::Ty, expr: &ast::Expr) -> ArrayType {
        ArrayType {
            ty: self.trans_type(ty),
            expr: self.trans_expr(expr),
        }
    }

    fn trans_struct_type(&mut self, fields: &[ast::FieldDef]) -> StructType {
        StructType {
            fields: self.trans_struct_fields(fields),
        }
    }

    fn trans_union_type(&mut self, fields: &[ast::FieldDef]) -> UnionType {
        UnionType {
            fields: self.trans_struct_fields(fields),
        }
    }

    fn trans_trait_type(&mut self, is_dyn: bool, is_impl: bool, bounds: &ast::GenericBounds) -> TraitType {
        TraitType {
            is_dyn,
            is_impl,
            bounds: self.trans_type_param_bounds(bounds),
        }
    }

    fn trans_bare_fn_type(&mut self, bare_fn: &ast::BareFnTy) -> BareFnType {
        BareFnType {
            lifetime_defs: self.trans_lifetime_defs(&bare_fn.generic_params),
            header: FnHeader {
                is_unsafe: is_unsafe(&bare_fn.unsafety),
                ext: ext_to_string(&bare_fn.ext),
                ..Default::default()
            },
            sig: self.trans_fn_sig(&bare_fn.decl),
        }
    }

    fn trans_const(&mut self, defaultness: &ast::Defaultness, ident: String,
                   ty: &ast::Ty, expr: &Option<ast::P<ast::Expr>>) -> Const {
        Const {
            is_default: is_default(defaultness),
            name: ident,
            ty: self.trans_type(ty),
            expr: map_trans!(self, expr, trans_expr),
        }
    }

    fn trans_static(&mut self, mutability: &ast::Mutability, ident: String,
                    ty: &ast::Ty, expr: &Option<ast::P<ast::Expr>>) -> Static {
        Static {
            is_mut: is_mut(mutability),
            name: ident,
            ty: self.trans_type(ty),
            expr: map_trans!(self, expr, trans_expr),
        }
    }

    fn trans_struct(&mut self, ident: String, generics: &ast::Generics, var: &ast::VariantData) -> Struct {
        Struct {
            name: ident,
            generics: self.trans_generics(generics),
            body: self.trans_struct_body(var),
        }
    }

    fn trans_struct_body(&mut self, var: &ast::VariantData) -> StructBody {
        match *var {
            ast::VariantData::Struct(ref fields, _) => {
                StructBody::Struct(self.trans_struct_fields(fields))
            },
            ast::VariantData::Tuple(ref fields, _) => {
                StructBody::Tuple(self.trans_tuple_fields(fields))
            },
            ast::VariantData::Unit(..) => StructBody::Unit,
        }
    }

    fn trans_struct_fields(&mut self, fields: &[ast::FieldDef]) -> Vec<StructField> {
        trans_list!(self, fields, trans_struct_field)
    }

    fn trans_struct_field(&mut self, field: &ast::FieldDef) -> StructField {
        let loc = self.loc(&field.span);
        let attrs = self.trans_thin_attrs(&field.attrs);
        let vis = self.trans_vis(&field.vis);
        let name = ident_to_string(&field.ident.unwrap());
        let ty = self.trans_type(&field.ty);
        self.set_loc(&loc);

        StructField {
            loc,
            attrs,
            vis,
            name,
            ty,
        }
    }

    fn trans_tuple_fields(&mut self, fields: &[ast::FieldDef]) -> Vec<TupleField> {
        trans_list!(self, fields, trans_tuple_field)
    }

    fn trans_tuple_field(&mut self, field: &ast::FieldDef) -> TupleField {
        let loc = self.loc(&field.span);
        let attrs = self.trans_thin_attrs(&field.attrs);
        let vis = self.trans_vis(&field.vis);
        let ty = self.trans_type(&field.ty);
        self.set_loc(&loc);

        TupleField {
            loc,
            attrs,
            vis,
            ty,
        }
    }

    fn trans_union(&mut self, ident: String, generics: &ast::Generics, var: &ast::VariantData) -> Union {
        let fields = match *var {
            ast::VariantData::Struct(ref fields, _) => {
                self.trans_struct_fields(fields)
            },
            _ => unreachable!("{:#?}", *var),
        };

        Union {
            name: ident,
            generics: self.trans_generics(generics),
            fields,
        }
    }

    fn trans_enum(&mut self, ident: String, generics: &ast::Generics, enum_def: &ast::EnumDef) -> Enum {
        Enum {
            name: ident,
            generics: self.trans_generics(generics),
            body: self.trans_enum_body(enum_def),
        }
    }

    fn trans_enum_body(&mut self, enum_def: &ast::EnumDef) -> EnumBody {
        EnumBody {
            fields: self.trans_enum_fields(&enum_def.variants),
        }
    }

    fn trans_enum_fields(&mut self, vars: &[ast::Variant]) -> Vec<EnumField> {
        trans_list!(self, vars, trans_enum_field)
    }

    fn trans_enum_field(&mut self, var: &ast::Variant) -> EnumField {
        let loc = self.loc(&var.span);
        let attrs = self.trans_thin_attrs(&var.attrs);
        let name = ident_to_string(&var.ident);
        let body = self.trans_struct_body(&var.data);
        let expr = var.disr_expr.as_ref().map(|ac| self.trans_expr(&ac.value));
        self.set_loc(&loc);

        EnumField {
            loc,
            attrs,
            name,
            body,
            expr,
        }
    }

    fn trans_fn_sig(&mut self, decl: &ast::FnDecl) -> FnSig {
        FnSig {
            params: self.trans_params(&decl.inputs),
            ret: self.trans_return(&decl.output),
        }
    }

    fn trans_params(&mut self, params: &[ast::Param]) -> Vec<Param> {
        trans_list!(self, params, trans_param)
    }

    fn trans_param(&mut self, param: &ast::Param) -> Param {
        let pattern = self.trans_pattern(&param.pat);
        let has_patten = has_patten(param, &pattern);
        Param {
            loc: pattern.loc,
            pattern,
            ty: self.trans_type(&param.ty),
            has_patten,
        }
    }

    fn trans_return(&mut self, output: &ast::FnRetTy) -> Return {
        let (nl, ret) = match *output {
            ast::FnRetTy::Ty(ref ty) => (self.is_return_nl(ty.span.lo().0), Some(self.trans_type(ty))),
            ast::FnRetTy::Default(..) => (false, None),
        };

        Return {
            nl,
            ret,
        }
    }

    fn is_return_nl(&self, pos: Pos) -> bool {
        let snippet = self.span_to_snippet(&span(self.last_loc.end, pos));
        let right_arrow_pos = self.last_loc.end + snippet.find("->").unwrap() as Pos;
        self.is_nl(right_arrow_pos)
    }

    fn trans_fn(&mut self, ident: String, fn_kind: &ast::FnKind) -> Fn {
        Fn {
            is_default: is_default(&fn_kind.0),
            header: self.trans_fn_header(&fn_kind.1.header),
            name: ident,
            sig: self.trans_fn_sig(&fn_kind.1.decl),
            generics: self.trans_generics(&fn_kind.2),
            block: map_trans!(self, &fn_kind.3, trans_block),
        }
    }

    fn trans_fn_header(&mut self, header: &ast::FnHeader) -> FnHeader {
        FnHeader {
            is_unsafe: is_unsafe(&header.unsafety),
            is_async: is_async(&header.asyncness),
            is_const: is_const(&header.constness),
            ext: ext_to_string(&header.ext),
        }
    }

    fn trans_foreign_mod(&mut self, module: &ast::ForeignMod) -> ForeignMod {
        ForeignMod {
            abi: abi_to_string(&module.abi),
            items: self.trans_foreign_items(&module.items),
        }
    }

    fn trans_foreign_items(&mut self, items: &[ast::P<ast::ForeignItem>]) -> Vec<ForeignItem> {
        trans_list!(self, items, trans_foreign_item)
    }

    fn trans_foreign_item(&mut self, item: &ast::ForeignItem) -> ForeignItem {
        let loc = self.loc(&item.span);
        let attrs = self.trans_attrs(&item.attrs);
        let vis = self.trans_vis(&item.vis);
        let ident = ident_to_string(&item.ident);
        let item = match item.kind {
            ast::ForeignItemKind::TyAlias(ref type_alias) => {
                ForeignKind::TypeAlias(self.trans_type_alias(ident, type_alias))
            },
            ast::ForeignItemKind::Static(ref ty, ref mutability, ref expr) => {
                ForeignKind::Static(self.trans_static(mutability, ident, ty, expr))
            },
            ast::ForeignItemKind::Fn(ref fn_kind) => {
                ForeignKind::Fn(self.trans_fn(ident, fn_kind))
            },
            ast::ForeignItemKind::MacCall(ref mac_call) => ForeignKind::MacroCall(self.trans_macro_call(mac_call)),
        };
        self.set_loc(&loc);

        ForeignItem {
            loc,
            attrs,
            vis,
            item,
        }
    }

    fn trans_trait(&mut self, ident: String, trait_kind: &ast::TraitKind) -> Trait {
        Trait {
            is_auto: is_auto(&trait_kind.0),
            is_unsafe: is_unsafe(&trait_kind.1),
            name: ident,
            generics: self.trans_generics(&trait_kind.2),
            bounds: self.trans_type_param_bounds(&trait_kind.3),
            items: self.trans_trait_items(&trait_kind.4),
        }
    }

    fn trans_trait_items(&mut self, items: &[ast::P<ast::AssocItem>]) -> Vec<TraitItem> {
        trans_list!(self, items, trans_trait_item)
    }

    fn trans_trait_item(&mut self, item: &ast::AssocItem) -> TraitItem {
        let loc = self.loc(&item.span);
        let attrs = self.trans_attrs(&item.attrs);
        let ident = ident_to_string(&item.ident);
        let item = match item.kind {
            ast::AssocItemKind::Const(ref defaultness, ref ty, ref expr) => {
                TraitItemKind::Const(self.trans_const(defaultness, ident, ty, expr))
            },
            ast::AssocItemKind::TyAlias(ref type_alias) => {
                TraitItemKind::TypeAlias(self.trans_type_alias(ident, type_alias))
            },
            ast::AssocItemKind::Fn(ref fn_kind) => {
                TraitItemKind::Fn(self.trans_fn(ident, fn_kind))
            },
            ast::AssocItemKind::MacCall(ref mac_call) => TraitItemKind::MacroCall(self.trans_macro_call(mac_call)),
        };
        self.set_loc(&loc);

        TraitItem {
            loc,
            attrs,
            item,
        }
    }

    fn trans_impl(&mut self, impl_kind: &ast::ImplKind) -> Impl {
        Impl {
            is_unsafe: is_unsafe(&impl_kind.unsafety),
            is_default: is_default(&impl_kind.defaultness),
            is_neg: is_neg(&impl_kind.polarity),
            generics: self.trans_generics(&impl_kind.generics),
            trait_ref: map_trans!(self, &impl_kind.of_trait, trans_trait_ref),
            ty: self.trans_type(&impl_kind.self_ty),
            items: self.trans_impl_items(&impl_kind.items),
        }
    }

    fn trans_impl_items(&mut self, items: &[ast::P<ast::AssocItem>]) -> Vec<ImplItem> {
        trans_list!(self, items, trans_impl_item)
    }

    fn trans_impl_item(&mut self, item: &ast::AssocItem) -> ImplItem {
        let loc = self.loc(&item.span);
        let attrs = self.trans_attrs(&item.attrs);
        let vis = self.trans_vis(&item.vis);
        let ident = ident_to_string(&item.ident);
        let item = match item.kind {
            ast::AssocItemKind::Const(ref defaultness, ref ty, ref expr) => {
                ImplItemKind::Const(self.trans_const(defaultness, ident, ty, expr))
            },
            ast::AssocItemKind::TyAlias(ref type_alias) => {
                ImplItemKind::TypeAlias(self.trans_type_alias(ident, type_alias))
            },
            ast::AssocItemKind::Fn(ref fn_kind) => {
                ImplItemKind::Fn(self.trans_fn(ident, fn_kind))
            },
            ast::AssocItemKind::MacCall(ref mac_call) => ImplItemKind::MacroCall(self.trans_macro_call(mac_call)),
        };
        self.set_loc(&loc);

        ImplItem {
            loc,
            attrs,
            vis,
            item,
        }
    }

    fn trans_block(&mut self, block: &ast::Block) -> Block {
        let loc = self.loc(&block.span);
        let stmts = self.trans_stmts(&block.stmts);
        self.set_loc(&loc);

        Block {
            loc,
            is_unsafe: is_block_unsafe(&block.rules),
            stmts,
        }
    }

    fn trans_stmts(&mut self, stmts: &[ast::Stmt]) -> Vec<Stmt> {
        trans_list!(self, stmts, trans_stmt)
    }

    fn trans_stmt(&mut self, stmt: &ast::Stmt) -> Stmt {
        let loc = self.loc(&stmt.span);
        let stmt = match stmt.kind {
            ast::StmtKind::Item(ref item) => StmtKind::Item(Box::new(self.trans_item(item))),
            ast::StmtKind::Local(ref local) => StmtKind::Let(Box::new(self.trans_let(local))),
            ast::StmtKind::Semi(ref expr) => StmtKind::Expr(self.trans_expr(expr), true),
            ast::StmtKind::Expr(ref expr) => StmtKind::Expr(self.trans_expr(expr), false),
            ast::StmtKind::MacCall(ref mac_call) => StmtKind::Macro(self.trans_macro_stmt(mac_call)),
            ast::StmtKind::Empty => StmtKind::None,
        };
        self.set_loc(&loc);

        Stmt {
            loc,
            stmt,
        }
    }

    fn trans_let(&mut self, local: &ast::Local) -> Let {
        let loc = self.loc(&local.span);
        let attrs = self.trans_thin_attrs(&local.attrs);
        let pattern = self.trans_pattern(&local.pat);
        let ty = map_trans!(self, &local.ty, trans_type);
        let init = map_trans!(self, &local.init, trans_expr);
        self.set_loc(&loc);

        Let {
            loc,
            attrs,
            pattern,
            ty,
            init,
        }
    }

    fn trans_patterns(&mut self, patterns: &[ast::P<ast::Pat>]) -> Vec<Pattern> {
        trans_list!(self, patterns, trans_pattern)
    }

    fn trans_pattern(&mut self, pattern: &ast::Pat) -> Pattern {
        let loc = self.loc(&pattern.span);
        let pattern = match pattern.kind {
            ast::PatKind::Wild => PattenKind::Symbol("_"),
            ast::PatKind::Rest => PattenKind::Symbol(".."),
            ast::PatKind::Lit(ref expr) => PattenKind::Literal(self.trans_expr(expr)),
            ast::PatKind::Box(ref pattern) => PattenKind::Box(Box::new(self.trans_pattern(pattern))),
            ast::PatKind::Range(ref start, ref end, ref range_end) => {
                PattenKind::Range(Box::new(self.trans_range_patten(start, end, &range_end.node)))
            },
            ast::PatKind::Ref(ref pattern, ref mutability) => {
                PattenKind::Ref(Box::new(self.trans_ref_patten(mutability, pattern)))
            },
            ast::PatKind::Path(ref qself, ref path) => {
                PattenKind::Path(self.trans_path_type(qself, path))
            },
            ast::PatKind::Ident(ref binding, ref ident, ref pattern) => {
                PattenKind::Ident(Box::new(self.trans_ident_patten(binding, ident, pattern)))
            },
            ast::PatKind::Struct(ref qself, ref path, ref fields, etc) => {
                PattenKind::Struct(self.trans_struct_patten(qself, path, fields, etc))
            },
            ast::PatKind::TupleStruct(ref qself, ref path, ref patterns) => {
                PattenKind::Enum(self.trans_enum_patten(qself, path, patterns))
            },
            ast::PatKind::Or(ref patterns) => PattenKind::Or(self.trans_or_patten(patterns)),
            ast::PatKind::Paren(ref pattern) => {
                PattenKind::Tuple(self.trans_tuple_patten(&[pattern.clone()]))
            },
            ast::PatKind::Tuple(ref patterns) => {
                PattenKind::Tuple(self.trans_tuple_patten(patterns))
            },
            ast::PatKind::Slice(ref patterns) => {
                PattenKind::Slice(Box::new(self.trans_slice_patten(patterns)))
            },
            ast::PatKind::MacCall(ref mac_call) => PattenKind::MacroCall(self.trans_macro_call(mac_call)),
        };
        self.set_loc(&loc);

        Pattern {
            loc,
            pattern,
        }
    }

    fn trans_range_patten(&mut self, start: &Option<ast::P<ast::Expr>>, end: &Option<ast::P<ast::Expr>>,
                          range_end: &ast::RangeEnd) -> RangePatten {
        RangePatten {
            start: map_trans!(self, start, trans_expr),
            end: map_trans!(self, end, trans_expr),
            is_inclusive: is_patten_inclusive(range_end),
        }
    }

    fn trans_ref_patten(&mut self, mutability: &ast::Mutability, pattern: &ast::P<ast::Pat>) -> RefPatten {
        RefPatten {
            is_mut: is_mut(mutability),
            pattern: self.trans_pattern(pattern),
        }
    }

    fn trans_ident_patten(&mut self, binding: &ast::BindingMode, ident: &ast::Ident, pattern: &Option<ast::P<ast::Pat>>)
    -> IdentPatten {
        let (is_ref, is_mut) = is_ref_mut(binding);
        IdentPatten {
            is_ref,
            is_mut,
            name: ident_to_string(ident),
            pattern: map_trans!(self, pattern, trans_pattern),
        }
    }

    fn trans_struct_patten(&mut self, qself: &Option<ast::QSelf>, path: &ast::Path, fields: &[ast::PatField],
                           omit: bool) -> StructPatten {
        StructPatten {
            qself: map_trans!(self, qself, trans_qself),
            path: self.trans_path(path),
            fields: self.trans_struct_field_patterns(fields),
            omit,
        }
    }

    fn trans_struct_field_patterns(&mut self, fields: &[ast::PatField]) -> Vec<StructFieldPatten> {
        trans_list!(self, fields, trans_struct_field_patten)
    }

    fn trans_struct_field_patten(&mut self, field: &ast::PatField) -> StructFieldPatten {
        let loc = self.loc(&field.span);
        let name = ident_to_string(&field.ident);
        let pattern = self.trans_pattern(&field.pat);
        let shorthand = field.is_shorthand;
        self.set_loc(&loc);

        StructFieldPatten {
            loc,
            name,
            pattern,
            shorthand,
        }
    }

    fn trans_enum_patten(&mut self, qself: &Option<ast::QSelf>, path: &ast::Path, patterns: &[ast::P<ast::Pat>])
    -> EnumPatten {
        EnumPatten {
            qself: map_trans!(self, qself, trans_qself),
            path: self.trans_path(path),
            patterns: self.trans_patterns(patterns),
        }
    }

    fn trans_or_patten(&mut self, patterns: &[ast::P<ast::Pat>]) -> OrPatten {
        OrPatten {
            patterns: self.trans_patterns(patterns),
        }
    }

    fn trans_tuple_patten(&mut self, patterns: &[ast::P<ast::Pat>]) -> TuplePatten {
        TuplePatten {
            patterns: self.trans_patterns(patterns),
        }
    }

    fn trans_slice_patten(&mut self, patterns: &[ast::P<ast::Pat>]) -> SlicePatten {
        SlicePatten {
            patterns: self.trans_patterns(patterns),
        }
    }

    fn trans_exprs(&mut self, exprs: &[ast::P<ast::Expr>]) -> Vec<Expr> {
        trans_list!(self, exprs, trans_expr)
    }

    fn trans_expr(&mut self, expr: &ast::Expr) -> Expr {
        let loc = self.loc(&expr.span);
        let attrs = self.trans_thin_attrs(&expr.attrs);
        let expr = match expr.kind {
            ast::ExprKind::Underscore => ExprKind::Symbol("_"),
            ast::ExprKind::Lit(ref lit) => ExprKind::Literal(self.trans_literal_expr(lit)),
            ast::ExprKind::Path(ref qself, ref path) => ExprKind::Path(self.trans_path_type(qself, path)),
            ast::ExprKind::Box(ref expr) => ExprKind::Box(Box::new(self.trans_expr(expr))),
            ast::ExprKind::AddrOf(ref borrow, ref mutabilitye, ref expr) => {
                ExprKind::Ref(Box::new(self.trans_ref_expr(borrow, mutabilitye, expr)))
            },
            ast::ExprKind::Unary(ref op, ref expr) => ExprKind::UnaryOp(Box::new(self.trans_unary_expr(op, expr))),
            ast::ExprKind::Try(ref expr) => ExprKind::Try(Box::new(self.trans_expr(expr))),
            ast::ExprKind::Binary(ref op, ref left, ref right) => {
                ExprKind::ListOp(Box::new(self.trans_binary_expr(op, left, right)))
            },
            ast::ExprKind::Assign(ref left, ref right, _) => {
                ExprKind::ListOp(Box::new(self.trans_assign_expr(left, right)))
            },
            ast::ExprKind::AssignOp(ref op, ref left, ref right) => {
                ExprKind::ListOp(Box::new(self.trans_op_assign_expr(op, left, right)))
            },
            ast::ExprKind::Repeat(ref expr, ref len) => ExprKind::Repeat(Box::new(self.trans_repeat_expr(expr, len))),
            ast::ExprKind::Array(ref exprs) => ExprKind::Array(self.trans_exprs(exprs)),
            ast::ExprKind::Tup(ref exprs) => ExprKind::Tuple(self.trans_exprs(exprs)),
            ast::ExprKind::Paren(ref expr) => ExprKind::Tuple(vec![self.trans_expr(expr)]),
            ast::ExprKind::Index(ref obj, ref index) => ExprKind::Index(Box::new(self.trans_index_expr(obj, index))),
            ast::ExprKind::Struct(ref expr) => ExprKind::Struct(Box::new(self.trans_struct_expr(expr))),
            ast::ExprKind::Field(ref expr, ref ident) => ExprKind::Field(Box::new(self.trans_field_expr(expr, ident))),
            ast::ExprKind::Type(ref expr, ref ty) => ExprKind::Type(Box::new(self.trans_type_expr(expr, ty))),
            ast::ExprKind::Cast(ref expr, ref ty) => ExprKind::Cast(Box::new(self.trans_cast_expr(expr, ty))),
            ast::ExprKind::Range(ref start, ref end, ref limit) => {
                ExprKind::Range(Box::new(self.trans_range_expr(start, end, limit)))
            },
            ast::ExprKind::Block(ref block, ref label) => {
                ExprKind::Block(Box::new(self.trans_block_expr(block, label)))
            },
            ast::ExprKind::If(ref expr, ref block, ref br) => {
                ExprKind::If(Box::new(self.trans_if_expr(expr, block, br)))
            },
            ast::ExprKind::While(ref expr, ref block, ref label) => {
                ExprKind::While(Box::new(self.trans_while_expr(expr, block, label)))
            },
            ast::ExprKind::Let(ref pattern, ref expr) => {
                ExprKind::Let(Box::new(self.trans_let_expr(pattern, expr)))
            },
            ast::ExprKind::ForLoop(ref pattern, ref expr, ref block, ref label) => {
                ExprKind::For(Box::new(self.trans_for_expr(pattern, expr, block, label)))
            },
            ast::ExprKind::Loop(ref block, ref label) => {
                ExprKind::Loop(Box::new(self.trans_loop_expr(block, label)))
            },
            ast::ExprKind::Break(ref label, ref expr) => ExprKind::Break(Box::new(self.trans_break_expr(label, expr))),
            ast::ExprKind::Continue(ref label) => ExprKind::Continue(Box::new(self.trans_continue_expr(label))),
            ast::ExprKind::Match(ref expr, ref arms) => {
                ExprKind::Match(Box::new(self.trans_match_expr(expr, arms)))
            },
            ast::ExprKind::Call(ref fn_name, ref params) => {
                ExprKind::FnCall(Box::new(self.trans_fn_call_expr(fn_name, params)))
            },
            ast::ExprKind::MethodCall(ref path, ref args, _) => {
                ExprKind::MethodCall(Box::new(self.trans_method_call_expr(path, args)))
            },
            ast::ExprKind::Closure(ref capture, ref asyncness, ref movability, ref sig, ref expr, _) => {
                ExprKind::Closure(Box::new(self.trans_closure_expr(capture, asyncness, movability, sig, expr)))
            },
            ast::ExprKind::Ret(ref expr) => ExprKind::Return(Box::new(self.trans_return_expr(expr))),
            ast::ExprKind::MacCall(ref mac_call) => ExprKind::MacroCall(self.trans_macro_call(mac_call)),
            ast::ExprKind::Async(ref capture, _, ref block) => ExprKind::Async(self.trans_async_expr(capture, block)),
            ast::ExprKind::Await(ref expr) => ExprKind::Await(Box::new(self.trans_expr(expr))),
            ast::ExprKind::TryBlock(ref block) => ExprKind::TryBlock(self.trans_block(block)),
            ast::ExprKind::ConstBlock(ref expr) => ExprKind::ConstBlock(Box::new(self.trans_expr(&expr.value))),
            ast::ExprKind::Yield(ref expr) => ExprKind::Yield(Box::new(self.trans_yield_expr(expr))),
            ast::ExprKind::Err => ExprKind::Err(LocStr::new(self.span_to_snippet(&expr.span))),
            ast::ExprKind::InlineAsm(..) | ast::ExprKind::LlvmInlineAsm(..) => unreachable!("{:#?}", expr.kind),
        };
        self.set_loc(&loc);

        Expr {
            loc,
            attrs,
            expr,
        }
    }

    fn trans_literal_expr(&mut self, lit: &ast::Lit) -> LocStr {
        LocStr {
            loc: self.leaf_loc(&lit.span),
            s: self.literal_to_string(lit),
        }
    }

    fn trans_ref_expr(&mut self, borrow: &ast::BorrowKind, mutabilitye: &ast::Mutability, expr: &ast::Expr) -> RefExpr {
        RefExpr {
            is_raw: is_raw(borrow),
            is_mut: is_mut(mutabilitye),
            expr: self.trans_expr(expr),
        }
    }

    fn trans_unary_expr(&mut self, op: &ast::UnOp, expr: &ast::Expr) -> UnaryOpExpr {
        UnaryOpExpr {
            op: uop_to_string(op),
            expr: self.trans_expr(expr),
        }
    }

    fn trans_binary_expr(&mut self, op: &ast::BinOp, left: &ast::Expr, right: &ast::Expr) -> ListOpExpr {
        ListOpExpr {
            op: self.trans_bop(op),
            exprs: vec![self.trans_expr(left), self.trans_expr(right)],
        }
    }

    fn trans_bop(&mut self, op: &ast::BinOp) -> LocStr {
        LocStr {
            loc: self.leaf_loc(&op.span),
            s: op.node.to_string().to_string(),
        }
    }

    fn trans_assign_expr(&mut self, left: &ast::Expr, right: &ast::Expr) -> ListOpExpr {
        ListOpExpr {
            op: LocStr::new("="),
            exprs: vec![self.trans_expr(left), self.trans_expr(right)],
        }
    }

    fn trans_op_assign_expr(&mut self, op: &ast::BinOp, left: &ast::Expr, right: &ast::Expr) -> ListOpExpr {
        ListOpExpr {
            op: self.trans_bop_assign(op),
            exprs: vec![self.trans_expr(left), self.trans_expr(right)],
        }
    }

    fn trans_bop_assign(&mut self, op: &ast::BinOp) -> LocStr {
        LocStr {
            loc: self.leaf_loc(&op.span),
            s: format!("{}=", op.node.to_string()),
        }
    }

    fn trans_repeat_expr(&mut self, expr: &ast::Expr, len: &ast::AnonConst) -> RepeatExpr {
        RepeatExpr {
            value: self.trans_expr(expr),
            len: self.trans_expr(&len.value),
        }
    }

    fn trans_index_expr(&mut self, obj: &ast::Expr, index: &ast::Expr) -> IndexExpr {
        IndexExpr {
            obj: self.trans_expr(obj),
            index: self.trans_expr(index),
        }
    }

    fn trans_struct_expr(&mut self, expr: &ast::StructExpr) -> StructExpr {
        let (has_rest, base) = match expr.rest {
            ast::StructRest::Base(ref expr) => (true, Some(self.trans_expr(expr))),
            ast::StructRest::Rest(_) => (true, None),
            ast::StructRest::None => (false, None),
        };

        StructExpr {
            qself: map_trans!(self, expr.qself, trans_qself),
            path: self.trans_path(&expr.path),
            fields: self.trans_struct_field_exprs(&expr.fields),
            has_rest,
            base,
        }
    }

    fn trans_struct_field_exprs(&mut self, fields: &[ast::ExprField]) -> Vec<StructFieldExpr> {
        trans_list!(self, fields, trans_struct_field_expr)
    }

    fn trans_struct_field_expr(&mut self, field: &ast::ExprField) -> StructFieldExpr {
        let loc = self.loc(&field.span);
        let name = ident_to_string(&field.ident);
        let value = self.trans_expr(&field.expr);
        self.set_loc(&loc);

        StructFieldExpr {
            loc,
            name,
            value,
        }
    }

    fn trans_field_expr(&mut self, expr: &ast::Expr, ident: &ast::Ident) -> FieldExpr {
        FieldExpr {
            expr: self.trans_expr(expr),
            field: ident_to_string(ident),
        }
    }

    fn trans_type_expr(&mut self, expr: &ast::Expr, ty: &ast::Ty) -> TypeExpr {
        TypeExpr {
            expr: self.trans_expr(expr),
            ty: self.trans_type(ty),
        }
    }

    fn trans_cast_expr(&mut self, expr: &ast::Expr, ty: &ast::Ty) -> CastExpr {
        CastExpr {
            expr: self.trans_expr(expr),
            ty: self.trans_type(ty),
        }
    }

    fn trans_range_expr(&mut self, start: &Option<ast::P<ast::Expr>>, end: &Option<ast::P<ast::Expr>>,
                        limit: &ast::RangeLimits) -> RangeExpr {
        RangeExpr {
            start: map_trans!(self, start, trans_expr),
            end: map_trans!(self, end, trans_expr),
            is_inclusive: is_inclusive(limit),
        }
    }

    fn trans_block_expr(&mut self, block: &ast::Block, label: &Option<ast::Label>) -> BlockExpr {
        BlockExpr {
            label: map_trans_label(label),
            block: self.trans_block(block),
        }
    }

    fn trans_if_expr(&mut self, expr: &ast::Expr, block: &ast::Block, br: &Option<ast::P<ast::Expr>>) -> IfExpr {
        IfExpr {
            expr: self.trans_expr(expr),
            block: self.trans_block(block),
            br: map_trans!(self, br, trans_expr),
        }
    }

    fn trans_while_expr(&mut self, expr: &ast::Expr, block: &ast::Block, label: &Option<ast::Label>) -> WhileExpr {
        WhileExpr {
            label: map_trans_label(label),
            expr: self.trans_expr(expr),
            block: self.trans_block(block),
        }
    }

    fn trans_let_expr(&mut self, pattern: &ast::Pat, expr: &ast::Expr) -> LetExpr {
        LetExpr {
            pattern: self.trans_pattern(pattern),
            expr: self.trans_expr(expr),
        }
    }

    fn trans_for_expr(&mut self, pattern: &ast::P<ast::Pat>, expr: &ast::Expr, block: &ast::Block,
                      label: &Option<ast::Label>) -> ForExpr {
        ForExpr {
            label: map_trans_label(label),
            pattern: self.trans_pattern(pattern),
            expr: self.trans_expr(expr),
            block: self.trans_block(block),
        }
    }

    fn trans_loop_expr(&mut self, block: &ast::Block, label: &Option<ast::Label>) -> LoopExpr {
        LoopExpr {
            label: map_trans_label(label),
            block: self.trans_block(block),
        }
    }

    fn trans_break_expr(&mut self, label: &Option<ast::Label>, expr: &Option<ast::P<ast::Expr>>) -> BreakExpr {
        BreakExpr {
            label: map_trans_label(label),
            expr: map_trans!(self, expr, trans_expr),
        }
    }

    fn trans_continue_expr(&mut self, label: &Option<ast::Label>) -> ContinueExpr {
        ContinueExpr {
            label: map_trans_label(label),
        }
    }

    fn trans_match_expr(&mut self, expr: &ast::Expr, arms: &[ast::Arm]) -> MatchExpr {
        MatchExpr {
            expr: self.trans_expr(expr),
            arms: self.trans_arms(arms),
        }
    }

    fn trans_arms(&mut self, arms: &[ast::Arm]) -> Vec<Arm> {
        trans_list!(self, arms, trans_arm)
    }

    fn trans_arm(&mut self, arm: &ast::Arm) -> Arm {
        let attrs = self.trans_thin_attrs(&arm.attrs);
        let pattern = self.trans_pattern(&arm.pat);
        let guard = map_trans!(self, &arm.guard, trans_expr);
        let body = self.trans_expr(&arm.body);

        Arm {
            loc: Loc {
                start: pattern.loc.start,
                end: body.loc.end,
                nl: false,
            },
            attrs,
            pattern,
            guard,
            body,
        }
    }

    fn trans_fn_call_expr(&mut self, fn_name: &ast::Expr, params: &[ast::P<ast::Expr>]) -> FnCallExpr {
        FnCallExpr {
            name: self.trans_expr(fn_name),
            params: self.trans_exprs(params),
        }
    }

    fn trans_method_call_expr(&mut self, path: &ast::PathSegment, args: &[ast::P<ast::Expr>]) -> MethodCallExpr {
        MethodCallExpr {
            path: self.trans_path_segment(path),
            args: self.trans_exprs(args),
        }
    }

    fn trans_closure_expr(&mut self, capture: &ast::CaptureBy, asyncness: &ast::Async, movability: &ast::Movability,
                          sig: &ast::FnDecl, expr: &ast::Expr) -> ClosureExpr {
        ClosureExpr {
            is_static: is_static(movability),
            is_async: is_async(asyncness),
            is_move: is_move(capture),
            sig: self.trans_fn_sig(sig),
            expr: self.trans_expr(expr),
        }
    }

    fn trans_return_expr(&mut self, expr: &Option<ast::P<ast::Expr>>) -> ReturnExpr {
        ReturnExpr {
            ret: map_trans!(self, expr, trans_expr),
        }
    }

    fn trans_async_expr(&mut self, capture: &ast::CaptureBy, block: &ast::Block) -> AsyncExpr {
        AsyncExpr {
            is_move: is_move(capture),
            block: self.trans_block(block),
        }
    }

    fn trans_yield_expr(&mut self, expr: &Option<ast::P<ast::Expr>>) -> YieldExpr {
        YieldExpr {
            expr: map_trans!(self, expr, trans_expr),
        }
    }

    fn trans_macro_def(&mut self, ident: String, macro_def: &ast::MacroDef) -> MacroDef {
        let s = match macro_def.body.span() {
            Some(ref span) => self.span_to_snippet(span),
            None => "".to_string(),
        };

        MacroDef {
            name: ident,
            s,
        }
    }

    fn trans_macro_stmt(&mut self, mac_call: &ast::MacCallStmt) -> MacroStmt {
        let loc = self.loc(&mac_call.mac.span());
        let attrs = self.trans_thin_attrs(&mac_call.attrs);
        let mac = self.trans_macro_call(&mac_call.mac);
        let is_semi = is_macro_semi(&mac_call.style);
        self.set_loc(&loc);

        MacroStmt {
            loc,
            attrs,
            mac,
            is_semi,
        }
    }

    fn trans_macro_call(&mut self, macro_call: &ast::MacCall) -> MacroCall {
        let style = macro_style(&macro_call.args.delim());
        if matches!(style, MacroStyle::Brace) {
            return self.trans_macro_raw(macro_call);
        }

        match self.trans_macro_exprs(&macro_call.args.inner_tokens()) {
            Some((exprs, seps)) => {
                let exprs = self.trans_exprs(&exprs);
                if exprs.iter().any(|expr| matches!(expr.expr, ExprKind::Err(..))) {
                    return self.trans_macro_raw(macro_call);
                }

                let name = path_to_string(&macro_call.path);
                MacroCall::Expr(MacroExpr {
                    name,
                    style,
                    exprs,
                    seps,
                })
            },
            None => self.trans_macro_raw(macro_call),
        }
    }

    fn trans_macro_exprs(&self, ts: &ast::TokenStream) -> Option<(Vec<ast::P<ast::Expr>>, Vec<MacroSep>)> {
        let mut exprs = Vec::new();
        let mut seps = Vec::new();

        if ts.is_empty() {
            return Some((exprs, seps));
        }

        let mut parser = parse::stream_to_parser(&self.sess, ts.clone(), None);
        loop {
            exprs.push(match parser.parse_expr() {
                Ok(expr) => expr,
                Err(mut e) => {
                    e.emit();
                    return None;
                },
            });

            match parser.token.kind {
                ast::TokenKind::Eof => break,
                ast::TokenKind::Literal(..) => (),
                _ => {
                    seps.push(self.token_to_macro_sep(&parser.token));

                    parser.bump();
                    if parser.token.kind == ast::TokenKind::Eof {
                        break;
                    }
                },
            }
        }
        Some((exprs, seps))
    }

    fn token_to_macro_sep(&self, token: &ast::Token) -> MacroSep {
        let (is_sep, s) = match token.kind {
            ast::TokenKind::At => (false, " @ ".to_string()),
            ast::TokenKind::Colon => (true, ":".to_string()),
            ast::TokenKind::Comma => (true, ",".to_string()),
            ast::TokenKind::CloseDelim(ref delim) => (false, delim_to_string(delim, false)),
            ast::TokenKind::DotDotDot => (false, "...".to_string()),
            ast::TokenKind::DotDotEq => (false, "..=".to_string()),
            ast::TokenKind::FatArrow => (true, " =>".to_string()),
            ast::TokenKind::Ident(ref sym, _) => (false, format!(" {} ", sym)),
            ast::TokenKind::OpenDelim(ref delim) => (false, delim_to_string(delim, true)),
            ast::TokenKind::Pound => (false, "#".to_string()),
            ast::TokenKind::RArrow => (false, " -> ".to_string()),
            ast::TokenKind::Semi => (true, ";".to_string()),
            _ => unreachable!("{:?}", token),
        };

        MacroSep {
            loc: self.span_to_loc(&token.span),
            is_sep,
            s,
        }
    }

    fn trans_macro_raw(&self, macro_call: &ast::MacCall) -> MacroCall {
        let s = self.span_to_snippet(&macro_call.span());
        MacroCall::Raw(LocStr::new(s))
    }

    fn span_to_loc(&self, span: &ast::Span) -> Loc {
        Loc {
            start: span.lo().0,
            end: span.hi().0,
            nl: self.is_nl(span.lo().0),
        }
    }

    fn loc(&mut self, span: &ast::Span) -> Loc {
        self.trans_comments(span.lo().0);
        self.span_to_loc(span)
    }

    fn set_loc(&mut self, loc: &Loc) {
        self.trans_comments(loc.end);
        self.last_loc = *loc;
    }

    fn leaf_loc(&mut self, span: &ast::Span) -> Loc {
        let loc = self.loc(span);
        self.set_loc(&loc);
        loc
    }

    fn is_nl(&self, pos: Pos) -> bool {
        let nl = self.src[..pos as usize].rfind('\n');
        if nl.is_none() {
            return false;
        }

        let mut prev = ' ';
        let start = nl.unwrap() + 1;
        for ch in self.src[start..pos as usize].chars() {
            if !ch.is_whitespace() && ch != '.' || !prev.is_whitespace() {
                return false;
            }
            prev = ch;
        }
        true
    }

    fn literal_to_string(&self, lit: &ast::Lit) -> String {
        self.span_to_snippet(&lit.span)
    }

    fn span_to_snippet(&self, span: &ast::Span) -> String {
        self.sess.source_map().span_to_snippet(*span).unwrap()
    }

    fn crate_file_end(&self) -> Pos {
        self.sess.source_map().files().last().unwrap().end_pos.0
    }
}
