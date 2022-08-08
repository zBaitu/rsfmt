use crate::{
    ir::*,
    need_nl_indent,
    need_wrap,
    typesetter::*
};

use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display}
};

const OK: fmt::Result = Ok(());

macro_rules! display_lines {
    ($f:expr, $lines:expr, $sep:expr) => ({
        for line in $lines {
            writeln!($f, "{}{}", line, $sep)?;
        }
        OK
    });
}

macro_rules! display_items {
    ($f:expr, $items: expr) => ({
        display_lines!($f, $items, "")
    });
}

macro_rules! display_lists {
    ($f:expr, $open:expr, $sep:expr, $close:expr, $($lists:expr),+) => ({
        write!($f, $open)?;

        let mut first = true;
        $(for e in $lists {
            if !first {
                write!($f, "{}", $sep)?;
            }
            Display::fmt(e, $f)?;
            first = false;
        })+

        write!($f, $close)
    });

    ($f:expr, $sep:expr, $($lists:expr),+) => ({
       display_lists!($f, "", $sep, "", $($lists)+)
    });
}

macro_rules! display_fields_block {
    ($f:expr, $fields: expr) => ({
        writeln!($f, " {{")?;
        display_fields!($f, $fields)?;
        write!($f, "}}")
    });
}

macro_rules! display_fields {
    ($f:expr, $fields:expr) => ({
        display_lines!($f, $fields, ",")
    });
}

macro_rules! display_decls_block {
    ($f:expr, $items: expr) => ({
        writeln!($f, " {{")?;
        display_decls!($f, $items)?;
        write!($f, "}}")
    });
}

macro_rules! display_decls {
    ($f:expr, $items:expr) => ({
        display_lines!($f, $items, ";")
    });
}

macro_rules! display_block {
    ($f:expr, $items: expr) => ({
        writeln!($f, " {{")?;
        display_items!($f, $items)?;
        write!($f, "}}")
    });
}

macro_rules! select_str {
    ($fn_name:ident, $flag:ident, $true_value:expr, $false_value:expr) => (
        fn $fn_name($flag: bool) -> &'static str {
            const TRUE_HEAD: &str = $true_value;
            const FALSE_HEAD: &str = $false_value;

            if $flag {
                TRUE_HEAD
            } else {
                FALSE_HEAD
            }
        }
    );
}

select_str!(ptr_head, is_mut, "*mut ", "*const ");
select_str!(static_head, is_mut, "static mut ", "static ");
select_str!(range, is_inclusive, "..=", "..");

impl Display for LocStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.s, f)
    }
}

impl Display for Crate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        display_items!(f, &self.items)
    }
}

impl Display for Attr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.attr {
            AttrKind::Doc(ref doc) => {
                write!(f, "/")?;
                display_doc_symbol(f, doc.is_block)?;
                if self.is_inner {
                    write!(f, "!")?;
                } else {
                    display_doc_symbol(f, doc.is_block)?;
                }
                Display::fmt(&doc.doc, f)?;
                if doc.is_block {
                    write!(f, "*/")?;
                }
                OK
            },
            AttrKind::Attr(ref meta) => {
                write!(f, "#")?;
                if self.is_inner {
                    write!(f, "!")?;
                }
                write!(f, "[")?;
                Display::fmt(&meta.name, f)?;
                if let Some(ref metas) = meta.metas {
                    display_lists!(f, "(", ", ", ")", &**metas)?;
                }
                write!(f, "]")
            },
            AttrKind::Raw(ref s) => {
                Display::fmt(s, f)
            },
        }
    }
}

impl Display for MetaAttr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.name, f)?;
        if let Some(ref metas) = self.metas {
            display_lists!(f, "(", ", ", ")", &**metas)?;
        }
        OK
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        display_vis(f, &self.vis)?;
        match self.item {
            ItemKind::ExternCrate(ref item) => Display::fmt(item, f),
            ItemKind::Use(ref item) => Display::fmt(item, f),
            ItemKind::ModDecl(ref item) => Display::fmt(item, f),
            ItemKind::Mod(ref item) => Display::fmt(item, f),
            ItemKind::TypeAlias(ref item) => Display::fmt(item, f),
            ItemKind::TraitAlias(ref item) => Display::fmt(item, f),
            ItemKind::Const(ref item) => Display::fmt(item, f),
            ItemKind::Static(ref item) => Display::fmt(item, f),
            ItemKind::Struct(ref item) => Display::fmt(item, f),
            ItemKind::Union(ref item) => Display::fmt(item, f),
            ItemKind::Enum(ref item) => Display::fmt(item, f),
            ItemKind::Fn(ref item) => Display::fmt(item, f),
            ItemKind::ForeignMod(ref item) => Display::fmt(item, f),
            ItemKind::Trait(ref item) => Display::fmt(item, f),
            ItemKind::Impl(ref item) => Display::fmt(item, f),
            ItemKind::MacroDef(ref item) => Display::fmt(item, f),
            ItemKind::MacroCall(ref item) => Display::fmt(item, f),
        }
    }
}

impl Display for ExternCrate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "extern crate {}", self.name)
    }
}

impl Display for Use {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "use {}", self.path)?;
        display_use_trees(f, &self.trees)
    }
}

impl Display for UseTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.path)?;
        display_use_trees(f, &self.trees)
    }
}

impl Display for ModDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}mod {}", unsafe_head(self.is_unsafe), self.name)
    }
}

impl Display for Mod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}mod {} {{", unsafe_head(self.is_unsafe), self.name)?;
        for item in &self.items {
            writeln!(f, "{}", item)?;
        }
        write!(f, "}}")
    }
}

impl Display for TypeAlias {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type {}{}", self.name, self.generics)?;
        try_display_type_param_bounds(f, &self.bounds)?;
        if let Some(ref ty) = self.ty {
            write!(f, " = {}", ty)?;
        }
        OK
    }
}

impl Display for TraitAlias {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "trait {}{} = {}", self.name, self.generics, self.bounds)
    }
}

impl Display for Generics {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.is_empty() {
            display_lists!(f, "<", ", ", ">", &self.lifetime_defs, &self.type_params, &self.const_params)?;
        }
        if !self.wh.is_empty() {
            write!(f, " where {}", self.wh)?;
        }
        OK
    }
}

impl Display for LifetimeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.lifetime, f)?;
        if !self.bounds.is_empty() {
            write!(f, ": ")?;
            display_lists!(f, " + ", &self.bounds)?
        }
        OK
    }
}

impl Display for TypeParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        try_display_type_param_bounds(f, &self.bounds)?;
        if let Some(ref ty) = self.default {
            write!(f, " = {}", ty)?;
        }
        OK
    }
}

impl Display for TypeParamBound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeParamBound::Lifetime(ref bound) => Display::fmt(bound, f),
            TypeParamBound::PolyTraitRef(ref bound) => Display::fmt(bound, f),
        }
    }
}

impl Display for TypeParamBounds {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_lists!(f, " + ", &self.0)
    }
}

impl Display for PolyTraitRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_for_liftime_defs(f, &self.lifetime_defs)?;
        Display::fmt(&self.trait_ref, f)
    }
}

impl Display for ConstParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "const {}: {}", self.name, self.ty)?;
        if let Some(ref expr) = self.default {
            write!(f, " = {}", expr)?;
        }
        OK
    }
}

impl Display for Where {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_lists!(f, ", ", &self.clauses)
    }
}

impl Display for WhereClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clause {
            WhereKind::LifetimeDef(ref wh) => Display::fmt(wh, f),
            WhereKind::Bound(ref wh) => Display::fmt(wh, f),
        }
    }
}

impl Display for WhereBound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_for_liftime_defs(f, &self.lifetime_defs)?;
        write!(f, "{}: ", &self.ty)?;
        display_type_param_bounds(f, &self.bounds)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_path_segments(f, &self.segments)
    }
}

impl Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.name, self.param)
    }
}

impl Display for PathParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PathParam::Angle(ref param) => Display::fmt(param, f),
            PathParam::Paren(ref param) => Display::fmt(param, f),
        }
    }
}

impl Display for AngleParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.is_empty() {
            display_lists!(f, "<", ", ", ">", &self.lifetimes, &self.types, &self.consts, &self.bindings)?;
        }
        OK
    }
}

impl Display for TypeBinding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.binding {
            TypeBindingKind::Eq(ref ty) => write!(f, "{}={}", self.name, ty),
            TypeBindingKind::Bound(ref bounds) => {
                write!(f, "{}: ", self.name)?;
                display_lists!(f, "+", &bounds.0)
            },
        }
    }
}

impl Display for ParenParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_paren_param_inputs(f, &self.inputs)?;
        if let Some(ref output) = self.output {
            write!(f, " -> {}", output)?;
        }
        OK
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ty {
            TypeKind::Symbol(ref ty) => Display::fmt(ty, f),
            TypeKind::Path(ref ty) => Display::fmt(ty, f),
            TypeKind::Ptr(ref ty) => Display::fmt(ty, f),
            TypeKind::Ref(ref ty) => Display::fmt(ty, f),
            TypeKind::Tuple(ref ty) => Display::fmt(ty, f),
            TypeKind::Slice(ref ty) => Display::fmt(ty, f),
            TypeKind::Array(ref ty) => Display::fmt(ty, f),
            TypeKind::Struct(ref ty) => Display::fmt(ty, f),
            TypeKind::Union(ref ty) => Display::fmt(ty, f),
            TypeKind::Trait(ref ty) => Display::fmt(ty, f),
            TypeKind::BareFn(ref ty) => Display::fmt(ty, f),
            TypeKind::MacroCall(ref ty) => Display::fmt(ty, f),
            TypeKind::Err(ref ty) => Display::fmt(ty, f),
        }
    }
}

impl Display for PathType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_qself_path(f, &self.qself, &self.path)
    }
}

impl Display for PtrType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", ptr_head(self.is_mut), self.ty)
    }
}

impl Display for RefType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", ref_head(&self.lifetime, false, self.is_mut), self.ty)
    }
}

impl Display for TupleType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_lists!(f, "(", ", ", ")", &self.types)
    }
}

impl Display for SliceType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.ty)
    }
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}; {}]", self.ty, self.expr)
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "struct")?;
        display_fields_block!(f, &self.fields)
    }
}

impl Display for UnionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "union")?;
        display_fields_block!(f, &self.fields)
    }
}

impl Display for TraitType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", trait_type_head(self.is_dyn, self.is_impl))?;
        display_type_param_bounds(f, &self.bounds)
    }
}

impl Display for BareFnType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_for_liftime_defs(f, &self.lifetime_defs)?;
        write!(f, "{}{}", fn_head(&self.header), self.sig)
    }
}

impl Display for TupleField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", vis_head(&self.vis), self.ty)
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}const {}: {}", default_head(self.is_default), self.name, self.ty)?;
        if let Some(ref expr) = self.expr {
            write!(f, " = {}", expr)?;
        }
        OK
    }
}

impl Display for Static {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}: {}", static_head(self.is_mut), self.name, self.ty)?;
        if let Some(ref expr) = self.expr {
            write!(f, " = {}", expr)?;
        }
        OK
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "struct {}{}{}", self.name, self.generics, self.body)
    }
}

impl Display for StructBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StructBody::Struct(ref fields) => {
                display_fields_block!(f, fields)
            },
            StructBody::Tuple(ref fields) => {
                display_lists!(f, "(", ", ", ")", fields)
            },
            StructBody::Unit => OK,
        }
    }
}

impl Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        display_vis(f, &self.vis)?;
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl Display for Union {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "union {}{}", self.name, self.generics)?;
        display_fields_block!(f, &self.fields)
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "enum {}{}{}", self.name, self.generics, self.body)
    }
}

impl Display for EnumBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_fields_block!(f, &self.fields)
    }
}

impl Display for EnumField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        write!(f, "{}{}", self.name, self.body)?;
        if let Some(ref expr) = self.expr {
            write!(f, " = {}", expr)?;
        }
        OK
    }
}

impl Display for FnSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_params(f, &self.params)?;
        Display::fmt(&self.ret, f)
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.has_patten {
            write!(f, "{}", self.pattern)?;
            match self.ty.ty {
                TypeKind::Symbol(s) if s == "_" => OK,
                _ => write!(f, ": {}", self.ty),
            }
        } else if let PattenKind::Ident(ref pattern) = self.pattern.pattern {
            write!(f, "{}{}", ident_patten_head(pattern.is_ref, pattern.is_mut), self.ty)
        } else {
            Display::fmt(&self.ty, f)
        }
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ret {
            Some(ref ty) => write!(f, " -> {}", ty),
            None => OK,
        }
    }
}

impl Display for Fn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", fn_head(&self.header), self.name)?;
        display_generics(f, &self.generics)?;
        Display::fmt(&self.sig, f)?;
        display_where(f, &self.generics)?;
        if let Some(ref block) = self.block {
            Display::fmt(block, f)?;
        }
        OK
    }
}

impl Display for ForeignMod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", foreign_head(&self.abi))?;
        display_decls_block!(f, &self.items)
    }
}

impl Display for ForeignItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        display_vis(f, &self.vis)?;
        Display::fmt(&self.item, f)
    }
}

impl Display for ForeignKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForeignKind::TypeAlias(ref item) => Display::fmt(item, f),
            ForeignKind::Static(ref item) => Display::fmt(item, f),
            ForeignKind::Fn(ref item) => Display::fmt(item, f),
            ForeignKind::MacroCall(ref item) => Display::fmt(item, f),
        }
    }
}

impl Display for Trait {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", trait_head(self.is_auto, self.is_unsafe), self.name)?;
        display_generics(f, &self.generics)?;
        try_display_type_param_bounds(f, &self.bounds)?;
        display_where(f, &self.generics)?;
        display_decls_block!(f, &self.items)
    }
}

impl Display for TraitItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        match self.item {
            TraitItemKind::Const(ref item) => Display::fmt(item, f),
            TraitItemKind::TypeAlias(ref item) => Display::fmt(item, f),
            TraitItemKind::Fn(ref item) => Display::fmt(item, f),
            TraitItemKind::MacroCall(ref item) => Display::fmt(item, f),
        }
    }
}

impl Display for Impl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", impl_head(self.is_unsafe, self.is_default))?;
        display_generics(f, &self.generics)?;
        write!(f, " ")?;

        if self.is_neg {
            write!(f, "!")?;
        }
        if let Some(ref trait_ref) = self.trait_ref {
            Display::fmt(trait_ref, f)?;
            write!(f, " for {}", self.ty)?;
        } else {
            Display::fmt(&self.ty, f)?;
        }
        display_where(f, &self.generics)?;

        display_block!(f, &self.items)
    }
}

impl Display for ImplItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        display_vis(f, &self.vis)?;

        let mut is_fn = false;
        match self.item {
            ImplItemKind::Const(ref item) => Display::fmt(item, f)?,
            ImplItemKind::TypeAlias(ref item) => Display::fmt(item, f)?,
            ImplItemKind::Fn(ref item) => {
                is_fn = true;
                Display::fmt(item, f)?
            },
            ImplItemKind::MacroCall(ref item) => Display::fmt(item, f)?,
        }
        if !is_fn {
            write!(f, ";")?;
        }
        OK
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.pattern {
            PattenKind::Symbol(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Literal(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Box(ref pattern) => write!(f, "box {}", pattern),
            PattenKind::Range(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Ref(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Path(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Ident(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Struct(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Enum(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Or(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Tuple(ref pattern) => Display::fmt(pattern, f),
            PattenKind::Slice(ref pattern) => Display::fmt(pattern, f),
            PattenKind::MacroCall(ref pattern) => Display::fmt(pattern, f),
        }
    }
}

impl Display for RangePatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref start) = self.start {
            Display::fmt(start, f)?;
        }
        write!(f, "{}", range(self.is_inclusive))?;
        if let Some(ref end) = self.end {
            Display::fmt(end, f)?;
        }
        OK
    }
}

impl Display for RefPatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", ref_head(&None, false, self.is_mut), self.pattern)
    }
}

impl Display for IdentPatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", ident_patten_head(self.is_ref, self.is_mut), self.name)?;
        if let Some(ref pattern) = self.pattern {
            write!(f, " @ {}", pattern)?;
        }
        OK
    }
}

impl Display for StructPatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_qself_path(f, &self.qself, &self.path)?;

        if self.fields.is_empty() {
            if self.omit {
                return write!(f, " {{..}}");
            } else {
                return write!(f, " {{}}");
            }
        }

        write!(f, " {{ ")?;
        display_lists!(f, ", ", &self.fields)?;
        if self.omit {
            write!(f, ", ..")?;
        }
        write!(f, " }}")
    }
}

impl Display for StructFieldPatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.shorthand {
            Display::fmt(&self.pattern, f)
        } else {
            write!(f, "{}: {}", self.name, self.pattern)
        }
    }
}

impl Display for EnumPatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_qself_path(f, &self.qself, &self.path)?;
        display_lists!(f, "(", ", ", ")", &self.patterns)
    }
}

impl Display for OrPatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_lists!(f, " | ", &self.patterns)
    }
}

impl Display for TuplePatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_lists!(f, "(", ", ", ")", &self.patterns)
    }
}

impl Display for SlicePatten {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_lists!(f, "[", ", ", "]", &self.patterns)
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", unsafe_head(self.is_unsafe))?;
        if self.is_one_line() {
            write!(f, " {{ {} }}", self.stmts[0])
        } else {
            display_block!(f, &self.stmts)
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.stmt {
            StmtKind::Item(ref item) => Display::fmt(item, f),
            StmtKind::Let(ref item) => Display::fmt(item, f),
            StmtKind::Expr(ref item, is_semi) => display_expr(f, item, is_semi),
            StmtKind::Macro(ref item) => Display::fmt(item, f),
            StmtKind::None => OK,
        }
    }
}

impl Display for Let {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        write!(f, "let {}", self.pattern)?;
        if let Some(ref ty) = self.ty {
            write!(f, ":{}", ty)?;
        }
        if let Some(ref expr) = self.init {
            write!(f, " = {}", expr)?;
        }
        OK
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.expr {
            ExprKind::Symbol(ref sym) => Display::fmt(sym, f),
            ExprKind::Literal(ref expr) => Display::fmt(expr, f),
            ExprKind::Path(ref expr) => Display::fmt(expr, f),
            ExprKind::Box(ref expr) => write!(f, "box {}", expr),
            ExprKind::Ref(ref expr) => Display::fmt(expr, f),
            ExprKind::UnaryOp(ref expr) => Display::fmt(expr, f),
            ExprKind::Try(ref expr) => write!(f, "{}?", expr),
            ExprKind::ListOp(ref expr) => Display::fmt(expr, f),
            ExprKind::Repeat(ref expr) => Display::fmt(expr, f),
            ExprKind::Array(ref exprs) => display_lists!(f, "[", ", ", "]", &**exprs),
            ExprKind::Tuple(ref exprs) => display_lists!(f, "(", ", ", ")", &**exprs),
            ExprKind::Index(ref expr) => Display::fmt(expr, f),
            ExprKind::Struct(ref expr) => Display::fmt(expr, f),
            ExprKind::Field(ref expr) => Display::fmt(expr, f),
            ExprKind::Type(ref expr) => Display::fmt(expr, f),
            ExprKind::Cast(ref expr) => Display::fmt(expr, f),
            ExprKind::Range(ref expr) => Display::fmt(expr, f),
            ExprKind::Block(ref expr) => Display::fmt(expr, f),
            ExprKind::If(ref expr) => Display::fmt(expr, f),
            ExprKind::While(ref expr) => Display::fmt(expr, f),
            ExprKind::Let(ref expr) => Display::fmt(expr, f),
            ExprKind::For(ref expr) => Display::fmt(expr, f),
            ExprKind::Loop(ref expr) => Display::fmt(expr, f),
            ExprKind::Break(ref expr) => Display::fmt(expr, f),
            ExprKind::Continue(ref expr) => Display::fmt(expr, f),
            ExprKind::Match(ref expr) => Display::fmt(expr, f),
            ExprKind::FnCall(ref expr) => Display::fmt(expr, f),
            ExprKind::MethodCall(ref expr) => Display::fmt(expr, f),
            ExprKind::Closure(ref expr) => Display::fmt(expr, f),
            ExprKind::Return(ref expr) => Display::fmt(expr, f),
            ExprKind::MacroCall(ref expr) => Display::fmt(expr, f),
            ExprKind::Async(ref expr) => Display::fmt(expr, f),
            ExprKind::Await(ref expr) => write!(f, "{}.await", expr),
            ExprKind::TryBlock(ref block) => write!(f, "try{}", block),
            ExprKind::ConstBlock(ref expr) => write!(f, "const{}", expr),
            ExprKind::Yield(ref expr) => Display::fmt(expr, f),
            ExprKind::Err(ref s) => Display::fmt(s, f),
        }
    }
}

impl Display for RefExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", ref_head(&None, self.is_raw, self.is_mut), self.expr)
    }
}

impl Display for UnaryOpExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.op, self.expr)
    }
}

impl Display for ListOpExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sep = format!(" {} ", self.op);
        display_lists!(f, sep, &self.exprs)
    }
}

impl Display for RepeatExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}; {}]", &self.value, &self.len)
    }
}

impl Display for IndexExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.obj, self.index)
    }
}

impl Display for StructExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_qself_path(f, &self.qself, &self.path)?;

        writeln!(f, " {{")?;
        display_fields!(f, &self.fields)?;
        if self.has_rest {
            if let Some(ref base) = self.base {
                writeln!(f, "..{}", base)?;
            } else {
                writeln!(f, "..")?;
            }
        }
        write!(f, "}}")
    }
}

impl Display for StructFieldExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.value.to_string();
        if self.name == value {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}: {}", self.name, value)
        }
    }
}

impl Display for FieldExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.expr, self.field)
    }
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.expr, self.ty)
    }
}

impl Display for CastExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} as {}", self.expr, self.ty)
    }
}

impl Display for RangeExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref start) = self.start {
            Display::fmt(start, f)?;
        }
        if self.is_inclusive {
            write!(f, "..=")?;
        } else {
            write!(f, "..")?;
        }
        if let Some(ref end) = self.end {
            Display::fmt(end, f)?;
        }
        OK
    }
}

impl Display for BlockExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref label) = self.label {
            write!(f, "{}:", label)?;
        }
        Display::fmt(&self.block, f)
    }
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if is_if_one_line(self) {
            let (if_value, else_value) = exract_if_else_value(self);
            return write!(f, "if {} {{ {} }} else {{ {} }}", self.expr, if_value, else_value);
        }

        write!(f, "if {}", self.expr)?;
        Display::fmt(&self.block, f)?;
        if let Some(ref br) = self.br {
            if is_block_expr(br) {
                write!(f, " else")?;
            } else {
                write!(f, " else ")?;
            }
            Display::fmt(br, f)?;
        }
        OK
    }
}

impl Display for WhileExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref label) = self.label {
            writeln!(f, "{}:", label)?;
        }
        write!(f, "while {}", self.expr)?;
        Display::fmt(&self.block, f)
    }
}

impl Display for LetExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.pattern, f)?;
        write!(f, " = {}", self.expr)
    }
}

impl Display for ForExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref label) = self.label {
            writeln!(f, "{}:", label)?;
        }
        write!(f, "for {} in {}", self.pattern, self.expr)?;
        Display::fmt(&self.block, f)
    }
}

impl Display for LoopExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref label) = self.label {
            writeln!(f, "{}:", label)?;
        }
        write!(f, "loop{}", self.block)
    }
}

impl Display for BreakExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "break ")?;
        if let Some(ref label) = self.label {
            Display::fmt(&label, f)?;
        }
        if let Some(ref expr) = self.expr {
            Display::fmt(&expr, f)?;
        }
        OK
    }
}

impl Display for ContinueExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "continue ")?;
        if let Some(ref label) = self.label {
            Display::fmt(&label, f)?;
        }
        OK
    }
}

impl Display for MatchExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "match {}", self.expr)?;
        display_fields_block!(f, &self.arms)
    }
}

impl Display for Arm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.pattern, f)?;
        if let Some(ref guard) = self.guard {
            write!(f, " if {}", guard)?;
        }
        if is_block_expr(&self.body) {
            write!(f, " =>")?;
        } else {
            write!(f, " => ")?;
        }
        Display::fmt(&self.body, f)
    }
}

impl Display for FnCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        display_lists!(f, "(", ", ", ")", &self.params)
    }
}

impl Display for MethodCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", &self.args[0], self.path.name)?;
        if !self.path.is_empty() {
            write!(f, "::{}", &self.path)?;
        }
        display_lists!(f, "(", ", ", ")", &self.args[1..])
    }
}

impl Display for ClosureExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", closure_head(self.is_static, self.is_async, self.is_move))?;
        display_lists!(f, "|", ", ", "|", &self.sig.params)?;
        Display::fmt(&self.sig.ret, f)?;

        match self.expr.expr {
            ExprKind::Block(ref block) => Display::fmt(block, f),
            _ => write!(f, " {}", self.expr),
        }
    }
}

impl Display for ReturnExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return")?;
        if let Some(ref expr) = self.ret {
            write!(f, " {}", expr)?;
        }
        OK
    }
}

impl Display for AsyncExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", async_head(self.is_move))?;
        Display::fmt(&self.block, f)
    }
}

impl Display for YieldExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "yield")?;
        if let Some(ref expr) = self.expr {
            write!(f, " {}", expr)?;
        }
        OK
    }
}

impl Display for MacroDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "macro_rules! {} {}", self.name, self.s)
    }
}

impl Display for MacroStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_attrs(f, &self.attrs)?;
        Display::fmt(&self.mac, f)?;
        if self.is_semi {
            write!(f, ";")?;
        }
        OK
    }
}

impl Display for MacroCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MacroCall::Raw(ref raw) => Display::fmt(raw, f),
            MacroCall::Expr(ref expr) => Display::fmt(expr, f),
        }
    }
}

impl Display for MacroExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (open, close) = match self.style {
            MacroStyle::Paren => ("(", ")"),
            MacroStyle::Bracket => ("[", "]"),
            MacroStyle::Brace => ("{", "}"),
        };

        write!(f, "{}!", self.name)?;
        write!(f, "{}", open)?;

        if self.seps.is_empty() {
            display_lists!(f, " ", &self.exprs)?;
            return write!(f, "{}", close);
        }

        let mut expr_idx = 0;
        let mut sep_idx = 0;
        while expr_idx < self.exprs.len() && sep_idx < self.seps.len() {
            let expr = &self.exprs[expr_idx];
            let sep = &self.seps[sep_idx];
            if expr.loc.start < sep.loc.start {
                Display::fmt(expr, f)?;
                expr_idx += 1;
            } else {
                Display::fmt(sep, f)?;
                sep_idx += 1;
            }
        }
        while expr_idx < self.exprs.len() {
            let expr = &self.exprs[expr_idx];
            Display::fmt(expr, f)?;
            expr_idx += 1;
        }

        write!(f, "{}", close)
    }
}

impl Display for MacroSep {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_sep {
            write!(f, "{} ", self.s)
        } else {
            write!(f, "{}", self.s)
        }
    }
}

fn display_doc_symbol(f: &mut fmt::Formatter, is_block: bool) -> fmt::Result {
    if is_block {
        write!(f, "*")
    } else {
        write!(f, "/")
    }
}

fn display_attrs(f: &mut fmt::Formatter, attrs: &[Attr]) -> fmt::Result {
    for attr in attrs {
        writeln!(f, "{}", attr)?;
    }
    OK
}

fn display_vis(f: &mut fmt::Formatter, vis: &Vis) -> fmt::Result {
    write!(f, "{}", vis_head(vis))
}

fn display_use_trees(f: &mut fmt::Formatter, trees: &Option<Vec<UseTree>>) -> fmt::Result {
    if trees.is_none() {
        return OK;
    }

    let trees: &Vec<UseTree> = trees.as_ref().unwrap();
    write!(f, "::")?;
    if trees.len() == 1 && !trees[0].path.starts_with("self") {
        write!(f, "{}", trees[0])
    } else {
        display_lists!(f, "{{", ", ", "}}", trees)
    }
}

fn try_display_type_param_bounds(f: &mut fmt::Formatter, bounds: &TypeParamBounds) -> fmt::Result {
    if !bounds.is_empty() {
        write!(f, ": ")?;
        display_type_param_bounds(f, bounds)?;
    }
    OK
}

fn display_type_param_bounds(f: &mut fmt::Formatter, bounds: &TypeParamBounds) -> fmt::Result {
    display_lists!(f, " + ", &bounds.0)
}

fn display_for_liftime_defs(f: &mut fmt::Formatter, lifetime_defs: &[LifetimeDef]) -> fmt::Result {
    if !lifetime_defs.is_empty() {
        display_lists!(f, "for<", ", ", "> ", lifetime_defs)?;
    }
    OK
}

fn display_paren_param_inputs(f: &mut fmt::Formatter, inputs: &[Type]) -> fmt::Result {
    display_lists!(f, "(", ", ", ")", inputs)
}

fn display_qself_path(f: &mut fmt::Formatter, qself: &Option<QSelf>, path: &Path) -> fmt::Result {
    match qself {
        Some(ref qself) => display_qself_and_path(f, qself, path),
        None => Display::fmt(path, f),
    }
}

fn display_qself_and_path(f: &mut fmt::Formatter, qself: &QSelf, path: &Path) -> fmt::Result {
    write!(f, "<{}", qself.ty)?;
    if qself.pos > 0 {
        write!(f, " as ")?;
        display_path_segments(f, &path.segments[0..qself.pos])?;
    }
    write!(f, ">")?;

    write!(f, "::")?;
    display_path_segments(f, &path.segments[qself.pos..])
}

fn display_path_segments(f: &mut fmt::Formatter, segments: &[PathSegment]) -> fmt::Result {
    display_lists!(f, "", "::", "", segments)
}

fn display_generics(f: &mut fmt::Formatter, generics: &Generics) -> fmt::Result {
    if !generics.is_empty() {
        display_lists!(f, "<", ", ", ">", &generics.lifetime_defs, &generics.type_params, &generics.const_params)?;
    }
    OK
}

fn display_where(f: &mut fmt::Formatter, generics: &Generics) -> fmt::Result {
    if !generics.wh.is_empty() {
        write!(f, " where {}", generics.wh)?;
    }
    OK
}

fn display_params(f: &mut fmt::Formatter, params: &[Param]) -> fmt::Result {
    display_lists!(f, "(", ", ", ")", params)
}

fn display_expr(f: &mut fmt::Formatter, expr: &Expr, is_semi: bool) -> fmt::Result {
    Display::fmt(expr, f)?;
    if is_semi {
        write!(f, ";")?;
    }
    OK
}

fn is_block_expr(expr: &Expr) -> bool {
    matches!(expr.expr, ExprKind::Block(..))
}

fn vis_head(vis: &Vis) -> String {
    let mut head = String::new();
    if !vis.is_empty() {
        head.push_str(vis);
        head.push(' ');
    }
    head
}

fn unsafe_head(is_unsafe: bool) -> &'static str {
    if is_unsafe { "unsafe " } else { "" }
}

fn default_head(is_default: bool) -> &'static str {
    if is_default { "default " } else { "" }
}

fn ref_head(lifetime: &Option<Lifetime>, is_raw: bool, is_mut: bool) -> String {
    let mut head = String::new();
    head.push('&');
    if is_raw {
        if is_mut {
            head.push_str("raw mut ");
        } else {
            head.push_str("raw const ");
        }
        return head;
    }

    if let Some(ref lifetime) = *lifetime {
        head.push_str(&lifetime.s);
        head.push(' ');
    }
    if is_mut {
        head.push_str("mut ");
    }
    head
}

fn trait_type_head(is_dyn: bool, is_impl: bool) -> String {
    let mut head = String::new();
    if is_dyn {
        head.push_str("dyn ");
    }
    if is_impl {
        head.push_str("impl ");
    }
    head
}

fn fn_head(header: &FnHeader) -> String {
    let mut head = String::new();
    if header.is_async {
        head.push_str("async ");
    }
    if header.is_unsafe {
        head.push_str("unsafe ");
    }
    if header.is_const {
        head.push_str("const ");
    }
    if let Some(ref ext) = header.ext {
        head.push_str(&format!("{} ", &ext));
    }
    head.push_str("fn");
    head
}

fn trait_head(is_auto: bool, is_unsafe: bool) -> String {
    let mut head = String::new();
    if is_auto {
        head.push_str("auto ");
    }
    if is_unsafe {
        head.push_str("unsafe ");
    }
    head.push_str("trait ");
    head
}

fn impl_head(is_unsafe: bool, is_default: bool) -> String {
    let mut head = String::new();
    if is_unsafe {
        head.push_str("unsafe ");
    }
    if is_default {
        head.push_str("default ");
    }
    head.push_str("impl");
    head
}

fn foreign_head(abi: &Option<String>) -> String {
    let mut head = String::new();
    head.push_str("extern");
    if let Some(ref abi) = abi {
        head.push(' ');
        head.push_str(abi);
    }
    head
}

fn ident_patten_head(is_ref: bool, is_mut: bool) -> String {
    let mut head = String::new();
    if is_ref {
        head.push_str("ref ");
    }
    if is_mut {
        head.push_str("mut ");
    }
    head
}

fn closure_head(is_static: bool, is_async: bool, is_move: bool) -> String {
    let mut head = String::new();
    if is_static {
        head.push_str("static ");
    }
    if is_async {
        head.push_str("async ");
    }
    if is_move {
        head.push_str("move ");
    }
    head
}

fn async_head(is_move: bool) -> String {
    let mut head = String::new();
    head.push_str("async");
    if is_move {
        head.push_str(" move");
    }
    head
}

fn is_if_one_line(expr: &IfExpr) -> bool {
    if expr.br.is_none() {
        return false;
    }

    if !expr.block.is_one_line() {
        return false;
    }

    match expr.br.as_ref().unwrap().expr {
        ExprKind::Block(ref block) => block.block.is_one_line(),
        _ => false,
    }
}

fn exract_if_else_value(expr: &IfExpr) -> (&Expr, &Expr) {
    let if_value = match &expr.block.stmts[0].stmt {
        StmtKind::Expr(ref expr, _) => expr,
        _ => unreachable!("{:#?}", expr.block.stmts[0].stmt),
    };

    let else_value = match expr.br.as_ref().unwrap().expr {
        ExprKind::Block(ref block) => {
            match &block.block.stmts[0].stmt {
                StmtKind::Expr(ref expr, _) => expr,
                _ => unreachable!("{:#?}", block.block.stmts[0].stmt),
            }
        },
        _ => unreachable!("{:#?}", expr.br.as_ref().unwrap().expr),
    };

    (if_value, else_value)
}

macro_rules! can_one_line {
    ($sf:expr, $e:expr) => ({
        $sf.ts.can_one_line(&$e.to_string())
    });
}

macro_rules! maybe_nl {
    ($sf:expr, $e:ident) => ({
        if $e.loc.nl {
            $sf.wrap();
        }
    });

    ($sf:expr, $e:expr) => ({
        if $e.loc.nl {
            $sf.wrap();
        }
    });
}

macro_rules! maybe_wrap {
    ($sf:expr, $sep:expr, $wrap_sep:expr, $e:expr) => ({
        if !need_wrap!($sf.ts, $sep, &$e.to_string()) {
            $sf.raw_insert($sep);
        } else {
            $sf.wrap();
            $sf.raw_insert($wrap_sep);
        }
    });

    ($sf:expr, $e:expr) => ({
        maybe_wrap!($sf, "", "", $e);
    });

    ($sf:expr, $sep:expr, $wrap_sep:expr, $e:expr, $fmt:ident) => ({
        maybe_wrap!($sf, $sep, $wrap_sep, $e);
        $sf.$fmt(&$e);
    });
}

macro_rules! maybe_nl_indent {
    ($sf:expr, $sep:expr, $wrap_sep:expr, $e:expr) => ({
        if !need_nl_indent!($sf.ts, $sep, &$e.to_string()) {
            $sf.raw_insert($sep);
        } else {
            $sf.nl_indent();
            $sf.raw_insert($wrap_sep);
        }
    });

    ($sf:expr, $sep:expr, $wrap_sep:expr, $e:expr, $fmt:ident) => ({
        maybe_nl_indent!($sf, $sep, $wrap_sep, $e);
        $sf.$fmt($e);
    });
}

macro_rules! insert_sep {
    ($sf:expr, $sep:expr, $e:expr) => ({
        $sf.raw_insert($sep);
        if !$e.loc.nl && !need_wrap!($sf.ts, " ", &$e.to_string()) {
            $sf.raw_insert(" ");
            false
        } else {
            $sf.wrap();
            true
        }
    });
}

macro_rules! fmt_sep_lists {
    ($sf:expr, $open:expr, $sep:expr, $close:expr, $($list:expr, $fmt:ident),+) => ({
        let mut is_wrap = false;
        let mut first = true;
        let mut first_nl = false;
        let mut empty = true;
        $(for e in $list {
            empty = false;

            if first {
                if e.loc.nl {
                    first_nl = true;
                    $sf.raw_insert($open);
                    $sf.indent();
                } else {
                    $sf.insert_mark_align($open);
                }
                first = false;
            } else {
                if first_nl {
                    $sf.raw_insert($sep);
                    if !e.loc.nl {
                        $sf.raw_insert(" ");
                    }
                } else {
                    is_wrap |= insert_sep!($sf, $sep, e);
                }
            }

            $sf.try_fmt_comments_in_list(&e.loc);
            if first_nl && e.loc.nl {
                $sf.nl_indent();
            }
            $sf.$fmt(e);
        })+

        if empty {
            $sf.insert_mark_align($open);
        }
        if first_nl {
            $sf.raw_insert($sep);
            $sf.outdent();
            $sf.nl_indent();
            $sf.raw_insert($close);
        } else {
            $sf.insert_unmark_align($close);
        }
        is_wrap
    });
}

macro_rules! fmt_comma_lists {
    ($sf:expr, $open:expr, $close:expr, $($list:expr, $fmt:ident),+) => ({
        fmt_sep_lists!($sf, $open, ",", $close, $($list, $fmt),+)
    });

    ($sf:expr, $($list:expr, $fmt:ident),+) => ({
        fmt_sep_lists!($sf, "", ",", "", $($list, $fmt),+)
    });
}

macro_rules! fmt_item_groups {
    ($sf:expr, $items:expr, $item_kind:path, $item_type:ty, $fmt_item:ident) => ({
        let mut group: Vec<(&Loc, &String, &Vec<Attr>, $item_type)> = Vec::new();

        for item in $items {
            match item.item {
                $item_kind(ref e) => {
                    if $sf.has_leading_comments(&item.loc) {
                        fmt_item_group!($sf, &group, $item_type, $fmt_item);
                        group.clear();

                        $sf.fmt_leading_comments(&item.loc);
                    }
                    group.push((&item.loc, &item.vis, &item.attrs, e));
                }
                _ => {
                    fmt_item_group!($sf, &group, $item_type, $fmt_item);
                    group.clear();
                }
            }
        }

        fmt_item_group!($sf, &group, $item_type, $fmt_item);
    });
}

macro_rules! fmt_item_group {
    ($sf:expr, $group:expr, $ty:ty, $fmt_item:ident) => ({
        let map: BTreeMap<String, (&Loc, &String, &Vec<Attr>, $ty)>
                = $group.into_iter().map(|e| (e.3.to_string(), *e)).collect();

        for (_, e) in map {
            $sf.fmt_attrs(e.2);

            $sf.insert_indent();
            $sf.fmt_vis(e.1);
            $sf.$fmt_item(e.3);

            $sf.try_fmt_trailing_comment(e.0);
            $sf.nl();
        }
    });
}

macro_rules! fmt_lists {
    ($sf:expr, $sep:expr, $wrap_sep:expr, $($list:expr, $act:ident),+) => ({
        let mut first = true;
        $(for e in $list {
            if !first {
                maybe_wrap!($sf, $sep, $wrap_sep, e, $act);
            } else {
                $sf.$act(e);
            }

            first = false;
        })+
    });

    ($sf:expr, $op:expr, $list:expr, $act:ident) => ({
        let mut first = true;
        for e in $list {
            if !first {
                let op_str = if $op.loc.nl {
                    $sf.wrap();
                    format!("{} ", $op)
                } else {
                    if e.loc.nl {
                        format!(" {}", $op)
                    } else {
                        format!(" {} ", $op)
                    }
                };
                $sf.raw_insert(&op_str);
            }

            $sf.$act(e);
            first = false;
        }
    });

    ($sf:expr, $($list:expr, $act:ident),+) => ({
        fmt_lists!($sf, " ", " ", $($list, $act)+);
    });
}

macro_rules! fmt_block {
    ($sf:expr, $items: expr, $block:expr, $fmt:ident) => ({
        if $items.is_empty() {
            if $sf.block_non_sep {
                $sf.raw_insert("{}");
                $sf.block_non_sep = false;
            } else {
                $sf.raw_insert(" {}");
            }
            return;
        }

        if $sf.block_non_sep {
            $sf.raw_insert("{");
            $sf.block_non_sep = false;
        } else {
            $sf.raw_insert(" {");
        }

        let loc = Loc {
            start: $sf.block_locs.last().unwrap().end,
            ..Default::default()
        };
        $sf.indent();
        $sf.nl();
        $sf.$fmt($block);
        $sf.try_fmt_leading_comments(&loc);

        $sf.outdent();
        $sf.insert_indent();
        $sf.raw_insert("}");
    });

    ($sf:expr, $items:expr, $fmt:ident) => ({
        fmt_block!($sf, $items, $items, $fmt);
    });
}

macro_rules! fmt_items {
    ($sf:ident, $items:expr, $fmt_item:ident) => ({
        for item in $items {
            $sf.try_fmt_leading_comments(&item.loc);
            $sf.fmt_attrs(&item.attrs);
            $sf.insert_indent();

            $sf.$fmt_item(item);

            $sf.try_fmt_trailing_comment(&item.loc);
            $sf.nl();
        }
    });
}

macro_rules! fmt_items_maybe_nl {
    ($sf:ident, $items:expr, $fmt_item:ident) => ({
        let mut nl = false;
        for item in $items {
            if !$sf.try_fmt_leading_comments(&item.loc) && nl {
                $sf.nl();
            }

            $sf.fmt_attrs(&item.attrs);
            $sf.insert_indent();
            nl = $sf.$fmt_item(item);

            $sf.try_fmt_trailing_comment(&item.loc);
            $sf.nl();
        }
    });
}

macro_rules! fmt_qself_path {
    ($sf:ident, $item:ident, $from_expr:expr) => ({
        match $item.qself {
            Some(ref qself) => {
                maybe_wrap!($sf, $item);
                $sf.fmt_qself_path(qself, &$item.path, $from_expr);
            },
            None => $sf.fmt_path(&$item.path, $from_expr),
        }
    });
}

pub fn format(krate: Crate, leading_cmnts: HashMap<Pos, Vec<String>>, trailing_cmnts: HashMap<Pos, String>)
-> TsResult {
    Formatter::new(leading_cmnts, trailing_cmnts).fmt_crate(krate)
}

struct Formatter {
    ts: Typesetter,

    leading_cmnts: HashMap<Pos, Vec<String>>,
    trailing_cmnts: HashMap<Pos, String>,
    block_locs: Vec<Loc>,
    if_stacks: u8,

    after_indent: bool,
    after_wrap: bool,
    block_non_sep: bool,
}

impl Formatter {
    fn new(leading_cmnts: HashMap<Pos, Vec<String>>, trailing_cmnts: HashMap<Pos, String>) -> Formatter {
        Formatter {
            ts: Typesetter::new(),

            leading_cmnts,
            trailing_cmnts,
            block_locs: Vec::new(),
            if_stacks: 0,

            after_indent: false,
            after_wrap: false,
            block_non_sep: false,
        }
    }

    fn fmt_crate(mut self, krate: Crate) -> TsResult {
        self.try_fmt_leading_comments(&krate.loc);
        self.fmt_attrs(&krate.attrs);
        self.fmt_group_items(&krate.items);
        self.fmt_items(&krate.items);
        self.fmt_left_comments(&krate.loc);
        self.ts.result()
    }

    fn has_leading_comments(&self, loc: &Loc) -> bool {
        self.leading_cmnts.contains_key(&loc.start)
    }

    fn try_fmt_leading_comments(&mut self, loc: &Loc) -> bool {
        if self.has_leading_comments(loc) {
            self.fmt_leading_comments(loc);
            true
        } else {
            false
        }
    }

    fn fmt_leading_comments(&mut self, loc: &Loc) {
        for cmnt in &self.leading_cmnts.remove(&loc.start).unwrap() {
            if !cmnt.is_empty() {
                self.insert_indent();
                self.raw_insert(cmnt);
            }
            self.nl();
        }
    }

    fn try_fmt_comments_in_list(&mut self, loc: &Loc) {
        let cmnts = self.leading_cmnts.remove(&loc.start);
        if cmnts.is_none() {
            return;
        }

        for cmnt in cmnts.unwrap() {
            if cmnt.is_empty() {
                self.nl();
            } else {
                self.nl_indent();
                self.raw_insert(&cmnt);
            }
        }
    }

    fn has_trailing_comment(&self, loc: &Loc) -> bool {
        self.trailing_cmnts.contains_key(&loc.end)
    }

    fn try_fmt_trailing_comment(&mut self, loc: &Loc) {
        if self.has_trailing_comment(loc) {
            self.fmt_trailing_comment(loc);
        }
    }

    fn fmt_trailing_comment(&mut self, loc: &Loc) {
        self.raw_insert(" ");
        let cmnt = self.trailing_cmnts.remove(&loc.end).unwrap();
        self.raw_insert(&cmnt);
    }

    fn fmt_left_comments(&mut self, loc: &Loc) {
        let poses: Vec<_> = self.leading_cmnts.keys().cloned().collect();
        for pos in poses {
            for cmnt in &self.leading_cmnts.remove(&pos).unwrap() {
                if pos > loc.end {
                    self.raw_insert(cmnt);
                    self.nl();
                }
            }
        }
    }

    fn fmt_loc_str(&mut self, loc_str: &LocStr) {
        maybe_nl!(self, loc_str);

        let mut first = true;
        for line in loc_str.s.split('\n') {
            if !first {
                self.nl();
            }

            self.insert(line);
            first = false;
        }
    }

    fn fmt_attrs(&mut self, attrs: &[Attr]) {
        let mut group: Vec<(bool, &MetaAttr)> = Vec::new();

        for attr in attrs {
            let is_inner = attr.is_inner;
            match attr.attr {
                AttrKind::Doc(ref doc) => {
                    self.fmt_meta_attr_group(&group);
                    group.clear();

                    self.fmt_doc_attr(&attr.loc, is_inner, doc);
                },
                AttrKind::Attr(ref meta) => {
                    if self.has_leading_comments(&attr.loc) {
                        self.fmt_meta_attr_group(&group);
                        group.clear();

                        self.fmt_leading_comments(&attr.loc);
                    }
                    group.push((is_inner, meta));
                },
                AttrKind::Raw(ref s) => {
                    self.fmt_meta_attr_group(&group);
                    group.clear();

                    self.fmt_loc_str(s);
                    self.nl();
                },
            }
        }

        self.fmt_meta_attr_group(&group);
    }

    fn fmt_doc_attr(&mut self, loc: &Loc, is_inner: bool, doc: &DocAttr) {
        self.try_fmt_leading_comments(loc);
        self.insert_indent();

        self.raw_insert("/");
        self.fmt_doc_symbol(doc.is_block);
        if is_inner {
            self.raw_insert("!");
        } else {
            self.fmt_doc_symbol(doc.is_block);
        }
        self.fmt_loc_str(&doc.doc);
        if doc.is_block {
            self.raw_insert("*/");
        }

        self.try_fmt_trailing_comment(loc);
        self.nl();
    }

    fn fmt_doc_symbol(&mut self, is_block: bool) {
        if is_block {
            self.raw_insert("*");
        } else {
            self.raw_insert("/");
        }
    }

    fn fmt_meta_attr_group(&mut self, group: &[(bool, &MetaAttr)]) {
        let sorted_metas: BTreeMap<_, _> = group.iter().map(|e| (e.1.to_string(), *e)).collect();
        for meta in sorted_metas.values() {
            self.insert_indent();
            self.fmt_meta_attr(meta.0, meta.1);
            self.try_fmt_trailing_comment(&meta.1.loc);
            self.nl();
        }
    }

    fn fmt_meta_attr(&mut self, is_inner: bool, meta: &MetaAttr) {
        self.raw_insert("#");
        if is_inner {
            self.raw_insert("!");
        }
        self.raw_insert("[");
        self.fmt_loc_str(&meta.name);
        if let Some(ref metas) = meta.metas {
            self.fmt_nested_metas(metas);
        }
        self.raw_insert("]");
    }

    fn fmt_nested_metas(&mut self, metas: &[MetaAttr]) {
        fmt_comma_lists!(self, "(", ")", metas, fmt_nested_meta);
    }

    fn fmt_nested_meta(&mut self, meta: &MetaAttr) {
        maybe_nl!(self, meta);
        self.fmt_loc_str(&meta.name);

        if let Some(ref metas) = meta.metas {
            self.fmt_nested_metas(metas);
        }
    }

    fn fmt_vis(&mut self, vis: &Vis) {
        if !vis.is_empty() {
            self.raw_insert(vis);
            self.raw_insert(" ");
        }
    }

    fn fmt_group_items(&mut self, items: &[Item]) {
        self.fmt_extern_crate_items(items);
        self.fmt_use_items(items);
        self.fmt_mod_decl_items(items);
    }

    fn fmt_extern_crate_items(&mut self, items: &[Item]) {
        fmt_item_groups!(self, items, ItemKind::ExternCrate, &ExternCrate, fmt_extern_crate);
    }

    fn fmt_extern_crate(&mut self, item: &ExternCrate) {
        self.insert(&format!("extern crate {};", &item.name));
    }

    fn fmt_use_items(&mut self, items: &[Item]) {
        fmt_item_groups!(self, items, ItemKind::Use, &Use, fmt_use);
    }

    fn fmt_use(&mut self, item: &Use) {
        self.insert(&format!("use {}", &item.path));
        self.fmt_use_trees(&item.trees, false);
        self.raw_insert(";");
    }

    fn fmt_use_trees(&mut self, trees: &Option<Vec<UseTree>>, wrap: bool) {
        if trees.is_none() {
            return;
        }

        self.insert("::");
        let trees: &Vec<UseTree> = trees.as_ref().unwrap();
        if trees.len() == 1 && !trees[0].path.starts_with("self") {
            self.fmt_use_one_tree(&trees[0]);
        } else {
            self.fmt_use_more_trees(trees, wrap);
        }
    }

    fn fmt_use_one_tree(&mut self, item: &UseTree) {
        self.insert(&item.path);
        self.fmt_use_trees(&item.trees, true);
    }

    fn fmt_use_more_trees(&mut self, trees: &[UseTree], wrap: bool) {
        self.insert_mark_align("{");
        let mut all_nl = false;

        let mut first = true;
        for tree in trees {
            if first {
                all_nl = tree.loc.nl;
                if all_nl {
                    self.indent();
                }
            }

            if !first {
                self.raw_insert(",");
                if !all_nl && !tree.loc.nl && !need_wrap!(self.ts, " ", &tree.to_string()) {
                    self.raw_insert(" ");
                } else if !all_nl || wrap {
                    self.wrap();
                }
            }
            if all_nl {
                self.nl_indent();
            }

            self.fmt_use_one_tree(tree);
            first = false;
        }

        if all_nl {
            self.outdent();
            self.nl_indent();
        }
        self.insert_unmark_align("}");
    }

    fn fmt_mod_decl_items(&mut self, items: &[Item]) {
        fmt_item_groups!(self, items, ItemKind::ModDecl, &ModDecl, fmt_mod_decl);
    }

    fn fmt_mod_decl(&mut self, item: &ModDecl) {
        self.insert(&format!("{}mod {};", unsafe_head(item.is_unsafe), &item.name));
    }

    fn fmt_items(&mut self, items: &[Item]) {
        let mut nl = false;
        for item in items {
            nl = match item.item {
                ItemKind::ExternCrate(_) | ItemKind::Use(_) | ItemKind::ModDecl(_) => false,
                _ => self.fmt_item(item, nl),
            }
        }
    }

    fn fmt_item(&mut self, item: &Item, nl: bool) -> bool {
        if !self.try_fmt_leading_comments(&item.loc) && nl {
            self.nl();
        }
        self.fmt_attrs(&item.attrs);
        self.insert_indent();
        self.fmt_vis(&item.vis);

        self.block_locs.push(item.loc);
        let nl = match item.item {
            ItemKind::ExternCrate(ref item) => {
                self.fmt_extern_crate(item);
                false
            },
            ItemKind::Use(ref item) => {
                self.fmt_use(item);
                false
            },
            ItemKind::ModDecl(ref item) => {
                self.fmt_mod_decl(item);
                false
            },
            ItemKind::Mod(ref item) => {
                self.fmt_mod(item);
                true
            },
            ItemKind::TypeAlias(ref item) => {
                self.fmt_type_alias(item);
                false
            },
            ItemKind::TraitAlias(ref item) => {
                self.fmt_trait_alias(item);
                false
            },
            ItemKind::Const(ref item) => {
                self.fmt_const(item);
                false
            },
            ItemKind::Static(ref item) => {
                self.fmt_static(item);
                false
            },
            ItemKind::Struct(ref item) => {
                self.fmt_struct(item)
            },
            ItemKind::Union(ref item) => {
                self.fmt_union(item);
                true
            },
            ItemKind::Enum(ref item) => {
                self.fmt_enum(item);
                true
            },
            ItemKind::Fn(ref item) => {
                self.fmt_fn(item);
                true
            },
            ItemKind::ForeignMod(ref item) => {
                self.fmt_foreign_mod(item);
                true
            },
            ItemKind::Trait(ref item) => {
                self.fmt_trait(item);
                true
            },
            ItemKind::Impl(ref item) => {
                self.fmt_impl(item);
                true
            },
            ItemKind::MacroDef(ref item) => {
                self.fmt_macro_def(item);
                true
            },
            ItemKind::MacroCall(ref item) => {
                self.fmt_macro_call(item, true);
                false
            },
        };
        self.block_locs.pop();

        self.try_fmt_trailing_comment(&item.loc);
        self.nl();
        nl
    }

    fn fmt_mod(&mut self, item: &Mod) {
        self.insert(&format!("{}mod {}", unsafe_head(item.is_unsafe), &item.name));
        fmt_block!(self, item.items, item, fmt_mod_items);
    }

    fn fmt_mod_items(&mut self, module: &Mod) {
        self.fmt_group_items(&module.items);
        self.fmt_items(&module.items);
    }

    fn fmt_type_alias(&mut self, item: &TypeAlias) {
        self.insert(&format!("type {}", &item.name));
        self.fmt_generics_and_where(&item.generics);
        self.try_fmt_type_param_bounds(&item.bounds);
        if let Some(ref ty) = item.ty {
            maybe_wrap!(self, " = ", "= ", ty, fmt_type);
        }
        self.raw_insert(";");
    }

    fn fmt_trait_alias(&mut self, item: &TraitAlias) {
        self.insert(&format!("trait {}", &item.name));
        self.fmt_generics_and_where(&item.generics);
        maybe_wrap!(self, " = ", "= ", item.bounds, fmt_type_param_bounds);
        self.raw_insert(";");
    }

    fn fmt_generics(&mut self, generics: &Generics) {
        if !generics.is_empty() {
            fmt_comma_lists!(self, "<", ">",
                             &generics.lifetime_defs, fmt_lifetime_def,
                             &generics.type_params, fmt_type_param,
                             &generics.const_params, fmt_const_param);
        }
    }

    fn fmt_lifetime_def(&mut self, lifetime_def: &LifetimeDef) {
        maybe_nl!(self, lifetime_def);
        maybe_wrap!(self, lifetime_def);

        self.fmt_lifetime(&lifetime_def.lifetime);
        if !lifetime_def.bounds.is_empty() {
            self.raw_insert(": ");
            fmt_lists!(self, " + ", "+ ", &lifetime_def.bounds, fmt_lifetime)
        }
    }

    fn fmt_lifetime(&mut self, lifetime: &Lifetime) {
        self.fmt_loc_str(lifetime);
    }

    fn fmt_type_param(&mut self, type_param: &TypeParam) {
        maybe_nl!(self, type_param);
        maybe_wrap!(self, type_param);

        self.insert(&type_param.name);
        if let Some(ref ty) = type_param.default {
            maybe_wrap!(self, " = ", "= ", ty, fmt_type);
        } else {
            self.try_fmt_type_param_bounds(&type_param.bounds);
        }
    }

    fn try_fmt_type_param_bounds(&mut self, bounds: &TypeParamBounds) {
        if !bounds.is_empty() {
            self.raw_insert(": ");
            self.fmt_type_param_bounds(bounds);
        }
    }

    fn fmt_type_param_bounds(&mut self, bounds: &TypeParamBounds) {
        fmt_lists!(self, " + ", "+ ", &bounds.0, fmt_type_param_bound)
    }

    fn fmt_type_param_bound(&mut self, bound: &TypeParamBound) {
        match *bound {
            TypeParamBound::Lifetime(ref lifetime) => self.fmt_lifetime(lifetime),
            TypeParamBound::PolyTraitRef(ref poly_trait_ref) => self.fmt_poly_trait_ref(poly_trait_ref),
        }
    }

    fn fmt_poly_trait_ref(&mut self, poly_trait_ref: &PolyTraitRef) {
        self.fmt_for_lifetime_defs(&poly_trait_ref.lifetime_defs);
        self.fmt_trait_ref(&poly_trait_ref.trait_ref);
    }

    fn fmt_for_lifetime_defs(&mut self, lifetime_defs: &[LifetimeDef]) {
        if !lifetime_defs.is_empty() {
            fmt_comma_lists!(self, "for<", "> ", lifetime_defs, fmt_lifetime_def);
        }
    }

    fn fmt_trait_ref(&mut self, trait_ref: &TraitRef) {
        self.fmt_path(trait_ref, false);
    }

    fn fmt_const_param(&mut self, const_param: &ConstParam) {
        maybe_nl!(self, const_param);
        maybe_wrap!(self, const_param);

        self.insert(&format!("const {}", const_param.name));
        insert_sep!(self, ":", const_param.ty);
        self.fmt_type(&const_param.ty);
        if let Some(ref expr) = const_param.default {
            maybe_wrap!(self, " = ", "= ", expr, fmt_expr);
        }
    }

    fn fmt_where(&mut self, generics: &Generics) {
        let wh = &generics.wh;
        if wh.is_empty() {
            return;
        }

        if wh.loc.nl {
            self.nl_indent();
            self.raw_insert("where ");
        } else {
            maybe_nl_indent!(self, " where ", "where ", wh);
        }
        self.fmt_where_clauses(&wh.clauses);
    }

    fn fmt_where_clauses(&mut self, clauses: &[WhereClause]) {
        fmt_comma_lists!(self, clauses, fmt_where_clause);
    }

    fn fmt_where_clause(&mut self, clause: &WhereClause) {
        match clause.clause {
            WhereKind::LifetimeDef(ref lifetime_def) => self.fmt_lifetime_def(lifetime_def),
            WhereKind::Bound(ref bound) => self.fmt_where_bound(bound),
        }
    }

    fn fmt_where_bound(&mut self, bound: &WhereBound) {
        maybe_wrap!(self, bound);
        self.fmt_for_lifetime_defs(&bound.lifetime_defs);
        self.fmt_type(&bound.ty);
        self.try_fmt_type_param_bounds(&bound.bounds);
    }

    fn fmt_generics_and_where(&mut self, generics: &Generics) {
        self.fmt_generics(generics);
        self.fmt_where(generics);
    }

    fn fmt_path(&mut self, path: &Path, from_expr: bool) {
        maybe_nl!(self, path);
        self.fmt_path_segments(&path.segments, from_expr);
    }

    fn fmt_path_segments(&mut self, segments: &[PathSegment], from_expr: bool) {
        let mut first = true;
        for seg in segments {
            if !first {
                maybe_wrap!(self, "::", "::", seg);
            }

            self.fmt_path_segment(seg, from_expr);
            first = false;
        };
    }

    fn fmt_path_segment(&mut self, seg: &PathSegment, from_expr: bool) {
        self.insert(&seg.name);
        self.fmt_path_param(&seg.param, from_expr);
    }

    fn fmt_path_param(&mut self, param: &PathParam, from_expr: bool) {
        match *param {
            PathParam::Angle(ref param) => self.fmt_angle_param(param, from_expr),
            PathParam::Paren(ref param) => self.fmt_paren_param(param),
        }
    }

    fn fmt_angle_param(&mut self, param: &AngleParam, from_expr: bool) {
        if param.is_empty() {
            return;
        }

        if from_expr {
            self.insert("::");
        }
        self.block_non_sep = true;
        fmt_comma_lists!(self, "<", ">",
                         &param.lifetimes, fmt_lifetime,
                         &param.types, fmt_type,
                         &param.consts, fmt_expr,
                         &param.bindings, fmt_type_binding);
        self.block_non_sep = false;
    }

    fn fmt_type_binding(&mut self, binding: &TypeBinding) {
        maybe_nl!(self, binding);
        maybe_wrap!(self, binding);

        self.insert(&binding.name);
        match binding.binding {
            TypeBindingKind::Eq(ref ty) => {
                self.raw_insert("=");
                self.fmt_type(ty);
            },
            TypeBindingKind::Bound(ref bounds) => {
                self.raw_insert(": ");
                fmt_lists!(self, "+", "+", &bounds.0, fmt_type_param_bound);
            },
        }
    }

    fn fmt_paren_param(&mut self, param: &ParenParam) {
        fmt_comma_lists!(self, "(", ")", &param.inputs, fmt_type);
        if let Some(ref output) = param.output {
            maybe_wrap!(self, " -> ", "-> ", output, fmt_type);
        }
    }

    fn fmt_qself_path(&mut self, qself: &QSelf, path: &Path, from_expr: bool) {
        self.insert_mark_align("<");
        self.fmt_type(&qself.ty);
        if qself.pos > 0 {
            self.raw_insert(" as ");
            self.fmt_path_segments(&path.segments[0..qself.pos], from_expr);
        }
        self.insert_unmark_align(">");

        self.insert("::");
        self.fmt_path_segments(&path.segments[qself.pos..], from_expr);
    }

    fn fmt_type(&mut self, ty: &Type) {
        maybe_nl!(self, ty);
        match ty.ty {
            TypeKind::Symbol(ty) => self.fmt_symbol_type(ty),
            TypeKind::Path(ref ty) => self.fmt_path_type(ty, false),
            TypeKind::Ptr(ref ty) => self.fmt_ptr_type(ty),
            TypeKind::Ref(ref ty) => self.fmt_ref_type(ty),
            TypeKind::Tuple(ref ty) => self.fmt_tuple_type(ty),
            TypeKind::Slice(ref ty) => self.fmt_slice_type(ty),
            TypeKind::Array(ref ty) => self.fmt_array_type(ty),
            TypeKind::Struct(ref ty) => self.fmt_struct_type(ty),
            TypeKind::Union(ref ty) => self.fmt_union_type(ty),
            TypeKind::Trait(ref ty) => self.fmt_trait_type(ty),
            TypeKind::BareFn(ref ty) => self.fmt_bare_fn_type(ty),
            TypeKind::MacroCall(ref ty) => self.fmt_macro_call(ty, false),
            TypeKind::Err(ref ty) => self.fmt_loc_str(ty),
        }
    }

    fn fmt_symbol_type(&mut self, ty: &str) {
        self.raw_insert(ty)
    }

    fn fmt_path_type(&mut self, ty: &PathType, from_expr: bool) {
        fmt_qself_path!(self, ty, from_expr);
    }

    fn fmt_ptr_type(&mut self, ty: &PtrType) {
        let head = ptr_head(ty.is_mut);
        maybe_wrap!(self, head, head, ty.ty, fmt_type);
    }

    fn fmt_ref_type(&mut self, ty: &RefType) {
        let head = &ref_head(&ty.lifetime, false, ty.is_mut);
        maybe_wrap!(self, head, head, ty.ty, fmt_type);
    }

    fn fmt_tuple_type(&mut self, ty: &TupleType) {
        fmt_comma_lists!(self, "(", ")", &ty.types, fmt_type);
    }

    fn fmt_slice_type(&mut self, ty: &SliceType) {
        self.insert_mark_align("[");
        self.fmt_type(&ty.ty);
        self.insert_unmark_align("]");
    }

    fn fmt_array_type(&mut self, ty: &ArrayType) {
        self.insert_mark_align("[");
        self.fmt_type(&ty.ty);
        insert_sep!(self, ";", ty.expr);
        self.fmt_expr(&ty.expr);
        self.insert_unmark_align("]");
    }

    fn fmt_struct_type(&mut self, ty: &StructType) {
        self.insert("struct");
        fmt_block!(self, &ty.fields, fmt_struct_fields);
    }

    fn fmt_union_type(&mut self, ty: &UnionType) {
        self.insert("union");
        fmt_block!(self, &ty.fields, fmt_struct_fields);
    }

    fn fmt_trait_type(&mut self, ty: &TraitType) {
        let head = &trait_type_head(ty.is_dyn, ty.is_impl);
        self.insert(head);
        self.fmt_type_param_bounds(&ty.bounds);
    }

    fn fmt_bare_fn_type(&mut self, ty: &BareFnType) {
        self.fmt_for_lifetime_defs(&ty.lifetime_defs);
        self.insert(&fn_head(&ty.header));
        self.fmt_fn_sig(&ty.sig);
    }

    fn fmt_const(&mut self, item: &Const) {
        self.insert(&format!("{}const {}", default_head(item.is_default), item.name));
        insert_sep!(self, ":", item.ty);
        self.fmt_type(&item.ty);
        if let Some(ref expr) = item.expr {
            maybe_wrap!(self, " = ", "= ", expr, fmt_expr);
        }
        self.raw_insert(";");
    }

    fn fmt_static(&mut self, item: &Static) {
        self.insert(&format!("{}{}", static_head(item.is_mut), item.name));
        insert_sep!(self, ":", item.ty);
        self.fmt_type(&item.ty);
        if let Some(ref expr) = item.expr {
            maybe_wrap!(self, " = ", "= ", expr, fmt_expr);
        }
        self.raw_insert(";");
    }

    fn fmt_struct(&mut self, item: &Struct) -> bool {
        self.insert(&format!("struct {}", item.name));
        self.fmt_generics_and_where(&item.generics);
        self.fmt_struct_body(&item.body);

        match item.body {
            StructBody::Tuple(_) | StructBody::Unit => {
                self.raw_insert(";");
                false
            },
            _ => true,
        }
    }

    fn fmt_struct_body(&mut self, body: &StructBody) {
        match *body {
            StructBody::Struct(ref fields) => fmt_block!(self, fields, fmt_struct_fields),
            StructBody::Tuple(ref fields) => self.fmt_tuple_fields(fields),
            StructBody::Unit => (),
        }
    }

    fn fmt_struct_fields(&mut self, fields: &[StructField]) {
        fmt_items!(self, fields, fmt_struct_field);
    }

    fn fmt_struct_field(&mut self, field: &StructField) {
        self.fmt_vis(&field.vis);
        self.insert(&field.name);
        insert_sep!(self, ":", field.ty);
        self.fmt_type(&field.ty);
        self.raw_insert(",");
    }

    fn fmt_tuple_fields(&mut self, fields: &[TupleField]) {
        fmt_comma_lists!(self, "(", ")", fields, fmt_tuple_field);
    }

    fn fmt_tuple_field(&mut self, field: &TupleField) {
        maybe_nl!(self, field);
        self.try_fmt_leading_comments(&field.loc);
        self.fmt_attrs(&field.attrs);
        self.fmt_vis(&field.vis);
        self.fmt_type(&field.ty);
    }

    fn fmt_union(&mut self, item: &Union) {
        self.insert(&format!("union {}", item.name));
        self.fmt_generics_and_where(&item.generics);
        fmt_block!(self, &item.fields, fmt_struct_fields);
    }

    fn fmt_enum(&mut self, item: &Enum) {
        self.insert(&format!("enum {}", item.name));
        self.fmt_generics_and_where(&item.generics);
        fmt_block!(self, &item.body.fields, fmt_enum_fields);
    }

    fn fmt_enum_fields(&mut self, fields: &[EnumField]) {
        fmt_items!(self, fields, fmt_enum_field);
    }

    fn fmt_enum_field(&mut self, field: &EnumField) {
        self.insert(&field.name);
        self.fmt_struct_body(&field.body);
        if let Some(ref expr) = field.expr {
            maybe_wrap!(self, " = ", "= ", expr, fmt_expr);
        }
        self.raw_insert(",");
    }

    fn fmt_fn_sig(&mut self, sig: &FnSig) {
        self.fmt_fn_params(&sig.params);
        self.fmt_fn_return(&sig.ret);
    }

    fn fmt_fn_params(&mut self, params: &[Param]) -> bool {
        fmt_comma_lists!(self, "(", ")", params, fmt_param)
    }

    fn fmt_param(&mut self, param: &Param) {
        maybe_nl!(self, param);
        maybe_wrap!(self, param);

        if param.has_patten {
            self.fmt_patten(&param.pattern);
            self.raw_insert(": ");
        } else if let PattenKind::Ident(ref pattern) = param.pattern.pattern {
            self.insert(&ident_patten_head(pattern.is_ref, pattern.is_mut));
        }
        self.fmt_type(&param.ty);
    }

    fn fmt_fn_return(&mut self, ret: &Return) {
        if ret.ret.is_none() {
            return;
        }

        let ty = ret.ret.as_ref().unwrap();
        if ret.nl {
            self.nl_indent();
            self.raw_insert("-> ");
        } else {
            maybe_nl_indent!(self, " -> ", "-> ", ty);
        }
        self.fmt_type(ty);
    }

    fn fmt_fn(&mut self, item: &Fn) -> bool {
        self.insert(&format!("{} {}", fn_head(&item.header), item.name));
        self.fmt_generics(&item.generics);
        self.fmt_fn_sig(&item.sig);
        self.fmt_where(&item.generics);
        if let Some(ref block) = item.block {
            self.fmt_block(block);
            true
        } else {
            self.raw_insert(";");
            false
        }
    }

    fn fmt_foreign_mod(&mut self, item: &ForeignMod) {
        self.insert(&foreign_head(&item.abi));
        fmt_block!(self, &item.items, fmt_foreign_items);
    }

    fn fmt_foreign_items(&mut self, items: &[ForeignItem]) {
        fmt_items_maybe_nl!(self, items, fmt_foreign_item);
    }

    fn fmt_foreign_item(&mut self, item: &ForeignItem) -> bool {
        self.fmt_vis(&item.vis);
        match item.item {
            ForeignKind::TypeAlias(ref item) => {
                self.fmt_type_alias(item);
                false
            },
            ForeignKind::Static(ref item) => {
                self.fmt_static(item);
                false
            },
            ForeignKind::Fn(ref item) => self.fmt_fn(item),
            ForeignKind::MacroCall(ref item) => {
                self.fmt_macro_call(item, true);
                false
            },
        }
    }

    fn fmt_trait(&mut self, item: &Trait) {
        self.insert(&format!("{}{}", trait_head(item.is_auto, item.is_unsafe), item.name));
        self.fmt_generics(&item.generics);
        self.try_fmt_type_param_bounds(&item.bounds);
        self.fmt_where(&item.generics);
        fmt_block!(self, &item.items, fmt_trait_items);
    }

    fn fmt_trait_items(&mut self, items: &[TraitItem]) {
        fmt_items_maybe_nl!(self, items, fmt_trait_item);
    }

    fn fmt_trait_item(&mut self, item: &TraitItem) -> bool {
        self.fmt_attrs(&item.attrs);

        let mut nl = false;
        match item.item {
            TraitItemKind::Const(ref item) => self.fmt_const(item),
            TraitItemKind::TypeAlias(ref item) => self.fmt_type_alias(item),
            TraitItemKind::Fn(ref item) => {
                nl = self.fmt_fn(item);
            },
            TraitItemKind::MacroCall(ref item) => {
                self.fmt_macro_call(item, true);
            },
        }
        nl
    }

    fn fmt_impl(&mut self, item: &Impl) {
        self.insert(&impl_head(item.is_unsafe, item.is_default));
        self.fmt_generics(&item.generics);
        self.raw_insert(" ");
        if item.is_neg {
            self.raw_insert("!");
        }

        if let Some(ref trait_ref) = item.trait_ref {
            self.fmt_trait_ref(trait_ref);
            maybe_wrap!(self, " for ", "for ", item.ty, fmt_type);
        } else {
            self.fmt_type(&item.ty);
        }
        self.fmt_where(&item.generics);
        fmt_block!(self, &item.items, fmt_impl_items);
    }

    fn fmt_impl_items(&mut self, items: &[ImplItem]) {
        fmt_items_maybe_nl!(self, items, fmt_impl_item);
    }

    fn fmt_impl_item(&mut self, item: &ImplItem) -> bool {
        self.fmt_vis(&item.vis);

        let mut nl = false;
        match item.item {
            ImplItemKind::Const(ref item) => self.fmt_const(item),
            ImplItemKind::TypeAlias(ref item) => self.fmt_type_alias(item),
            ImplItemKind::Fn(ref item) => {
                nl = self.fmt_fn(item);
            },
            ImplItemKind::MacroCall(ref item) => {
                self.fmt_macro_call(item, true);
            },
        }
        nl
    }

    fn fmt_block(&mut self, block: &Block) {
        if block.is_one_line() {
            self.fmt_block_one_line(block);
        } else {
            self.fmt_block_multi_lines(block);
        }
    }

    fn fmt_block_one_line(&mut self, block: &Block) {
        if self.block_non_sep {
            self.raw_insert("{ ");
            self.block_non_sep = false
        } else {
            self.raw_insert(" { ");
        }
        if let StmtKind::Expr(ref expr, _) = block.stmts[0].stmt {
            self.fmt_expr(expr);
        }
        self.raw_insert(" }");
    }

    fn fmt_block_multi_lines(&mut self, block: &Block) {
        self.block_locs.push(block.loc);
        self.insert(unsafe_head(block.is_unsafe));
        fmt_block!(self, &block.stmts, fmt_stmts);
        self.block_locs.pop();
    }

    fn fmt_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.fmt_stmt(stmt);
        }
    }

    fn fmt_stmt(&mut self, stmt: &Stmt) {
        self.block_locs.push(stmt.loc);
        self.try_fmt_leading_comments(&stmt.loc);
        match stmt.stmt {
            StmtKind::Item(ref item) => {
                self.fmt_item(item, false);
            },
            StmtKind::Let(ref local) => self.fmt_let(local),
            StmtKind::Expr(ref expr, is_semi) => self.fmt_expr_stmt(expr, is_semi),
            StmtKind::Macro(ref mac) => self.fmt_macro_stmt(mac),
            StmtKind::None => (),
        }
        self.block_locs.pop();
    }

    fn fmt_let(&mut self, local: &Let) {
        self.try_fmt_leading_comments(&local.loc);
        self.fmt_attrs(&local.attrs);
        self.insert_indent();

        self.raw_insert("let ");
        self.fmt_patten(&local.pattern);
        if let Some(ref ty) = local.ty {
            maybe_wrap!(self, ": ", ":", ty, fmt_type);
        }
        if let Some(ref expr) = local.init {
            maybe_wrap!(self, " = ", "= ", expr, fmt_expr);
        }

        self.raw_insert(";");
        self.try_fmt_trailing_comment(&local.loc);
        self.nl();
    }

    fn fmt_patten(&mut self, pattern: &Pattern) {
        maybe_nl!(self, pattern);
        match pattern.pattern {
            PattenKind::Symbol(pattern) => self.insert(pattern),
            PattenKind::Literal(ref pattern) => self.fmt_expr(pattern),
            PattenKind::Box(ref pattern) => self.fmt_box_patten(pattern),
            PattenKind::Range(ref pattern) => self.fmt_range_patten(pattern),
            PattenKind::Ref(ref pattern) => self.fmt_ref_patten(pattern),
            PattenKind::Path(ref pattern) => self.fmt_path_type(pattern, true),
            PattenKind::Ident(ref pattern) => self.fmt_ident_patten(pattern),
            PattenKind::Struct(ref pattern) => self.fmt_struct_patten(pattern),
            PattenKind::Enum(ref pattern) => self.fmt_enum_patten(pattern),
            PattenKind::Or(ref pattern) => self.fmt_or_patten(pattern),
            PattenKind::Tuple(ref pattern) => self.fmt_tuple_patten(pattern),
            PattenKind::Slice(ref pattern) => self.fmt_slice_patten(pattern),
            PattenKind::MacroCall(ref pattern) => self.fmt_macro_call(pattern, false),
        }
    }

    fn fmt_patterns(&mut self, patterns: &[Pattern]) {
        fmt_lists!(self, " | ", "| ", patterns, fmt_patten);
    }

    fn fmt_box_patten(&mut self, pattern: &Pattern) {
        self.insert("box ");
        self.fmt_patten(pattern);
    }

    fn fmt_range_patten(&mut self, pattern: &RangePatten) {
        if let Some(ref start) = pattern.start {
            self.fmt_expr(start);
        }
        self.insert(range(pattern.is_inclusive));
        if let Some(ref end) = pattern.end {
            self.fmt_expr(end);
        }
    }

    fn fmt_ref_patten(&mut self, pattern: &RefPatten) {
        self.insert(&ref_head(&None, false, pattern.is_mut));
        self.fmt_patten(&pattern.pattern);
    }

    fn fmt_ident_patten(&mut self, pattern: &IdentPatten) {
        self.insert(&format!("{}{}", ident_patten_head(pattern.is_ref, pattern.is_mut), pattern.name));
        if let Some(ref pattern) = pattern.pattern {
            maybe_wrap!(self, " @ ", "@ ", pattern, fmt_patten);
        }
    }

    fn fmt_struct_patten(&mut self, pattern: &StructPatten) {
        fmt_qself_path!(self, pattern, false);

        if pattern.fields.is_empty() {
            if pattern.omit {
                self.raw_insert(" {..}");
            } else {
                self.raw_insert(" {}");
            }
            return;
        }

        if can_one_line!(self, pattern) {
            self.fmt_struct_patten_one_line(pattern);
            return;
        }

        self.open_brace();
        let loc = Loc {
            start: self.block_locs.last().unwrap().end,
            ..Default::default()
        };

        self.fmt_struct_field_patterns(&pattern.fields);
        if pattern.omit {
            self.insert_indent();
            self.raw_insert("..");
            self.nl();
        }

        self.try_fmt_leading_comments(&loc);
        self.close_brace();
    }

    fn fmt_struct_patten_one_line(&mut self, pattern: &StructPatten) {
        self.raw_insert(" { ");
        fmt_lists!(self, &pattern.fields, fmt_struct_field_patten);
        if pattern.omit {
            self.raw_insert(" ..");
        }
        self.raw_insert(" }");
    }

    fn fmt_struct_field_patterns(&mut self, fields: &[StructFieldPatten]) {
        for field in fields {
            self.try_fmt_leading_comments(&field.loc);
            self.insert_indent();
            self.fmt_struct_field_patten(field);
            self.try_fmt_trailing_comment(&field.loc);
            self.nl();
        }
    }

    fn fmt_struct_field_patten(&mut self, field: &StructFieldPatten) {
        if field.shorthand {
            self.fmt_patten(&field.pattern);
        } else {
            self.insert(&field.name);
            maybe_wrap!(self, ": ", ":", field.pattern, fmt_patten);
        }
        self.raw_insert(",");
    }

    fn fmt_enum_patten(&mut self, pattern: &EnumPatten) {
        fmt_qself_path!(self, pattern, false);
        fmt_comma_lists!(self, "(", ")", &pattern.patterns, fmt_patten);
    }

    fn fmt_or_patten(&mut self, pattern: &OrPatten) {
        self.fmt_patterns(&pattern.patterns);
    }

    fn fmt_tuple_patten(&mut self, pattern: &TuplePatten) {
        fmt_comma_lists!(self, "(", ")", &pattern.patterns, fmt_patten);
    }

    fn fmt_slice_patten(&mut self, pattern: &SlicePatten) {
        fmt_comma_lists!(self, "[", "]", &pattern.patterns, fmt_patten);
    }

    fn fmt_expr_stmt(&mut self, expr: &Expr, is_semi: bool) {
        self.block_locs.push(expr.loc);
        self.try_fmt_leading_comments(&expr.loc);
        self.fmt_attrs(&expr.attrs);
        self.insert_indent();

        let mut loc = expr.loc;
        self.fmt_expr(expr);
        if is_semi {
            self.raw_insert(";");
            loc.end += 1;
        }

        self.try_fmt_trailing_comment(&loc);
        self.nl();
        self.block_locs.pop();
    }

    fn fmt_expr(&mut self, expr: &Expr) {
        self.block_locs.push(expr.loc);
        maybe_nl!(self, expr);
        match expr.expr {
            ExprKind::Symbol(sym) => self.fmt_symbol_expr(sym),
            ExprKind::Literal(ref expr) => self.fmt_literal_expr(expr),
            ExprKind::Path(ref expr) => self.fmt_path_expr(expr),
            ExprKind::Box(ref expr) => self.fmt_box_expr(expr),
            ExprKind::Ref(ref expr) => self.fmt_ref_expr(expr),
            ExprKind::UnaryOp(ref expr) => self.fmt_unary_op_expr(expr),
            ExprKind::Try(ref expr) => self.fmt_try_expr(expr),
            ExprKind::ListOp(ref expr) => self.fmt_list_op_expr(expr),
            ExprKind::Repeat(ref expr) => self.fmt_repeat_expr(expr),
            ExprKind::Array(ref exprs) => self.fmt_array_expr(exprs),
            ExprKind::Tuple(ref exprs) => self.fmt_tuple_expr(exprs),
            ExprKind::Index(ref expr) => self.fmt_index_expr(expr),
            ExprKind::Struct(ref expr) => self.fmt_struct_expr(expr),
            ExprKind::Field(ref expr) => self.fmt_field_expr(expr),
            ExprKind::Type(ref expr) => self.fmt_type_expr(expr),
            ExprKind::Cast(ref expr) => self.fmt_cast_expr(expr),
            ExprKind::Range(ref expr) => self.fmt_range_expr(expr),
            ExprKind::Block(ref expr) => self.fmt_block_expr(expr),
            ExprKind::If(ref expr) => self.fmt_if_expr(expr),
            ExprKind::While(ref expr) => self.fmt_while_expr(expr),
            ExprKind::Let(ref expr) => self.fmt_let_expr(expr),
            ExprKind::For(ref expr) => self.fmt_for_expr(expr),
            ExprKind::Loop(ref expr) => self.fmt_loop_expr(expr),
            ExprKind::Break(ref expr) => self.fmt_break_expr(expr),
            ExprKind::Continue(ref expr) => self.fmt_continue_expr(expr),
            ExprKind::Match(ref expr) => self.fmt_match_expr(expr),
            ExprKind::FnCall(ref expr) => self.fmt_fn_call_expr(expr),
            ExprKind::MethodCall(ref expr) => self.fmt_method_call_expr(expr),
            ExprKind::Closure(ref expr) => self.fmt_closure_expr(expr),
            ExprKind::Return(ref expr) => self.fmt_return_expr(expr),
            ExprKind::MacroCall(ref expr) => self.fmt_macro_call(expr, false),
            ExprKind::Async(ref expr) => self.fmt_async_expr(expr),
            ExprKind::Await(ref expr) => self.fmt_await_expr(expr),
            ExprKind::TryBlock(ref block) => self.fmt_try_block_expr(block),
            ExprKind::ConstBlock(ref expr) => self.fmt_const_expr(expr),
            ExprKind::Yield(ref expr) => self.fmt_yield_expr(expr),
            ExprKind::Err(ref s) => self.fmt_loc_str(s),
        }
        self.block_locs.pop();
    }

    fn fmt_symbol_expr(&mut self, sym: &str) {
        self.raw_insert(sym)
    }

    fn fmt_literal_expr(&mut self, expr: &LocStr) {
        self.fmt_loc_str(expr);
    }

    fn fmt_path_expr(&mut self, expr: &PathExpr) {
        self.fmt_path_type(expr, true);
    }

    fn fmt_box_expr(&mut self, expr: &Expr) {
        self.insert("box ");
        self.fmt_expr(expr);
    }

    fn fmt_ref_expr(&mut self, expr: &RefExpr) {
        let head = &ref_head(&None, expr.is_raw, expr.is_mut);
        maybe_wrap!(self, head, head, expr.expr, fmt_expr);
    }

    fn fmt_unary_op_expr(&mut self, expr: &UnaryOpExpr) {
        maybe_wrap!(self, expr.op, expr.op, expr.expr, fmt_expr);
    }

    fn fmt_try_expr(&mut self, expr: &Expr) {
        self.fmt_expr(expr);
        self.raw_insert("?");
    }

    fn fmt_list_op_expr(&mut self, expr: &ListOpExpr) {
        fmt_lists!(self, &expr.op, &expr.exprs, fmt_expr);
    }

    fn fmt_repeat_expr(&mut self, expr: &RepeatExpr) {
        self.insert_mark_align("[");
        self.fmt_expr(&expr.value);
        insert_sep!(self, ";", expr.len);
        self.fmt_expr(&expr.len);
        self.insert_unmark_align("]");
    }

    fn fmt_array_expr(&mut self, exprs: &[Expr]) {
        fmt_comma_lists!(self, "[", "]", exprs, fmt_expr);
    }

    fn fmt_tuple_expr(&mut self, exprs: &[Expr]) {
        fmt_comma_lists!(self, "(", ")", exprs, fmt_expr);
    }

    fn fmt_index_expr(&mut self, expr: &IndexExpr) {
        self.fmt_expr(&expr.obj);
        self.insert_mark_align("[");
        self.fmt_expr(&expr.index);
        self.insert_unmark_align("]");
    }

    fn fmt_struct_expr(&mut self, expr: &StructExpr) {
        fmt_qself_path!(self, expr, false);

        if expr.fields.is_empty() && !expr.has_rest {
            self.insert(" {}");
            return;
        }

        self.open_brace();
        let loc = Loc {
            start: self.block_locs.last().unwrap().end,
            ..Default::default()
        };

        self.fmt_struct_field_exprs(&expr.fields);
        if expr.has_rest {
            self.insert_indent();
            self.insert("..");
            if let Some(ref base) = expr.base {
                self.fmt_expr(base);
                self.try_fmt_trailing_comment(&base.loc);
            }
            self.nl();
        }

        self.try_fmt_leading_comments(&loc);
        self.close_brace();
    }

    fn fmt_struct_field_exprs(&mut self, fields: &[StructFieldExpr]) {
        for field in fields {
            self.try_fmt_leading_comments(&field.loc);
            self.insert_indent();
            self.fmt_struct_field_expr(field);
            self.try_fmt_trailing_comment(&field.loc);
            self.nl();
        }
    }

    fn fmt_struct_field_expr(&mut self, field: &StructFieldExpr) {
        self.insert(&field.name);
        let value = field.value.to_string();
        if field.name != value {
            insert_sep!(self, ":", field.value);
            self.fmt_expr(&field.value);
        }
        self.raw_insert(",");
    }

    fn fmt_field_expr(&mut self, expr: &FieldExpr) {
        maybe_wrap!(self, expr);
        self.fmt_expr(&expr.expr);
        self.insert(&format!(".{}", &expr.field));
    }

    fn fmt_type_expr(&mut self, expr: &TypeExpr) {
        self.fmt_expr(&expr.expr);
        maybe_wrap!(self, ": ", ":", expr.ty, fmt_type);
    }

    fn fmt_cast_expr(&mut self, expr: &CastExpr) {
        self.fmt_expr(&expr.expr);
        maybe_wrap!(self, " as ", "as ", expr.ty, fmt_type);
    }

    fn fmt_range_expr(&mut self, expr: &RangeExpr) {
        maybe_wrap!(self, expr);

        if let Some(ref start) = expr.start {
            self.fmt_expr(start);
        }
        if expr.is_inclusive {
            self.insert("..=");
        } else {
            self.insert("..");
        }
        if let Some(ref end) = expr.end {
            self.fmt_expr(end);
        }
    }

    fn fmt_block_expr(&mut self, expr: &BlockExpr) {
        self.block_non_sep = true;
        if let Some(ref label) = expr.label {
            self.insert(&format!("{}: ", label));
        }
        self.fmt_block(&expr.block);
    }

    fn fmt_if_expr(&mut self, expr: &IfExpr) {
        if self.if_stacks == 0 && is_if_one_line(expr) {
            return self.fmt_if_expr_one_line(expr);
        }

        self.block_non_sep = false;
        self.raw_insert("if ");
        self.fmt_expr(&expr.expr);
        self.fmt_block(&expr.block);

        if let Some(ref br) = expr.br {
            self.if_stacks += 1;
            self.block_non_sep = true;
            self.raw_insert(" else ");
            self.fmt_expr(br);
            self.if_stacks -= 1;
        }
    }

    fn fmt_if_expr_one_line(&mut self, expr: &IfExpr) {
        self.block_non_sep = false;
        self.raw_insert("if ");
        self.fmt_expr(&expr.expr);

        let (if_value, else_value) = exract_if_else_value(expr);
        self.raw_insert(" { ");
        self.fmt_expr(if_value);
        self.raw_insert(" } else { ");
        self.fmt_expr(else_value);
        self.raw_insert(" }");
    }

    fn fmt_while_expr(&mut self, expr: &WhileExpr) {
        self.fmt_label(&expr.label);
        self.raw_insert("while ");
        self.fmt_expr(&expr.expr);
        self.fmt_block(&expr.block);
    }

    fn fmt_label(&mut self, label: &Option<String>) {
        if let Some(ref label) = *label {
            self.insert(&format!("{}:", label));
            self.nl();
            self.insert_indent();
        }
    }

    fn fmt_let_expr(&mut self, expr: &LetExpr) {
        self.raw_insert("let ");
        self.fmt_patten(&expr.pattern);
        maybe_wrap!(self, " = ", "= ", expr.expr, fmt_expr);
    }

    fn fmt_for_expr(&mut self, expr: &ForExpr) {
        self.fmt_label(&expr.label);
        self.raw_insert("for ");
        self.fmt_patten(&expr.pattern);
        maybe_wrap!(self, " in ", "in ", expr.expr, fmt_expr);
        self.fmt_block(&expr.block);
    }

    fn fmt_loop_expr(&mut self, expr: &LoopExpr) {
        self.fmt_label(&expr.label);
        self.raw_insert("loop");
        self.fmt_block(&expr.block);
    }

    fn fmt_break_expr(&mut self, expr: &BreakExpr) {
        self.fmt_jump_label("break", &expr.label);
        if let Some(ref expr) = expr.expr {
            self.raw_insert(" ");
            self.fmt_expr(expr);
        }
    }

    fn fmt_continue_expr(&mut self, expr: &ContinueExpr) {
        self.fmt_jump_label("continue", &expr.label);
    }

    fn fmt_jump_label(&mut self, keyword: &str, label: &Option<String>) {
        let label = if let Some(ref label) = *label {
            format!(" {}", label)
        } else {
            String::new()
        };
        self.insert(&format!("{}{}", keyword, label));
    }

    fn fmt_match_expr(&mut self, expr: &MatchExpr) {
        self.raw_insert("match ");
        self.fmt_expr(&expr.expr);
        fmt_block!(self, &expr.arms, fmt_arms);
    }

    fn fmt_arms(&mut self, arms: &[Arm]) {
        fmt_items!(self, arms, fmt_arm);
    }

    fn fmt_arm(&mut self, arm: &Arm) {
        self.fmt_patten(&arm.pattern);
        if let Some(ref guard) = arm.guard {
            maybe_wrap!(self, " if ", "if ", guard, fmt_expr);
        }

        if is_block_expr(&arm.body) {
            self.raw_insert(" => ");
            self.fmt_expr(&arm.body);
        } else {
            self.raw_insert(" =>");
            maybe_wrap!(self, " ", "", &arm.body, fmt_expr);
        }
        self.raw_insert(",");
    }

    fn fmt_fn_call_expr(&mut self, expr: &FnCallExpr) {
        self.fmt_expr(&expr.name);
        fmt_comma_lists!(self, "(", ")", &expr.params, fmt_expr);
    }

    fn fmt_method_call_expr(&mut self, expr: &MethodCallExpr) {
        self.fmt_expr(&expr.args[0]);
        maybe_nl!(self, expr.path);
        self.raw_insert(".");
        self.fmt_path_segment(&expr.path, true);
        fmt_comma_lists!(self, "(", ")", &expr.args[1..], fmt_expr);
    }

    fn fmt_closure_expr(&mut self, expr: &ClosureExpr) {
        self.insert(&closure_head(expr.is_static, expr.is_async, expr.is_move));
        self.fmt_closure_args(&expr.sig.params);
        self.fmt_fn_return(&expr.sig.ret);

        match expr.expr.expr {
            ExprKind::Block(ref block) => self.fmt_block(&block.block),
            _ => {
                self.raw_insert(" ");
                self.fmt_expr(&expr.expr);
            },
        }
    }

    fn fmt_closure_args(&mut self, args: &[Param]) {
        fmt_comma_lists!(self, "|", "|", args, fmt_closure_arg);
    }

    fn fmt_closure_arg(&mut self, arg: &Param) {
        maybe_nl!(self, arg);
        maybe_wrap!(self, arg);

        self.fmt_patten(&arg.pattern);
        match arg.ty.ty {
            TypeKind::Symbol(ref s) if s == &"_" => {},
            _ => {
                self.raw_insert(": ");
                self.fmt_type(&arg.ty)
            },
        }
    }

    fn fmt_return_expr(&mut self, expr: &ReturnExpr) {
        self.raw_insert("return");
        if let Some(ref expr) = expr.ret {
            maybe_wrap!(self, " ", "", expr, fmt_expr);
        }
    }

    fn fmt_async_expr(&mut self, expr: &AsyncExpr) {
        self.raw_insert(&async_head(expr.is_move));
        self.fmt_block(&expr.block);
    }

    fn fmt_await_expr(&mut self, expr: &Expr) {
        self.fmt_expr(expr);
        self.insert(".await");
    }

    fn fmt_try_block_expr(&mut self, block: &Block) {
        self.raw_insert("try");
        self.fmt_block(block);
    }

    fn fmt_const_expr(&mut self, expr: &Expr) {
        self.raw_insert("const ");
        self.fmt_expr(expr);
    }

    fn fmt_yield_expr(&mut self, expr: &YieldExpr) {
        self.raw_insert("yield");
        if let Some(ref expr) = expr.expr {
            maybe_wrap!(self, " ", "", expr, fmt_expr);
        }
    }

    fn fmt_macro_def(&mut self, item: &MacroDef) {
        self.raw_insert(&format!("macro_rules! {} ", item.name));
        self.force_insert(&item.s);
    }

    fn fmt_macro_stmt(&mut self, mac: &MacroStmt) {
        self.try_fmt_leading_comments(&mac.loc);
        self.fmt_attrs(&mac.attrs);
        self.insert_indent();

        let mut loc = mac.loc;
        self.fmt_macro_call(&mac.mac, false);
        if mac.is_semi {
            self.raw_insert(";");
            loc.end += 1;
        }

        self.try_fmt_trailing_comment(&loc);
        self.nl();
    }

    fn fmt_macro_call(&mut self, item: &MacroCall, semi: bool) {
        match item {
            MacroCall::Raw(ref s) => self.fmt_loc_str(s),
            MacroCall::Expr(ref expr) => {
                self.fmt_macro_expr(expr);
                if semi {
                    self.raw_insert(";");
                }
            },
        }
    }

    fn fmt_macro_expr(&mut self, mac: &MacroExpr) {
        self.insert(&format!("{}!", mac.name));

        let (open, close) = match mac.style {
            MacroStyle::Paren => ("(", ")"),
            MacroStyle::Bracket => ("[", "]"),
            MacroStyle::Brace => ("{", "}"),
        };

        if mac.seps.is_empty() {
            fmt_sep_lists!(self, open, "", close, &mac.exprs, fmt_expr);
            return;
        }

        let mut first = true;
        let mut first_nl = false;
        let mut expr_idx = 0;
        let mut sep_idx = 0;
        while expr_idx < mac.exprs.len() && sep_idx < mac.seps.len() {
            let expr = &mac.exprs[expr_idx];
            let sep = &mac.seps[sep_idx];

            let nl = if expr.loc.start < sep.loc.start {
                expr.loc.nl
            } else {
                sep.loc.nl
            };
            if first {
                if nl {
                    first_nl = true;
                    self.raw_insert(open);
                    self.indent();
                } else {
                    self.insert_mark_align(open);
                }
                first = false;
            }

            if expr.loc.start < sep.loc.start {
                if first_nl && expr.loc.nl {
                    self.nl_indent();
                }
                self.fmt_expr(expr);
                expr_idx += 1;
            } else {
                if sep.is_sep {
                    if expr.loc.nl {
                        self.raw_insert(&sep.s);
                    } else {
                        insert_sep!(self, &sep.s, expr);
                    }
                } else {
                    self.insert(&sep.s);
                }
                sep_idx += 1;
            }
        }

        while expr_idx < mac.exprs.len() {
            let expr = &mac.exprs[expr_idx];
            if first_nl && expr.loc.nl {
                self.nl_indent();
            }
            self.fmt_expr(expr);
            expr_idx += 1;
        }

        if first_nl {
            self.outdent();
            self.nl_indent();
            self.raw_insert(close);
        } else {
            self.insert_unmark_align(close);
        }
    }

    fn clear_flag(&mut self) {
        self.after_indent = false;
        self.after_wrap = false;
    }

    fn insert_indent(&mut self) {
        self.ts.insert_indent();
        self.after_indent = true;
    }

    fn force_insert(&mut self, s: &str) {
        self.ts.force_insert(s);
        self.clear_flag();
    }

    fn raw_insert(&mut self, s: &str) {
        if !s.is_empty() {
            self.ts.raw_insert(s);
            self.clear_flag();
        }
    }

    fn insert(&mut self, s: &str) {
        if !s.is_empty() {
            self.ts.insert(s);
            self.clear_flag();
        }
    }

    fn nl(&mut self) {
        self.ts.nl();
        self.clear_flag();
    }

    fn wrap(&mut self) {
        if !self.after_indent && !self.after_wrap {
            self.ts.wrap();
            self.after_wrap = true;
        }
    }

    fn insert_mark_align(&mut self, s: &str) {
        self.ts.insert_mark_align(s);
        self.clear_flag();
    }

    fn insert_unmark_align(&mut self, s: &str) {
        self.ts.insert_unmark_align(s);
        self.clear_flag();
    }

    fn nl_indent(&mut self) {
        if !self.after_indent {
            self.ts.nl_indent();
            self.after_indent = true;
        }
    }

    fn indent(&mut self) {
        self.ts.indent();
    }

    fn outdent(&mut self) {
        self.ts.outdent();
    }

    fn open_brace(&mut self) {
        self.raw_insert(" {");
        self.indent();
        self.nl();
    }

    fn close_brace(&mut self) {
        self.outdent();
        self.insert_indent();
        self.raw_insert("}");
    }
}
