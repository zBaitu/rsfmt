use std::fmt::{self, Debug, Display};

pub type Pos = u32;

#[derive(Clone, Copy, Default)]
pub struct Loc {
    pub start: Pos,
    pub end: Pos,
    pub nl: bool,
}

impl Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.nl {
            write!(f, "Loc({}, {}, nl)", self.start, self.end)
        } else {
            write!(f, "Loc({}, {})", self.start, self.end)
        }
    }
}

impl Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Debug, PartialEq)]
pub enum CommentKind {
    Leading,
    Trailing,
}

#[derive(Debug)]
pub struct Comment {
    pub pos: Pos,
    pub kind: CommentKind,
    pub lines: Vec<String>,
}

#[derive(Debug, Default)]
pub struct LocStr {
    pub loc: Loc,
    pub s: String,
}

impl LocStr {
    pub fn new<S>(loc: Loc, s: S) -> LocStr where S: Into<String> {
        LocStr {
            loc,
            s: s.into(),
        }
    }

    pub fn from<S>(s: S) -> LocStr where S: Into<String> {
        LocStr {
            loc: Default::default(),
            s: s.into(),
        }
    }
}

#[derive(Debug)]
pub struct Crate {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct Attr {
    pub loc: Loc,
    pub is_inner: bool,
    pub attr: AttrKind,
}

#[derive(Debug)]
pub enum AttrKind {
    Doc(DocAttr),
    Attr(MetaAttr),
    Raw(LocStr),
}

#[derive(Debug)]
pub struct DocAttr {
    pub is_block: bool,
    pub doc: LocStr,
}

#[derive(Debug)]
pub struct MetaAttr {
    pub loc: Loc,
    pub name: LocStr,
    pub metas: Option<Vec<MetaAttr>>,
}

pub type Vis = String;

#[derive(Debug)]
pub struct Item {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub vis: Vis,
    pub item: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    ExternCrate(ExternCrate),
    Use(Use),
    ModDecl(ModDecl),
    Mod(Mod),
    TypeAlias(TypeAlias),
    TraitAlias(TraitAlias),
    Const(Const),
    Static(Static),
    Struct(Struct),
    Union(Union),
    Enum(Enum),
    ForeignMod(ForeignMod),
    Fn(Fn),
    Trait(Trait),
    Impl(Impl),
    MacroDef(MacroDef),
    MacroCall(MacroCall),
}

#[derive(Debug)]
pub struct ExternCrate {
    pub name: String,
}

#[derive(Debug)]
pub struct Use {
    pub path: String,
    pub trees: Option<Vec<UseTree>>,
}

#[derive(Debug)]
pub struct UseTree {
    pub loc: Loc,
    pub path: String,
    pub trees: Option<Vec<UseTree>>,
}

#[derive(Debug)]
pub struct ModDecl {
    pub is_unsafe: bool,
    pub name: String,
}

#[derive(Debug)]
pub struct Mod {
    pub is_unsafe: bool,
    pub name: String,
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct TypeAlias {
    pub is_default: bool,
    pub name: String,
    pub generics: Generics,
    pub bounds: TypeParamBounds,
    pub ty: Option<Type>,
}

#[derive(Debug)]
pub struct TraitAlias {
    pub name: String,
    pub generics: Generics,
    pub bounds: TypeParamBounds,
}

#[derive(Debug)]
pub struct Generics {
    pub lifetime_defs: Vec<LifetimeDef>,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub wh: Where,
}

impl Generics {
    pub fn is_empty(&self) -> bool {
        self.lifetime_defs.is_empty() && self.type_params.is_empty() && self.const_params.is_empty()
    }
}

#[derive(Debug)]
pub struct LifetimeDef {
    pub loc: Loc,
    pub lifetime: Lifetime,
    pub bounds: Vec<Lifetime>,
}

pub type Lifetime = LocStr;

#[derive(Debug)]
pub struct TypeParam {
    pub loc: Loc,
    pub name: String,
    pub bounds: TypeParamBounds,
    pub default: Option<Type>,
}

#[derive(Debug)]
pub enum TypeParamBound {
    Lifetime(Lifetime),
    PolyTraitRef(PolyTraitRef),
}

#[derive(Debug)]
pub struct TypeParamBounds(pub Vec<TypeParamBound>);

impl TypeParamBounds {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug)]
pub struct PolyTraitRef {
    pub loc: Loc,
    pub lifetime_defs: Vec<LifetimeDef>,
    pub trait_ref: TraitRef,
}

impl PolyTraitRef {
    pub fn new_sized(loc: Loc) -> PolyTraitRef {
        PolyTraitRef {
            loc,
            lifetime_defs: Vec::new(),
            trait_ref: TraitRef::new_sized(loc),
        }
    }
}

pub type TraitRef = Path;

#[derive(Debug)]
pub struct ConstParam {
    pub loc: Loc,
    pub name: String,
    pub ty: Type,
    pub default: Option<Expr>,
}

#[derive(Debug)]
pub struct Where {
    pub loc: Loc,
    pub clauses: Vec<WhereClause>,
}

impl Where {
    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }
}

#[derive(Debug)]
pub struct WhereClause {
    pub loc: Loc,
    pub clause: WhereKind,
}

#[derive(Debug)]
pub enum WhereKind {
    LifetimeDef(LifetimeDef),
    Bound(WhereBound),
}

#[derive(Debug)]
pub struct WhereBound {
    pub lifetime_defs: Vec<LifetimeDef>,
    pub ty: Type,
    pub bounds: TypeParamBounds,
}

#[derive(Debug)]
pub struct Path {
    pub loc: Loc,
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn new_sized(loc: Loc) -> Path {
        Path {
            loc,
            segments: vec![PathSegment::new_sized()],
        }
    }
}

#[derive(Debug)]
pub struct PathSegment {
    pub loc: Loc,
    pub name: String,
    pub param: PathParam,
}

impl PathSegment {
    pub fn new_sized() -> PathSegment {
        PathSegment {
            loc: Default::default(),
            name: "?Sized".to_string(),
            param: PathParam::Angle(Default::default()),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self.param {
            PathParam::Angle(ref param) => param.is_empty(),
            PathParam::Paren(ref param) => param.is_empty(),
        }
    }
}

#[derive(Debug)]
pub enum PathParam {
    Angle(AngleParam),
    Paren(ParenParam),
}

#[derive(Debug, Default)]
pub struct AngleParam {
    pub lifetimes: Vec<Lifetime>,
    pub types: Vec<Type>,
    pub consts: Vec<Expr>,
    pub bindings: Vec<TypeBinding>,
}

impl AngleParam {
    pub fn is_empty(&self) -> bool {
        self.lifetimes.is_empty() && self.types.is_empty() && self.consts.is_empty() && self.bindings.is_empty()
    }
}

#[derive(Debug)]
pub enum TypeBindingKind {
    Eq(Type),
    Bound(TypeParamBounds),
}

#[derive(Debug)]
pub struct TypeBinding {
    pub loc: Loc,
    pub name: String,
    pub binding: TypeBindingKind,
}

#[derive(Debug)]
pub struct ParenParam {
    pub loc: Loc,
    pub inputs: Vec<Type>,
    pub output: Option<Type>,
}

impl ParenParam {
    pub fn is_empty(&self) -> bool {
        self.inputs.is_empty()
    }
}

#[derive(Debug)]
pub struct QSelf {
    pub ty: Type,
    pub pos: usize,
}

#[derive(Debug)]
pub struct Type {
    pub loc: Loc,
    pub ty: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Symbol(&'static str),
    Path(Box<PathType>),
    Ptr(Box<PtrType>),
    Ref(Box<RefType>),
    Tuple(Box<TupleType>),
    Slice(Box<SliceType>),
    Array(Box<ArrayType>),
    Struct(StructType),
    Union(UnionType),
    Trait(Box<TraitType>),
    BareFn(Box<BareFnType>),
    MacroCall(MacroCall),
    Err(LocStr),
}

#[derive(Debug)]
pub struct PathType {
    pub qself: Option<QSelf>,
    pub path: Path,
}

#[derive(Debug)]
pub struct PtrType {
    pub is_mut: bool,
    pub ty: Type,
}

#[derive(Debug)]
pub struct RefType {
    pub lifetime: Option<Lifetime>,
    pub is_mut: bool,
    pub ty: Type,
}

#[derive(Debug)]
pub struct TupleType {
    pub types: Vec<Type>,
}

#[derive(Debug)]
pub struct SliceType {
    pub ty: Type,
}

#[derive(Debug)]
pub struct ArrayType {
    pub ty: Type,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct UnionType {
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct TraitType {
    pub is_dyn: bool,
    pub is_impl: bool,
    pub bounds: TypeParamBounds,
}

#[derive(Debug)]
pub struct BareFnType {
    pub lifetime_defs: Vec<LifetimeDef>,
    pub header: FnHeader,
    pub sig: FnSig,
}

#[derive(Debug)]
pub struct Const {
    pub is_default: bool,
    pub name: String,
    pub ty: Type,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct Static {
    pub is_mut: bool,
    pub name: String,
    pub ty: Type,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub generics: Generics,
    pub body: StructBody,
}

#[derive(Debug)]
pub enum StructBody {
    Struct(Vec<StructField>),
    Tuple(Vec<TupleField>),
    Unit,
}

#[derive(Debug)]
pub struct StructField {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub vis: Vis,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct TupleField {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub vis: Vis,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Union {
    pub name: String,
    pub generics: Generics,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct Enum {
    pub name: String,
    pub generics: Generics,
    pub body: EnumBody,
}

#[derive(Debug)]
pub struct EnumBody {
    pub fields: Vec<EnumField>,
}

#[derive(Debug)]
pub struct EnumField {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub name: String,
    pub body: StructBody,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct FnSig {
    pub params: Vec<Param>,
    pub ret: Return,
}

#[derive(Debug)]
pub struct Param {
    pub loc: Loc,
    pub pattern: Pattern,
    pub ty: Type,
    pub has_patten: bool,
}

#[derive(Debug)]
pub struct Return {
    pub nl: bool,
    pub ret: Option<Type>,
}

#[derive(Debug, Default)]
pub struct FnHeader {
    pub is_unsafe: bool,
    pub is_async: bool,
    pub is_const: bool,
    pub ext: Option<String>,
}

#[derive(Debug)]
pub struct Fn {
    pub is_default: bool,
    pub header: FnHeader,
    pub name: String,
    pub generics: Generics,
    pub sig: FnSig,
    pub block: Option<Block>,
}

#[derive(Debug)]
pub struct ForeignMod {
    pub abi: Option<String>,
    pub items: Vec<ForeignItem>,
}

#[derive(Debug)]
pub struct ForeignItem {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub vis: Vis,
    pub item: ForeignKind,
}

#[derive(Debug)]
pub enum ForeignKind {
    TypeAlias(TypeAlias),
    Static(Static),
    Fn(Fn),
    MacroCall(MacroCall),
}

#[derive(Debug)]
pub struct Trait {
    pub is_auto: bool,
    pub is_unsafe: bool,
    pub name: String,
    pub generics: Generics,
    pub bounds: TypeParamBounds,
    pub items: Vec<TraitItem>,
}

#[derive(Debug)]
pub struct TraitItem {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub item: TraitItemKind,
}

#[derive(Debug)]
pub enum TraitItemKind {
    Const(Const),
    TypeAlias(TypeAlias),
    Fn(Fn),
    MacroCall(MacroCall),
}

#[derive(Debug)]
pub struct Impl {
    pub is_unsafe: bool,
    pub is_default: bool,
    pub is_neg: bool,
    pub generics: Generics,
    pub trait_ref: Option<TraitRef>,
    pub ty: Type,
    pub items: Vec<ImplItem>,
}

#[derive(Debug)]
pub struct ImplItem {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub vis: Vis,
    pub item: ImplItemKind,
}

#[derive(Debug)]
pub enum ImplItemKind {
    Const(Const),
    TypeAlias(TypeAlias),
    Fn(Fn),
    MacroCall(MacroCall),
}

#[derive(Debug)]
pub struct Block {
    pub loc: Loc,
    pub is_unsafe: bool,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn is_one_line(&self) -> bool {
        if self.stmts.len() != 1 {
            return false;
        }

        match &self.stmts[0].stmt {
            StmtKind::Expr(ref expr, _) => {
                if expr.loc.nl {
                    return false
                }
                match expr.expr {
                    ExprKind::Literal(_) | ExprKind::Path(_) | ExprKind::FnCall(_) | ExprKind::MethodCall(_)
                            | ExprKind::Ref(_) | ExprKind::ListOp(_) => true,
                    ExprKind::MacroCall(ref mac) => matches!(mac, MacroCall::Expr(_)),
                    _ => false,
                }
            },
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub loc: Loc,
    pub stmt: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    None,
    Item(Box<Item>),
    Let(Box<Let>),
    Expr(Expr, bool),
    Macro(MacroStmt),
}

#[derive(Debug)]
pub struct Let {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub pattern: Pattern,
    pub ty: Option<Type>,
    pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct Pattern {
    pub loc: Loc,
    pub pattern: PattenKind,
}

#[derive(Debug)]
pub enum PattenKind {
    Symbol(&'static str),
    Literal(Expr),
    Box(Box<Pattern>),
    Range(Box<RangePatten>),
    Ref(Box<RefPatten>),
    Path(PathPatten),
    Ident(Box<IdentPatten>),
    Struct(StructPatten),
    Enum(EnumPatten),
    Or(OrPatten),
    Tuple(TuplePatten),
    Slice(Box<SlicePatten>),
    MacroCall(MacroCall),
}

#[derive(Debug)]
pub struct RangePatten {
    pub start: Option<Expr>,
    pub end: Option<Expr>,
    pub is_inclusive: bool,
}

#[derive(Debug)]
pub struct RefPatten {
    pub is_mut: bool,
    pub pattern: Pattern,
}

pub type PathPatten = PathType;

#[derive(Debug)]
pub struct IdentPatten {
    pub is_ref: bool,
    pub is_mut: bool,
    pub name: String,
    pub pattern: Option<Pattern>,
}

#[derive(Debug)]
pub struct StructPatten {
    pub qself: Option<QSelf>,
    pub path: Path,
    pub fields: Vec<StructFieldPatten>,
    pub omit: bool,
}

#[derive(Debug)]
pub struct StructFieldPatten {
    pub loc: Loc,
    pub name: String,
    pub pattern: Pattern,
    pub shorthand: bool,
}

#[derive(Debug)]
pub struct EnumPatten {
    pub qself: Option<QSelf>,
    pub path: Path,
    pub patterns: Vec<Pattern>,
}

#[derive(Debug)]
pub struct OrPatten {
    pub patterns: Vec<Pattern>,
}

#[derive(Debug)]
pub struct TuplePatten {
    pub patterns: Vec<Pattern>,
}

#[derive(Debug)]
pub struct SlicePatten {
    pub patterns: Vec<Pattern>,
}

#[derive(Debug)]
pub struct Expr {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub expr: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Symbol(&'static str),
    Literal(LocStr),
    Path(PathExpr),
    Box(Box<Expr>),
    Ref(Box<RefExpr>),
    UnaryOp(Box<UnaryOpExpr>),
    Try(Box<Expr>),
    ListOp(Box<ListOpExpr>),
    Repeat(Box<RepeatExpr>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Index(Box<IndexExpr>),
    Struct(Box<StructExpr>),
    Field(Box<FieldExpr>),
    Type(Box<TypeExpr>),
    Cast(Box<CastExpr>),
    Range(Box<RangeExpr>),
    Block(Box<BlockExpr>),
    If(Box<IfExpr>),
    While(Box<WhileExpr>),
    Let(Box<LetExpr>),
    For(Box<ForExpr>),
    Loop(Box<LoopExpr>),
    Break(Box<BreakExpr>),
    Continue(Box<ContinueExpr>),
    Match(Box<MatchExpr>),
    FnCall(Box<FnCallExpr>),
    MethodCall(Box<MethodCallExpr>),
    Closure(Box<ClosureExpr>),
    Return(Box<ReturnExpr>),
    MacroCall(MacroCall),
    Async(AsyncExpr),
    Await(Box<Expr>),
    TryBlock(Block),
    ConstBlock(Box<Expr>),
    Yield(Box<YieldExpr>),
    Err(LocStr),
}

pub type PathExpr = PathType;

#[derive(Debug)]
pub struct RefExpr {
    pub is_raw: bool,
    pub is_mut: bool,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct UnaryOpExpr {
    pub op: &'static str,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct ListOpExpr {
    pub op: LocStr,
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct RepeatExpr {
    pub value: Expr,
    pub len: Expr,
}

#[derive(Debug)]
pub struct IndexExpr {
    pub obj: Expr,
    pub index: Expr,
}

#[derive(Debug)]
pub struct StructExpr {
    pub qself: Option<QSelf>,
    pub path: Path,
    pub fields: Vec<StructFieldExpr>,
    pub has_rest: bool,
    pub base: Option<Expr>,
}

#[derive(Debug)]
pub struct StructFieldExpr {
    pub loc: Loc,
    pub name: String,
    pub value: Expr,
}

#[derive(Debug)]
pub struct FieldExpr {
    pub expr: Expr,
    pub field: String,
}

#[derive(Debug)]
pub struct TypeExpr {
    pub expr: Expr,
    pub ty: Type,
}

#[derive(Debug)]
pub struct CastExpr {
    pub expr: Expr,
    pub ty: Type,
}

#[derive(Debug)]
pub struct RangeExpr {
    pub start: Option<Expr>,
    pub end: Option<Expr>,
    pub is_inclusive: bool,
}

#[derive(Debug)]
pub struct BlockExpr {
    pub label: Option<String>,
    pub block: Block,
}

#[derive(Debug)]
pub struct IfExpr {
    pub expr: Expr,
    pub block: Block,
    pub br: Option<Expr>,
}

#[derive(Debug)]
pub struct WhileExpr {
    pub label: Option<String>,
    pub expr: Expr,
    pub block: Block,
}

#[derive(Debug)]
pub struct LetExpr {
    pub pattern: Pattern,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct ForExpr {
    pub label: Option<String>,
    pub pattern: Pattern,
    pub expr: Expr,
    pub block: Block,
}

#[derive(Debug)]
pub struct LoopExpr {
    pub label: Option<String>,
    pub block: Block,
}

#[derive(Debug)]
pub struct BreakExpr {
    pub label: Option<String>,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct ContinueExpr {
    pub label: Option<String>,
}

#[derive(Debug)]
pub struct MatchExpr {
    pub expr: Expr,
    pub arms: Vec<Arm>,
}

#[derive(Debug)]
pub struct Arm {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug)]
pub struct FnCallExpr {
    pub name: Expr,
    pub params: Vec<Expr>,
}

#[derive(Debug)]
pub struct MethodCallExpr {
    pub path: PathSegment,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct ClosureExpr {
    pub is_static: bool,
    pub is_async: bool,
    pub is_move: bool,
    pub sig: FnSig,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct ReturnExpr {
    pub ret: Option<Expr>,
}

#[derive(Debug)]
pub struct AsyncExpr {
    pub is_move: bool,
    pub block: Block,
}

#[derive(Debug)]
pub struct YieldExpr {
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct MacroDef {
    pub name: String,
    pub s: String,
}

#[derive(Debug)]
pub struct MacroStmt {
    pub loc: Loc,
    pub attrs: Vec<Attr>,
    pub mac: MacroCall,
    pub is_semi: bool,
}

#[derive(Debug)]
pub enum MacroCall {
    Raw(LocStr),
    Expr(MacroExpr),
}

#[derive(Debug)]
pub enum MacroStyle {
    Paren,
    Bracket,
    Brace,
}

#[derive(Debug)]
pub struct MacroSep {
    pub loc: Loc,
    pub is_sep: bool,
    pub s: String,
}

#[derive(Debug)]
pub struct MacroExpr {
    pub name: String,
    pub style: MacroStyle,
    pub exprs: Vec<Expr>,
    pub seps: Vec<MacroSep>,
}
