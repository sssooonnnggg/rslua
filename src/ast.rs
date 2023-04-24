use crate::tokens::{Token, TokenType};
use crate::types::{FloatType, IntType};
use rslua_traits::Comments;

#[derive(Clone, PartialEq, Debug)]
pub enum UnOp {
    Minus(Token),
    BNot(Token),
    Not(Token),
    Len(Token),
    None,
}

impl Comments for UnOp {
    fn get_comments(&self) -> Vec<&str> {
        match self {
            UnOp::Minus(token) => token.get_comments(),
            UnOp::BNot(token) => token.get_comments(),
            UnOp::Not(token) => token.get_comments(),
            UnOp::Len(token) => token.get_comments(),
            UnOp::None => vec![],
        }
    }
}

impl UnOp {
    pub fn from_token(token: Token) -> UnOp {
        match token.t {
            TokenType::Minus => UnOp::Minus(token),
            TokenType::BXor => UnOp::BNot(token),
            TokenType::Not => UnOp::Not(token),
            TokenType::Len => UnOp::Len(token),
            _ => UnOp::None,
        }
    }
    pub fn priority(&self) -> u8 {
        match self {
            _ => 12,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinOp {
    Add(Token),
    Minus(Token),
    Mul(Token),
    Mod(Token),
    Pow(Token),
    Div(Token),
    IDiv(Token),
    BAnd(Token),
    BOr(Token),
    BXor(Token),
    Shl(Token),
    Shr(Token),
    Concat(Token),
    Ne(Token),
    Eq(Token),
    Lt(Token),
    Le(Token),
    Gt(Token),
    Ge(Token),
    And(Token),
    Or(Token),
    None,
}

pub struct BinOpPriority {
    pub left: u8,
    pub right: u8,
}

impl BinOp {
    pub fn from_token(token: Token) -> BinOp {
        match token.t {
            TokenType::Add => BinOp::Add(token),
            TokenType::Minus => BinOp::Minus(token),
            TokenType::Mul => BinOp::Mul(token),
            TokenType::Mod => BinOp::Mod(token),
            TokenType::Pow => BinOp::Pow(token),
            TokenType::Div => BinOp::Div(token),
            TokenType::IDiv => BinOp::IDiv(token),
            TokenType::BAnd => BinOp::BAnd(token),
            TokenType::BOr => BinOp::BOr(token),
            TokenType::BXor => BinOp::BXor(token),
            TokenType::Shl => BinOp::Shl(token),
            TokenType::Shr => BinOp::Shr(token),
            TokenType::Concat => BinOp::Concat(token),
            TokenType::Ne => BinOp::Ne(token),
            TokenType::Eq => BinOp::Eq(token),
            TokenType::Lt => BinOp::Lt(token),
            TokenType::Le => BinOp::Le(token),
            TokenType::Gt => BinOp::Gt(token),
            TokenType::Ge => BinOp::Ge(token),
            TokenType::And => BinOp::And(token),
            TokenType::Or => BinOp::Or(token),
            _ => BinOp::None,
        }
    }

    pub fn priority(&self) -> BinOpPriority {
        match self {
            BinOp::Or(_) => BinOpPriority { left: 1, right: 1 },
            BinOp::And(_) => BinOpPriority { left: 2, right: 2 },
            BinOp::Eq(_)
            | BinOp::Ne(_)
            | BinOp::Lt(_)
            | BinOp::Gt(_)
            | BinOp::Le(_)
            | BinOp::Ge(_) => BinOpPriority { left: 3, right: 3 },
            BinOp::BOr(_) => BinOpPriority { left: 4, right: 4 },
            BinOp::BXor(_) => BinOpPriority { left: 5, right: 5 },
            BinOp::BAnd(_) => BinOpPriority { left: 6, right: 6 },
            BinOp::Shl(_) | BinOp::Shr(_) => BinOpPriority { left: 7, right: 7 },
            BinOp::Concat(_) => BinOpPriority { left: 9, right: 8 },
            BinOp::Add(_) | BinOp::Minus(_) => BinOpPriority {
                left: 10,
                right: 10,
            },
            BinOp::Mul(_) | BinOp::Mod(_) | BinOp::Div(_) | BinOp::IDiv(_) => BinOpPriority {
                left: 11,
                right: 11,
            },
            BinOp::Pow(_) => BinOpPriority {
                left: 14,
                right: 13,
            },
            _ => unreachable!(),
        }
    }

    pub fn is_comp(&self) -> bool {
        match self {
            BinOp::Le(_)
            | BinOp::Ge(_)
            | BinOp::Ne(_)
            | BinOp::Eq(_)
            | BinOp::Lt(_)
            | BinOp::Gt(_) => true,
            _ => false,
        }
    }
}

impl Comments for BinOp {
    fn get_comments(&self) -> Vec<&str> {
        // refactor this
        match self {
            BinOp::Add(token) => token.get_comments(),
            BinOp::Minus(token) => token.get_comments(),
            BinOp::Mul(token) => token.get_comments(),
            BinOp::Mod(token) => token.get_comments(),
            BinOp::Pow(token) => token.get_comments(),
            BinOp::Div(token) => token.get_comments(),
            BinOp::IDiv(token) => token.get_comments(),
            BinOp::BAnd(token) => token.get_comments(),
            BinOp::BOr(token) => token.get_comments(),
            BinOp::BXor(token) => token.get_comments(),
            BinOp::Shl(token) => token.get_comments(),
            BinOp::Shr(token) => token.get_comments(),
            BinOp::Concat(token) => token.get_comments(),
            BinOp::Ne(token) => token.get_comments(),
            BinOp::Eq(token) => token.get_comments(),
            BinOp::Lt(token) => token.get_comments(),
            BinOp::Le(token) => token.get_comments(),
            BinOp::Gt(token) => token.get_comments(),
            BinOp::Ge(token) => token.get_comments(),
            BinOp::And(token) => token.get_comments(),
            BinOp::Or(token) => token.get_comments(),
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Nil(Token),
    True(Token),
    False(Token),
    VarArg(Token),
    Float(FloatExpr),
    Int(IntExpr),
    String(StringExpr),
    Name(StringExpr),
    ParenExpr(Box<Expr>),
    FuncBody(FuncBody),
    Table(Table),
    BinExpr(BinExpr),
    UnExpr(UnExpr),
    SuffixedExpr(SuffixedExpr),
}

impl Expr {
    pub fn to_assignable(self) -> Assignable {
        match self {
            Expr::Name(s) => Assignable::Name(s),
            Expr::SuffixedExpr(s) => Assignable::SuffixedExpr(s),
            _ => unreachable!(),
        }
    }

    pub fn unwrap_as_int(&self) -> IntType {
        match &self {
            Expr::Int(expr) => expr.value(),
            _ => unreachable!(),
        }
    }

    pub fn unwrap_as_float(&self) -> FloatType {
        match &self {
            Expr::Float(expr) => expr.value(),
            _ => unreachable!(),
        }
    }

    pub fn unwrap_as_string(&self) -> String {
        match &self {
            Expr::String(expr) => expr.value(),
            _ => unreachable!(),
        }
    }

    pub fn unwrap_as_name(&self) -> &StringExpr {
        match &self {
            Expr::Name(expr) => expr,
            _ => unreachable!(),
        }
    }
}

impl Comments for Expr {
    fn get_comments(&self) -> Vec<&str> {
        unimplemented!()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Assignable {
    Name(StringExpr),
    SuffixedExpr(SuffixedExpr),
}

impl Assignable {
    pub fn unwrap_as_name(&self) -> &StringExpr {
        match &self {
            Assignable::Name(name) => name,
            _ => unreachable!(),
        }
    }
    pub fn unwrap_as_suffix(&self) -> &SuffixedExpr {
        match &self {
            Assignable::SuffixedExpr(suffix) => suffix,
            _ => unreachable!(),
        }
    }
}

impl Comments for Assignable {
    fn get_comments(&self) -> Vec<&str> {
        unimplemented!()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FloatExpr {
    pub token: Token,
}

impl FloatExpr {
    pub fn value(&self) -> FloatType {
        self.token.get_float()
    }
}

impl Comments for FloatExpr {
    fn get_comments(&self) -> Vec<&str> {
        self.token.get_comments()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IntExpr {
    pub token: Token,
}

impl IntExpr {
    pub fn value(&self) -> IntType {
        self.token.get_int()
    }
}

impl Comments for IntExpr {
    fn get_comments(&self) -> Vec<&str> {
        self.token.get_comments()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct StringExpr {
    pub token: Token,
}

impl StringExpr {
    pub fn value(&self) -> String {
        self.token.get_string()
    }
}

impl Comments for StringExpr {
    fn get_comments(&self) -> Vec<&str> {
        self.token.get_comments()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct SuffixedExpr {
    pub primary: Box<Expr>,
    pub suffixes: Vec<Suffix>,
}

impl Comments for SuffixedExpr {
    fn get_comments(&self) -> Vec<&str> {
        unimplemented!()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Suffix {
    // '.' NAME
    Attr(Token, StringExpr),
    // '[' expr ']'
    Index(Token, Expr, Token),
    // ':' NAME
    Method(Token, StringExpr),
    FuncArgs(FuncArgs),
}

impl Suffix {
    pub fn unwrap_as_func_args(&self) -> &FuncArgs {
        match &self {
            Suffix::FuncArgs(args) => args,
            _ => unreachable!(),
        }
    }
}

impl Comments for Suffix {
    fn get_comments(&self) -> Vec<&str> {
        match &self {
            Suffix::Attr(token, _) => token.get_comments(),
            Suffix::Index(token, _, _) => token.get_comments(),
            Suffix::Method(token, _) => token.get_comments(),
            Suffix::FuncArgs(args) => args.get_comments(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum FuncArgs {
    // '(' [ exprlist ] ')'
    Exprs(Token, ExprList, Token),
    Table(Table),
    String(StringExpr),
}

impl Comments for FuncArgs {
    fn get_comments(&self) -> Vec<&str> {
        match &self {
            FuncArgs::Exprs(token, _, _) => token.get_comments(),
            FuncArgs::Table(table) => table.get_comments(),
            FuncArgs::String(string) => string.get_comments(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ExprList {
    pub exprs: Vec<Expr>,
    pub commas: Vec<Token>,
}

impl ExprList {
    pub fn new() -> Self {
        ExprList {
            exprs: Vec::new(),
            commas: Vec::new(),
        }
    }
}

impl Comments for ExprList {
    fn get_comments(&self) -> Vec<&str> {
        if let Some(expr) = self.exprs.first() {
            expr.get_comments()
        } else {
            Vec::new()
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Table {
    pub lb: Token,
    pub fields: Vec<Field>,
    pub rb: Token,
}

impl Comments for Table {
    fn get_comments(&self) -> Vec<&str> {
        self.lb.get_comments()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Field {
    RecField(RecField),
    ListField(ListField),
}

impl Field {
    pub fn unwrap_as_list_field(&self) -> &ListField {
        match &self {
            Field::ListField(field) => field,
            _ => unreachable!(),
        }
    }
    pub fn unwrap_as_rec_field(&self) -> &RecField {
        match &self {
            Field::RecField(field) => field,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecField {
    pub key: FieldKey,
    pub equal: Token,
    pub value: Expr,
    pub sep: Option<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ListField {
    pub value: Expr,
    pub sep: Option<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FieldKey {
    Name(StringExpr),
    // '[' expr ']'
    Expr(Token, Expr, Token),
}

impl FieldKey {
    pub fn unwrap_as_name(&self) -> &StringExpr {
        match &self {
            FieldKey::Name(name) => name,
            _ => unreachable!(),
        }
    }
    pub fn unwrap_as_expr(&self) -> &Expr {
        match &self {
            FieldKey::Expr(_, expr, _) => expr,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct UnExpr {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct BinExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct IfStat {
    pub cond_blocks: Vec<CondBlock>,
    pub else_: Option<Token>,
    pub else_block: Option<Block>,
    pub end: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct CondBlock {
    pub if_: Token,
    pub cond: Expr,
    pub then: Token,
    pub block: Block,
}

#[derive(Clone, PartialEq, Debug)]
pub struct WhileStat {
    pub while_: Token,
    pub cond: Expr,
    pub do_: Token,
    pub block: Block,
    pub end: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct DoBlock {
    pub do_: Token,
    pub block: Block,
    pub end: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ForStat {
    ForNum(ForNum),
    ForList(ForList),
}

#[derive(Clone, PartialEq, Debug)]
pub struct ForNum {
    pub for_: Token,
    pub var: StringExpr,
    pub equal: Token,
    pub init: Expr,
    pub init_comma: Token,
    pub limit: Expr,
    pub limit_comma: Option<Token>,
    pub step: Option<Expr>,
    pub do_: Token,
    pub body: Block,
    pub end: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ForList {
    pub for_: Token,
    pub vars: VarList,
    pub in_: Token,
    pub exprs: ExprList,
    pub do_: Token,
    pub body: Block,
    pub end: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct VarList {
    pub vars: Vec<StringExpr>,
    pub delimiters: Vec<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct RepeatStat {
    pub repeat: Token,
    pub block: Block,
    pub until: Token,
    pub cond: Expr,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FuncType {
    Global,
    Local(Token),
}

#[derive(Clone, PartialEq, Debug)]
pub struct FuncStat {
    pub func_type: FuncType,
    pub function: Token,
    pub func_name: FuncName,
    pub body: FuncBody,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FuncName {
    // NAME {'.' NAME}
    pub fields: VarList,
    // [':' NAME]
    pub method: Option<(Token, StringExpr)>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FuncBody {
    pub lp: Token,
    pub params: ParamList,
    pub rp: Token,
    pub block: Block,
    pub end: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ParamList {
    pub params: Vec<Param>,
    pub commas: Vec<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Param {
    VarArg(Token),
    Name(StringExpr),
}

impl Param {
    pub fn unwrap_as_name(&self) -> String {
        match self {
            Param::Name(expr) => expr.value(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct LocalStat {
    pub local: Token,
    pub names: VarList,
    pub equal: Option<Token>,
    pub exprs: Option<ExprList>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct LabelStat {
    pub ldc: Token,
    pub label: StringExpr,
    pub rdc: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct RetStat {
    pub return_: Token,
    pub exprs: Option<ExprList>,
    pub semi: Option<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct BreakStat {
    pub token: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub struct GotoStat {
    pub goto: Token,
    pub label: StringExpr,
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssignStat {
    pub left: AssignableList,
    pub equal: Token,
    pub right: ExprList,
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssignableList {
    pub assignables: Vec<Assignable>,
    pub commas: Vec<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct CallStat {
    pub call: Assignable,
}

#[derive(Clone, PartialEq, Debug)]
pub struct CommentStat {
    pub is_single_line: bool,
    pub comment: StringExpr,
}

impl CommentStat {
    pub fn new(token: Token) -> Self {
        CommentStat {
            is_single_line: token.t == TokenType::SComment,
            comment: StringExpr { token },
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Stat {
    IfStat(IfStat),
    WhileStat(WhileStat),
    DoBlock(DoBlock),
    ForStat(ForStat),
    RepeatStat(RepeatStat),
    FuncStat(FuncStat),
    LocalStat(LocalStat),
    LabelStat(LabelStat),
    RetStat(RetStat),
    BreakStat(BreakStat),
    GotoStat(GotoStat),
    AssignStat(AssignStat),
    CallStat(CallStat),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Block {
    pub stats: Vec<Stat>,
}
