use crate::tokens::{Token, TokenType};
use crate::types::{FloatType, IntType, Source};

#[derive(Clone, PartialEq, Debug)]
pub enum UnOp {
    Minus(Token),
    BNot(Token),
    Not(Token),
    Len(Token),
    None,
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

#[derive(PartialEq, Clone, Debug)]
pub enum Assignable {
    Name(StringExpr),
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

#[derive(PartialEq, Clone, Debug)]
pub struct IntExpr {
    pub token: Token,
}

impl IntExpr {
    pub fn value(&self) -> IntType {
        self.token.get_int()
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

#[derive(PartialEq, Clone, Debug)]
pub struct SuffixedExpr {
    pub primary: Box<Expr>,
    pub suffixes: Vec<Suffix>,
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

#[derive(Clone, PartialEq, Debug)]
pub enum FuncArgs {
    // '(' [ exprlist ] ')'
    Exprs(Token, ExprList, Token),
    Table(Table),
    String(StringExpr),
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

#[derive(Clone, PartialEq, Debug)]
pub struct Table {
    pub lb: Token,
    pub fields: Vec<Field>,
    pub rb: Token,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Field {
    RecField(RecField),
    ListField(Expr),
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecField {
    pub key: FieldKey,
    pub equal: Token,
    pub value: Expr,
    pub comma: Option<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FieldKey {
    Name(StringExpr),
    // '[' expr ']'
    Expr(Token, Expr, Token),
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
    CommentStat(CommentStat),
}

impl Stat {
    pub fn to_stat_info(self) -> StatInfo {
        StatInfo {
            stat: self,
            source: Source::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StatInfo {
    pub stat: Stat,
    pub source: Source,
}

impl StatInfo {
    pub fn from_stat(stat: Stat) -> Self {
        StatInfo {
            stat,
            source: Source::new(),
        }
    }
}

impl PartialEq for StatInfo {
    fn eq(&self, other: &Self) -> bool {
        self.stat == other.stat
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Block {
    pub stats: Vec<StatInfo>,
}
