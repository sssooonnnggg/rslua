use crate::tokens::TokenType;
use crate::types::Source;
use crate::types::{FloatType, IntType};
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnOp {
    Minus,
    BNot,
    Not,
    Len,
    None,
}

impl UnOp {
    pub fn from_token(token: TokenType) -> UnOp {
        match token {
            TokenType::Minus => UnOp::Minus,
            TokenType::BXor => UnOp::BNot,
            TokenType::Not => UnOp::Not,
            TokenType::Len => UnOp::Len,
            _ => UnOp::None,
        }
    }
    pub fn priority(self) -> u8 {
        match self {
            _ => 12,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum BinOp {
    Add,
    Minus,
    Mul,
    Mod,
    Pow,
    Div,
    IDiv,
    BAnd,
    BOr,
    BXor,
    Shl,
    Shr,
    Concat,
    Ne,
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    None,
}

pub struct BinOpPriority {
    pub left: u8,
    pub right: u8,
}

impl BinOp {
    pub fn from_token(token: TokenType) -> BinOp {
        match token {
            TokenType::Add => BinOp::Add,
            TokenType::Minus => BinOp::Minus,
            TokenType::Mul => BinOp::Mul,
            TokenType::Mod => BinOp::Mod,
            TokenType::Pow => BinOp::Pow,
            TokenType::Div => BinOp::Div,
            TokenType::IDiv => BinOp::IDiv,
            TokenType::BAnd => BinOp::BAnd,
            TokenType::BOr => BinOp::BOr,
            TokenType::BXor => BinOp::BXor,
            TokenType::Shl => BinOp::Shl,
            TokenType::Shr => BinOp::Shr,
            TokenType::Concat => BinOp::Concat,
            TokenType::Ne => BinOp::Ne,
            TokenType::Eq => BinOp::Eq,
            TokenType::Lt => BinOp::Lt,
            TokenType::Le => BinOp::Le,
            TokenType::Gt => BinOp::Gt,
            TokenType::Ge => BinOp::Ge,
            TokenType::And => BinOp::And,
            TokenType::Or => BinOp::Or,
            _ => BinOp::None,
        }
    }

    pub fn priority(self) -> BinOpPriority {
        match self {
            BinOp::Or => BinOpPriority { left: 1, right: 1 },
            BinOp::And => BinOpPriority { left: 2, right: 2 },
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                BinOpPriority { left: 3, right: 3 }
            }
            BinOp::BOr => BinOpPriority { left: 4, right: 4 },
            BinOp::BXor => BinOpPriority { left: 5, right: 5 },
            BinOp::BAnd => BinOpPriority { left: 6, right: 6 },
            BinOp::Shl | BinOp::Shr => BinOpPriority { left: 7, right: 7 },
            BinOp::Concat => BinOpPriority { left: 9, right: 8 },
            BinOp::Add | BinOp::Minus => BinOpPriority {
                left: 10,
                right: 10,
            },
            BinOp::Mul | BinOp::Mod | BinOp::Div | BinOp::IDiv => BinOpPriority {
                left: 11,
                right: 11,
            },
            BinOp::Pow => BinOpPriority {
                left: 14,
                right: 13,
            },
            _ => unreachable!(),
        }
    }

    pub fn is_comp(&self) -> bool {
        match self {
            BinOp::Le | BinOp::Ge | BinOp::Ne | BinOp::Eq | BinOp::Lt | BinOp::Gt => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Nil,
    True,
    False,
    VarArg,
    Float(FloatType),
    Int(IntType),
    String(String),
    Name(String),
    ParenExpr(Box<Expr>),
    FuncBody(FuncBody),
    Table(Table),
    BinExpr(BinExpr),
    UnExpr(UnExpr),
    SuffixedExpr(SuffixedExpr),
}

impl Expr {
    pub fn has_multi_ret(&self) -> bool {
        match self {
            Expr::SuffixedExpr(s) => s.has_multi_ret(),
            Expr::VarArg => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Assignable {
    Name(String),
    ParenExpr(Box<Expr>),
    SuffixedExpr(SuffixedExpr),
}

impl Expr {
    pub fn to_assignable(self) -> Assignable {
        match self {
            Expr::Name(s) => Assignable::Name(s),
            Expr::ParenExpr(p) => Assignable::ParenExpr(p),
            Expr::SuffixedExpr(s) => Assignable::SuffixedExpr(s),
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct SuffixedExpr {
    pub primary: Box<Expr>,
    pub suffixes: Vec<Suffix>,
}

impl SuffixedExpr {
    pub fn has_multi_ret(&self) -> bool {
        todo!()
    }
}

#[derive(PartialEq, Debug)]
pub enum Suffix {
    Attr(String),
    Index(Expr),
    Method(String),
    FuncArgs(FuncArgs),
}

#[derive(PartialEq, Debug)]
pub enum FuncArgs {
    Exprs(Vec<Expr>),
    Table(Table),
    String(String),
}

#[derive(PartialEq, Debug)]
pub struct Table {
    pub fields: Vec<Field>,
}

#[derive(PartialEq, Debug)]
pub enum Field {
    ListField(Expr),
    RecField(RecField),
}

#[derive(PartialEq, Debug)]
pub struct RecField {
    pub key: FieldKey,
    pub value: Expr,
}

#[derive(PartialEq, Debug)]
pub enum FieldKey {
    Name(String),
    Expr(Expr),
}

#[derive(PartialEq, Debug)]
pub struct UnExpr {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct BinExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct IfStat {
    pub cond_blocks: Vec<CondBlock>,
    pub else_block: Option<Block>,
}

#[derive(PartialEq, Debug)]
pub struct CondBlock {
    pub cond: Expr,
    pub block: Block,
}

#[derive(PartialEq, Debug)]
pub struct WhileStat {
    pub cond: Expr,
    pub block: Block,
}

#[derive(PartialEq, Debug)]
pub struct DoBlock {
    pub block: Block,
}

#[derive(PartialEq, Debug)]
pub enum ForStat {
    ForNum(ForNum),
    ForList(ForList),
}

#[derive(PartialEq, Debug)]
pub struct ForNum {
    pub var: String,
    pub init: Expr,
    pub limit: Expr,
    pub step: Option<Expr>,
    pub body: Block,
}

#[derive(PartialEq, Debug)]
pub struct ForList {
    pub vars: Vec<String>,
    pub exprs: Vec<Expr>,
    pub body: Block,
}

#[derive(PartialEq, Debug)]
pub struct RepeatStat {
    pub cond: Expr,
    pub block: Block,
}

#[derive(PartialEq, Debug)]
pub enum FuncType {
    Global,
    Local,
}

#[derive(PartialEq, Debug)]
pub struct FuncStat {
    pub func_type: FuncType,
    pub func_name: FuncName,
    pub body: FuncBody,
}

#[derive(PartialEq, Debug)]
pub struct FuncName {
    pub fields: Vec<String>,
    pub method: Option<String>,
}

#[derive(PartialEq, Debug)]
pub struct FuncBody {
    pub params: Vec<Param>,
    pub block: Block,
}

#[derive(PartialEq, Debug)]
pub enum Param {
    VarArg,
    Name(String),
}

#[derive(PartialEq, Debug)]
pub struct LocalStat {
    pub names: Vec<String>,
    pub exprs: Vec<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct LabelStat {
    pub label: String,
}

#[derive(PartialEq, Debug)]
pub struct RetStat {
    pub exprs: Vec<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct BreakStat {}

#[derive(PartialEq, Debug)]
pub struct GotoStat {
    pub label: String,
}

#[derive(PartialEq, Debug)]
pub struct AssignStat {
    pub left: Vec<Assignable>,
    pub right: Vec<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct CallStat {
    pub call: Assignable,
}

#[derive(PartialEq, Debug)]
pub struct CommentStat {
    pub is_single_line: bool,
    pub comment: String,
}

#[derive(PartialEq, Debug)]
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

#[derive(Debug)]
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

#[derive(PartialEq, Debug)]
pub struct Block {
    pub stats: Vec<StatInfo>,
}
