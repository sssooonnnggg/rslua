use crate::tokens::{Token, TokenType};
use crate::types::Source;
use crate::types::{FloatType, IntType};
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnOp<'a> {
    Minus(&'a Token),
    BNot(&'a Token),
    Not(&'a Token),
    Len(&'a Token),
    None(&'a Token),
}

impl<'a> UnOp<'a> {
    pub fn from_token(token: &'a Token) -> UnOp<'a> {
        match token.t {
            TokenType::Minus => UnOp::Minus(token),
            TokenType::BXor => UnOp::BNot(token),
            TokenType::Not => UnOp::Not(token),
            TokenType::Len => UnOp::Len(token),
            _ => UnOp::None(token),
        }
    }
    pub fn priority(self) -> u8 {
        match self {
            _ => 12,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum BinOp<'a> {
    Add(&'a Token),
    Minus(&'a Token),
    Mul(&'a Token),
    Mod(&'a Token),
    Pow(&'a Token),
    Div(&'a Token),
    IDiv(&'a Token),
    BAnd(&'a Token),
    BOr(&'a Token),
    BXor(&'a Token),
    Shl(&'a Token),
    Shr(&'a Token),
    Concat(&'a Token),
    Ne(&'a Token),
    Eq(&'a Token),
    Lt(&'a Token),
    Le(&'a Token),
    Gt(&'a Token),
    Ge(&'a Token),
    And(&'a Token),
    Or(&'a Token),
    None(&'a Token),
}

pub struct BinOpPriority {
    pub left: u8,
    pub right: u8,
}

impl<'a> BinOp<'a> {
    pub fn from_token(token: &'a Token) -> BinOp<'a> {
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
            _ => BinOp::None(token),
        }
    }

    pub fn priority(self) -> BinOpPriority {
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

#[derive(PartialEq, Debug)]
pub enum Expr<'a> {
    Nil(&'a Token),
    True(&'a Token),
    False(&'a Token),
    VarArg(&'a Token),
    Float(FloatExpr<'a>),
    Int(IntExpr<'a>),
    String(StringExpr<'a>),
    Name(StringExpr<'a>),
    ParenExpr(Box<Expr<'a>>),
    FuncBody(FuncBody<'a>),
    Table(Table<'a>),
    BinExpr(BinExpr<'a>),
    UnExpr(UnExpr<'a>),
    SuffixedExpr(SuffixedExpr<'a>),
}

#[derive(PartialEq, Debug)]
pub enum Assignable<'a> {
    Name(StringExpr<'a>),
    ParenExpr(Box<Expr<'a>>),
    SuffixedExpr(SuffixedExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn to_assignable(self) -> Assignable<'a> {
        match self {
            Expr::Name(s) => Assignable::Name(s),
            Expr::ParenExpr(p) => Assignable::ParenExpr(p),
            Expr::SuffixedExpr(s) => Assignable::SuffixedExpr(s),
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct FloatExpr<'a> {
    pub value: FloatType,
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct IntExpr<'a> {
    pub value: IntType,
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct StringExpr<'a> {
    pub value: String,
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct SuffixedExpr<'a> {
    pub primary: Box<Expr<'a>>,
    pub suffixes: Vec<Suffix<'a>>,
}

#[derive(PartialEq, Debug)]
pub enum Suffix<'a> {
    Attr(StringExpr<'a>),
    Index(Expr<'a>),
    Method(StringExpr<'a>),
    FuncArgs(FuncArgs<'a>),
}

#[derive(PartialEq, Debug)]
pub enum FuncArgs<'a> {
    Exprs(Vec<Expr<'a>>),
    Table(Table<'a>),
    String(StringExpr<'a>),
}

#[derive(PartialEq, Debug)]
pub struct Table<'a> {
    pub fields: Vec<Field<'a>>,
}

#[derive(PartialEq, Debug)]
pub enum Field<'a> {
    ListField(Expr<'a>),
    RecField(RecField<'a>),
}

#[derive(PartialEq, Debug)]
pub struct RecField<'a> {
    pub key: FieldKey<'a>,
    pub value: Expr<'a>,
}

#[derive(PartialEq, Debug)]
pub enum FieldKey<'a> {
    Name(StringExpr<'a>),
    Expr(Expr<'a>),
}

#[derive(PartialEq, Debug)]
pub struct UnExpr<'a> {
    pub op: UnOp<'a>,
    pub expr: Box<Expr<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct BinExpr<'a> {
    pub op: BinOp<'a>,
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct IfStat<'a> {
    pub cond_blocks: Vec<CondBlock<'a>>,
    pub else_block: Option<Block<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct CondBlock<'a> {
    pub cond: Expr<'a>,
    pub block: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub struct WhileStat<'a> {
    pub cond: Expr<'a>,
    pub block: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub struct DoBlock<'a> {
    pub block: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub enum ForStat<'a> {
    ForNum(ForNum<'a>),
    ForList(ForList<'a>),
}

#[derive(PartialEq, Debug)]
pub struct ForNum<'a> {
    pub var: String,
    pub init: Expr<'a>,
    pub limit: Expr<'a>,
    pub step: Option<Expr<'a>>,
    pub body: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub struct ForList<'a> {
    pub vars: Vec<StringExpr<'a>>,
    pub exprs: Vec<Expr<'a>>,
    pub body: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub struct RepeatStat<'a> {
    pub cond: Expr<'a>,
    pub block: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub enum FuncType<'a> {
    Global,
    Local(&'a Token),
}

#[derive(PartialEq, Debug)]
pub struct FuncStat<'a> {
    pub func_type: FuncType<'a>,
    pub func_name: FuncName<'a>,
    pub body: FuncBody<'a>,
}

#[derive(PartialEq, Debug)]
pub struct FuncName<'a> {
    pub fields: Vec<StringExpr<'a>>,
    pub method: Option<StringExpr<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct FuncBody<'a> {
    pub params: Vec<Param<'a>>,
    pub block: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub enum Param<'a> {
    VarArg(&'a Token),
    Name(StringExpr<'a>),
}

#[derive(PartialEq, Debug)]
pub struct LocalStat<'a> {
    pub names: Vec<StringExpr<'a>>,
    pub exprs: Vec<Expr<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct LabelStat<'a> {
    pub label: StringExpr<'a>,
}

#[derive(PartialEq, Debug)]
pub struct RetStat<'a> {
    pub exprs: Vec<Expr<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct BreakStat<'a> {
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct GotoStat<'a> {
    pub label: StringExpr<'a>,
}

#[derive(PartialEq, Debug)]
pub struct AssignStat<'a> {
    pub left: Vec<Assignable<'a>>,
    pub right: Vec<Expr<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct CallStat<'a> {
    pub call: Assignable<'a>,
}

#[derive(PartialEq, Debug)]
pub struct CommentStat<'a> {
    pub is_single_line: bool,
    pub comment: StringExpr<'a>,
}

#[derive(PartialEq, Debug)]
pub enum Stat<'a> {
    IfStat(IfStat<'a>),
    WhileStat(WhileStat<'a>),
    DoBlock(DoBlock<'a>),
    ForStat(ForStat<'a>),
    RepeatStat(RepeatStat<'a>),
    FuncStat(FuncStat<'a>),
    LocalStat(LocalStat<'a>),
    LabelStat(LabelStat<'a>),
    RetStat(RetStat<'a>),
    BreakStat(BreakStat<'a>),
    GotoStat(GotoStat<'a>),
    AssignStat(AssignStat<'a>),
    CallStat(CallStat<'a>),
    CommentStat(CommentStat<'a>),
}

impl<'a> Stat<'a> {
    pub fn to_stat_info(self) -> StatInfo<'a> {
        StatInfo {
            stat: self,
            source: Source::new(),
        }
    }
}

#[derive(Debug)]
pub struct StatInfo<'a> {
    pub stat: Stat<'a>,
    pub source: Source,
}

impl<'a> StatInfo<'a> {
    pub fn from_stat(stat: Stat<'a>) -> Self {
        StatInfo {
            stat,
            source: Source::new(),
        }
    }
}

impl PartialEq for StatInfo<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.stat == other.stat
    }
}

#[derive(PartialEq, Debug)]
pub struct Block<'a> {
    pub stats: Vec<StatInfo<'a>>,
}
