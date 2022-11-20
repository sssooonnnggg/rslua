use crate::tokens::{Token, TokenType};
use crate::types::Source;
use crate::types::{FloatType, IntType};
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnOp<'a> {
    Minus(&'a Token),
    BNot(&'a Token),
    Not(&'a Token),
    Len(&'a Token),
    None,
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
    None,
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
    SuffixedExpr(SuffixedExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn to_assignable(self) -> Assignable<'a> {
        match self {
            Expr::Name(s) => Assignable::Name(s),
            Expr::SuffixedExpr(s) => Assignable::SuffixedExpr(s),
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct FloatExpr<'a> {
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct IntExpr<'a> {
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct StringExpr<'a> {
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct SuffixedExpr<'a> {
    pub primary: Box<Expr<'a>>,
    pub suffixes: Vec<Suffix<'a>>,
}

#[derive(PartialEq, Debug)]
pub enum Suffix<'a> {
    // '.' NAME
    Attr(&'a Token, StringExpr<'a>),
    // '[' expr ']'
    Index(&'a Token, Expr<'a>, &'a Token),
    // ':' NAME
    Method(&'a Token, StringExpr<'a>),
    FuncArgs(FuncArgs<'a>),
}

#[derive(PartialEq, Debug)]
pub enum FuncArgs<'a> {
    // '(' [ exprlist ] ')'
    Exprs(&'a Token, ExprList<'a>, &'a Token),
    // '{' TABLE '}'
    Table(&'a Token, Table<'a>, &'a Token),
    String(StringExpr<'a>),
}

#[derive(PartialEq, Debug)]
pub struct ExprList<'a> {
    pub exprs: Vec<Expr<'a>>,
    pub commas: Vec<&'a Token>,
}

#[derive(PartialEq, Debug)]
pub struct Table<'a> {
    pub lb: &'a Token,
    pub fields: Vec<Field<'a>>,
    pub rb: &'a Token,
}

#[derive(PartialEq, Debug)]
pub enum Field<'a> {
    RecField(RecField<'a>),
    ListField(Expr<'a>),
}

#[derive(PartialEq, Debug)]
pub struct RecField<'a> {
    pub key: FieldKey<'a>,
    pub equal: &'a Token,
    pub value: Expr<'a>,
    pub comma: Option<&'a Token>,
}

#[derive(PartialEq, Debug)]
pub enum FieldKey<'a> {
    Name(StringExpr<'a>),
    // '[' expr ']'
    Expr((&'a Token, Expr<'a>, &'a Token)),
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
    pub else_: Option<&'a Token>,
    pub else_block: Option<Block<'a>>,
    pub end: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct CondBlock<'a> {
    pub if_: &'a Token,
    pub cond: Expr<'a>,
    pub then: &'a Token,
    pub block: Block<'a>,
}

#[derive(PartialEq, Debug)]
pub struct WhileStat<'a> {
    pub while_: &'a Token,
    pub cond: Expr<'a>,
    pub do_: &'a Token,
    pub block: Block<'a>,
    pub end: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct DoBlock<'a> {
    pub do_: &'a Token,
    pub block: Block<'a>,
    pub end: &'a Token,
}

#[derive(PartialEq, Debug)]
pub enum ForStat<'a> {
    ForNum(ForNum<'a>),
    ForList(ForList<'a>),
}

#[derive(PartialEq, Debug)]
pub struct ForNum<'a> {
    pub for_: &'a Token,
    pub var: StringExpr<'a>,
    pub equal: &'a Token,
    pub init: Expr<'a>,
    pub init_comma: &'a Token,
    pub limit: Expr<'a>,
    pub limit_comma: Option<&'a Token>,
    pub step: Option<Expr<'a>>,
    pub do_: &'a Token,
    pub body: Block<'a>,
    pub end: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct ForList<'a> {
    pub for_: &'a Token,
    pub vars: VarList<'a>,
    pub in_: &'a Token,
    pub exprs: ExprList<'a>,
    pub do_: &'a Token,
    pub body: Block<'a>,
    pub end: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct VarList<'a> {
    pub vars: Vec<StringExpr<'a>>,
    pub delimiters: Vec<&'a Token>,
}

#[derive(PartialEq, Debug)]
pub struct RepeatStat<'a> {
    pub repeat: &'a Token,
    pub block: Block<'a>,
    pub until: &'a Token,
    pub cond: Expr<'a>,
}

#[derive(PartialEq, Debug)]
pub enum FuncType<'a> {
    Global,
    Local(&'a Token),
}

#[derive(PartialEq, Debug)]
pub struct FuncStat<'a> {
    pub func_type: FuncType<'a>,
    pub function: &'a Token,
    pub func_name: FuncName<'a>,
    pub body: FuncBody<'a>,
}

#[derive(PartialEq, Debug)]
pub struct FuncName<'a> {
    // NAME {'.' NAME}
    pub fields: VarList<'a>,
    // [':' NAME]
    pub method: Option<(&'a Token, StringExpr<'a>)>,
}

#[derive(PartialEq, Debug)]
pub struct FuncBody<'a> {
    pub lp: &'a Token,
    pub params: ParamList<'a>,
    pub rp: &'a Token,
    pub block: Block<'a>,
    pub end: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct ParamList<'a> {
    pub params: Vec<Param<'a>>,
    pub commas: Vec<&'a Token>,
}

#[derive(PartialEq, Debug)]
pub enum Param<'a> {
    VarArg(&'a Token),
    Name(StringExpr<'a>),
}

#[derive(PartialEq, Debug)]
pub struct LocalStat<'a> {
    pub local: &'a Token,
    pub names: VarList<'a>,
    pub equal: Option<&'a Token>,
    pub exprs: Option<ExprList<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct LabelStat<'a> {
    pub ldc: &'a Token,
    pub label: StringExpr<'a>,
    pub rdc: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct RetStat<'a> {
    pub return_: &'a Token,
    pub exprs: Option<ExprList<'a>>,
    pub semi: Option<&'a Token>,
}

#[derive(PartialEq, Debug)]
pub struct BreakStat<'a> {
    pub token: &'a Token,
}

#[derive(PartialEq, Debug)]
pub struct GotoStat<'a> {
    pub goto: &'a Token,
    pub label: StringExpr<'a>,
}

#[derive(PartialEq, Debug)]
pub struct AssignStat<'a> {
    pub left: AssignableList<'a>,
    pub equal: &'a Token,
    pub right: ExprList<'a>,
}

#[derive(PartialEq, Debug)]
pub struct AssignableList<'a> {
    pub assignables: Vec<Assignable<'a>>,
    pub commas: Vec<&'a Token>,
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

impl<'a> CommentStat<'a> {
    pub fn new(token: &'a Token) -> Self {
        CommentStat {
            is_single_line: token.t == TokenType::SComment,
            comment: StringExpr {
                value: token.get_string(),
                token: token,
            },
        }
    }
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
