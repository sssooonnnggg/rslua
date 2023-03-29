use crate::error;

use crate::ast::*;
use crate::tokens::{Token, TokenType, TokenValue};
use rslua_derive::Debugable;

#[derive(Debugable)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    debug: bool,
}

#[derive(Debug)]
pub struct SyntaxError(String);

type ParseResult<T> = Result<T, SyntaxError>;

macro_rules! syntax_error {
    ($self:ident, $msg:expr) => {{
        let token = $self.tokens[$self.current].clone();
        let ident = match token.value {
            TokenValue::None => format!("{:?}", token.t),
            _ => format!("{:?}", token.value),
        };
        let error_msg = format!(
            "[syntax error] {} at line [{}:{}] near [{}]",
            $msg, token.source.line, token.source.col, ident
        );
        error!($self, SyntaxError, error_msg)
    }};
}

macro_rules! error_expected {
    ($self:ident, $expected:expr) => {
        syntax_error!($self, &format!("{:?} expected", $expected))?
    };
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            tokens: Vec::new(),
            current: 0,
            debug: false,
        }
    }

    pub fn run(&mut self, tokens: Vec<Token>) -> ParseResult<Block> {
        self.reset();
        self.tokens = tokens;
        self.block()
    }

    // block -> { stat [';'] }
    fn block(&mut self) -> ParseResult<Block> {
        let mut stats: Vec<Stat> = Vec::new();
        while !self.is_block_end() {
            let stat = self.stat()?;
            if let Some(stat) = stat {
                let should_break = match stat {
                    Stat::RetStat(_) => true,
                    _ => false,
                };
                stats.push(stat);
                if should_break {
                    break;
                }
            }
        }
        Ok(Block { stats })
    }

    fn stat(&mut self) -> ParseResult<Option<Stat>> {
        let stat = match self.current_token_type() {
            // stat -> ';' (empty stat)
            TokenType::Semi | TokenType::SComment | TokenType::MComment => {
                self.next();
                return Ok(None);
            }
            // stat -> if stat
            TokenType::If => Stat::IfStat(self.ifstat()?),
            // stat -> while stat
            TokenType::While => Stat::WhileStat(self.whilestat()?),
            // stat -> DO block END
            TokenType::Do => Stat::DoBlock(self.doblock()?),
            // stat -> forstat
            TokenType::For => Stat::ForStat(self.forstat()?),
            // stat -> repeatstat
            TokenType::Repeat => Stat::RepeatStat(self.repeatstat()?),
            // stat -> funcstat
            TokenType::Function => Stat::FuncStat(self.funcstat()?),
            // stat -> localstat | localfunc
            TokenType::Local => {
                let local = self.next_and_skip_comment();
                if self.test(TokenType::Function) {
                    Stat::FuncStat(self.localfunc(local)?)
                } else {
                    Stat::LocalStat(self.localstat(local)?)
                }
            }
            // stat -> label
            TokenType::DbColon => Stat::LabelStat(self.labelstat()?),
            // stat -> retstat
            TokenType::Return => Stat::RetStat(self.retstat()?),
            // stat -> breakstat
            TokenType::Break => Stat::BreakStat(self.breakstat()?),
            // stat -> gotostat
            TokenType::Goto => Stat::GotoStat(self.gotostat()?),
            // stat -> func | assignment
            _ => self.exprstat()?,
        };
        Ok(Some(stat))
    }

    fn commentstat(&mut self) -> ParseResult<CommentStat> {
        let stat = CommentStat::new(self.current_token());
        self.next();
        Ok(stat)
    }

    fn doblock(&mut self) -> ParseResult<DoBlock> {
        let line = self.current_line();
        let do_ = self.next();
        let block = self.block()?;
        let end = self.check_match(TokenType::End, TokenType::Do, line)?;
        Ok(DoBlock { do_, block, end })
    }

    // ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
    fn ifstat(&mut self) -> ParseResult<IfStat> {
        let line = self.current_line();
        let mut cond_blocks: Vec<CondBlock> = Vec::new();
        cond_blocks.push(self.test_then_block()?);
        while self.current_token_type() == TokenType::ElseIf {
            cond_blocks.push(self.test_then_block()?);
        }
        let mut else_ = None;
        let mut else_block = None;
        if let Some(else_token) = self.test_next(TokenType::Else) {
            else_ = Some(else_token);
            else_block = Some(self.block()?);
        }
        let end = self.check_match(TokenType::End, TokenType::If, line)?;
        Ok(IfStat {
            cond_blocks,
            else_,
            else_block,
            end,
        })
    }

    //  [IF | ELSEIF] cond THEN block
    fn test_then_block(&mut self) -> ParseResult<CondBlock> {
        let if_ = self.next_and_skip_comment();
        let cond = self.cond()?;
        let then = self.check_next(TokenType::Then)?;
        let block = self.block()?;
        Ok(CondBlock {
            if_,
            cond,
            then,
            block,
        })
    }

    // whilestat -> WHILE cond DO block END
    fn whilestat(&mut self) -> ParseResult<WhileStat> {
        let line = self.current_line();
        let while_ = self.next_and_skip_comment();
        let cond = self.cond()?;
        let do_ = self.check_next(TokenType::Do)?;
        let block = self.block()?;
        let end = self.check_match(TokenType::End, TokenType::While, line)?;
        Ok(WhileStat {
            while_,
            cond,
            do_,
            block,
            end,
        })
    }

    fn cond(&mut self) -> ParseResult<Expr> {
        self.expr()
    }

    // forstat -> FOR (fornum | forlist) END
    fn forstat(&mut self) -> ParseResult<ForStat> {
        let line = self.current_line();
        let for_ = self.next_and_skip_comment();
        let var = self.check_name()?;
        match self.current_token_type() {
            TokenType::Assign => self.forenum(line, for_, var),
            TokenType::Comma | TokenType::In => self.forlist(line, for_, var),
            _ => syntax_error!(self, "'=' or 'in' expected"),
        }
    }

    // fornum -> NAME = exp1,exp1[,exp1] forbody
    fn forenum(&mut self, line: usize, for_: Token, var: StringExpr) -> ParseResult<ForStat> {
        let equal = self.next_and_skip_comment();
        let init = self.expr()?;
        let init_commas = self.check_next(TokenType::Comma)?;
        self.skip_comment();
        let limit = self.expr()?;
        let limit_commas = self.test_next(TokenType::Comma);
        let step = match limit_commas {
            Some(_) => Some(self.expr()?),
            None => None,
        };
        let do_ = self.check_next(TokenType::Do)?;
        let body = self.block()?;
        let end = self.check_match(TokenType::End, TokenType::For, line)?;
        Ok(ForStat::ForNum(ForNum {
            for_,
            var,
            equal,
            init,
            init_comma: init_commas,
            limit,
            limit_comma: limit_commas,
            step,
            do_,
            body,
            end,
        }))
    }

    // forlist -> NAME {,NAME} IN explist forbody
    fn forlist(&mut self, line: usize, for_: Token, var: StringExpr) -> ParseResult<ForStat> {
        let vars = self.varlist(TokenType::Comma, Some(var))?;
        let in_ = self.check_next(TokenType::In)?;
        self.skip_comment();
        let exprs = self.exprlist()?;
        let do_ = self.check_next(TokenType::Do)?;
        let body = self.block()?;
        let end = self.check_match(TokenType::End, TokenType::For, line)?;
        Ok(ForStat::ForList(ForList {
            for_,
            vars,
            in_,
            exprs,
            do_,
            body,
            end,
        }))
    }

    fn varlist(
        &mut self,
        delimiter: TokenType,
        first_var: Option<StringExpr>,
    ) -> ParseResult<VarList> {
        let mut vars = VarList {
            vars: Vec::new(),
            delimiters: Vec::new(),
        };
        let var = match first_var {
            Some(var) => var,
            None => self.check_name()?,
        };
        vars.vars.push(var);
        while let Some(comma) = self.test_next(delimiter) {
            vars.delimiters.push(comma);
            vars.vars.push(self.check_name()?);
        }
        Ok(vars)
    }

    // repeatstat -> REPEAT block UNTIL cond
    fn repeatstat(&mut self) -> ParseResult<RepeatStat> {
        let line = self.current_line();
        let repeat = self.next();
        let block = self.block()?;
        let until = self.check_match(TokenType::Until, TokenType::Repeat, line)?;
        let cond = self.cond()?;
        Ok(RepeatStat {
            repeat,
            block,
            until,
            cond,
        })
    }

    // funcstat -> FUNCTION funcname body
    fn funcstat(&mut self) -> ParseResult<FuncStat> {
        let function = self.next_and_skip_comment();
        let func_name = self.funcname()?;
        let body = self.funcbody()?;
        Ok(FuncStat {
            function,
            func_type: FuncType::Global,
            func_name,
            body,
        })
    }

    // funcname -> NAME {'.' NAME} [':' NAME]
    fn funcname(&mut self) -> ParseResult<FuncName> {
        let fields = self.varlist(TokenType::Attr, None)?;
        let mut method = None;
        if let Some(colon) = self.test_next(TokenType::Colon) {
            method = Some((colon, self.check_name()?));
        }
        Ok(FuncName { fields, method })
    }

    // body ->  '(' parlist ')' block END
    fn funcbody(&mut self) -> ParseResult<FuncBody> {
        let line = self.current_line();
        let lp = self.check_next(TokenType::Lp)?;
        self.skip_comment();
        let mut params = ParamList {
            params: Vec::new(),
            commas: Vec::new(),
        };
        loop {
            if self.test(TokenType::Rp) {
                break;
            }
            match self.current_token_type() {
                TokenType::Dots => {
                    params.params.push(Param::VarArg(self.current_token()));
                    self.next_and_skip_comment();
                }
                TokenType::Name => params.params.push(Param::Name(self.check_name()?)),
                _ => syntax_error!(self, "<name> or '...' expected")?,
            };
            if let Some(commas) = self.test_next(TokenType::Comma) {
                params.commas.push(commas);
            } else {
                break;
            }
        }
        let rp = self.check_next(TokenType::Rp)?;
        let block = self.block()?;
        let end = self.check_match(TokenType::End, TokenType::Function, line)?;
        Ok(FuncBody {
            lp,
            params,
            rp,
            block,
            end,
        })
    }

    // funcstat -> local FUNCTION funcname body
    fn localfunc(&mut self, token: Token) -> ParseResult<FuncStat> {
        let function = self.current_token();
        self.next_and_skip_comment();
        let func_name = self.funcname()?;
        let body = self.funcbody()?;
        Ok(FuncStat {
            func_type: FuncType::Local(token),
            function,
            func_name,
            body,
        })
    }

    // stat -> LOCAL NAME {',' NAME} ['=' explist]
    fn localstat(&mut self, local: Token) -> ParseResult<LocalStat> {
        let names = self.varlist(TokenType::Comma, None)?;
        let equal = self.test_next(TokenType::Assign);
        let exprs = if let Some(_) = equal {
            Some(self.exprlist()?)
        } else {
            None
        };
        Ok(LocalStat {
            local,
            names,
            equal,
            exprs,
        })
    }

    // label -> '::' NAME '::'
    fn labelstat(&mut self) -> ParseResult<LabelStat> {
        let ldc = self.next();
        self.skip_comment();
        let label = self.check_name()?;
        let rdc = self.check_next(TokenType::DbColon)?;
        self.skip_comment();
        Ok(LabelStat { ldc, label, rdc })
    }

    // stat -> RETURN [explist] [';']
    fn retstat(&mut self) -> ParseResult<RetStat> {
        let return_ = self.next_and_skip_comment();
        let exprs = if !self.is_block_end() && self.current_token_type() != TokenType::Semi {
            Some(self.exprlist()?)
        } else {
            None
        };
        let semi = self.test_next(TokenType::Semi);
        Ok(RetStat {
            return_,
            exprs,
            semi,
        })
    }

    fn breakstat(&mut self) -> ParseResult<BreakStat> {
        let token = self.next_and_skip_comment();
        Ok(BreakStat { token })
    }

    fn gotostat(&mut self) -> ParseResult<GotoStat> {
        let goto = self.next_and_skip_comment();
        let label = self.check_name()?;
        Ok(GotoStat { goto, label })
    }

    // stat -> func call | assignment
    fn exprstat(&mut self) -> ParseResult<Stat> {
        let expr = self.suffixedexpr()?;
        if self.test(TokenType::Assign) || self.test(TokenType::Comma) {
            Ok(Stat::AssignStat(self.assignment(expr.to_assignable())?))
        } else {
            Ok(Stat::CallStat(CallStat {
                call: expr.to_assignable(),
            }))
        }
    }

    // assignment -> ',' suffixedexp assignment
    // assignment -> '=' explist
    fn assignment(&mut self, first: Assignable) -> ParseResult<AssignStat> {
        let mut left = AssignableList {
            assignables: Vec::new(),
            commas: Vec::new(),
        };
        left.assignables.push(first);
        while let Some(comma) = self.test_next(TokenType::Comma) {
            left.commas.push(comma);
            left.assignables.push(self.suffixedexpr()?.to_assignable())
        }
        let equal = self.check_next(TokenType::Assign)?;
        self.skip_comment();
        let right = self.exprlist()?;
        Ok(AssignStat { left, equal, right })
    }

    // exprlist -> expr { ',' expr }
    fn exprlist(&mut self) -> ParseResult<ExprList> {
        let mut exprs = ExprList::new();
        exprs.exprs.push(self.expr()?);
        while let Some(comma) = self.test_next(TokenType::Comma) {
            exprs.commas.push(comma);
            exprs.exprs.push(self.expr()?)
        }
        Ok(exprs)
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        self.subexpr(0)
    }

    fn get_unop(&self) -> UnOp {
        UnOp::from_token(self.current_token())
    }

    fn get_binop(&self) -> BinOp {
        BinOp::from_token(self.current_token())
    }

    // subexpr -> (simpleexpr | unop subexpr) { binop subexpr }
    // where 'binop' is any binary operator with a priority higher than 'limit'
    fn subexpr(&mut self, limit: u8) -> ParseResult<Expr> {
        let mut left;
        let unop = self.get_unop();
        if unop != UnOp::None {
            self.next_and_skip_comment();
            let expr = Box::new(self.subexpr(unop.priority())?);
            left = Expr::UnExpr(UnExpr {
                op: unop.clone(),
                expr,
            });
        } else {
            left = self.simpleexpr()?;
        }
        let mut binop = self.get_binop();
        while binop != BinOp::None && binop.priority().left > limit {
            self.next_and_skip_comment();
            let right = self.subexpr(binop.priority().right)?;
            left = Expr::BinExpr(BinExpr {
                left: Box::new(left),
                right: Box::new(right),
                op: binop,
            });
            binop = self.get_binop();
        }
        Ok(left)
    }

    // simpleexpr -> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor | FUNCTION body | suffixedexp
    fn simpleexpr(&mut self) -> ParseResult<Expr> {
        let token = self.current_token();
        let expr = match token.t {
            TokenType::Flt => Expr::Float(FloatExpr { token }),
            TokenType::Int => Expr::Int(IntExpr { token }),
            TokenType::String => Expr::String(StringExpr { token }),
            TokenType::Nil => Expr::Nil(token),
            TokenType::True => Expr::True(token),
            TokenType::False => Expr::False(token),
            TokenType::Dots => Expr::VarArg(token),
            TokenType::Lb => return Ok(Expr::Table(self.table()?)),
            TokenType::Function => {
                self.next_and_skip_comment();
                return Ok(Expr::FuncBody(self.funcbody()?));
            }
            _ => return Ok(self.suffixedexpr()?),
        };
        self.next_and_skip_comment();
        Ok(expr)
    }

    // suffixedexpr -> primaryexpr { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs }
    fn suffixedexpr(&mut self) -> ParseResult<Expr> {
        let primary = self.primaryexpr()?;
        let mut suffixes: Vec<Suffix> = Vec::new();
        loop {
            match self.current_token_type() {
                TokenType::Attr => {
                    let attr = self.next_and_skip_comment();
                    suffixes.push(Suffix::Attr(attr, self.check_name()?));
                }
                TokenType::Ls => {
                    let line = self.current_line();
                    let ls = self.next_and_skip_comment();
                    suffixes.push(Suffix::Index(ls, self.expr()?, self.check_match(TokenType::Rs, TokenType::Ls, line)?));
                }
                TokenType::Colon => {
                    let colon = self.next_and_skip_comment();
                    let name = self.check_name()?;
                    suffixes.push(Suffix::Method(colon, name));
                }
                TokenType::Lp | TokenType::Lb | TokenType::String => {
                    suffixes.push(Suffix::FuncArgs(self.funcargs()?));
                }
                _ => break,
            }
        }

        if suffixes.is_empty() {
            Ok(primary)
        } else {
            Ok(Expr::SuffixedExpr(SuffixedExpr {
                primary: Box::new(primary),
                suffixes,
            }))
        }
    }

    // primaryexp -> NAME | '(' expr ')'
    fn primaryexpr(&mut self) -> ParseResult<Expr> {
        let expr = match self.current_token_type() {
            TokenType::Name => Expr::Name(self.check_name()?),
            TokenType::Lp => {
                let line = self.current_line();
                self.next_and_skip_comment();
                let expr = self.expr()?;
                self.check_match(TokenType::Rp, TokenType::Lp, line)?;
                Expr::ParenExpr(Box::new(expr))
            }
            _ => {
                return syntax_error!(
                    self,
                    &format!("unexpected symbol '{:?}'", self.current_token_type())
                )
            }
        };
        Ok(expr)
    }

    // table constructor -> '{' [ field { sep field } [sep] ] '}'
    // sep -> ',' | ';'
    fn table(&mut self) -> ParseResult<Table> {
        let line = self.current_line();
        let lb = self.check_next(TokenType::Lb)?;
        self.skip_comment();
        let mut fields: Vec<Field> = Vec::new();
        while !self.test(TokenType::Rb) {
            fields.push(self.field()?);
        }
        let rb = self.check_match(TokenType::Rb, TokenType::Lb, line)?;
        Ok(Table { lb, fields, rb })
    }

    // field -> listfield | recfield
    fn field(&mut self) -> ParseResult<Field> {
        let field = match self.current_token_type() {
            TokenType::Name => {
                if self.next_token_type() == TokenType::Assign {
                    self.recfield()?
                } else {
                    self.listfield()?
                }
            }
            TokenType::Ls => self.recfield()?,
            _ => self.listfield()?,
        };
        self.skip_comment();
        Ok(field)
    }

    // recfield -> (NAME | '['exp1']') = exp1 ','
    fn recfield(&mut self) -> ParseResult<Field> {
        let key = match self.current_token_type() {
            TokenType::Name => FieldKey::Name(self.check_name()?),
            TokenType::Ls => {
                let line = self.current_line();
                let ls = self.next_and_skip_comment();
                let expr = self.expr()?;
                let rs = self.check_match(TokenType::Rs, TokenType::Ls, line)?;
                FieldKey::Expr(ls, expr, rs)
            }
            _ => unreachable!(),
        };
        let equal = self.check_next(TokenType::Assign)?;
        self.skip_comment();
        let value = self.expr()?;
        self.skip_comment();
        let sep = self.test_next(TokenType::Comma).or_else(|| self.test_next(TokenType::Semi));
        Ok(Field::RecField(RecField {
            key,
            equal,
            value,
            sep,
        }))
    }

    // listfield -> expr
    fn listfield(&mut self) -> ParseResult<Field> {
        let expr = self.expr()?;
        let sep = self.test_next(TokenType::Comma).or_else(|| self.test_next(TokenType::Semi));
        Ok(Field::ListField(ListField { value: expr, sep }))
    }

    // funcargs -> '(' [ explist ] ') | table constructor | STRING
    fn funcargs(&mut self) -> ParseResult<FuncArgs> {
        let func_args = match self.current_token_type() {
            TokenType::Lp => {
                let line = self.current_line();
                let lp = self.next_and_skip_comment();

                // empty arg list
                if let Some(rp) = self.test_next(TokenType::Rp) {
                    return Ok(FuncArgs::Exprs(lp, ExprList::new(), rp));
                }

                let exprs = self.exprlist()?;
                let rp = self.check_match(TokenType::Rp, TokenType::Lp, line)?;
                FuncArgs::Exprs(lp, exprs, rp)
            }
            TokenType::Lb => FuncArgs::Table(self.table()?),
            TokenType::String => FuncArgs::String(StringExpr {
                token: self.next_and_skip_comment(),
            }),
            _ => return syntax_error!(self, "function arguments expected"),
        };
        Ok(func_args)
    }

    fn reset(&mut self) {
        self.current = 0;
    }

    fn current_token(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn next_token(&self) -> &Token {
        let mut current = self.current + 1;
        while self.tokens[current].is_comment() {
            current += 1;
        }
        &self.tokens[current]
    }

    fn current_token_type(&self) -> TokenType {
        let token = self.current_token();
        token.t
    }

    fn current_line(&self) -> usize {
        let token = self.current_token();
        token.source.line
    }

    fn next_token_type(&self) -> TokenType {
        let token = self.next_token();
        token.t
    }

    fn next_and_skip_comment(&mut self) -> Token {
        let token = self.current_token();
        self.current += 1;
        self.skip_comment();
        token
    }

    fn next(&mut self) -> Token {
        let token = self.current_token();
        self.current += 1;
        token
    }

    fn skip_comment(&mut self) -> usize {
        let old = self.current;
        while self.current_token().is_comment() {
            self.current += 1;
        }
        old
    }

    // if reach a block end
    fn is_block_end(&self) -> bool {
        let token = self.current_token();
        match token.t {
            TokenType::Else
            | TokenType::ElseIf
            | TokenType::End
            | TokenType::Until
            | TokenType::Eos => true,
            _ => false,
        }
    }

    fn check_match(&mut self, end: TokenType, start: TokenType, line: usize) -> ParseResult<Token> {
        self.skip_comment();
        if self.current_token_type() != end {
            if line == self.current_line() {
                error_expected!(self, end);
            } else {
                syntax_error!(
                    self,
                    &format!("{:?} expected (to close {:?} at line {})", end, start, line)
                )?;
            }
        }
        Ok(self.next())
    }

    fn test(&self, expected: TokenType) -> bool {
        self.current_token_type() == expected
    }

    fn test_next(&mut self, expected: TokenType) -> Option<Token> {
        let origin = self.skip_comment();
        if self.test(expected) {
            Some(self.next())
        } else {
            self.current = origin;
            None
        }
    }

    fn check(&self, expected: TokenType) -> ParseResult<Token> {
        if self.current_token_type() != expected {
            error_expected!(self, expected)
        } else {
            Ok(self.current_token())
        }
    }

    fn check_next(&mut self, expected: TokenType) -> ParseResult<Token> {
        self.skip_comment();
        self.check(expected)?;
        Ok(self.next())
    }

    fn check_name(&mut self) -> ParseResult<StringExpr> {
        self.skip_comment();
        self.check(TokenType::Name)?;
        Ok(StringExpr { token: self.next() })
    }
}
