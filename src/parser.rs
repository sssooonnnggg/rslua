use crate::{debuggable, error};

use crate::ast::*;
use crate::tokens::{Token, TokenType, TokenValue};
use crate::types::Source;

pub struct Parser<'a> {
    tokens: Option<&'a Vec<Token>>,
    current: usize,
    debug: bool,
}

#[derive(Debug)]
pub struct SyntaxError(String);

type ParseResult<T> = Result<T, SyntaxError>;

macro_rules! syntax_error {
    ($self:ident, $msg:expr) => {{
        let token = &$self.tokens.unwrap()[$self.current];
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

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Parser {
            tokens: None,
            current: 0,
            debug: false,
        }
    }

    pub fn run(&mut self, tokens: &'a Vec<Token>) -> ParseResult<Block> {
        self.reset();
        self.tokens = Some(tokens);
        self.block()
    }

    // block -> { stat [';'] }
    fn block(&mut self) -> ParseResult<Block<'a>> {
        let mut stats: Vec<StatInfo> = Vec::new();
        let saved = self.current_source();
        while !self.is_block_end() {
            let stat = self.stat()?;
            if let Some(stat) = stat {
                let source = self.current_source() - saved;
                stats.push(StatInfo { source, stat });
            }
            if let Some(Stat::RetStat(_)) = stat {
                break;
            }
        }
        Ok(Block { stats })
    }

    fn stat(&mut self) -> ParseResult<Option<Stat>> {
        let line = self.current_line();
        let stat = match self.current_token_type() {
            // stat -> ';' (empty stat)
            TokenType::Semi => {
                self.next();
                return Ok(None);
            }
            // stat -> comment
            TokenType::SComment | TokenType::MComment => Stat::CommentStat(self.commentstat()?),
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
                let local = self.current_token();
                self.next_and_skip_comment();
                if self.test(TokenType::Function) {
                    Stat::FuncStat(self.localfunc(local)?)
                } else {
                    Stat::LocalStat(self.localstat(local)?)
                }
            }
            // stat -> label
            TokenType::DbColon => {
                self.next_and_skip_comment();
                Stat::LabelStat(self.labelstat()?)
            }
            // stat -> retstat
            TokenType::Return => {
                self.next_and_skip_comment();
                Stat::RetStat(self.retstat()?)
            }
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
    fn forenum(
        &mut self,
        line: usize,
        for_: &'a Token,
        var: StringExpr<'a>,
    ) -> ParseResult<ForStat<'a>> {
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
            init_commas,
            limit,
            limit_commas,
            step,
            do_,
            body,
            end,
        }))
    }

    // forlist -> NAME {,NAME} IN explist forbody
    fn forlist(
        &mut self,
        line: usize,
        for_: &'a Token,
        var: StringExpr<'a>,
    ) -> ParseResult<ForStat<'a>> {
        let mut vars = VarList {
            vars: Vec::new(),
            delimiters: Vec::new(),
        };
        vars.vars.push(var);
        while let Some(comma) = self.test_next(TokenType::Comma) {
            vars.delimiters.push(comma);
            vars.vars.push(self.check_name()?);
        }
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

    // repeatstat -> REPEAT block UNTIL cond
    fn repeatstat(&mut self) -> ParseResult<RepeatStat> {
        let line = self.current_line();
        self.next();
        let block = self.block()?;
        self.check_match(TokenType::Until, TokenType::Repeat, line)?;
        let cond = self.cond()?;
        Ok(RepeatStat { block, cond })
    }

    // funcstat -> FUNCTION funcname body
    fn funcstat(&mut self) -> ParseResult<FuncStat> {
        self.next_and_skip_comment();
        let func_name = self.funcname()?;
        let body = self.funcbody()?;
        Ok(FuncStat {
            func_type: FuncType::Global,
            func_name,
            body,
        })
    }

    // funcname -> NAME {'.' NAME} [':' NAME]
    fn funcname(&mut self) -> ParseResult<FuncName> {
        let mut fields: Vec<String> = Vec::new();
        fields.push(self.check_name()?);
        while self.test_next(TokenType::Attr) {
            fields.push(self.check_name()?);
        }
        let mut method = None;
        if self.test_next(TokenType::Colon) {
            method = Some(self.check_name()?);
        }
        Ok(FuncName { fields, method })
    }

    // body ->  '(' parlist ')' block END
    fn funcbody(&mut self) -> ParseResult<FuncBody> {
        let line = self.current_line();
        self.check_next(TokenType::Lp)?;
        self.skip_comment();
        let mut params: Vec<Param> = Vec::new();
        loop {
            if self.test(TokenType::Rp) {
                break;
            }
            match self.current_token_type() {
                TokenType::Dots => {
                    params.push(Param::VarArg);
                    self.next_and_skip_comment()
                }
                TokenType::Name => params.push(Param::Name(self.check_name()?)),
                _ => syntax_error!(self, "<name> or '...' expected")?,
            };
            if !self.test_next(TokenType::Comma) {
                break;
            }
        }
        self.check_next(TokenType::Rp)?;
        let block = self.block()?;
        self.check_match(TokenType::End, TokenType::Function, line)?;
        Ok(FuncBody { params, block })
    }

    // funcstat -> local FUNCTION funcname body
    fn localfunc(&mut self, token: &Token) -> ParseResult<FuncStat<'a>> {
        let function_ = self.current_token();
        self.next_and_skip_comment();
        let func_name = self.funcname()?;
        let body = self.funcbody()?;
        Ok(FuncStat {
            func_type: FuncType::Local(token),
            function_,
            func_name,
            body,
        })
    }

    // stat -> LOCAL NAME {',' NAME} ['=' explist]
    fn localstat(&mut self, token: &Token) -> ParseResult<LocalStat> {
        let mut names: Vec<String> = Vec::new();
        loop {
            names.push(self.check_name()?);
            if !self.test_next(TokenType::Comma) {
                break;
            }
        }
        let mut exprs: Vec<Expr> = Vec::new();
        if self.test_next(TokenType::Assign) {
            exprs = self.exprlist()?;
        }
        Ok(LocalStat { names, exprs })
    }

    // label -> '::' NAME '::'
    fn labelstat(&mut self) -> ParseResult<LabelStat> {
        let label = self.check_name()?;
        self.check_next(TokenType::DbColon)?;
        self.skip_comment();
        Ok(LabelStat { label })
    }

    // stat -> RETURN [explist] [';']
    fn retstat(&mut self) -> ParseResult<RetStat> {
        let mut exprs: Vec<Expr> = Vec::new();
        if !self.is_block_end() && self.current_token_type() != TokenType::Semi {
            exprs = self.exprlist()?;
        }
        self.test_next(TokenType::Semi);
        Ok(RetStat { exprs })
    }

    fn breakstat(&mut self) -> ParseResult<BreakStat> {
        self.next_and_skip_comment();
        Ok(BreakStat {})
    }

    fn gotostat(&mut self) -> ParseResult<GotoStat> {
        self.next_and_skip_comment();
        let label = self.check_name()?;
        Ok(GotoStat { label })
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
        let mut left: Vec<Assignable> = Vec::new();
        left.push(first);
        while self.test_next(TokenType::Comma) {
            left.push(self.suffixedexpr()?.to_assignable())
        }
        self.check_next(TokenType::Assign)?;
        self.skip_comment();
        let right = self.exprlist()?;
        Ok(AssignStat { left, right })
    }

    // exprlist -> expr { ',' expr }
    fn exprlist(&mut self) -> ParseResult<ExprList<'a>> {
        let mut exprs = ExprList {
            exprs: Vec::new(),
            commas: Vec::new(),
        };
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
        UnOp::from_token(self.current_token_type())
    }

    fn get_binop(&self) -> BinOp {
        BinOp::from_token(self.current_token_type())
    }

    // subexpr -> (simpleexpr | unop subexpr) { binop subexpr }
    // where 'binop' is any binary operator with a priority higher than 'limit'
    fn subexpr(&mut self, limit: u8) -> ParseResult<Expr> {
        let mut left;
        let unop = self.get_unop();
        if unop != UnOp::None {
            self.next_and_skip_comment();
            let expr = Box::new(self.subexpr(unop.priority())?);
            left = Expr::UnExpr(UnExpr { op: unop, expr });
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
            TokenType::Flt => Expr::Float(token.get_float()),
            TokenType::Int => Expr::Int(token.get_int()),
            TokenType::String => Expr::String(token.get_string()),
            TokenType::Nil => Expr::Nil,
            TokenType::True => Expr::True,
            TokenType::False => Expr::False,
            TokenType::Dots => Expr::VarArg,
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
                    self.next_and_skip_comment();
                    suffixes.push(Suffix::Attr(self.check_name()?));
                }
                TokenType::Ls => {
                    let line = self.current_line();
                    self.next_and_skip_comment();
                    suffixes.push(Suffix::Index(self.expr()?));
                    self.check_match(TokenType::Rs, TokenType::Ls, line)?;
                }
                TokenType::Colon => {
                    self.next_and_skip_comment();
                    let name = self.check_name()?;
                    suffixes.push(Suffix::Method(name));
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
        self.check_next(TokenType::Lb)?;
        self.skip_comment();
        let mut fields: Vec<Field> = Vec::new();
        loop {
            if self.test(TokenType::Rb) {
                break;
            }
            fields.push(self.field()?);
            if !self.test_next(TokenType::Comma) && !self.test_next(TokenType::Semi) {
                break;
            } else {
                // TODO : reverse comment for table fields
                self.skip_comment();
            }
        }
        self.check_match(TokenType::Rb, TokenType::Lb, line)?;
        Ok(Table { fields })
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
        Ok(field)
    }

    // recfield -> (NAME | '['exp1']') = exp1
    fn recfield(&mut self) -> ParseResult<Field> {
        let key;
        match self.current_token_type() {
            TokenType::Name => key = FieldKey::Name(self.check_name()?),
            TokenType::Ls => {
                let line = self.current_line();
                self.next_and_skip_comment();
                key = FieldKey::Expr(self.expr()?);
                self.check_match(TokenType::Rs, TokenType::Ls, line)?;
            }
            _ => unreachable!(),
        };
        self.check_next(TokenType::Assign)?;
        self.skip_comment();
        let value = self.expr()?;
        Ok(Field::RecField(RecField { key, value }))
    }

    // listfield -> expr
    fn listfield(&mut self) -> ParseResult<Field> {
        Ok(Field::ListField(self.expr()?))
    }

    // funcargs -> '(' [ explist ] ') | table constructor | STRING
    fn funcargs(&mut self) -> ParseResult<FuncArgs> {
        let func_args = match self.current_token_type() {
            TokenType::Lp => {
                let line = self.current_line();
                self.next_and_skip_comment();

                // empty arg list
                if self.test_next(TokenType::Rp) {
                    return Ok(FuncArgs::Exprs(Vec::<Expr>::new()));
                }

                let exprs = self.exprlist()?;
                self.check_match(TokenType::Rp, TokenType::Lp, line)?;
                FuncArgs::Exprs(exprs)
            }
            TokenType::Lb => FuncArgs::Table(self.table()?),
            TokenType::String => {
                let arg = FuncArgs::String(self.current_token().get_string());
                self.next_and_skip_comment();
                arg
            }
            _ => return syntax_error!(self, "function arguments expected"),
        };
        Ok(func_args)
    }

    fn reset(&mut self) {
        self.current = 0;
    }

    fn current_token(&self) -> &'a Token {
        &self.tokens.unwrap()[self.current]
    }

    fn next_token(&self) -> &Token {
        let mut current = self.current + 1;
        while self.tokens.unwrap()[current].is_comment() {
            current += 1;
        }
        &self.tokens.unwrap()[current]
    }

    fn current_token_type(&self) -> TokenType {
        let token = self.current_token();
        token.t
    }

    fn current_source(&self) -> Source {
        let token = self.current_token();
        token.source
    }

    fn current_line(&self) -> usize {
        let token = self.current_token();
        token.source.line
    }

    fn next_token_type(&self) -> TokenType {
        let token = self.next_token();
        token.t
    }

    fn next_and_skip_comment(&mut self) -> &'a Token {
        let token = self.current_token();
        self.current += 1;
        self.skip_comment();
        token
    }

    fn next(&mut self) -> &Token {
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

    fn check_match(
        &mut self,
        end: TokenType,
        start: TokenType,
        line: usize,
    ) -> ParseResult<&'a Token> {
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

    fn test_next(&mut self, expected: TokenType) -> Option<&'a Token> {
        let origin = self.skip_comment();
        if self.test(expected) {
            Some(self.next())
        } else {
            self.current = origin;
            None
        }
    }

    fn check(&self, expected: TokenType) -> ParseResult<&Token> {
        if self.current_token_type() != expected {
            error_expected!(self, expected)
        } else {
            Ok(self.current_token())
        }
    }

    fn check_next(&mut self, expected: TokenType) -> ParseResult<&'a Token> {
        self.skip_comment();
        self.check(expected)?;
        Ok(self.next())
    }

    fn check_name(&mut self) -> ParseResult<StringExpr<'a>> {
        self.skip_comment();
        self.check(TokenType::Name)?;
        Ok(StringExpr { token: self.next() })
    }

    debuggable!();
}
