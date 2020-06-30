use crate::ast::*;
use crate::ast_walker::{ast_walker, AstVisitor};
use crate::consts::Const;
use crate::opcodes::*;
use crate::proto::{Proto, ProtoContext};
use crate::types::Source;
use crate::{debuggable, error, success};

pub struct Compiler {
    debug: bool,
    proto_contexts: Vec<ProtoContext>,
}

pub struct CompileError(pub String);

impl CompileError {
    pub fn new(str: &str) -> Self {
        CompileError(str.to_string())
    }
}

type CompileResult = Result<Proto, CompileError>;

macro_rules! compile_error {
    ($self:ident, $error:ident, $source:ident) => {{
        let error_msg = format!("[compile error] {} at line [{}].", $error.0, $source.line);
        error!($self, CompileError, error_msg)
    }};
}

pub struct Reg {
    pub reg: u32,
    pub temp: bool,
    pub mutable: bool,
}

impl Reg {
    pub fn is_temp(&self) -> bool {
        self.temp
    }

    pub fn is_const(&self) -> bool {
        !self.mutable
    }

    pub fn resolve(&self, context: &mut ProtoContext) {
        if self.is_temp() {
            context.free_reg(1)
        }
    }
}

pub struct Jump {
    pub reg: Reg,
    pub pc: usize,
    pub true_jumps: Vec<usize>,
    pub false_jumps: Vec<usize>,
}

impl Jump {
    pub fn new(reg: Reg, pc: usize) -> Self {
        Jump {
            reg,
            pc,
            true_jumps: Vec::new(),
            false_jumps: Vec::new(),
        }
    }

    pub fn resolve(&self, context: &mut ProtoContext) {
        self.reg.resolve(context);
        // TODO
    }

    pub fn inverse(&self, context: &mut ProtoContext) {
        let proto = &mut context.proto;
        let cond = self.pc - 1;
        let instruction = proto.get_instruction(cond);
        instruction.set_arg_A(1 - instruction.get_arg_A());
    }
}

pub enum ExprResult {
    ConstIndex(u32),
    RegIndex(Reg),
    Nil,
    True,
    False,
    Jump(Jump),
}

impl ExprResult {
    pub fn new_const(k: u32) -> Self {
        ExprResult::ConstIndex(k)
    }

    pub fn new_reg(reg: u32) -> Self {
        ExprResult::RegIndex(Reg {
            reg,
            temp: false,
            mutable: true,
        })
    }

    pub fn new_temp_reg(reg: u32) -> Self {
        ExprResult::RegIndex(Reg {
            reg,
            temp: true,
            mutable: true,
        })
    }

    pub fn new_const_reg(reg: u32) -> Self {
        ExprResult::RegIndex(Reg {
            reg,
            temp: false,
            mutable: false,
        })
    }

    pub fn new_jump(reg: Reg, pc: usize) -> Self {
        ExprResult::Jump(Jump::new(reg, pc))
    }

    pub fn get_rk(&self) -> u32 {
        match self {
            ExprResult::ConstIndex(k) => MASK_K | *k,
            ExprResult::RegIndex(i) => i.reg,
            ExprResult::Jump(j) => j.reg.reg,
            _ => unreachable!(),
        }
    }

    pub fn resolve(&self, context: &mut ProtoContext) {
        match self {
            ExprResult::RegIndex(r) => r.resolve(context),
            ExprResult::Jump(j) => j.resolve(context),
            _ => (),
        };
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            debug: false,
            proto_contexts: Vec::new(),
        }
    }

    pub fn run(&mut self, block: &Block) -> CompileResult {
        self.main_func(block)
    }

    fn main_func(&mut self, block: &Block) -> CompileResult {
        self.push_proto();
        self.proto().open();
        ast_walker::walk_block(block, self)?;
        self.proto().close();
        Ok(self.pop_proto())
    }

    fn push_proto(&mut self) {
        self.proto_contexts.push(ProtoContext::new());
    }

    fn pop_proto(&mut self) -> Proto {
        if let Some(context) = self.proto_contexts.pop() {
            return context.proto;
        }
        unreachable!()
    }

    // get current proto ref from stack
    fn proto(&mut self) -> &mut Proto {
        &mut self.context().proto
    }

    // get current proto context
    fn context(&mut self) -> &mut ProtoContext {
        if let Some(last) = self.proto_contexts.last_mut() {
            return last;
        }
        unreachable!()
    }

    fn adjust_assign(&mut self, num_left: usize, exprs: &Vec<Expr>) -> i32 {
        let extra = num_left as i32 - exprs.len() as i32;
        if let Some(last_expr) = exprs.last() {
            if last_expr.has_mult_ret() {
                // TODO : process multi return value
                todo!("process mult ret")
            }
        }

        if extra > 0 {
            let context = self.context();
            let from = context.get_reg_top();
            context.reserve_regs(extra as u32);
            context.proto.code_nil(from, extra as u32);
        }

        extra
    }

    // process expr and return const index or register index
    fn expr(&mut self, expr: &Expr, reg: Option<u32>) -> Result<ExprResult, CompileError> {
        let proto = self.proto();
        let result = match expr {
            Expr::Int(i) => {
                let k = proto.add_const(Const::Int(*i));
                ExprResult::new_const(k)
            }
            Expr::Float(f) => {
                let k = proto.add_const(Const::Float(*f));
                ExprResult::new_const(k)
            }
            Expr::String(s) => {
                let k = proto.add_const(Const::Str(s.clone()));
                ExprResult::new_const(k)
            }
            Expr::Nil => ExprResult::Nil,
            Expr::True => ExprResult::True,
            Expr::False => ExprResult::False,
            Expr::Name(name) => {
                if let Some(src) = proto.get_local_var(name) {
                    return Ok(ExprResult::new_const_reg(src));
                }
                // TODO : process upval and globals
                todo!()
            }
            Expr::BinExpr(_) | Expr::UnExpr(_) => self.folding_or_code(expr, reg)?,
            Expr::ParenExpr(expr) => self.folding_or_code(&expr, reg)?,
            _ => todo!(),
        };
        Ok(result)
    }

    // try constant foding first, if failed then generate code
    fn folding_or_code(
        &mut self,
        expr: &Expr,
        reg: Option<u32>,
    ) -> Result<ExprResult, CompileError> {
        if let Some(k) = self.try_const_folding(expr)? {
            let k = self.proto().add_const(k);
            Ok(ExprResult::new_const(k))
        } else {
            self.code_expr(expr, reg)
        }
    }

    // try constant folding expr
    fn try_const_folding(&self, expr: &Expr) -> Result<Option<Const>, CompileError> {
        match expr {
            Expr::Int(i) => return success!(Const::Int(*i)),
            Expr::Float(f) => return success!(Const::Float(*f)),
            Expr::String(s) => return success!(Const::Str(s.clone())),
            Expr::BinExpr(bin) => match bin.op {
                BinOp::Add
                | BinOp::Minus
                | BinOp::Mul
                | BinOp::Div
                | BinOp::IDiv
                | BinOp::Mod
                | BinOp::Pow
                | BinOp::BAnd
                | BinOp::BOr
                | BinOp::BXor
                | BinOp::Shl
                | BinOp::Shr => {
                    if let (Some(l), Some(r)) = (
                        self.try_const_folding(&bin.left)?,
                        self.try_const_folding(&bin.right)?,
                    ) {
                        if let Some(k) = self.const_folding_bin_op(bin.op, l, r)? {
                            return success!(k);
                        }
                    }
                }
                _ => (),
            },
            Expr::UnExpr(un) => match un.op {
                UnOp::BNot | UnOp::Minus => {
                    if let Some(k) = self.try_const_folding(&un.expr)? {
                        if let Some(k) = self.const_folding_un_op(un.op, k)? {
                            return success!(k);
                        }
                    }
                }
                _ => (),
            },
            Expr::ParenExpr(expr) => return self.try_const_folding(&expr),
            _ => (),
        }
        Ok(None)
    }

    fn code_expr(&mut self, expr: &Expr, reg: Option<u32>) -> Result<ExprResult, CompileError> {
        match expr {
            Expr::BinExpr(bin) => self.code_bin_op(bin.op, reg, &bin.left, &bin.right),
            Expr::UnExpr(un) => {
                if un.op == UnOp::Not {
                    self.code_not(reg, &un.expr)
                } else {
                    let result = self.expr(&un.expr, reg)?;
                    self.code_un_op(un.op, reg, result)
                }
            }
            _ => unreachable!(),
        }
    }

    fn const_folding_bin_op(
        &self,
        op: BinOp,
        l: Const,
        r: Const,
    ) -> Result<Option<Const>, CompileError> {
        let result = match op {
            BinOp::Add => l.add(r)?,
            BinOp::Minus => l.sub(r)?,
            BinOp::Mul => l.mul(r)?,
            BinOp::Div => l.div(r)?,
            BinOp::IDiv => l.idiv(r)?,
            BinOp::Mod => l.mod_(r)?,
            BinOp::Pow => l.pow(r)?,
            BinOp::BAnd => l.band(r)?,
            BinOp::BOr => l.bor(r)?,
            BinOp::BXor => l.bxor(r)?,
            BinOp::Shl => l.shl(r)?,
            BinOp::Shr => l.shr(r)?,
            _ => None,
        };
        Ok(result)
    }

    fn const_folding_un_op(&self, op: UnOp, k: Const) -> Result<Option<Const>, CompileError> {
        let result = match op {
            UnOp::Minus => k.minus()?,
            UnOp::BNot => k.bnot()?,
            _ => None,
        };
        Ok(result)
    }

    fn code_bin_op(
        &mut self,
        op: BinOp,
        input: Option<u32>,
        left_expr: &Expr,
        right_expr: &Expr,
    ) -> Result<ExprResult, CompileError> {
        // left expr and right expr try use input reg
        let left = self.expr(left_expr, input)?;

        // if input reg is not used by left expr, apply it to right expr
        let mut right_input = None;
        let is_input_reusable = |r:u32, input:u32| { r < input };
        if let Some(input_reg) = input {
            right_input = match &left {
                ExprResult::RegIndex(r) if !is_input_reusable(r.reg, input_reg)=> None,
                ExprResult::Jump(j) if !is_input_reusable(j.reg.reg, input_reg)=> None,
                _ => input,
            };
        };

        let right = self.expr(right_expr, right_input)?;

        // get rk of left and right expr
        let left_rk = left.get_rk();
        let right_rk = right.get_rk();

        // try use input reg otherwise alloc one
        let context = self.context();
        let reg = input.unwrap_or_else(|| context.reserve_regs(1));

        // resolve previous expr result
        left.resolve(context);
        right.resolve(context);

        let mut result = if let Some(_) = input {
            ExprResult::new_reg(reg)
        } else {
            ExprResult::new_temp_reg(reg)
        };

        // gennerate opcode of binop
        let proto = self.proto();
        match op {
            _ if op.is_comp() => {
                result = self.code_comp(op, result, left_rk, right_rk);
            }
            _ => {
                proto.code_bin_op(op, reg, left_rk, right_rk);
            }
        };

        Ok(result)
    }

    fn code_comp(&mut self, op: BinOp, target: ExprResult, left: u32, right: u32) -> ExprResult {
        match target {
            ExprResult::RegIndex(reg) => {
                // covert >= to <=, > to <
                let (left, right) = match op {
                    BinOp::Ge | BinOp::Gt => (right, left),
                    _ => (left, right),
                };

                let target = reg.reg;
                let proto = self.proto();
                proto.code_comp(op, left, right);
                let jump = proto.code_jmp(1, 0);
                proto.code_bool(target, false, 1);
                proto.code_bool(target, true, 0);
                ExprResult::new_jump(reg, jump)
            }
            _ => unreachable!(),
        }
    }

    fn code_un_op(
        &mut self,
        op: UnOp,
        input: Option<u32>,
        expr: ExprResult,
    ) -> Result<ExprResult, CompileError> {
        let src = expr.get_rk();
        let target = input.unwrap_or_else(|| self.context().reserve_regs(1));

        // resolve previous result
        expr.resolve(self.context());

        // gennerate opcode of unop
        let proto = self.proto();
        proto.code_un_op(op, target, src);

        if let Some(input_reg) = input {
            Ok(ExprResult::new_reg(input_reg))
        } else {
            Ok(ExprResult::new_temp_reg(target))
        }
    }

    fn code_not(&mut self, input: Option<u32>, expr: &Expr) -> Result<ExprResult, CompileError> {
        if let Some(_) = self.try_const_folding(expr)? {
            Ok(ExprResult::False)
        } else {
            let result = self.expr(expr, input)?;
            match &result {
                ExprResult::Jump(j) => {
                    j.inverse(self.context());
                    Ok(result)
                }
                ExprResult::Nil | ExprResult::False => Ok(ExprResult::True),
                ExprResult::ConstIndex(_) | ExprResult::True => Ok(ExprResult::False),
                _ => self.code_un_op(UnOp::Not, input, result),
            }
        }
    }

    // process expr and save to register
    fn expr_and_save(&mut self, expr: &Expr, save_reg: Option<u32>) -> Result<u32, CompileError> {
        let reg = save_reg.unwrap_or_else(|| self.context().reserve_regs(1));

        // use a register to store temp result
        let temp_reg = if Some(reg) != save_reg {
            reg
        } else {
            self.context().reserve_regs(1)
        };

        let result = self.expr(expr, Some(temp_reg))?;
        let proto = self.proto();
        match result {
            ExprResult::ConstIndex(k) => proto.code_const(reg, k),
            ExprResult::RegIndex(src) if src.is_const() => proto.code_move(reg, src.reg),
            ExprResult::RegIndex(_) => proto.save(reg),
            ExprResult::True => proto.code_bool(reg, true, 0),
            ExprResult::False => proto.code_bool(reg, false, 0),
            ExprResult::Nil => proto.code_nil(reg, 1),
            ExprResult::Jump(j) => {
                j.resolve(self.context());
                0
            }
        };

        if temp_reg != reg {
            self.context().free_reg(1);
        }

        Ok(reg)
    }

    fn get_assinable_reg(&mut self, assignable: &Assignable) -> u32 {
        match assignable {
            Assignable::Name(name) => self.proto().get_local_var(name).unwrap(),
            Assignable::ParenExpr(_) => todo!(),
            Assignable::SuffixedExpr(_) => todo!(),
        }
    }

    debuggable!();
}

impl AstVisitor<CompileError> for Compiler {
    // error handler
    fn error(&mut self, e: CompileError, source: &Source) -> Result<(), CompileError> {
        compile_error!(self, e, source)
    }

    // compile local stat
    fn local_stat(&mut self, stat: &LocalStat) -> Result<(), CompileError> {
        let proto = self.proto();
        for name in stat.names.iter() {
            proto.add_local_var(name);
        }
        for expr in stat.exprs.iter() {
            self.expr_and_save(expr, None)?;
        }
        self.adjust_assign(stat.names.len(), &stat.exprs);
        Ok(())
    }

    // compile assign stat
    fn assign_stat(&mut self, stat: &AssignStat) -> Result<(), CompileError> {
        let use_temp_reg = stat.right.len() != stat.left.len();
        let mut to_move: Vec<(u32, u32)> = Vec::new();

        // move rules:
        // if num of left != num of right:
        //      MOVE temp[1..n] right[1..n]
        //      MOVE left[1..n] temp[1..n]
        // if num of left == num of right:
        //      MOVE temp[1..(n-1)] right[1..(n-1)]
        //      MOVE left[n] right[n]
        //      MOVE left[1..(n-1)] temp[1..(n-1)]
        for (i, expr) in stat.right.iter().enumerate() {
            if i != stat.right.len() - 1 || use_temp_reg {
                let reg = self.expr_and_save(expr, None)?;
                if i < stat.left.len() {
                    let target = self.get_assinable_reg(&stat.left[i]);
                    to_move.push((target, reg));
                }
            } else {
                let reg = self.get_assinable_reg(&stat.left[i]);
                self.expr_and_save(expr, Some(reg))?;
            };
        }

        // nil move
        let reg = self.context().get_reg_top();
        let extra = self.adjust_assign(stat.left.len(), &stat.right);
        if extra > 0 {
            let left_start = stat.left.len() as i32 - extra;
            for i in 0..extra {
                let target = self.get_assinable_reg(&stat.left[(left_start + i) as usize]);
                let src = (reg as i32 + i) as u32;
                to_move.push((target, src));
            }
        }

        // apply moves
        for (target, src) in to_move.iter().rev() {
            self.proto().code_move(*target, *src);
            self.context().free_reg(1);
        }

        // free extra regs
        if extra < 0 {
            self.context().free_reg(-extra as u32);
        }

        Ok(())
    }
}
