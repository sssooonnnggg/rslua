use crate::ast::*;
use crate::ast_walker;
use crate::ast_walker::AstVisitor;
use crate::consts::Const;
use crate::opcodes::*;
use crate::proto::{Proto, ProtoContext};
use crate::types::Source;
use crate::utils::success;
use rslua_derive::Traceable;
use rslua_traits::Error;

#[derive(Default, Traceable)]
pub struct Compiler {
    proto_contexts: Vec<ProtoContext>,
}

pub struct CompileError(pub String);

impl CompileError {
    pub fn new(str: &str) -> Self {
        CompileError(str.to_string())
    }
}

impl Error for CompileError {
    fn what(&self) -> &str {
        &self.0
    }
}

type CompileResult = Result<Proto, CompileError>;

pub struct Reg {
    pub reg: u32,
    pub temp: bool,
    pub mutable: bool,
}

impl Reg {
    pub fn new(reg: u32) -> Self {
        Reg {
            reg,
            temp: false,
            mutable: true,
        }
    }

    pub fn new_temp(reg: u32) -> Self {
        Reg {
            reg,
            temp: true,
            mutable: true,
        }
    }

    pub fn is_temp(&self) -> bool {
        self.temp
    }

    pub fn is_const(&self) -> bool {
        !self.mutable
    }

    pub fn free(&self, context: &mut ProtoContext) {
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
    pub reg_should_move: Option<u32>,
}

impl Jump {
    pub fn new(reg: Reg, pc: usize) -> Self {
        Jump {
            reg,
            pc,
            true_jumps: Vec::new(),
            false_jumps: Vec::new(),
            reg_should_move: None,
        }
    }

    pub fn free(&self, context: &mut ProtoContext) {
        let proto = &mut context.proto;
        let target = self.reg.reg;
        if let Some(from) = self.reg_should_move {
            proto.code_move(target, from);
        }
        let false_pos = proto.code_bool(target, false, 1);
        let true_pos = proto.code_bool(target, true, 0);
        self.fix(true_pos, false_pos, proto);
        self.reg.free(context);
    }

    pub fn free_reg(&self, context: &mut ProtoContext) {
        self.reg.free(context);
    }

    pub fn inverse_cond(&self, context: &mut ProtoContext) {
        let proto = &mut context.proto;
        let cond = self.pc - 1;
        let instruction = proto.get_instruction(cond);
        instruction.set_arg_A(1 - instruction.get_arg_A());
    }

    pub fn concat_true_jumps(&mut self, other: &mut Jump) {
        self.true_jumps.append(&mut other.true_jumps);
        self.true_jumps.push(other.pc);
    }

    pub fn concat_false_jumps(&mut self, other: &mut Jump) {
        self.false_jumps.append(&mut other.false_jumps);
        self.false_jumps.push(other.pc);
    }

    pub fn set_reg_should_move(&mut self, from: u32) {
        self.reg_should_move = Some(from)
    }

    fn fix(&self, true_pos: usize, false_pos: usize, proto: &mut Proto) {
        proto.fix_cond_jump_pos(true_pos, false_pos, self.pc);
        for pc in self.true_jumps.iter() {
            proto.fix_jump_pos(true_pos, *pc)
        }
        for pc in self.false_jumps.iter() {
            proto.fix_jump_pos(false_pos, *pc)
        }
    }
}

pub enum ExprResult {
    Const(Const),
    Reg(Reg),
    Jump(Jump),
    Nil,
    True,
    False,
}

impl ExprResult {
    pub fn new_const(k: Const) -> Self {
        ExprResult::Const(k)
    }

    pub fn new_const_reg(reg: u32) -> Self {
        ExprResult::Reg(Reg {
            reg,
            temp: false,
            mutable: false,
        })
    }

    pub fn new_jump(reg: Reg, pc: usize) -> Self {
        ExprResult::Jump(Jump::new(reg, pc))
    }

    pub fn get_rk(&self, context: &mut ProtoContext) -> u32 {
        match self {
            ExprResult::Const(k) => {
                let index = context.proto.add_const(k.clone());
                MASK_K | index
            }
            ExprResult::Reg(i) => i.reg,
            ExprResult::Jump(j) => j.reg.reg,
            _ => unreachable!(),
        }
    }

    pub fn resolve(&self, context: &mut ProtoContext) {
        match self {
            ExprResult::Reg(r) => r.free(context),
            ExprResult::Jump(j) => j.free(context),
            _ => (),
        };
    }
}

impl Compiler {
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

    fn adjust_assign(&mut self, num_left: usize, right_exprs: Option<&ExprList>) -> i32 {
        let extra = num_left as i32 - right_exprs.map_or(0, |v| v.exprs.len()) as i32;
        if right_exprs.and_then(|v| v.exprs.last()).is_some() {
            // todo!("process multi return value")
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
            Expr::Int(i) => ExprResult::new_const(Const::Int(i.value())),
            Expr::Float(f) => ExprResult::new_const(Const::Float(f.value())),
            Expr::String(s) => {
                // const string will always be added to consts
                let k = Const::Str(s.value());
                proto.add_const(k.clone());
                ExprResult::new_const(k)
            }
            Expr::Nil(_) => ExprResult::Nil,
            Expr::True(_) => ExprResult::True,
            Expr::False(_) => ExprResult::False,
            Expr::Name(name) => {
                if let Some(src) = proto.get_local_var(&name.value()) {
                    return Ok(ExprResult::new_const_reg(src));
                }
                // TODO : process upval and globals
                todo!()
            }
            Expr::BinExpr(_) | Expr::UnExpr(_) => self.folding_or_code(expr, reg)?,
            Expr::ParenExpr(expr) => self.folding_or_code(expr, reg)?,
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
            Ok(ExprResult::new_const(k))
        } else {
            self.code_expr(expr, reg)
        }
    }

    // try constant folding expr
    fn try_const_folding(&self, expr: &Expr) -> Result<Option<Const>, CompileError> {
        match expr {
            Expr::Int(i) => return success(Const::Int(i.value())),
            Expr::Float(f) => return success(Const::Float(f.value())),
            Expr::String(s) => return success(Const::Str(s.value())),
            Expr::BinExpr(bin) => match bin.op {
                BinOp::Add(_)
                | BinOp::Minus(_)
                | BinOp::Mul(_)
                | BinOp::Div(_)
                | BinOp::IDiv(_)
                | BinOp::Mod(_)
                | BinOp::Pow(_)
                | BinOp::BAnd(_)
                | BinOp::BOr(_)
                | BinOp::BXor(_)
                | BinOp::Shl(_)
                | BinOp::Shr(_) => {
                    if let (Some(l), Some(r)) = (
                        self.try_const_folding(&bin.left)?,
                        self.try_const_folding(&bin.right)?,
                    ) {
                        if let Some(k) = self.const_folding_bin_op(&bin.op, l, r)? {
                            return success(k);
                        }
                    }
                }
                _ => (),
            },
            Expr::UnExpr(un) => match un.op {
                UnOp::BNot(_) | UnOp::Minus(_) => {
                    if let Some(k) = self.try_const_folding(&un.expr)? {
                        if let Some(k) = self.const_folding_un_op(&un.op, k)? {
                            return success(k);
                        }
                    }
                }
                _ => (),
            },
            Expr::ParenExpr(expr) => return self.try_const_folding(expr),
            _ => (),
        }
        Ok(None)
    }

    fn code_expr(&mut self, expr: &Expr, reg: Option<u32>) -> Result<ExprResult, CompileError> {
        match expr {
            Expr::BinExpr(bin) => match bin.op {
                BinOp::And(_) => self.code_and(reg, &bin.left, &bin.right),
                _ => self.code_bin_op(&bin.op, reg, &bin.left, &bin.right),
            },
            Expr::UnExpr(un) => {
                if let UnOp::Not(_) = un.op {
                    self.code_not(&un.op, reg, &un.expr)
                } else {
                    let result = self.expr(&un.expr, reg)?;
                    self.code_un_op(&un.op, reg, result)
                }
            }
            _ => unreachable!(),
        }
    }

    fn const_folding_bin_op(
        &self,
        op: &BinOp,
        l: Const,
        r: Const,
    ) -> Result<Option<Const>, CompileError> {
        let result = match op {
            BinOp::Add(_) => (l + r)?,
            BinOp::Minus(_) => (l - r)?,
            BinOp::Mul(_) => (l * r)?,
            BinOp::Div(_) => (l / r)?,
            BinOp::IDiv(_) => l.idiv(r)?,
            BinOp::Mod(_) => (l % r)?,
            BinOp::Pow(_) => l.pow(r)?,
            BinOp::BAnd(_) => (l & r)?,
            BinOp::BOr(_) => (l | r)?,
            BinOp::BXor(_) => (l ^ r)?,
            BinOp::Shl(_) => (l << r)?,
            BinOp::Shr(_) => (l >> r)?,
            _ => None,
        };
        Ok(result)
    }

    fn const_folding_un_op(&self, op: &UnOp, k: Const) -> Result<Option<Const>, CompileError> {
        let result = match op {
            UnOp::Minus(_) => k.minus()?,
            UnOp::BNot(_) => k.bnot()?,
            _ => None,
        };
        Ok(result)
    }

    fn get_right_input(&mut self, input: Option<u32>, left: &ExprResult) -> Option<u32> {
        let mut right_input = None;
        let is_input_reusable = |r: u32, input: u32| r < input;
        if let Some(input_reg) = input {
            right_input = match &left {
                ExprResult::Reg(r) if !is_input_reusable(r.reg, input_reg) => None,
                ExprResult::Jump(j) if !is_input_reusable(j.reg.reg, input_reg) => None,
                _ => input,
            };
        };
        right_input
    }

    fn alloc_reg(&mut self, input: &Option<u32>) -> Reg {
        let reg = input.unwrap_or_else(|| self.context().reserve_regs(1));
        if Some(reg) == *input {
            Reg::new(reg)
        } else {
            Reg::new_temp(reg)
        }
    }

    fn code_bin_op(
        &mut self,
        op: &BinOp,
        input: Option<u32>,
        left_expr: &Expr,
        right_expr: &Expr,
    ) -> Result<ExprResult, CompileError> {
        // get left expr result
        let left = self.expr(left_expr, input)?;
        // resolve previous expr result
        left.resolve(self.context());

        // if input reg is not used by left expr, apply it to right expr
        let right_input = self.get_right_input(input, &left);

        // get right expr result
        let right = self.expr(right_expr, right_input)?;

        // resolve previous expr result
        right.resolve(self.context());

        let alloc_reg = self.alloc_reg(&input);
        let reg = alloc_reg.reg;
        let mut result = ExprResult::Reg(alloc_reg);

        // get rk of left and right expr
        let mut get_rk = || {
            let left_rk = left.get_rk(self.context());
            let right_rk = right.get_rk(self.context());
            (left_rk, right_rk)
        };

        // gennerate opcode of binop
        match op {
            _ if op.is_comp() => {
                let (left_rk, right_rk) = get_rk();
                result = self.code_comp(op, result, left_rk, right_rk);
            }
            _ => {
                let (left_rk, right_rk) = get_rk();
                self.proto().code_bin_op(op, reg, left_rk, right_rk);
            }
        };

        Ok(result)
    }

    fn code_comp(&mut self, op: &BinOp, target: ExprResult, left: u32, right: u32) -> ExprResult {
        match target {
            ExprResult::Reg(reg) => {
                // covert >= to <=, > to <
                let (left, right) = match op {
                    BinOp::Ge(_) | BinOp::Gt(_) => (right, left),
                    _ => (left, right),
                };

                let proto = self.proto();
                proto.code_comp(op, left, right);
                let jump = proto.code_jmp(NO_JUMP, 0);
                ExprResult::new_jump(reg, jump)
            }
            _ => unreachable!(),
        }
    }

    fn code_and(
        &mut self,
        input: Option<u32>,
        left_expr: &Expr,
        right_expr: &Expr,
    ) -> Result<ExprResult, CompileError> {
        // get left expr result
        let mut left = self.expr(left_expr, input)?;
        match &mut left {
            // do const folding if left is const value
            ExprResult::True | ExprResult::Const(_) => self.expr(right_expr, input),
            ExprResult::Jump(j) => {
                j.inverse_cond(self.context());
                let mut right = self.expr(right_expr, Some(j.reg.reg))?;
                match &mut right {
                    ExprResult::Jump(rj) => rj.concat_false_jumps(j),
                    _ => todo!(),
                };
                Ok(right)
            }
            ExprResult::Reg(_reg) => self.code_test(input, left, right_expr),
            _ => todo!(),
        }
    }

    fn code_test(
        &mut self,
        input: Option<u32>,
        left: ExprResult,
        right: &Expr,
    ) -> Result<ExprResult, CompileError> {
        match &left {
            ExprResult::Reg(r) => {
                let proto = self.proto();
                proto.code_test_set(NO_REG, r.reg, 0);
                let jump = proto.code_jmp(NO_JUMP, 0);
                let right_input = self.get_right_input(input, &left);
                let right_result = self.expr(right, right_input)?;
                let mut jump = Jump::new(self.alloc_reg(&input), jump);
                match &right_result {
                    ExprResult::Reg(r) if r.is_const() => jump.set_reg_should_move(r.reg),
                    _ => (),
                };
                Ok(ExprResult::Jump(jump))
            }
            _ => unreachable!(),
        }
    }

    fn code_un_op(
        &mut self,
        op: &UnOp,
        input: Option<u32>,
        expr: ExprResult,
    ) -> Result<ExprResult, CompileError> {
        let src = expr.get_rk(self.context());

        // resolve previous result
        expr.resolve(self.context());

        let alloc_reg = self.alloc_reg(&input);
        let reg = alloc_reg.reg;
        let result = ExprResult::Reg(alloc_reg);

        // generate opcode of unop
        let proto = self.proto();
        proto.code_un_op(op, reg, src);

        Ok(result)
    }

    fn code_not(
        &mut self,
        op: &UnOp,
        input: Option<u32>,
        expr: &Expr,
    ) -> Result<ExprResult, CompileError> {
        if self.try_const_folding(expr)?.is_some() {
            Ok(ExprResult::False)
        } else {
            let result = self.expr(expr, input)?;
            match &result {
                ExprResult::Jump(j) => {
                    j.inverse_cond(self.context());
                    Ok(result)
                }
                ExprResult::Nil | ExprResult::False => Ok(ExprResult::True),
                ExprResult::Const(_) | ExprResult::True => Ok(ExprResult::False),
                _ => self.code_un_op(op, input, result),
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
            ExprResult::Const(k) => {
                let index = proto.add_const(k);
                proto.code_const(reg, index)
            }
            ExprResult::Reg(src) if src.is_const() => proto.code_move(reg, src.reg),
            ExprResult::Reg(_) => proto.save(reg),
            ExprResult::True => proto.code_bool(reg, true, 0),
            ExprResult::False => proto.code_bool(reg, false, 0),
            ExprResult::Nil => proto.code_nil(reg, 1),
            ExprResult::Jump(j) => {
                j.free(self.context());
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
            Assignable::Name(name) => self.proto().get_local_var(&name.value()).unwrap(),
            Assignable::SuffixedExpr(_) => todo!(),
        }
    }

    fn compile_error<T>(&self, e: CompileError, source: &Source) -> Result<T, CompileError> {
        let error_msg = format!("[compile error] {} at line [{}].", e.0, source.line);
        Compiler::trace_error(CompileError(error_msg))
    }
}

impl AstVisitor<CompileError> for Compiler {
    // error handler
    fn error(&mut self, e: CompileError, source: &Source) -> Result<(), CompileError> {
        self.compile_error(e, source)
    }

    // compile local stat
    fn local_stat(&mut self, stat: &LocalStat) -> Result<(), CompileError> {
        let proto = self.proto();
        for name in stat.names.vars.iter() {
            proto.add_local_var(&name.value());
        }
        if let Some(expr_list) = &stat.exprs {
            for expr in expr_list.exprs.iter() {
                self.expr_and_save(expr, None)?;
            }
        }
        self.adjust_assign(stat.names.vars.len(), stat.exprs.as_ref());
        Ok(())
    }

    // compile assign stat
    fn assign_stat(&mut self, stat: &AssignStat) -> Result<(), CompileError> {
        let use_temp_reg = stat.right.exprs.len() != stat.left.assignables.len();
        let mut to_move: Vec<(u32, u32)> = Vec::new();

        // move rules:
        // if num of left != num of right:
        //      MOVE temp[1..n] right[1..n]
        //      MOVE left[1..n] temp[1..n]
        // if num of left == num of right:
        //      MOVE temp[1..(n-1)] right[1..(n-1)]
        //      MOVE left[n] right[n]
        //      MOVE left[1..(n-1)] temp[1..(n-1)]
        for (i, expr) in stat.right.exprs.iter().enumerate() {
            if i != stat.right.exprs.len() - 1 || use_temp_reg {
                let reg = self.expr_and_save(expr, None)?;
                if i < stat.left.assignables.len() {
                    let target = self.get_assinable_reg(&stat.left.assignables[i]);
                    to_move.push((target, reg));
                }
            } else {
                let reg = self.get_assinable_reg(&stat.left.assignables[i]);
                self.expr_and_save(expr, Some(reg))?;
            };
        }

        // nil move
        let reg = self.context().get_reg_top();
        let extra = self.adjust_assign(stat.left.assignables.len(), Some(&stat.right));
        if extra > 0 {
            let left_start = stat.left.assignables.len() as i32 - extra;
            for i in 0..extra {
                let target =
                    self.get_assinable_reg(&stat.left.assignables[(left_start + i) as usize]);
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
