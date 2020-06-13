use crate::ast::*;
use crate::ast_walker::{ast_walker, AstVisitor};
use crate::proto::{Const, Proto, ProtoContext};
use crate::types::{FloatType, IntType};

pub struct Compiler {
    proto_contexts: Vec<ProtoContext>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            proto_contexts: Vec::new(),
        }
    }

    pub fn run(&mut self, block: &Block) -> Proto {
        self.main_func(block)
    }

    fn main_func(&mut self, block: &Block) -> Proto {
        self.push_proto();
        self.proto().open();
        ast_walker::walk_block(block, self);
        self.proto().close();
        self.pop_proto()
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
            context.reverse_regs(extra as u32);
            context.proto.code_nil(from, extra as u32);
        }

        extra
    }

    fn expr(&mut self, expr: &Expr, reg: u32) {
        let proto = self.proto();
        match expr {
            Expr::Int(i) => {
                let k = proto.add_const(Const::Int(*i));
                proto.code_const(reg, k);
            }
            Expr::Float(f) => {
                let k = proto.add_const(Const::Float(*f));
                proto.code_const(reg, k);
            }
            Expr::String(s) => {
                let k = proto.add_const(Const::Str(s.clone()));
                proto.code_const(reg, k);
            }
            Expr::Nil => proto.code_nil(reg, 1),
            Expr::True => proto.code_bool(reg, true),
            Expr::False => proto.code_bool(reg, false),
            Expr::Name(name) => {
                if let Some(src) = proto.get_local_var(name) {
                    proto.code_move(reg, src);
                }
                // TODO : process upval and globals
            }
            _ => todo!(),
        }
    }

    fn get_assinable_reg(&mut self, assignable: &Assignable) -> u32 {
        match assignable {
            Assignable::Name(name) => self.proto().get_local_var(name).unwrap(),
            Assignable::ParenExpr(expr) => todo!(),
            Assignable::SuffixedExpr(expr) => todo!(),
        }
    }
}

impl AstVisitor for Compiler {
    fn local_stat(&mut self, stat: &LocalStat) {
        let proto = self.proto();
        for name in stat.names.iter() {
            proto.add_local_var(name);
        }
        for expr in stat.exprs.iter() {
            let reg = self.context().reverse_regs(1);
            self.expr(expr, reg);
        }
        self.adjust_assign(stat.names.len(), &stat.exprs);
    }

    fn assign_stat(&mut self, stat: &AssignStat) {
        let last_use_temp_reg = stat.right.len() != stat.left.len();
        let mut to_move: Vec<(u32, u32)> = Vec::new();

        // normal move
        // the last right one direct move to left register
        for (i, expr) in stat.right.iter().enumerate() {
            if i != stat.right.len() - 1 || last_use_temp_reg {
                let reg = self.context().reverse_regs(1);
                self.expr(expr, reg);
                if i < stat.left.len() {
                    let target = self.get_assinable_reg(&stat.left[i]);
                    to_move.push((target, reg));
                }
            } else {
                let reg = self.get_assinable_reg(&stat.left[i]);
                self.expr(expr, reg);
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
    }
}
