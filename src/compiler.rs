use crate::ast::*;
use crate::ast_walker::{ast_walker, AstVisitor};
use crate::proto::Proto;

pub struct Compiler {
    proto_stack: Vec<Proto>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            proto_stack: Vec::new(),
        }
    }

    pub fn run(&mut self, block: &Block) -> Proto {
        self.main_func(block)
    }

    fn main_func(&mut self, block: &Block) -> Proto {
        self.push_proto(Proto::new());
        self.proto().open();
        ast_walker::walk_block(block, self);
        self.proto().close();
        self.pop_proto()
    }

    fn push_proto(&mut self, proto: Proto) {
        self.proto_stack.push(proto);
    }

    fn pop_proto(&mut self) -> Proto {
        if let Some(proto) = self.proto_stack.pop() {
            return proto;
        }
        unreachable!()
    }

    // get current proto ref from stack
    fn proto(&mut self) -> &mut Proto {
        if let Some(last) = self.proto_stack.last_mut() {
            return last;
        }
        unreachable!()
    }

    fn adjust_assign(&mut self, names: &Vec<String>, exprs: &Vec<Expr>) {
        let extra = names.len() as i32 - exprs.len() as i32;
        if let Some(last_expr) = exprs.last() {
            if last_expr.has_mult_ret() {
                todo!("process mult ret")
            }
        }

        if extra > 0 {}
    }
}

impl AstVisitor for Compiler {
    fn local_stat(&mut self, stat: &LocalStat) {
        let proto = self.proto();
        for name in stat.names.iter() {
            proto.add_local_var(name);
        }
        ast_walker::walk_exprlist(&stat.exprs, self);
        self.adjust_assign(&stat.names, &stat.exprs);
    }
}
