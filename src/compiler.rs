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
}

impl AstVisitor for Compiler {
    fn local_stat(&mut self, stat: &LocalStat) {
        let proto = self.proto();
        for name in stat.names.iter() {
            proto.add_local_var(name);
        }
    }
}
