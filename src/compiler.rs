use crate::ast::*;
use crate::ast_walker::{ast_walker, AstVisitor};
use crate::opcodes::Instruction;
use crate::types::{FloatType, IntType};

pub enum Const {
    Int(IntType),
    Float(FloatType),
    Str(String),
}
pub struct LocalVal {}
pub struct UpVal {}

pub struct Proto {
    pub param_count: usize,
    pub code: Vec<Instruction>,
    pub consts: Vec<Const>,
    pub local_vars: Vec<LocalVal>,
    pub up_vars: Vec<UpVal>,
    pub protos: Vec<Proto>,
}

impl Proto {
    pub fn new() -> Proto {
        Proto {
            param_count: 0,
            code: Vec::new(),
            consts: Vec::new(),
            local_vars: Vec::new(),
            up_vars: Vec::new(),
            protos: Vec::new(),
        }
    }
}

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
        ast_walker::walk_block(block, self);
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

    fn get_proto(&mut self) -> &mut Proto {
        if let Some(last) = self.proto_stack.last_mut() {
            return last;
        }
        unreachable!()
    }
}

impl AstVisitor for Compiler {}
