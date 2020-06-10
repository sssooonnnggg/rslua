use crate::ast::*;
use crate::ast_walker::{ast_walker, AstVisitor};
use crate::opcodes::{Instruction, OpCode};
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

    pub fn append_return(&mut self, first: u32, nret: u32) {
        self.code
            .push(Instruction::create_ABC(OpCode::Return, first, nret + 1, 0));
    }
}

use std::fmt;
impl fmt::Debug for Proto {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "instructions :")?;
        writeln!(
            f,
            "| {:<5} | {:<10} | {:<5} | {:<5} | {:<5} |",
            "line", "OP", "A", "B", "C"
        )?;
        for instruction in self.code.iter().enumerate() {
            writeln!(f, "| {:<5} {:?}", instruction.0 + 1, instruction.1)?;
        }
        Ok(())
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
        self.open_func();
        ast_walker::walk_block(block, self);
        self.close_func();
        self.pop_proto()
    }

    fn open_func(&mut self) {}

    fn close_func(&mut self) {
        self.get_proto().append_return(0, 0);
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
