use crate::ast::*;
use crate::ast_walker::AstVisitor;
use crate::opcodes::Instruction;
use crate::types::{FloatType, IntType};

enum Const {
    Int(IntType),
    Float(FloatType),
    Str(String),
}
pub struct LocalVal {}
pub struct UpVal {}

pub struct Proto {
    code: Vec<Instruction>,
    consts: Vec<Const>,
    local_vars: Vec<LocalVal>,
    up_vars: Vec<UpVal>,
    protos: Vec<Proto>,
}

impl Proto {
    pub fn new() -> Proto {
        Proto {
            code: Vec::new(),
            consts: Vec::new(),
            local_vars: Vec::new(),
            up_vars: Vec::new(),
            protos: Vec::new(),
        }
    }
}

pub struct Compiler {}

impl Compiler {
    pub fn run(&mut self, block: &Block) -> Proto {
        Proto::new()
    }
}

impl AstVisitor for Compiler {}
