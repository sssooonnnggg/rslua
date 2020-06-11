use crate::opcodes::{Instruction, OpCode};
use crate::types::{FloatType, IntType};

pub enum Const {
    Int(IntType),
    Float(FloatType),
    Str(String),
}
pub struct LocalVal {
    name: String,
}

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

    pub fn open(&mut self) {}

    pub fn close(&mut self) {
        self.code_return(0, 0);
    }

    pub fn code_return(&mut self, first: u32, nret: u32) {
        self.code
            .push(Instruction::create_ABC(OpCode::Return, first, nret + 1, 0));
    }

    pub fn add_local_var(&mut self, name: &str) {
        self.local_vars.push(LocalVal {
            name: name.to_string(),
        });
    }
}

use std::fmt;
impl fmt::Debug for Proto {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;

        writeln!(f, "locals :")?;
        for (i, local) in self.local_vars.iter().enumerate() {
            writeln!(f, "| {:<5} | {:<10} |", i, local.name)?;
        }

        writeln!(f, "instructions :")?;
        writeln!(
            f,
            "| {:<5} | {:<10} | {:<5} | {:<5} | {:<5} |",
            "line", "OP", "A", "B", "C"
        )?;
        for (i, instruction) in self.code.iter().enumerate() {
            writeln!(f, "| {:<5} {:?}", i + 1, instruction)?;
        }

        Ok(())
    }
}
