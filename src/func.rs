use crate::ast::*;
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
    pub stack_size: u32,
    pub param_count: u32,
    pub code: Vec<Instruction>,
    pub consts: Vec<Const>,
    pub local_vars: Vec<LocalVal>,
    pub up_vars: Vec<UpVal>,
    pub protos: Vec<Proto>,
}

impl Proto {
    pub fn new() -> Proto {
        Proto {
            stack_size: 2,
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

    pub fn code_nil(&mut self, from: u32, n: u32) {
        // TODO : optimize for duplicate LoadNil
        self.code
            .push(Instruction::create_ABC(OpCode::LoadNil, from, n - 1, 0));
    }

    pub fn code_const(&mut self, reg_index: u32, const_index: u32) {
        self.code.push(Instruction::create_ABx(
            OpCode::LoadK,
            reg_index,
            const_index,
        ));
    }

    pub fn add_local_var(&mut self, name: &str) {
        self.local_vars.push(LocalVal {
            name: name.to_string(),
        });
    }

    pub fn add_const(&mut self, k: Const) -> u32 {
        let index = self.consts.len();
        self.consts.push(k);
        index as u32
    }
}

use std::fmt;
impl fmt::Debug for Proto {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;

        writeln!(f, "stack size : {}", self.stack_size)?;

        writeln!(f, "consts :")?;
        for (i, k) in self.consts.iter().enumerate() {
            writeln!(
                f,
                "| {:<5} | {:<10} |",
                i,
                match k {
                    Const::Int(i) => i.to_string(),
                    Const::Float(f) => f.to_string(),
                    Const::Str(s) => s.clone(),
                }
            )?;
        }

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

pub struct ProtoContext {
    pub free_reg: u32,
    pub proto: Proto,
}

impl ProtoContext {
    pub fn new() -> Self {
        ProtoContext {
            free_reg: 0,
            proto: Proto::new(),
        }
    }

    pub fn check_stack(&mut self, n: u32) {
        let new_stack = self.free_reg + n;
        if new_stack > self.proto.stack_size {
            self.proto.stack_size = new_stack;
        }
    }

    pub fn reverse_regs(&mut self, n: u32) {
        self.check_stack(n);
        self.free_reg += n;
    }

    pub fn load_expr_to_reg(&mut self, expr: &Expr, reg: u32) {
        let proto = &mut self.proto;
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
            _ => todo!(),
        }
    }
}
