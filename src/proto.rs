use std::collections::HashMap;

use crate::ast::{BinOp, UnOp};
use crate::consts::Const;
use crate::opcodes::{Instruction, OpCode};

pub struct LocalVal {
    name: String,
}

pub struct UpVal {}

pub struct Proto {
    pub stack_size: u32,
    pub param_count: u32,
    pub code: Vec<Instruction>,
    pub consts: Vec<Const>,
    pub const_map: HashMap<Const, u32>,
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
            const_map: HashMap::new(),
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

    pub fn code_nil(&mut self, start_reg: u32, n: u32) {
        // TODO : optimize for duplicate LoadNil
        self.code.push(Instruction::create_ABC(
            OpCode::LoadNil,
            start_reg,
            n - 1,
            0,
        ));
    }

    pub fn code_bool(&mut self, reg: u32, v: bool) {
        self.code.push(Instruction::create_ABC(
            OpCode::LoadBool,
            reg,
            if v { 1 } else { 0 },
            0,
        ));
    }

    pub fn code_const(&mut self, reg_index: u32, const_index: u32) {
        self.code.push(Instruction::create_ABx(
            OpCode::LoadK,
            reg_index,
            const_index,
        ));
    }

    pub fn code_move(&mut self, reg: u32, src: u32) {
        self.code
            .push(Instruction::create_ABC(OpCode::Move, reg, src, 0));
    }

    pub fn code_bin_op(&mut self, op: BinOp, target: u32, left: u32, right: u32) {
        let op_code = match op {
            BinOp::Add => OpCode::Add,
            BinOp::Minus => OpCode::Sub,
            BinOp::Mul => OpCode::Mul,
            BinOp::Mod => OpCode::Mod,
            BinOp::Pow => OpCode::Pow,
            BinOp::Div => OpCode::Div,
            BinOp::IDiv => OpCode::IDiv,
            BinOp::BAnd => OpCode::BAdd,
            BinOp::BOr => OpCode::BOr,
            BinOp::BXor => OpCode::BXor,
            BinOp::Shl => OpCode::Shl,
            BinOp::Shr => OpCode::Shr,
            BinOp::Concat => OpCode::Concat,
            BinOp::Ne => todo!(),
            BinOp::Eq => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Le => todo!(),
            BinOp::Gt => todo!(),
            BinOp::Ge => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            BinOp::None => unreachable!(),
        };
        self.code
            .push(Instruction::create_ABC(op_code, target, left, right));
    }

    pub fn code_un_op(&mut self, op: UnOp, target: u32, src: u32) {
        let op_code = match op {
            UnOp::Minus => OpCode::Unm,
            UnOp::BNot => OpCode::BNot,
            UnOp::Not => OpCode::Not,
            UnOp::Len => OpCode::Len,
            _ => unimplemented!(),
        };
        self.code
            .push(Instruction::create_ABC(op_code, target, src, 0));
    }

    pub fn add_local_var(&mut self, name: &str) {
        self.local_vars.push(LocalVal {
            name: name.to_string(),
        });
    }

    pub fn get_local_var(&self, name: &str) -> Option<u32> {
        for (i, var) in self.local_vars.iter().enumerate() {
            if var.name == name {
                return Some(i as u32);
            }
        }
        None
    }

    pub fn add_const(&mut self, k: Const) -> u32 {
        match self.const_map.get(&k) {
            Some(index) => *index,
            None => {
                let index = self.consts.len();
                self.consts.push(k.clone());
                self.const_map.insert(k, index as u32);
                index as u32
            }
        }
    }

    // save result to target reg
    pub fn save(&mut self, target: u32) {
        let last = self.code.last_mut();
        if let Some(code) = last {
            code.save(target);
        }
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
                    Const::Str(s) => format!("\"{}\"", s.clone()),
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
    pub reg_top: u32,
    pub proto: Proto,
}

impl ProtoContext {
    pub fn new() -> Self {
        ProtoContext {
            reg_top: 0,
            proto: Proto::new(),
        }
    }

    pub fn check_stack(&mut self, n: u32) {
        let new_stack = self.reg_top + n;
        if new_stack > self.proto.stack_size {
            self.proto.stack_size = new_stack;
        }
    }

    pub fn reserve_regs(&mut self, n: u32) -> u32 {
        self.check_stack(n);
        let index = self.reg_top;
        self.reg_top += n;
        index
    }

    pub fn get_reg_top(&self) -> u32 {
        self.reg_top
    }

    pub fn free_reg(&mut self, n: u32) {
        self.reg_top -= n;
    }
}
