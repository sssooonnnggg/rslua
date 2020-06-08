// Comments from lopcodes.h
// We assume that instructions are unsigned numbers.
// All instructions have an opcode in the first 6 bits.
// Instructions can have the following fields:
// 	'A' : 8 bits
// 	'B' : 9 bits
// 	'C' : 9 bits
// 	'Ax' : 26 bits ('A', 'B', and 'C' together)
// 	'Bx' : 18 bits ('B' and 'C' together)
//  'sBx' : signed Bx

// A signed argument is represented in excess K; that is, the number
// value is the unsigned value minus K. K is exactly the maximum value
// for that argument (so that -max is represented by 0, and +max is
// represented by 2*max), which is half the maximum for the corresponding
// unsigned argument.

pub enum OpMode {
    iABC,
    iABx,
    iAsBx,
    iAx,
}

pub const SIZE_OP: u32 = 6;
pub const SIZE_A: u32 = 8;
pub const SIZE_B: u32 = 9;
pub const SIZE_C: u32 = 9;
pub const SIZE_Ax: u32 = SIZE_C + SIZE_B + SIZE_A;
pub const SIZE_Bx: u32 = SIZE_C + SIZE_B;

pub const POS_OP: u32 = 0;
pub const POS_A: u32 = POS_OP + SIZE_OP;
pub const POS_C: u32 = POS_A + SIZE_A;
pub const POS_B: u32 = POS_C + SIZE_C;
pub const POS_Bx: u32 = POS_C;
pub const POS_Ax: u32 = POS_A;

pub const MAXARG_A: u32 = ((1 << SIZE_A) - 1);
pub const MAXARG_B: u32 = ((1 << SIZE_B) - 1);
pub const MAXARG_C: u32 = ((1 << SIZE_C) - 1);
pub const MAXARG_Ax: u32 = (1 << SIZE_Ax) - 1;
pub const MAXARG_Bx: u32 = (1 << SIZE_Bx) - 1;
pub const MAXARG_sBx: i32 = (MAXARG_Bx as i32) >> 1;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    // A B
    // R(A) := R(B)
    Move = 0,
    // A Bx
    // R(A) := Kst(Bx)
    LoadK,
    // A
    // R(A) := Kst(extra arg)
    LoadKx,
    // A B C
    // R(A) := (bool)B;
    // if (C) pc++
    LoadBool,
    // A B
    // R(A), R(A + 1), ..., R(A + B) := nil
    LoadNil,
    // A B
    // R(A) := UpValue[B]
    GetUpVal,

    // A B C
    // R(A) := UpValue[B][RK(C)]
    GetTabUp,
    // A B C
    // R(A) := R(B)[RK(C)]
    GetTable,

    // A B C
    // UpValue[A][RK(B)] := RK(C)
    SetTabUp,
    // A B
    // UpValue[B] := R(A)
    SetUpVal,
    // A B C
    // R(A)[RK(B)] := RK(C)
    SetTable,

    // A B C
    // R(A) := {} (size = B, C)
    NewTable,

    // A B C
    // R(A + 1) := R(B);
    // R(A) := R(B)[RK(C)]
    Self_,

    // A B C
    // R(A) := RK(B) + RK(C)
    Add,
    // A B C
    // R(A) := RK(B) - RK(C)
    Sub,
    // A B C
    // R(A) := RK(B) * RK(C)
    Mul,
    // A B C
    // R(A) := RK(B) % RK(C)
    Mod,
    // A B C
    // R(A) := RK(B) ^ RK(C)
    Pow,
    // A B C
    // R(A) := RK(B) / RK(C)
    Div,
    // A B C
    // R(A) := RK(B) // RK(C)
    IDiv,
    // A B C
    // R(A) := RK(B) & RK(C)
    BAdd,
    // A B C
    // R(A) := RK(B) | RK(C)
    BOr,
    // A B C
    // R(A) := RK(B) ~ RK(C)
    BXor,
    // A B C
    // R(A) := RK(B) << RK(C)
    Shl,
    // A B C
    // R(A) := RK(B) >> RK(C)
    Shr,

    // A B
    // R(A) := -R(B)
    Unm,
    // A B
    // R(A) := ~R(B)
    BNot,
    // A B
    // R(A) := not R(B)
    Not,
    // A B
    // R(A) := # R(B)
    Len,

    // A B C
    // R(A) := R(B).. ... ..R(C)
    Concat,

    // A sBx
    // pc += sBx;
    // if (A) close all upvalues >= R(A - 1)
    Jmp,

    // A B C
    // if ((RK(B) == RK(C)) ~= A) then pc++
    Eq,
    // A B C
    // if ((RK(B) < RK(C)) ~= A) then pc++
    Lt,
    // A B C
    // if ((RK(B) <= RK(C)) ~= A) then pc++
    Le,

    // A C
    // if not (R(A) <=> C) then pc++
    Test,
    // A B C
    // if (R(B) <=> C) then R(A) := R(B) else pc++
    TestSet,

    // A B C
    // R(A), ... , R(A + C - 2) := R(A)(R(A + 1), ... , R(A + B - 1))
    Call,

    // A B C
    // return R(A)(R(A + 1), ..., R(A + B - 1))
    TailCall,

    // A B
    // return R(A), ... , R(A + B - 2)
    Return,

    // A sBx
    // R(A) += R(A+2);
    // if R(A) <= R(A + 1) then { pc += sBx; R(A+3) = R(A) }
    ForLoop,
    // A sBx
    // R(A) -= R(A + 2);
    // pc += sBx
    ForPrep,

    // A C
    // R(A + 3), ... R(A + 2 + C) := R(A)(R(A + 1), R(A + 2))
    TForCall,
    // A sBx
    // if R(A + 1) ~= nil then { R(A) = R(A + 1); pc += sBx }
    TForLoop,

    // A B C
    // R(A)[(C-1)*FPF + i] := R(A + i), i <= i <= B
    SetList,

    // A Bx
    // R(A) := closure(KPROTO[Bx])
    Closure,

    // A B
    // R(A), R(A + 1), ..., R(A + B - 2) = vararg
    Vararg,

    // Ax
    // extra (larger) argument for previous opcode
    ExtraArg,
}

impl OpCode {
    pub fn from_u32(u: u32) -> OpCode {
        match u {
            _ if OpCode::Move as u32 == u => OpCode::Move,
            _ if OpCode::LoadK as u32 == u => OpCode::LoadK,
            _ if OpCode::LoadKx as u32 == u => OpCode::LoadKx,
            _ if OpCode::LoadBool as u32 == u => OpCode::LoadBool,
            _ if OpCode::LoadNil as u32 == u => OpCode::LoadNil,
            _ if OpCode::GetUpVal as u32 == u => OpCode::GetUpVal,
            _ if OpCode::GetTabUp as u32 == u => OpCode::GetTabUp,
            _ if OpCode::GetTable as u32 == u => OpCode::GetTable,
            _ if OpCode::SetTabUp as u32 == u => OpCode::SetTabUp,
            _ if OpCode::SetUpVal as u32 == u => OpCode::SetUpVal,
            _ if OpCode::SetTable as u32 == u => OpCode::SetTable,
            _ if OpCode::NewTable as u32 == u => OpCode::NewTable,
            _ if OpCode::Self_ as u32 == u => OpCode::Self_,
            _ if OpCode::Add as u32 == u => OpCode::Add,
            _ if OpCode::Sub as u32 == u => OpCode::Sub,
            _ if OpCode::Mul as u32 == u => OpCode::Mul,
            _ if OpCode::Mod as u32 == u => OpCode::Mod,
            _ if OpCode::Pow as u32 == u => OpCode::Pow,
            _ if OpCode::Div as u32 == u => OpCode::Div,
            _ if OpCode::IDiv as u32 == u => OpCode::IDiv,
            _ if OpCode::BAdd as u32 == u => OpCode::BAdd,
            _ if OpCode::BOr as u32 == u => OpCode::BOr,
            _ if OpCode::BXor as u32 == u => OpCode::BXor,
            _ if OpCode::Shl as u32 == u => OpCode::Shl,
            _ if OpCode::Shr as u32 == u => OpCode::Shr,
            _ if OpCode::Unm as u32 == u => OpCode::Unm,
            _ if OpCode::BNot as u32 == u => OpCode::BNot,
            _ if OpCode::Not as u32 == u => OpCode::Not,
            _ if OpCode::Len as u32 == u => OpCode::Len,
            _ if OpCode::Concat as u32 == u => OpCode::Concat,
            _ if OpCode::Jmp as u32 == u => OpCode::Jmp,
            _ if OpCode::Eq as u32 == u => OpCode::Eq,
            _ if OpCode::Lt as u32 == u => OpCode::Lt,
            _ if OpCode::Le as u32 == u => OpCode::Le,
            _ if OpCode::Test as u32 == u => OpCode::Test,
            _ if OpCode::TestSet as u32 == u => OpCode::TestSet,
            _ if OpCode::Call as u32 == u => OpCode::Call,
            _ if OpCode::TailCall as u32 == u => OpCode::TailCall,
            _ if OpCode::Return as u32 == u => OpCode::Return,
            _ if OpCode::ForLoop as u32 == u => OpCode::ForLoop,
            _ if OpCode::ForPrep as u32 == u => OpCode::ForPrep,
            _ if OpCode::TForCall as u32 == u => OpCode::TForCall,
            _ if OpCode::TForLoop as u32 == u => OpCode::TForLoop,
            _ if OpCode::SetList as u32 == u => OpCode::SetList,
            _ if OpCode::Closure as u32 == u => OpCode::Closure,
            _ if OpCode::Vararg as u32 == u => OpCode::Vararg,
            _ if OpCode::ExtraArg as u32 == u => OpCode::ExtraArg,
            _ => unreachable!("unknown op code : {}!", u),
        }
    }
}

pub struct Instruction(u32);

#[allow(dead_code)]
#[allow(non_snake_case)]
impl Instruction {
    pub fn get_op(&self) -> OpCode {
        OpCode::from_u32(((self.0) >> POS_OP) & Instruction::mask1(SIZE_OP, 0))
    }
    pub fn set_op(&mut self, op: OpCode) {
        self.set_arg(op as u32, POS_OP, SIZE_OP)
    }
    pub fn get_arg_A(&self) -> u32 {
        self.get_arg(POS_A, SIZE_A)
    }
    pub fn set_arg_A(&mut self, value: u32) {
        self.set_arg(value, POS_A, SIZE_A);
    }
    pub fn get_arg_B(&self) -> u32 {
        self.get_arg(POS_B, SIZE_B)
    }
    pub fn set_arg_B(&mut self, value: u32) {
        self.set_arg(value, POS_B, SIZE_B);
    }
    pub fn get_arg_C(&self) -> u32 {
        self.get_arg(POS_C, SIZE_C)
    }
    pub fn set_arg_C(&mut self, value: u32) {
        self.set_arg(value, POS_C, SIZE_C);
    }
    pub fn get_arg_Ax(&self) -> u32 {
        self.get_arg(POS_Ax, SIZE_Ax)
    }
    pub fn set_arg_Ax(&mut self, value: u32) {
        self.set_arg(value, POS_Ax, SIZE_Ax);
    }
    pub fn get_arg_Bx(&self) -> u32 {
        self.get_arg(POS_Bx, SIZE_Bx)
    }
    pub fn set_arg_Bx(&mut self, value: u32) {
        self.set_arg(value, POS_Bx, SIZE_Bx);
    }
    pub fn get_arg_sBx(&mut self) -> i32 {
        (self.get_arg(POS_Bx, SIZE_Bx) as i32) - MAXARG_sBx
    }
    pub fn set_argsBx(&mut self, value: i32) {
        self.set_arg((value + MAXARG_sBx) as u32, POS_Bx, SIZE_Bx);
    }
    fn get_arg(&self, pos: u32, size: u32) -> u32 {
        (self.0 >> pos) & Instruction::mask1(size, 0)
    }
    fn set_arg(&mut self, value: u32, pos: u32, size: u32) {
        self.0 = Instruction::mask0(size, pos) | (value << POS_OP) & Instruction::mask1(size, pos)
    }
    fn mask1(n: u32, p: u32) -> u32 {
        (!((!0u32) << n)) << p
    }
    fn mask0(n: u32, p: u32) -> u32 {
        !Instruction::mask1(n, p)
    }
}
