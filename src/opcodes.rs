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

pub const SIZE_OP: usize = 6;
pub const SIZE_A: usize = 8;
pub const SIZE_B: usize = 9;
pub const SIZE_C: usize = 9;
pub const SIZE_Ax: usize = SIZE_C + SIZE_B + SIZE_A;
pub const SIZE_Bx: usize = SIZE_C + SIZE_B;

pub const POS_OP: usize = 0;
pub const POS_A: usize = POS_OP + SIZE_OP;
pub const POS_C: usize = POS_A + SIZE_A;
pub const POS_B: usize = POS_C + SIZE_C;
pub const POS_Bx: usize = POS_C;
pub const POS_Ax: usize = POS_A;

pub const MAXARG_A: usize = ((1 << SIZE_A) - 1);
pub const MAXARG_B: usize = ((1 << SIZE_B) - 1);
pub const MAXARG_C: usize = ((1 << SIZE_C) - 1);
pub const MAXARG_Ax: usize = (1 << SIZE_Ax) - 1;
pub const MAXARG_Bx: usize = (1 << SIZE_Bx) - 1;
pub const MAXARG_sBx: usize = MAXARG_Bx >> 1;

pub enum OpCode {
    // A B
    // R(A) := R(B)
    Move,
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
