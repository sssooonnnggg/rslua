use num_traits::Float;
use std::hash::{Hash, Hasher};
use crate::types::{FloatType, IntType};
#[derive(Clone, PartialEq)]
pub enum Const {
    Int(IntType),
    Float(FloatType),
    Str(String),
}

impl Eq for Const {}

impl Hash for Const {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Const::Int(i) => i.hash(state),
            Const::Float(f) => {
                let (m, e, s) = Float::integer_decode(*f);
                m.hash(state);
                e.hash(state);
                s.hash(state);
            }
            Const::Str(s) => s.hash(state),
        }
    }
}

macro_rules! bin_op {
    ($name:ident, $int_int:expr, $int_float:expr, $float_int:expr, $float_float:expr) => {
        pub fn $name(self, other: Const) -> Option<Const> {
            match self {
                Const::Int(a) => match other {
                    Const::Int(b) => $int_int(a, b),
                    Const::Float(b) => $int_float(a, b),
                    _ => unreachable!()
                },
                Const::Float(a) => match other {
                    Const::Int(b) => $float_int(a, b),
                    Const::Float(b) => $float_float(a, b),
                    _ => unreachable!()
                },
                _ => unreachable!()
            }
        }    
    };
}

macro_rules! bin_op_normal {
    ($name:ident, $op:tt) => {
        bin_op! {
            $name, 
            |a, b| Some(Const::Int(a $op b)), 
            |a, b| Some(Const::Float(a as FloatType $op b)), 
            |a, b| Some(Const::Float(a $op b as FloatType)), 
            |a, b| Some(Const::Float(a $op b))
        } 
    };
}

macro_rules! bin_op_int {
    ($name:ident, $op:tt) => {
        bin_op! {
            $name, 
            |a, b| Some(Const::Int(a $op b)), 
            |a, b| None, 
            |a, b| None, 
            |a, b| None
        } 
    };
}

impl Const {
    bin_op_normal! {add, +}
    bin_op_normal! {sub, -}
    bin_op_normal! {mul, *}
    
    bin_op! {
        div,
        |a, b| Some(Const::Float(a as FloatType / b as FloatType)),
        |a, b| Some(Const::Float(a as FloatType / b)),
        |a, b| Some(Const::Float(a / b as FloatType)),
        |a, b| Some(Const::Float(a / b as FloatType))
    }

    bin_op! {
        idiv,
        |a, b| Some(Const::Int(a / b)),
        |a, b| Some(Const::Float((a / b as IntType) as FloatType)),
        |a, b| Some(Const::Float((a as IntType / b) as FloatType)),
        |a, b| Some(Const::Float((a as IntType / b as IntType) as FloatType))
    }

    bin_op! {
        mod_,
        |a, b| Some(Const::Int(a % b)),
        |a, b| Some(Const::Float(a as FloatType % b)),
        |a, b| Some(Const::Float(a % b as FloatType)),
        |a, b| Some(Const::Float(a % b))
    }

    bin_op! {
        pow,
        |a, b| Some(Const::Float((a as FloatType).powf(b as FloatType))),
        |a, b| Some(Const::Float((a as FloatType).powf(b))),
        |a:FloatType, b| Some(Const::Float(a.powf(b as FloatType))),
        |a:FloatType, b| Some(Const::Float(a.powf(b)))
    }

    bin_op_int! {band, &}
    bin_op_int! {bor, |}
    bin_op_int! {bxor, ^}
    bin_op_int! {shl, <<}
    bin_op_int! {shr, >>}
}