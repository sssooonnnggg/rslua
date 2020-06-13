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

macro_rules! bin_op_normal {
    ($name:ident, $op:tt) => {
        pub fn $name(self, other: Const) -> Option<Const> {
            match self {
                Const::Int(a) => match other {
                    Const::Int(b) => Some(Const::Int(a $op b)),
                    Const::Float(b) => Some(Const::Float(a as f64 $op b)),
                    _ => unreachable!()
                },
                Const::Float(a) => match other {
                    Const::Int(b) => Some(Const::Float(a $op b as f64)),
                    Const::Float(b) => Some(Const::Float(a $op b)),
                    _ => unreachable!()
                },
                _ => unreachable!()
            }
        }    
    };
}

macro_rules! bin_op_int {
    ($name:ident, $op:tt) => {
        pub fn $name(self, other: Const) -> Option<Const> {
            match self {
                Const::Int(a) => match other {
                    Const::Int(b) => Some(Const::Int(a $op b)),
                    _ => None
                },
                Const::Float(a) => match other {
                    _ => None
                },
                _ => None
            }
        }    
    };
}

impl Const {
    bin_op_normal! {add, +}
    bin_op_normal! {sub, -}
    bin_op_normal! {mul, *}

    pub fn div(self, other: Const) -> Option<Const> {
        match self {
            Const::Int(a) => match other {
                Const::Int(b) => Some(Const::Float(a as f64 / b as f64)),
                Const::Float(b) => Some(Const::Float(a as f64 / b)),
                _ => unreachable!()
            },
            Const::Float(a) => match other {
                Const::Int(b) => Some(Const::Float(a / b as f64)),
                Const::Float(b) => Some(Const::Float(a / b)),
                _ => unreachable!()
            },
            _ => unreachable!()
        }
    }
    
    pub fn idiv(self, other: Const) -> Option<Const> {
        match self {
            Const::Int(a) => match other {
                Const::Int(b) => Some(Const::Int(a / b)),
                Const::Float(b) => Some(Const::Float((a / b as i64) as f64)),
                _ => unreachable!()
            },
            Const::Float(a) => match other {
                Const::Int(b) => Some(Const::Float((a as i64 / b) as f64)),
                Const::Float(b) => Some(Const::Float((a as i64 / b as i64) as f64)),
                _ => unreachable!()
            },
            _ => unreachable!()
        }
    }

    pub fn mod_(self, other: Const) -> Option<Const> {
        match self {
            Const::Int(a) => match other {
                Const::Int(b) => Some(Const::Int(a % b)),
                Const::Float(b) => Some(Const::Float(a as f64 % b)),
                _ => unreachable!()
            },
            Const::Float(a) => match other {
                Const::Int(b) => Some(Const::Float(a % b as f64)),
                Const::Float(b) => Some(Const::Float(a % b)),
                _ => unreachable!()
            },
            _ => unreachable!()
        }
    }

    pub fn pow(self, other: Const) -> Option<Const> {
        match self {
            Const::Int(a) => match other {
                Const::Int(b) => Some(Const::Float((a as f64).powf(b as f64))),
                Const::Float(b) => Some(Const::Float((a as f64).powf(b))),
                _ => unreachable!()
            },
            Const::Float(a) => match other {
                Const::Int(b) => Some(Const::Float(a.powf(b as f64))),
                Const::Float(b) => Some(Const::Float(a.powf(b))),
                _ => unreachable!()
            },
            _ => unreachable!()
        }
    }

    bin_op_int! {band, &}
    bin_op_int! {bor, |}
    bin_op_int! {bxor, ^}
    bin_op_int! {shl, <<}
    bin_op_int! {shr, >>}
}