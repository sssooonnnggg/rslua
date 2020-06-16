use crate::compiler::CompileError;
use crate::success;
use crate::types::{FloatType, IntType};
use num_traits::Float;
use std::hash::{Hash, Hasher};
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

fn float_to_int(f: FloatType) -> Option<IntType> {
    if f.floor() == f {
        Some(f as IntType)
    } else {
        None
    }
}

macro_rules! bin_op {
    ($name:ident, $int_int:expr, $int_float:expr, $float_int:expr, $float_float:expr) => {
        pub fn $name(self, other: Const) -> Result<Option<Const>, CompileError> {
            let result = match self {
                Const::Int(a) => match other {
                    Const::Int(b) => $int_int(a, b),
                    Const::Float(b) => $int_float(a, b),
                    _ => unreachable!(),
                },
                Const::Float(a) => match other {
                    Const::Int(b) => $float_int(a, b),
                    Const::Float(b) => $float_float(a, b),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };

            // compitibale with lua
            // not constant folding Nan/inf/0.0
            match &result {
                Ok(k) => match k {
                    Some(k) => match k {
                        Const::Float(f) if (*f).is_nan() || (*f).is_infinite() || *f == 0.0 => Ok(None),
                        _ => result,
                    },
                    _ => result,
                },
                _ => result,
            }
        }
    };
}

macro_rules! bin_op_normal {
    ($name:ident, $op:tt) => {
        bin_op! {
            $name,
            |a, b| success!(Const::Int(a $op b)),
            |a, b| success!(Const::Float(a as FloatType $op b)),
            |a, b| success!(Const::Float(a $op b as FloatType)),
            |a, b| success!(Const::Float(a $op b))
        }
    };
}

macro_rules! bin_op_int {
    ($name:ident, $op:tt) => {
        bin_op! {
            $name,
            |a, b| success!(Const::Int(a $op b)),
            |a, b| Ok(float_to_int(b).map(|b| Const::Int(a $op b))),
            |a, b| Ok(float_to_int(a).map(|a| Const::Int(a $op b))),
            |a, b| Ok(float_to_int(a).and_then(|a| float_to_int(b).and_then(|b| Some(Const::Int(a $op b)))))
        }
    };
}

impl Const {
    bin_op_normal! {add, +}
    bin_op_normal! {sub, -}
    bin_op_normal! {mul, *}

    bin_op! {
        div,
        |a, b| success!(Const::Float(a as FloatType / b as FloatType)),
        |a, b| success!(Const::Float(a as FloatType / b)),
        |a, b| success!(Const::Float(a / b as FloatType)),
        |a, b| success!(Const::Float(a / b))
    }

    bin_op! {
        idiv,
        |a, b| success!(Const::Int(a / b)),
        |_, _| Ok(None),
        |_, _| Ok(None),
        |_, _| Ok(None)
    }

    bin_op! {
        mod_,
        |a, b| success!(Const::Int(a % b)),
        |a, b| success!(Const::Float(a as FloatType % b)),
        |a, b| success!(Const::Float(a % b as FloatType)),
        |a, b| success!(Const::Float(a % b))
    }

    bin_op! {
        pow,
        |a, b| success!(Const::Float((a as FloatType).powf(b as FloatType))),
        |a, b| success!(Const::Float((a as FloatType).powf(b))),
        |a:FloatType, b| success!(Const::Float(a.powf(b as FloatType))),
        |a:FloatType, b| success!(Const::Float(a.powf(b)))
    }

    bin_op_int! {band, &}
    bin_op_int! {bor, |}
    bin_op_int! {bxor, ^}
    bin_op_int! {shl, <<}
    bin_op_int! {shr, >>}
}
