pub type IntType = i64;
pub type FloatType = f64;
pub enum Number {
    Int(IntType),
    Float(FloatType),
    None,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Source {
    pub line: usize,
    pub col: usize,
    pub length: usize,
}

impl Source {
    pub fn new() -> Self {
        Source {
            length: 0,
            line: 0,
            col: 0,
        }
    }
}
