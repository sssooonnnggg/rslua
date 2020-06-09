pub type IntType = i64;
pub type FloatType = f64;
pub enum Number {
    Int(IntType),
    Float(FloatType),
    None,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Source {
    pub pos: usize,
    pub length: usize,
    pub line: usize,
    pub col: usize,
}

impl Source {
    pub fn new() -> Self {
        Source {
            pos: 0,
            length: 0,
            line: 0,
            col: 0,
        }
    }
}
use std::ops;
impl ops::Sub<Source> for Source {
    type Output = Source;
    fn sub(self, rhs: Source) -> Source {
        Source {
            pos: self.pos,
            length: rhs.pos + rhs.length - self.pos,
            line: self.line,
            col: self.col,
        }
    }
}
