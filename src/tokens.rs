use rslua_traits::Comments;

use crate::types::{FloatType, IntType, Source};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    Goto,
    // '//'
    IDiv,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
    // ..
    Concat,
    // ...
    Dots,
    // ==
    Eq,
    // >=
    Ge,
    // <=
    Le,
    // ~=
    Ne,
    // <<
    Shl,
    // >>
    Shr,
    // ::
    DbColon,
    Eos,
    // float number
    Flt,
    // int number
    Int,
    // name
    Name,
    // string literal
    String,
    // () [] {}
    Lp,
    Rp,
    Ls,
    Rs,
    Lb,
    Rb,
    // + - * / % ^ #
    Add,
    Minus,
    Mul,
    Div,
    Mod,
    Pow,
    Len,
    // =
    Assign,
    // < >
    Lt,
    Gt,
    // & | ~
    BAnd,
    BOr,
    BXor,
    // : , ;
    Colon,
    Comma,
    Semi,
    // .
    Attr,
    // single line comment
    SComment,
    // multi-line comment
    MComment,
}

impl TokenType {
    // convert keyword to token type.
    pub fn from_keyword(word: &str) -> Option<TokenType> {
        match word {
            "and" => Some(TokenType::And),
            "break" => Some(TokenType::Break),
            "do" => Some(TokenType::Do),
            "else" => Some(TokenType::Else),
            "elseif" => Some(TokenType::ElseIf),
            "end" => Some(TokenType::End),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "function" => Some(TokenType::Function),
            "goto" => Some(TokenType::Goto),
            "if" => Some(TokenType::If),
            "in" => Some(TokenType::In),
            "local" => Some(TokenType::Local),
            "nil" => Some(TokenType::Nil),
            "not" => Some(TokenType::Not),
            "or" => Some(TokenType::Or),
            "repeat" => Some(TokenType::Repeat),
            "return" => Some(TokenType::Return),
            "then" => Some(TokenType::Then),
            "true" => Some(TokenType::True),
            "until" => Some(TokenType::Until),
            "while" => Some(TokenType::While),
            _ => None,
        }
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, TokenType::SComment | TokenType::MComment)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    None,
    Float(FloatType),
    Int(IntType),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub t: TokenType,
    pub value: TokenValue,
    pub source: Source,
    pub comments: Vec<Token>,
}

impl Token {
    pub fn get_float(&self) -> FloatType {
        match self.value {
            TokenValue::Float(f) => f,
            _ => unreachable!(),
        }
    }
    pub fn get_int(&self) -> IntType {
        match self.value {
            TokenValue::Int(i) => i,
            _ => unreachable!(),
        }
    }
    pub fn get_string(&self) -> String {
        match &self.value {
            TokenValue::Str(s) => s.clone(),
            _ => unreachable!(),
        }
    }
    pub fn get_str(&self) -> &str {
        match &self.value {
            TokenValue::Str(s) => s,
            _ => unreachable!(),
        }
    }
    pub fn is_comment(&self) -> bool {
        self.t.is_comment()
    }
}

impl Comments for Token {
    fn get_comments(&self) -> Vec<&str> {
        return self.comments.iter().map(|t| t.get_str()).collect();
    }
}
