#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Illegal,

    // Identifiers + literals
    Ident(String),
    Int(usize),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}
