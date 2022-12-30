#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Illegal,

    // Identifiers + literals
    Ident(String),
    Int(usize),

    // Operators
    Assign,

    Plus,
    Minus,
    Slash,
    Asterisk,

    Equal,
    Bang,
    NotEqual,
    LessThan,
    GreaterThan,

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
    True,
    False,
    If,
    Else,
    Return,
}

pub static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "fn" => Token::Function,
    "let" => Token::Let,
    "true" => Token::True,
    "false" => Token::False,
    "if" => Token::If,
    "else" => Token::Else,
    "return" => Token::Return,
};

