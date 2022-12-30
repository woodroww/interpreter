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

impl Token {
    pub fn literal(&self) -> &'static str {
        match self {
            Token::Illegal => "ILLEGAL",
            Token::Ident(_) => "IDENT",
            Token::Int(_) => "INT",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Slash => "/",
            Token::Asterisk => "*",
            Token::Equal => "==",
            Token::Bang => "!",
            Token::NotEqual => "!=",
            Token::LessThan => "<",
            Token::GreaterThan => ">",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::Lparen => "(",
            Token::Rparen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Function => "FUNCTION",
            Token::Let => "LET",
            Token::True => "TRUE",
            Token::False => "FALSE",
            Token::If => "IF",
            Token::Else => "ELSE",
            Token::Return => "RETURN",
        }
    }
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

