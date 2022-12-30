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
    pub fn literal(&self) -> String {
        match self {
            Token::Illegal => "illegal".to_string(),
            Token::Ident(ident) => ident.to_string(),
            Token::Int(n) => n.to_string(),
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Slash => "/".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Equal => "==".to_string(),
            Token::Bang => "!".to_string(),
            Token::NotEqual => "!=".to_string(),
            Token::LessThan => "<".to_string(),
            Token::GreaterThan => ">".to_string(),
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Lparen => "(".to_string(),
            Token::Rparen => ")".to_string(),
            Token::Lbrace => "{".to_string(),
            Token::Rbrace => "}".to_string(),
            Token::Function => "function".to_string(),
            Token::Let => "let".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
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

