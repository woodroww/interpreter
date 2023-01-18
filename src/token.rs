
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: &str) -> Self {
        Self { token_type, literal: literal.to_string() }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Copy, Hash, Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    Illegal,

    // Identifiers + literals
    Ident,
    Int,

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

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_string())
    }
}

impl TokenType {

    pub fn type_from_str(input: &str) -> Option<TokenType> {
        Some(match input {
            "illegal" => TokenType::Illegal,
            "=" => TokenType::Assign,
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "/" => TokenType::Slash,
            "*" => TokenType::Asterisk,
            "==" => TokenType::Equal,
            "!" => TokenType::Bang,
            "!=" => TokenType::NotEqual,
            "<" => TokenType::LessThan,
            ">" => TokenType::GreaterThan,
            "," => TokenType::Comma,
            ";" => TokenType::Semicolon,
            "(" => TokenType::Lparen,
            ")" => TokenType::Rparen,
            "{" => TokenType::Lbrace,
            "}" => TokenType::Rbrace,
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            _ => TokenType::Illegal,
        })
    }

    pub fn literal(&self) -> String {
        match self {
            TokenType::Illegal => "illegal".to_string(),
            TokenType::Ident => "".to_string(),
            TokenType::Int => "".to_string(),
            TokenType::Assign => "=".to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Slash => "/".to_string(),
            TokenType::Asterisk => "*".to_string(),
            TokenType::Equal => "==".to_string(),
            TokenType::Bang => "!".to_string(),
            TokenType::NotEqual => "!=".to_string(),
            TokenType::LessThan => "<".to_string(),
            TokenType::GreaterThan => ">".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::Lparen => "(".to_string(),
            TokenType::Rparen => ")".to_string(),
            TokenType::Lbrace => "{".to_string(),
            TokenType::Rbrace => "}".to_string(),
            TokenType::Function => "fn".to_string(),
            TokenType::Let => "let".to_string(),
            TokenType::True => "true".to_string(),
            TokenType::False => "false".to_string(),
            TokenType::If => "if".to_string(),
            TokenType::Else => "else".to_string(),
            TokenType::Return => "return".to_string(),
        }
    }

    pub const fn token_string(&self) -> &'static str {
        match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Ident => "IDENT",
            TokenType::Int => "INT",
            TokenType::Assign => "ASSIGN",
            TokenType::Plus => "PLUS",
            TokenType::Minus => "MINUS",
            TokenType::Slash => "SLASH",
            TokenType::Asterisk => "ASTERISK",
            TokenType::Equal => "EQUAL",
            TokenType::Bang => "BANG",
            TokenType::NotEqual => "NOTEQUAL",
            TokenType::LessThan => "LESSTHAN",
            TokenType::GreaterThan => "GREATERTHAN",
            TokenType::Comma => "COMMA",
            TokenType::Semicolon => "SEMICOLON",
            TokenType::Lparen => "LPAREN",
            TokenType::Rparen => "RPAREN",
            TokenType::Lbrace => "LBRACE",
            TokenType::Rbrace => "RBRACE",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",
        }
    }
}

pub static KEYWORDS: phf::Map<&'static str, TokenType> = phf::phf_map! {
    "fn" => TokenType::Function,
    "let" => TokenType::Let,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "return" => TokenType::Return,
};

