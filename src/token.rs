
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Copy, Hash, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum TokenType {
    Illegal,

    // Identifiers + literals
    Ident,
    Int,
    String,

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
    Lbracket,
    Rbracket,
    Colon,

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
            "[" => TokenType::Lbracket,
            "]" => TokenType::Rbracket,
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
            TokenType::Illegal => "illegal",
            TokenType::Ident => "",
            TokenType::Int => "",
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Slash => "/",
            TokenType::Asterisk => "*",
            TokenType::Equal => "==",
            TokenType::Bang => "!",
            TokenType::NotEqual => "!=",
            TokenType::LessThan => "<",
            TokenType::GreaterThan => ">",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Function => "fn",
            TokenType::Let => "let",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
            TokenType::String => "",
            TokenType::Lbracket => "[",
            TokenType::Rbracket => "]",
            TokenType::Colon => ":",
        }.to_string()
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
            TokenType::Lbracket => "LBRACKET",
            TokenType::Rbracket => "RBRACKET",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",
            TokenType::String => "STRING",
            TokenType::Colon => "COLON",
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

