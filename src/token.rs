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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal())
    }
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

    pub fn token_type(&self) -> String {
        match self {
            Token::Illegal => "ILLEGAL".to_string(),
            Token::Ident(_) => "IDENT".to_string(),
            Token::Int(_) => "INT".to_string(),
            Token::Assign => "ASSIGN".to_string(),
            Token::Plus => "PLUS".to_string(),
            Token::Minus => "MINUS".to_string(),
            Token::Slash => "SLASH".to_string(),
            Token::Asterisk => "ASTERISK".to_string(),
            Token::Equal => "EQUAL".to_string(),
            Token::Bang => "BANG".to_string(),
            Token::NotEqual => "NOTEQUAL".to_string(),
            Token::LessThan => "LESSTHAN".to_string(),
            Token::GreaterThan => "GREATERTHAN".to_string(),
            Token::Comma => "COMMA".to_string(),
            Token::Semicolon => "SEMICOLON".to_string(),
            Token::Lparen => "LPAREN".to_string(),
            Token::Rparen => "RPAREN".to_string(),
            Token::Lbrace => "LBRACE".to_string(),
            Token::Rbrace => "RBRACE".to_string(),
            Token::Function => "FUNCTION".to_string(),
            Token::Let => "LET".to_string(),
            Token::True => "TRUE".to_string(),
            Token::False => "FALSE".to_string(),
            Token::If => "IF".to_string(),
            Token::Else => "ELSE".to_string(),
            Token::Return => "RETURN".to_string(),
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

