use crate::token::{Token, TokenType, KEYWORDS};
use std::{iter::Peekable, str::Chars};

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let c = self.chars.next();
        if c.is_none() {
            return None;
        }
        let c = c.unwrap();
        let mut token = Token::new(TokenType::Illegal, "");
        match c {
            '+' => token.token_type = TokenType::Plus,
            '-' => token.token_type = TokenType::Minus,
            '/' => token.token_type = TokenType::Slash,
            '*' => token.token_type = TokenType::Asterisk,
            ',' => token.token_type = TokenType::Comma,
            ';' => token.token_type = TokenType::Semicolon,
            '(' => token.token_type = TokenType::Lparen,
            ')' => token.token_type = TokenType::Rparen,
            '{' => token.token_type = TokenType::Lbrace,
            '}' => token.token_type = TokenType::Rbrace,
            '<' => token.token_type = TokenType::LessThan,
            '>' => token.token_type = TokenType::GreaterThan,
            '=' => {
                if let Some(next) = self.chars.peek() {
                    if *next == '=' {
                        self.chars.next();
                        token.token_type = TokenType::Equal;
                    } else {
                        token.token_type = TokenType::Assign;
                    }
                } else {
                    token.token_type = TokenType::Assign;
                }
            }
            '!' => {
                if let Some(next) = self.chars.peek() {
                    if *next == '=' {
                        self.chars.next();
                        token.token_type = TokenType::NotEqual;
                    } else {
                        token.token_type = TokenType::Bang;
                    }
                } else {
                    token.token_type = TokenType::Bang;
                }
            }
            c if c.is_numeric() => {
                token.token_type = TokenType::Int;
                token.literal = self.read_number(c).to_string();
            }
            c if !c.is_whitespace() => {
                let ident = self.read_identifier(c);
                let entry: Option<&TokenType> = KEYWORDS.get(&ident);
                if let Some(keyword_token) = entry {
                    token.token_type = keyword_token.clone();
                } else {
                    token.token_type = TokenType::Ident;
                }
            }
            _ => token.token_type = TokenType::Illegal,
        };
        Some(token)
    }
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        Lexer {
            chars: code.chars().peekable(),
        }
    }

    pub fn read_char(&mut self) {
        self.chars.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.chars.next();
        }
    }

    fn read_identifier(&mut self, start_char: char) -> String {
        let mut ident = start_char.to_string();
        while let Some(c) = self.chars.peek() {
            if !valid_identifier_char(*c) {
                break;
            }
            ident.push(*c);
            self.chars.next();
        }
        ident
    }

    fn read_number(&mut self, start_char: char) -> usize {
        let mut number = start_char.to_string();
        while let Some(c) = self.chars.peek() {
            if c.is_numeric() {
                number.push(*c);
                self.chars.next();
            } else {
                break;
            }
        }
        number.parse::<usize>().expect("idk what happened")
    }
}

fn valid_identifier_char(c: char) -> bool {
    !c.is_whitespace() && (c.is_alphabetic() || c == '_')
}

#[cfg(test)]
mod test {

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let tokens = Lexer::new(input)
            .into_iter()
            .map(|x| x.token_type)
            .collect::<Vec<TokenType>>();
        let expected = vec![
            TokenType::Assign,
            TokenType::Plus,
            TokenType::Lparen,
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Rbrace,
            TokenType::Comma,
            TokenType::Semicolon,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_next_token_2() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);";
        let tokens = Lexer::new(input).into_iter().map(|x| x.token_type).collect::<Vec<TokenType>>();
        let expected = vec![
            TokenType::Let,
            TokenType::Ident, //("five".to_string()),
            TokenType::Assign,
            TokenType::Int, //(5),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("ten".to_string()),
            TokenType::Assign,
            TokenType::Int, //(10),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("add".to_string()),
            TokenType::Assign,
            TokenType::Function,
            TokenType::Lparen,
            TokenType::Ident, //("x".to_string()),
            TokenType::Comma,
            TokenType::Ident, //("y".to_string()),
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Ident, //("x".to_string()),
            TokenType::Plus,
            TokenType::Ident, //("y".to_string()),
            TokenType::Semicolon,
            TokenType::Rbrace,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("result".to_string()),
            TokenType::Assign,
            TokenType::Ident, //("add".to_string()),
            TokenType::Lparen,
            TokenType::Ident, //("five".to_string()),
            TokenType::Comma,
            TokenType::Ident, //("ten".to_string()),
            TokenType::Rparen,
            TokenType::Semicolon,
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_next_token_3() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;";
        let tokens = Lexer::new(input).into_iter().map(|x| x.token_type).collect::<Vec<TokenType>>();
        let expected = vec![
            TokenType::Let,
            TokenType::Ident, //("five".to_string()),
            TokenType::Assign,
            TokenType::Int, //(5),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("ten".to_string()),
            TokenType::Assign,
            TokenType::Int, //(10),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("add".to_string()),
            TokenType::Assign,
            TokenType::Function,
            TokenType::Lparen,
            TokenType::Ident, //("x".to_string()),
            TokenType::Comma,
            TokenType::Ident, //("y".to_string()),
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Ident, //("x".to_string()),
            TokenType::Plus,
            TokenType::Ident, //("y".to_string()),
            TokenType::Semicolon,
            TokenType::Rbrace,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("result".to_string()),
            TokenType::Assign,
            TokenType::Ident, //("add".to_string()),
            TokenType::Lparen,
            TokenType::Ident, //("five".to_string()),
            TokenType::Comma,
            TokenType::Ident, //("ten".to_string()),
            TokenType::Rparen,
            TokenType::Semicolon,
            // !-/*5;
            // 5 < 10 > 5;";
            TokenType::Bang,
            TokenType::Minus,
            TokenType::Slash,
            TokenType::Asterisk,
            TokenType::Int, //(5),
            TokenType::Semicolon,
            TokenType::Int, //(5),
            TokenType::LessThan,
            TokenType::Int, //(10),
            TokenType::GreaterThan,
            TokenType::Int, //(5),
            TokenType::Semicolon,
        ];
        //Equal,
        //NotEqual,
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_next_token_4() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}";
        let tokens = Lexer::new(input).into_iter().map(|x| x.token_type).collect::<Vec<TokenType>>();
        let expected = vec![
            TokenType::Let,
            TokenType::Ident, //("five".to_string()),
            TokenType::Assign,
            TokenType::Int, //(5),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("ten".to_string()),
            TokenType::Assign,
            TokenType::Int, //(10),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("add".to_string()),
            TokenType::Assign,
            TokenType::Function,
            TokenType::Lparen,
            TokenType::Ident, //("x".to_string()),
            TokenType::Comma,
            TokenType::Ident, //("y".to_string()),
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Ident, //("x".to_string()),
            TokenType::Plus,
            TokenType::Ident, //("y".to_string()),
            TokenType::Semicolon,
            TokenType::Rbrace,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident, //("result".to_string()),
            TokenType::Assign,
            TokenType::Ident, //("add".to_string()),
            TokenType::Lparen,
            TokenType::Ident, //("five".to_string()),
            TokenType::Comma,
            TokenType::Ident, //("ten".to_string()),
            TokenType::Rparen,
            TokenType::Semicolon,
            TokenType::Bang,
            TokenType::Minus,
            TokenType::Slash,
            TokenType::Asterisk,
            TokenType::Int, //(5),
            TokenType::Semicolon,
            TokenType::Int, //(5),
            TokenType::LessThan,
            TokenType::Int, //(10),
            TokenType::GreaterThan,
            TokenType::Int, //(5),
            TokenType::Semicolon,
            TokenType::If,
            TokenType::Lparen,
            TokenType::Int, //(5),
            TokenType::LessThan,
            TokenType::Int, //(10),
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Return,
            TokenType::True,
            TokenType::Semicolon,
            TokenType::Rbrace,
            TokenType::Else,
            TokenType::Lbrace,
            TokenType::Return,
            TokenType::False,
            TokenType::Semicolon,
            TokenType::Rbrace,
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_next_token_5() {
        let input = "10 == 10;
10 != 9;";
        let tokens = Lexer::new(input).into_iter().map(|x| x.token_type).collect::<Vec<TokenType>>();
        let expected = vec![
            TokenType::Int, //(10),
            TokenType::Equal,
            TokenType::Int, //(10),
            TokenType::Semicolon,
            TokenType::Int, //(10),
            TokenType::NotEqual,
            TokenType::Int, //(9),
            TokenType::Semicolon,
        ];
        assert_eq!(tokens, expected);
    }
}
