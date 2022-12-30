use std::{iter::Peekable, str::Chars};
use crate::token::{Token, KEYWORDS};

struct Lexer<'a> {
    code: &'a str,
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
        let token = match c {
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '/' => Some(Token::Slash),
            '*' => Some(Token::Asterisk),
            ',' => Some(Token::Comma),
            ';' => Some(Token::Semicolon),
            '(' => Some(Token::Lparen),
            ')' => Some(Token::Rparen),
            '{' => Some(Token::Lbrace),
            '}' => Some(Token::Rbrace),
            '<' => Some(Token::LessThan),
            '>' => Some(Token::GreaterThan),
            '=' => {
                if let Some(next) = self.chars.peek() {
                    if *next == '=' {
                        self.chars.next();
                        Some(Token::Equal)
                    } else {
                        Some(Token::Assign)
                    }
                } else {
                    Some(Token::Assign)
                }
            }
            '!' => {
                if let Some(next) = self.chars.peek() {
                    if *next == '=' {
                        self.chars.next();
                        Some(Token::NotEqual)
                    } else {
                        Some(Token::Bang)
                    }
                } else {
                    Some(Token::Bang)
                }
            }
            c if c.is_numeric() => {
                Some(Token::Int(self.read_number(c)))
            }
            c if !c.is_whitespace() => {
                let ident = self.read_identifier(c);
                let entry: Option<&Token> = KEYWORDS.get(&ident);
                if let Some(token) = entry {
                    Some(token.clone())
                } else {
                    Some(Token::Ident(ident))
                }
            }
            _ => Some(Token::Illegal),
        };
        token
    }
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        Lexer { code, chars: code.chars().peekable() }
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
        let tokens = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
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
        let tokens = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
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
        let tokens = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
// !-/*5;
// 5 < 10 > 5;";
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::Semicolon,
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
        let tokens = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_next_token_5() {
        let input = "10 == 10;
10 != 9;";
        let tokens = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        let expected = vec![
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEqual,
            Token::Int(9),
            Token::Semicolon,
        ];
        assert_eq!(tokens, expected);
    }
}
