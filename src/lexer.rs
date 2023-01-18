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
        let token_literal; //= String::new();
        let token_type;

        match c {
            '+' => {
                token_type = TokenType::Plus;
                token_literal = c.to_string();
            }
            '-' => {
                token_type = TokenType::Minus;
                token_literal = c.to_string();
            }
            '/' => {
                token_type = TokenType::Slash;
                token_literal = c.to_string();
            }
            '*' => {
                token_type = TokenType::Asterisk;
                token_literal = c.to_string();
            }
            ',' => {
                token_type = TokenType::Comma;
                token_literal = c.to_string();
            }
            ';' => {
                token_type = TokenType::Semicolon;
                token_literal = c.to_string();
            }
            '(' => {
                token_type = TokenType::Lparen;
                token_literal = c.to_string();
            }
            ')' => {
                token_type = TokenType::Rparen;
                token_literal = c.to_string();
            }
            '{' => {
                token_type = TokenType::Lbrace;
                token_literal = c.to_string();
            }
            '}' => {
                token_type = TokenType::Rbrace;
                token_literal = c.to_string();
            }
            '<' => {
                token_type = TokenType::LessThan;
                token_literal = c.to_string();
            }
            '>' => {
                token_type = TokenType::GreaterThan;
                token_literal = c.to_string();
            }
            '=' => {
                if let Some(next) = self.chars.peek() {
                    if *next == '=' {
                        self.chars.next();
                        token_type = TokenType::Equal;
                    } else {
                        token_type = TokenType::Assign;
                    }
                } else {
                    token_type = TokenType::Assign;
                }
                token_literal = token_type.literal();
            }
            '!' => {
                if let Some(next) = self.chars.peek() {
                    if *next == '=' {
                        self.chars.next();
                        token_type = TokenType::NotEqual;
                    } else {
                        token_type = TokenType::Bang;
                    }
                } else {
                    token_type = TokenType::Bang;
                }
                token_literal = token_type.literal();
            }
            c if c.is_numeric() => {
                token_type = TokenType::Int;
                token_literal = self.read_number(c).to_string();
            }
            c if !c.is_whitespace() => {
                token_literal = self.read_identifier(c);
                let entry: Option<&TokenType> = KEYWORDS.get(&token_literal);
                if let Some(keyword_token) = entry {
                    //println!("keyword_token: {}, literal: {}", keyword_token, keyword_token.literal());
                    token_type = keyword_token.clone();
                } else {
                    //println!("ident literal: {}", ident);
                    token_type = TokenType::Ident;
                }
            }
            _ => {
                token_type = TokenType::Illegal;
                //println!("unknown char: {}", c);
                token_literal = c.to_string();
            }
        };
        Some(Token::new(token_type, &token_literal))
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
        let tokens = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        let expected = vec![
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "five".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "ten".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "add".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Function, literal: TokenType::Function.literal() },
            Token { token_type: TokenType::Lparen, literal: TokenType::Lparen.literal() },
            Token { token_type: TokenType::Ident, literal: "x".to_string() },
            Token { token_type: TokenType::Comma, literal: TokenType::Comma.literal() },
            Token { token_type: TokenType::Ident, literal: "y".to_string() },
            Token { token_type: TokenType::Rparen, literal: TokenType::Rparen.literal() },
            Token { token_type: TokenType::Lbrace, literal: TokenType::Lbrace.literal() },
            Token { token_type: TokenType::Ident, literal: "x".to_string() },
            Token { token_type: TokenType::Plus, literal: TokenType::Plus.literal() },
            Token { token_type: TokenType::Ident, literal: "y".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Rbrace, literal: TokenType::Rbrace.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "result".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Ident, literal: "add".to_string() },
            Token { token_type: TokenType::Lparen, literal: TokenType::Lparen.literal() },
            Token { token_type: TokenType::Ident, literal: "five".to_string() },
            Token { token_type: TokenType::Comma, literal: TokenType::Comma.literal() },
            Token { token_type: TokenType::Ident, literal: "ten".to_string() },
            Token { token_type: TokenType::Rparen, literal: TokenType::Rparen.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
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
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "five".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "ten".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "add".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Function, literal: TokenType::Function.literal() },
            Token { token_type: TokenType::Lparen, literal: TokenType::Lparen.literal() },
            Token { token_type: TokenType::Ident, literal: "x".to_string() },
            Token { token_type: TokenType::Comma, literal: TokenType::Comma.literal() },
            Token { token_type: TokenType::Ident, literal: "y".to_string() },
            Token { token_type: TokenType::Rparen, literal: TokenType::Rparen.literal() },
            Token { token_type: TokenType::Lbrace, literal: TokenType::Lbrace.literal() },
            Token { token_type: TokenType::Ident, literal: "x".to_string() },
            Token { token_type: TokenType::Plus, literal: TokenType::Plus.literal() },
            Token { token_type: TokenType::Ident, literal: "y".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Rbrace, literal: TokenType::Rbrace.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "result".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Ident, literal: "add".to_string() },
            Token { token_type: TokenType::Lparen, literal: TokenType::Lparen.literal() },
            Token { token_type: TokenType::Ident, literal: "five".to_string() },
            Token { token_type: TokenType::Comma, literal: TokenType::Comma.literal() },
            Token { token_type: TokenType::Ident, literal: "ten".to_string() },
            Token { token_type: TokenType::Rparen, literal: TokenType::Rparen.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            // !-/*5;
            // 5 < 10 > 5;";
            Token { token_type: TokenType::Bang, literal: TokenType::Bang.literal() },
            Token { token_type: TokenType::Minus, literal: TokenType::Minus.literal() },
            Token { token_type: TokenType::Slash, literal: TokenType::Slash.literal() },
            Token { token_type: TokenType::Asterisk, literal: TokenType::Asterisk.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::LessThan, literal: TokenType::LessThan.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::GreaterThan, literal: TokenType::GreaterThan.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
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
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "five".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "ten".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "add".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Function, literal: TokenType::Function.literal() },
            Token { token_type: TokenType::Lparen, literal: TokenType::Lparen.literal() },
            Token { token_type: TokenType::Ident, literal: "x".to_string() },
            Token { token_type: TokenType::Comma, literal: TokenType::Comma.literal() },
            Token { token_type: TokenType::Ident, literal: "y".to_string() },
            Token { token_type: TokenType::Rparen, literal: TokenType::Rparen.literal() },
            Token { token_type: TokenType::Lbrace, literal: TokenType::Lbrace.literal() },
            Token { token_type: TokenType::Ident, literal: "x".to_string() },
            Token { token_type: TokenType::Plus, literal: TokenType::Plus.literal() },
            Token { token_type: TokenType::Ident, literal: "y".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Rbrace, literal: TokenType::Rbrace.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Let, literal: TokenType::Let.literal() },
            Token { token_type: TokenType::Ident, literal: "result".to_string() },
            Token { token_type: TokenType::Assign, literal: TokenType::Assign.literal() },
            Token { token_type: TokenType::Ident, literal: "add".to_string() },
            Token { token_type: TokenType::Lparen, literal: TokenType::Lparen.literal() },
            Token { token_type: TokenType::Ident, literal: "five".to_string() },
            Token { token_type: TokenType::Comma, literal: TokenType::Comma.literal() },
            Token { token_type: TokenType::Ident, literal: "ten".to_string() },
            Token { token_type: TokenType::Rparen, literal: TokenType::Rparen.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            // !-/*5;
            // 5 < 10 > 5;";
            Token { token_type: TokenType::Bang, literal: TokenType::Bang.literal() },
            Token { token_type: TokenType::Minus, literal: TokenType::Minus.literal() },
            Token { token_type: TokenType::Slash, literal: TokenType::Slash.literal() },
            Token { token_type: TokenType::Asterisk, literal: TokenType::Asterisk.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::LessThan, literal: TokenType::LessThan.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::GreaterThan, literal: TokenType::GreaterThan.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },

            Token { token_type: TokenType::If, literal: TokenType::If.literal() },
            Token { token_type: TokenType::Lparen, literal: TokenType::Lparen.literal() },
            Token { token_type: TokenType::Int, literal: "5".to_string() },
            Token { token_type: TokenType::LessThan, literal: TokenType::LessThan.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::Rparen, literal: TokenType::Rparen.literal() },
            Token { token_type: TokenType::Lbrace, literal: TokenType::Lbrace.literal() },
            Token { token_type: TokenType::Return, literal: TokenType::Return.literal() },
            Token { token_type: TokenType::True, literal: TokenType::True.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Rbrace, literal: TokenType::Rbrace.literal() },
            Token { token_type: TokenType::Else, literal: TokenType::Else.literal() },
            Token { token_type: TokenType::Lbrace, literal: TokenType::Lbrace.literal() },
            Token { token_type: TokenType::Return, literal: TokenType::Return.literal() },
            Token { token_type: TokenType::False, literal: TokenType::False.literal() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Rbrace, literal: TokenType::Rbrace.literal() },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_next_token_5() {
        let input = "10 == 10;
10 != 9;";
        let tokens = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        let expected = vec![
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::Equal, literal: TokenType::Equal.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
            Token { token_type: TokenType::Int, literal: "10".to_string() },
            Token { token_type: TokenType::NotEqual, literal: TokenType::NotEqual.literal() },
            Token { token_type: TokenType::Int, literal: "9".to_string() },
            Token { token_type: TokenType::Semicolon, literal: TokenType::Semicolon.literal() },
        ];
        assert_eq!(tokens, expected);
    }
}
