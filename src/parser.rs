use std::{rc::Rc, cell::RefCell, any::Any};
use crate::{
    ast::{Identifier, LetStatement, NodeInterface},
    ast::{Program, Statement, StatementInterface, ReturnStatement, StatementType},
    lexer::Lexer,
    token::Token,
};

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut result = Self {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
        };
        result.next_token();
        result.next_token();
        result
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn next_token(&mut self) {
        self.current_token = std::mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program: Program = Program::new();
        while let Some(token) = &self.current_token {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }
        Some(program)
    }

    fn parse_statement(&mut self) -> Option<StatementType> {
        if self.current_token.is_none() {
            return None;
        }
        let token = self.current_token.as_ref().unwrap();
        match token {
            Token::Let => {
                let s = self.parse_let_statement();
                if let Some(statement) = s {
                    Some(StatementType::Let(statement))
                } else {
                    None
                }
            }
            Token::Return => {
                let s = self.parse_return_statement();
                if let Some(statement) = s {
                    Some(StatementType::Return(statement))
                } else {
                    None
                }
            }
            _ => {
                None
            }
        }
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        if self.current_token.is_none() {
            return None;
        }
        let mut statement = ReturnStatement::new(self.current_token.as_ref().unwrap().clone());
        self.next_token();
        while !self.current_token_is(&Token::Semicolon) {
            self.next_token();
        }
        Some(statement)
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if self.current_token.is_none() {
            return None;
        }
        let mut statement = LetStatement::new(self.current_token.as_ref().unwrap().clone());

        if !self.expect_peek(&Token::Ident("".to_string())) {
            return None;
        }

        if let Some(current) = self.current_token.clone() {
            if let Token::Ident(ref ident) = current {
                statement.name = Some(Identifier::new(
                    self.current_token.clone().unwrap(),
                    ident.to_string(),
                ));
                if !self.expect_peek(&Token::Assign) {
                    return None;
                }
                while !self.current_token_is(&Token::Semicolon) {
                    self.next_token();
                }
                return Some(statement);
            }
        }
        None
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        if let Some(t) = &self.peek_token {
            variant_eq(t, token)
        } else {
            false
        }
    }

    fn current_token_is(&self, token: &Token) -> bool {
        if let Some(t) = &self.current_token {
            variant_eq(t, token)
        } else {
            false
        }
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        //println!("expect_peek {}, expected: {}", self.peek_token.as_ref().unwrap(), token);
        if self.peek_token_is(token) {
            self.next_token();
            true
        } else {
            self.peek_error(&token);
            false
        }
    }

    fn peek_error(&mut self, expected: &Token) {
        let peek = self.peek_token.as_ref().unwrap();
        let message = format!(
            "expected next token to be '{}, {}', got '{}, {}' instead",
            expected.token_type(), expected, peek.token_type(), peek
        );
        self.errors.push(message);
    }
}

fn variant_eq(a: &Token, b: &Token) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[cfg(test)]
mod test {

    use super::*;
    use pretty_assertions::assert_eq;

    fn test_let_statement(s: &LetStatement, name: &str) {
        assert_eq!(s.token_literal(), "let");
        assert_eq!(s.name.as_ref().unwrap().value, name);
        assert_eq!(s.name.as_ref().unwrap().token_literal(), name)
    }

    fn check_parser_errors(parser: &Parser) -> Option<String> {
        let errors = parser.errors();
        if errors.len() == 0 {
            return None;
        }
        let mut error_string = format!("parser has {} errors\n", errors.len());
        for e in errors {
            error_string.push_str(&format!("parser error: {}\n", e));
        }
        Some(error_string)
    }

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 3);
        let expected = vec!["x", "y", "foobar"];

        // so this needs a test_let_statement helper function in this module
        // and there are more tests in the source code for the book
        for (statement, expected) in program.statements.iter().zip(expected) {
            if let StatementType::Let(s) = statement {
                test_let_statement(s, expected);
            } else {
                panic!();
            }
        }
    }

    #[test]
    fn test_let_statement_errors() {
        let input = "
let x 5;
let = 10;
let 838383;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let _program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if let Some(err) = error_string {
            println!("{}", err);
            assert!(err.len() > 0);
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let _error_string = check_parser_errors(&parser);
        assert_eq!(program.statements.len(), 3);

        let expected = vec!["x", "y", "foobar"];
        for (statement, _expected) in program.statements.iter().zip(expected) {
            if let StatementType::Return(s) = statement {
                assert_eq!(s.token_literal(), "return");
            } else {
                panic!();
            }
        }
    }
}
