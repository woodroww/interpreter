use crate::{
    ast::{Identifier, LetStatement, NodeInterface},
    ast::{Program, Statement, StatementInterface},
    lexer::Lexer,
    token::Token,
};

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let mut result = Self {
            lexer,
            current_token: None,
            peek_token: None,
        };
        result.next_token();
        result.next_token();
        result
    }

    fn next_token(&mut self) {
        self.current_token = std::mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    fn parse_program(&mut self) -> Option<Program<LetStatement>> {
        let mut program: Program<LetStatement> = Program::new();
        while let Some(token) = &self.current_token {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }
        Some(program)
    }

    fn parse_statement(&mut self) -> Option<LetStatement> {
        if self.current_token.is_none() {
            return None;
        }
        let token = self.current_token.as_ref().unwrap();
        match token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let token = self.current_token.as_ref().unwrap();
        let mut statement = LetStatement::new(token.clone());

        //if self.expect_peek(Token::Ident(_)) {
        if let Some(peek) = &self.peek_token {
            if let Token::Ident(_ident) = peek {
                self.next_token();
                let id_token = self
                    .current_token
                    .clone()
                    .expect("is this the right token? is it Some? we've called next_token() above");
                let literal = id_token.literal().to_string();
                statement.name = Some(Identifier::new(id_token, literal));
                if !self.expect_peek(Token::Assign) {
                    return None;
                }
                while !self.current_token_is(Token::Semicolon) {
                    self.next_token();
                }
                return Some(statement);
            }
        }
        return None;
    }

    fn peek_token_is(&self, token: Token) -> bool {
        if let Some(t) = &self.peek_token {
            *t == token
        } else {
            false
        }
    }

    fn current_token_is(&self, token: Token) -> bool {
        if let Some(t) = &self.current_token {
            *t == token
        } else {
            false
        }
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use pretty_assertions::assert_eq;

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
            test_let_statement(statement, expected);
        }
    }

    fn test_let_statement(s: &LetStatement, name: &str) {
        assert_eq!(s.token_literal(), "let");
        assert_eq!(s.name.as_ref().unwrap().value, name);
        assert_eq!(s.name.as_ref().unwrap().token_literal(), name)
    }
}
