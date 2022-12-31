use std::collections::HashMap;

use crate::{
    ast::{
        Expression, ExpressionStatement, ExpressionType, Identifier, LetStatement, NodeInterface,
        Program, ReturnStatement, StatementType,
    },
    lexer::Lexer,
    token::Token,
};

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_fns: HashMap<Token, PrefixFn>,
    //infix_fns: HashMap<Token, fn(&mut Parser<'a>) -> Option<Expression>>,
}

type PrefixFn = fn(&mut Parser, Token) -> Option<Identifier>;

impl<'a> Parser<'a> {
    /*
    identifiers as arguments in a function call
    add(foobar, barfoo);
    identifiers as operands in an infix expression
    foobar + barfoo;
    identifier as a standalone expression as part of a conditional
    if (foobar) {

    just like any other expression, identifiers produce a value, the value they are bound to
    */

    fn new(lexer: Lexer<'a>) -> Self {
        let mut result = Self {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
            prefix_fns: HashMap::new(),
            //infix_fns: HashMap::new(),
        };
        result.next_token();
        result.next_token();

        //result.register_prefix(Token::Ident("".to_string()), Parser::parse_identifier);

        result
    }

    fn register_prefix<F>(&mut self, token: Token, f: PrefixFn) {
        self.prefix_fns.insert(token, f);
    }

    /*fn register_infix(&mut self, token: Token, f: fn() -> Expression) {
        self.infix_fns.insert(token, f);
    }*/

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn next_token(&mut self) {
        self.current_token = std::mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program: Program = Program::new();
        while let Some(_token) = &self.current_token {
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
                if let Some(statement) = self.parse_let_statement() {
                    return Some(StatementType::Let(statement));
                }
            }
            Token::Return => {
                if let Some(statement) = self.parse_return_statement() {
                    return Some(StatementType::Return(statement));
                }
            }
            _ => {
                if let Some(statement) = self.parse_expression_statement() {
                    return Some(StatementType::Expression(statement));
                }
            }
        }
        None
    }

    fn parse_identifier(&mut self, token: Token) -> Option<Identifier> {
        Some(Identifier::new(self.current_token.clone().unwrap()))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ExpressionType> {
        // call parse_prefix for self.current_token
        // return result of that
        match &self.current_token {
            Some(token) => match token {
                Token::Ident(_data) => Some(ExpressionType::Expression(Expression::new())),
                _ => None,
            },
            None => None,
        }
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        if self.current_token.is_none() {
            return None;
        }
        let mut statement = ExpressionStatement::new(self.current_token.as_ref().unwrap().clone());
        statement.expression = match self.parse_expression(Precedence::Lowest) {
            Some(e) => e,
            None => ExpressionType::NoExpression,
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(statement)
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
            if let Token::Ident(ref _ident) = current {
                statement.name = Some(Identifier::new(
                    self.current_token.clone().unwrap(),
                    //ident.to_string(),
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
            expected.token_type(),
            expected,
            peek.token_type(),
            peek
        );
        self.errors.push(message);
    }

    fn parse_prefix() -> Expression {
        todo!()
    }

    fn parse_infix(_lhs: Expression) -> Expression {
        todo!()
    }
}

fn variant_eq(a: &Token, b: &Token) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

#[cfg(test)]
mod test {

    use super::*;
    use pretty_assertions::assert_eq;

    fn test_let_statement(s: &LetStatement, name: &str) {
        assert_eq!(s.token_literal(), "let");
        //assert_eq!(s.name.as_ref().unwrap().value, name);
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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let _error_string = check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        if let StatementType::Expression(_expression_statement) = statement {
            // ok it is an ExpressionStatement

            //expression_statement.expression
            // this is supposed to be Identifier idk how
            // this Identifier is supposed to be foobar
            // the Identifier.token_literal() should == "foobar"
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let _error_string = check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        if let StatementType::Expression(expression_statement) = statement {
            if let Token::Int(n) = expression_statement.token {
                assert_eq!(n, 5);
            } else {
                assert!(false);
            }
            assert_eq!(expression_statement.token_literal(), "5");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let inputs = vec!["!5;", "-15;"];
        let operators = vec!["!", "-"];
        let int_values = vec![5, 15];

        for ((input, operator), int_value) in inputs.iter().zip(operators).zip(int_values) {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let _error_string = check_parser_errors(&parser);

            println!("{}", program);
            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];
            if let StatementType::Expression(expression_statement) = statement {
                if let ExpressionType::Prefix(prefix_expression) = &expression_statement.expression {
                    if prefix_expression.operator() == operator {
                    } else {
                        panic!("expected operator {}, got {}", operator, prefix_expression.operator());
                    }
                } else {
                    panic!("expected Expression::Prefix, got {}", expression_statement.expression);
                }
            } else {
                panic!("expected StatementType::Expression, got {}", statement);
            }
        }
    }
}
