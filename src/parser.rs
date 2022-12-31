use std::collections::HashMap;

use crate::{
    ast::{
        Expression, ExpressionStatement, ExpressionType, Identifier, LetStatement, NodeInterface,
        Program, ReturnStatement, StatementType, PrefixExpression,
    },
    lexer::Lexer,
    token::Token,
};

pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    //prefix_fns: HashMap<Token, PrefixFn>,
    //infix_fns: HashMap<Token, fn(&mut Parser<'a>) -> Option<Expression>>,
    precedences: HashMap<Token, Precedence>, 
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
        let precedences = HashMap::from([
            (Token::Equal, Precedence::Equals),
            (Token::NotEqual, Precedence::Equals),
            (Token::LessThan, Precedence::LessGreater),
            (Token::GreaterThan, Precedence::LessGreater),
            (Token::Plus, Precedence::Sum),
            (Token::Minus, Precedence::Sum),
            (Token::Slash, Precedence::Product),
            (Token::Asterisk, Precedence::Product),
        ]);
        let mut result = Self {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
            precedences,
        };
        result.next_token();
        result.next_token();

        result
    }

    /*fn register_prefix<F>(&mut self, token: Token, f: PrefixFn) {
        self.prefix_fns.insert(token, f);
    }

    fn register_infix(&mut self, token: Token, f: fn() -> Expression) {
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

    fn parse_identifier(&mut self) -> Option<Identifier> {
        Some(Identifier::new(self.current_clone()))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ExpressionType> {
        // call parse_prefix for self.current_token
        // return result of that
        let token = self.current_clone();
        match token {
            Token::Ident(_data) => {
                let ident = self.parse_identifier().unwrap();
                Some(ExpressionType::Identifier(ident))
            }
            Token::Int(value) => {
                Some(ExpressionType::Int(value))
            }
            Token::Bang | Token::Minus => {
                let what = self.parse_prefix();
                Some(ExpressionType::Prefix(what))
            }
            _ => None,
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

    fn current_clone(&self) -> Token {
        self.current_token.as_ref().unwrap().clone()
    }

    fn parse_prefix(&mut self) -> PrefixExpression {
        let exp = PrefixExpression::new(self.current_clone());
        self.next_token();
        let _right /*exp.right*/ = self.parse_expression(Precedence::Prefix);
        exp
    }

    fn parse_infix(&mut self, _lhs: Expression) -> Expression {
        todo!()
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
                panic!("expected StatementType::Let, got {} instead", statement);
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
                panic!("expected StatementType::Return, got {} instead", statement);
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
        if let StatementType::Expression(expression_statement) = statement {
            if let ExpressionType::Identifier(ident) = &expression_statement.expression {
                assert_eq!(ident.token_literal(), "foobar");
            } else {
                panic!("expected ExpressionType::Identifier, got {} instead", expression_statement.expression);
            }
        } else {
            panic!("expected StatementType::Expression, got {} instead", statement);
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
            if let ExpressionType::Int(n) = expression_statement.expression {
                assert_eq!(n, 5);
            } else {
                panic!("expected ExpressionType::Int, got {} instead", expression_statement.expression);
            }
            assert_eq!(expression_statement.token_literal(), "5");
        } else {
            panic!("expected StatementType::Expression, got {} instead", statement);
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

    #[test]
    fn test_parsing_infix_expressions() {
        let inputs = vec!["5 + 5;", "5 - 5;", "5 * 5;",  "5 / 5;", "5 > 5", "5 < 5", "5 == 5;", "5 != 5"];
        let left_values = vec![5, 5, 5, 5, 5, 5, 5, 5];
        let operators = vec![ "+", "-", "*", "/", ">", "<", "==", "!="];
        let right_values = vec![5, 5, 5, 5, 5, 5, 5, 5];

        for i in 0..inputs.len() {
            let input = inputs[i];
            let left = left_values[i];
            let operator = operators[i];
            let right = right_values[i];

            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let _error_string = check_parser_errors(&parser);

            println!("{}", program);
            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];
            if let StatementType::Expression(expression_statement) = &statement {
                if let ExpressionType::Infix(infix_expression) = &expression_statement.expression {
                    if let ExpressionType::Int(n) = *infix_expression.left {
                        assert_eq!(n, left);
                    } else {
                        panic!("expected ExpressionType::Int, got {} instead", infix_expression.left);
                    }
                    assert_eq!(infix_expression.operator(), operator);
                    if let ExpressionType::Int(n) = *infix_expression.right {
                        assert_eq!(n, right);
                    } else {
                        panic!("expected ExpressionType::Int, got {} instead", infix_expression.right);
                    }
                } else {
                    panic!("expected ExpressionType::Infix, got {} instead", expression_statement.expression);
                }
            } else {
                panic!("expected StatementType::Expression, got {} instead", statement);
            }
        }
    }
}
