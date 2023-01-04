use std::collections::HashMap;

use crate::{
    ast::{
        ExpressionStatement, ExpressionType, Identifier, InfixExpression, LetStatement,
        PrefixExpression, Program, ReturnStatement, StatementType,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

/*
    prefixParseFns map[token.TokenType]prefixParseFn
    infixParseFns  map[token.TokenType]infixParseFn

    p.registerPrefix(token.IDENT, p.parseIdentifier)
    fn parse_identifier(&mut self) -> Option<Identifier> {

    p.registerPrefix(token.INT, p.parseIntegerLiteral)
    in parse_expression Some(ExpressionType::Int(value))

    p.registerPrefix(token.BANG, p.parsePrefixExpression)
    fn parse_prefix(&mut self) -> PrefixExpression {

    p.registerPrefix(token.MINUS, p.parsePrefixExpression)
    fn parse_infix(&mut self, lhs: ExpressionType) -> InfixExpression {

// not yet
    p.registerPrefix(token.TRUE, p.parseBoolean)
    p.registerPrefix(token.FALSE, p.parseBoolean)
    p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
    p.registerPrefix(token., p.)


    fn parse_infix(&mut self, lhs: ExpressionType) -> InfixExpression {

    p.registerInfix(token.PLUS, p.parseInfixExpression)
    p.registerInfix(token.MINUS, p.parseInfixExpression)
    p.registerInfix(token.SLASH, p.parseInfixExpression)
    p.registerInfix(token.ASTERISK, p.parseInfixExpression)
    p.registerInfix(token.EQ, p.parseInfixExpression)
    p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
    p.registerInfix(token.LT, p.parseInfixExpression)
    p.registerInfix(token.GT, p.parseInfixExpression)

// not yet
    p.registerInfix(token.LPAREN, p.parseCallExpression)
*/
/*
identifiers as arguments in a function call
add(foobar, barfoo);
identifiers as operands in an infix expression
foobar + barfoo;
identifier as a standalone expression as part of a conditional
if (foobar) {

just like any other expression, identifiers produce a value, the value they are bound to
*/

#[derive(Clone, Copy)]
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
    prefix_fns: HashMap<TokenType, ParseFn>,
    infix_fns: HashMap<TokenType, ParseInfixFn>,
    precedences: HashMap<TokenType, Precedence>,
}

type ParseFn = fn(&mut Parser) -> Option<ExpressionType>;
type ParseInfixFn = fn(&mut Parser, ExpressionType) -> Option<ExpressionType>;

fn parse_identifier(parser: &mut Parser) -> Option<ExpressionType> {
    Some(ExpressionType::Identifier(Identifier::new(
        parser.current_clone()
    )))
}

fn parse_integer_literal(parser: &mut Parser) -> Option<ExpressionType> {
    let token = parser.current_clone();
    if let TokenType::Int = token.token_type {
        Some(ExpressionType::Int(token.literal.parse::<usize>().unwrap()))
    } else {
        panic!("where is my integer");
    }
}

fn parse_prefix_expression(parser: &mut Parser) -> Option<ExpressionType> {
    let token = parser.current_clone();
    let literal = token.literal.clone();
    let mut expression = PrefixExpression::new(token, &literal);
    parser.next_token();
    let right = parser.parse_expression(Precedence::Prefix);
    let right = match right {
        Some(exp) => exp,
        None => ExpressionType::NoExpression,
    };
    expression.right = Box::new(right);
    Some(ExpressionType::Prefix(expression))
}

fn parse_infix_expression(parser: &mut Parser, left: ExpressionType) -> Option<ExpressionType> {
    let token = parser.current_clone();
    let literal = token.literal.clone();
    let mut expression = InfixExpression::new(token, &literal, left);
    let precedence = parser.current_precedence();
    parser.next_token();
    let right = parser.parse_expression(precedence);
    let right = match right {
        Some(exp) => exp,
        None => ExpressionType::NoExpression,
    };
    expression.right = Box::new(right);
    Some(ExpressionType::Infix(expression))
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let precedences = HashMap::from([
            (TokenType::Equal, Precedence::Equals),
            (TokenType::NotEqual, Precedence::Equals),
            (TokenType::LessThan, Precedence::LessGreater),
            (TokenType::GreaterThan, Precedence::LessGreater),
            (TokenType::Plus, Precedence::Sum),
            (TokenType::Minus, Precedence::Sum),
            (TokenType::Slash, Precedence::Product),
            (TokenType::Asterisk, Precedence::Product),
        ]);
        let mut parser = Self {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
            precedences,
            infix_fns: HashMap::new(),
            prefix_fns: HashMap::new(),
        };
        parser.next_token();
        parser.next_token();

        parser.register_prefix(TokenType::Ident, parse_identifier);
        parser.register_prefix(TokenType::Int, parse_integer_literal);
        parser.register_prefix(TokenType::Bang, parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, parse_prefix_expression);

        parser.register_infix(TokenType::Plus, parse_infix_expression);
        parser.register_infix(TokenType::Minus, parse_infix_expression);
        parser.register_infix(TokenType::Slash, parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, parse_infix_expression);
        parser.register_infix(TokenType::Equal, parse_infix_expression);
        parser.register_infix(TokenType::NotEqual, parse_infix_expression);
        parser.register_infix(TokenType::LessThan, parse_infix_expression);
        parser.register_infix(TokenType::GreaterThan, parse_infix_expression);

        parser
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
        let token = self.current_clone();
        match token.token_type {
            TokenType::Let => {
                if let Some(statement) = self.parse_let_statement() {
                    return Some(StatementType::Let(statement));
                }
            }
            TokenType::Return => {
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

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        if self.current_token.is_none() {
            return None;
        }
        println!(
            "parse_expression_statement with token: {}",
            self.current_clone()
        );
        let mut statement = ExpressionStatement::new(self.current_clone());
        statement.expression = match self.parse_expression(Precedence::Lowest) {
            Some(e) => e,
            None => ExpressionType::NoExpression,
        };

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(statement)
    }

    // page 70
    fn parse_expression(&mut self, precedence: Precedence) -> Option<ExpressionType> {
        let token = self.current_clone();
        let mut left_exp = match self.prefix_fns.get(&token.token_type) {
            Some(prefix_fn) => prefix_fn(self),
            None => {
                self.no_prefix_parse_fn_error(token);
                return None;
            }
        };
        while !self.peek_token_is(&TokenType::Semicolon)
            && (precedence as usize) < (*self.peek_precedence() as usize)
        {
            let peek = self.peek_token.clone().unwrap();
            let infix = self.infix_fns.get(&peek.token_type);
            if infix.is_none() {
                return left_exp;
            }
            let infix = infix.unwrap().clone();
            self.next_token();
            left_exp = infix(self, left_exp.unwrap());
        }

        left_exp
    }

    fn no_prefix_parse_fn_error(&mut self, token: Token) {
        let msg = format!("no prefix parse function found for {}", token);
        self.errors.push(msg);
    }

    fn peek_precedence(&self) -> &Precedence {
        self.precedences
            .get(
                &self
                    .peek_token
                    .as_ref()
                    .expect("I'll have to deal with this sometime")
                    .token_type,
            )
            .expect("there should be an entry for this")
    }

    fn current_precedence(&self) -> Precedence {
        *self
            .precedences
            .get(
                &self
                    .current_token
                    .as_ref()
                    .expect("I'll have to deal with this sometime")
                    .token_type,
            )
            .expect("there should be an entry for this")
    }

    fn register_prefix(&mut self, token: TokenType, f: ParseFn) {
        self.prefix_fns.insert(token, f);
    }

    fn register_infix(&mut self, token: TokenType, f: ParseInfixFn) {
        self.infix_fns.insert(token, f);
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn next_token(&mut self) {
        self.current_token = std::mem::take(&mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        if self.current_token.is_none() {
            return None;
        }
        let mut token = self.current_clone();
        token.literal = "return".to_string();
        //println!("so we have a parse_return_statement\nwith a token {}", token);
        let statement = ReturnStatement::new(token);
        self.next_token();
        while !self.current_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        Some(statement)
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if self.current_token.is_none() {
            return None;
        }
        let mut token = self.current_clone();
        let mut statement = LetStatement::new(token.clone());

        if !self.expect_peek(&TokenType::Ident) {
            println!("ERROR: found let keyword but next token is not Ident");
            return None;
        }

        let token = self.current_clone();
        if TokenType::Ident == token.token_type {
            //println!("ident literal is {}", literal);
            statement.name = Some(Identifier::new(token));
            if !self.expect_peek(&TokenType::Assign) {
                return None;
            }
            while !self.current_token_is(&TokenType::Semicolon) {
                self.next_token();
            }
            return Some(statement);
        }
        None
    }

    fn peek_token_is(&self, token: &TokenType) -> bool {
        token == &self.peek_token.as_ref().unwrap().token_type
    }

    fn current_token_is(&self, token: &TokenType) -> bool {
        token == &self.current_token.as_ref().unwrap().token_type
    }

    fn expect_peek(&mut self, token: &TokenType) -> bool {
        //println!("expect_peek {}, expected: {}", self.peek_token.as_ref().unwrap(), token);
        if self.peek_token_is(&token) {
            self.next_token();
            true
        } else {
            self.peek_error(&token);
            false
        }
    }

    fn peek_error(&mut self, expected: &TokenType) {
        let peek = self.peek_token.as_ref().unwrap();
        let message = format!(
            "expected next token to be '{}', got '{}, {}' instead",
            expected, peek.token_type, peek
        );
        self.errors.push(message);
    }

    fn current_clone(&self) -> Token {
        self.current_token.as_ref().expect("will this fail").clone()
    }

    fn parse_prefix(&mut self) -> PrefixExpression {
        let token = self.current_clone();
        let literal = token.literal.clone();
        let exp = PrefixExpression::new(token, &literal);
        self.next_token();
        let _right /*exp.right*/ = self.parse_expression(Precedence::Prefix);
        exp
    }

    fn parse_infix(&mut self, lhs: ExpressionType) -> InfixExpression {
        let token = self.current_clone();
        let literal = token.literal.clone();
        let mut expression = InfixExpression::new(token, &literal, lhs);
        let precedence = self.current_precedence();
        self.next_token();
        expression.right = Box::new(
            self.parse_expression(precedence.clone())
                .expect("did we get an expression"),
        );
        expression
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::ast::NodeInterface;
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
        let error_string = check_parser_errors(&parser);
        if let Some(err) = error_string {
            println!("{}", err);
        }
        println!("{}", program);
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
                panic!(
                    "expected ExpressionType::Identifier, got {} instead",
                    expression_statement.expression
                );
            }
        } else {
            panic!(
                "expected StatementType::Expression, got {} instead",
                statement
            );
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
                panic!(
                    "expected ExpressionType::Int, got {} instead",
                    expression_statement.expression
                );
            }
            assert_eq!(expression_statement.token_literal(), "5");
        } else {
            panic!(
                "expected StatementType::Expression, got {} instead",
                statement
            );
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
                if let ExpressionType::Prefix(prefix_expression) = &expression_statement.expression
                {
                    if prefix_expression.operator == operator {
                    } else {
                        panic!(
                            "expected operator {}, got {}",
                            operator, prefix_expression.operator
                        );
                    }
                } else {
                    panic!(
                        "expected Expression::Prefix, got {}",
                        expression_statement.expression
                    );
                }
            } else {
                panic!("expected StatementType::Expression, got {}", statement);
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let inputs = vec![
            "5 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 > 5;", "5 < 5;", "5 == 5;", "5 != 5;",
        ];
        let left_values = vec![5, 5, 5, 5, 5, 5, 5, 5];
        let operators = vec!["+", "-", "*", "/", ">", "<", "==", "!="];
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
                        panic!(
                            "expected ExpressionType::Int, got {} instead",
                            infix_expression.left
                        );
                    }
                    assert_eq!(infix_expression.operator, operator);
                    if let ExpressionType::Int(n) = *infix_expression.right {
                        assert_eq!(n, right);
                    } else {
                        panic!(
                            "expected ExpressionType::Int, got {} instead",
                            infix_expression.right
                        );
                    }
                } else {
                    panic!(
                        "expected ExpressionType::Infix, got {} instead",
                        expression_statement.expression
                    );
                }
            } else {
                panic!(
                    "expected StatementType::Expression, got {} instead",
                    statement
                );
            }
        }
    }
}

/*
    fn old_parse_expression(&mut self, precedence: Precedence) -> Option<ExpressionType> {
        // call parse_prefix for self.current_token
        // return result of that
        let token = self.current_clone();
        let left_exp = match token {
            Token::Ident(_data) => {
                let ident = self.parse_identifier().unwrap();
                Some(ExpressionType::Identifier(ident))
            }
            Token::Int(value) => Some(ExpressionType::Int(value)),
            Token::Bang | Token::Minus => {
                Some(ExpressionType::Prefix(self.parse_prefix()))
            }
            /* where does this belong?
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => {
                Some(ExpressionType::Prefix(self.parse_prefix()))
            }*/
            _ => None,
        };
        if left_exp.is_none() {
            return None;
        }
        let mut left_exp = left_exp.expect("we checked above");

        while !self.peek_token_is(&Token::Semicolon)
            && (precedence as usize) < (*self.peek_precedence() as usize)
        {
            self.next_token();
            left_exp = ExpressionType::Infix(self.parse_infix(left_exp));
        }
        None
    }
*/
