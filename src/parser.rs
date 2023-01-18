use std::collections::HashMap;

use crate::{
    ast::{
        BlockStatement, BooleanExpression, Expression, ExpressionStatement, FunctionLiteralExpression,
        Identifier, IfExpression, InfixExpression, LetStatement, PrefixExpression, Program,
        ReturnStatement, StatementType, CallExpression,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

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

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_fns: HashMap<TokenType, ParseFn>,
    infix_fns: HashMap<TokenType, ParseInfixFn>,
    precedences: HashMap<TokenType, Precedence>,
}

type ParseFn = fn(&mut Parser) -> Option<Expression>;
type ParseInfixFn = fn(&mut Parser, Expression) -> Option<Expression>;

fn parse_identifier(parser: &mut Parser) -> Option<Expression> {
    Some(Expression::Identifier(Identifier::new(
        parser.current_clone(),
    )))
}

fn parse_integer_literal(parser: &mut Parser) -> Option<Expression> {
    let token = parser.current_clone();
    if let TokenType::Int = token.token_type {
        Some(Expression::Int(token.literal.parse::<isize>().unwrap()))
    } else {
        panic!("where is my integer");
    }
}

fn parse_boolean(parser: &mut Parser) -> Option<Expression> {
    let token = parser.current_clone();
    let expression = BooleanExpression::new(token);
    Some(Expression::Boolean(expression))
}

fn parse_prefix_expression(parser: &mut Parser) -> Option<Expression> {
    let token = parser.current_clone();
    let mut expression = PrefixExpression::new(token);
    parser.next_token();
    let right = parser.parse_expression(Precedence::Prefix);
    if right.is_some() {
        expression.right = Some(Box::new(right.unwrap()));
    }
    Some(Expression::Prefix(expression))
}

fn parse_infix_expression(parser: &mut Parser, left: Expression) -> Option<Expression> {
    let token = parser.current_clone();
    //let literal = token.literal.clone();
    let mut expression = InfixExpression::new(token).with_left(left);
    let precedence = parser.current_precedence();
    parser.next_token();
    let right = parser.parse_expression(precedence);
    if right.is_some() {
        expression.right = Some(Box::new(right.unwrap()));
    }
    Some(Expression::Infix(expression))
}

fn parse_call_expression(parser: &mut Parser, left: Expression) -> Option<Expression> {
    let mut expression = CallExpression::new(parser.current_clone()).with_function(left);
    let args = parser.parse_call_arguments();
    if args.is_some() {
        expression.arguments = args.unwrap();
    }
    Some(Expression::Call(expression))
}

fn parse_grouped_expression(parser: &mut Parser) -> Option<Expression> {
    parser.next_token();
    let expression = parser.parse_expression(Precedence::Lowest);
    if !parser.expect_peek(&TokenType::Rparen) {
        None
    } else {
        expression
    }
}

fn parse_if_expression(parser: &mut Parser) -> Option<Expression> {
    let mut expression = IfExpression::new(parser.current_clone());

    if !parser.expect_peek(&TokenType::Lparen) {
        return None;
    }

    parser.next_token();
    expression.condition = match parser.parse_expression(Precedence::Lowest) {
        Some(expression) => Some(Box::new(expression)),
        None => None,
    };

    if !parser.expect_peek(&TokenType::Rparen) {
        return None;
    }

    if !parser.expect_peek(&TokenType::Lbrace) {
        return None;
    }

    expression.consequence = match parser.parse_block_statement() {
        Some(expression) => Some(Box::new(expression)),
        None => None,
    };

    if parser.peek_token_is(&TokenType::Else) {
        parser.next_token();
        if !parser.expect_peek(&TokenType::Lbrace) {
            return None;
        }
        expression.alternative = match parser.parse_block_statement() {
            Some(expresssion) => Some(Box::new(expresssion)),
            None => None,
        };
    }

    Some(Expression::If(expression))
}

fn parse_fn_literal(parser: &mut Parser) -> Option<Expression> {
    let mut literal = FunctionLiteralExpression::new(parser.current_clone());

    if !parser.expect_peek(&TokenType::Lparen) {
        return None;
    }

    let parameters = parser.parse_function_parameters();
    if parameters.is_some() {
        literal.parameters = parameters.unwrap();
    }

    if !parser.expect_peek(&TokenType::Lbrace) {
        return None;
    }

    literal.body = parser.parse_block_statement();

    Some(Expression::FunctionLiteral(literal))
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let precedences = HashMap::from([
            (TokenType::Equal, Precedence::Equals),
            (TokenType::NotEqual, Precedence::Equals),
            (TokenType::LessThan, Precedence::LessGreater),
            (TokenType::GreaterThan, Precedence::LessGreater),
            (TokenType::Plus, Precedence::Sum),
            (TokenType::Minus, Precedence::Sum),
            (TokenType::Slash, Precedence::Product),
            (TokenType::Asterisk, Precedence::Product),
            (TokenType::Lparen, Precedence::Call),
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
        parser.register_prefix(TokenType::True, parse_boolean);
        parser.register_prefix(TokenType::False, parse_boolean);
        parser.register_prefix(TokenType::Lparen, parse_grouped_expression);
        parser.register_prefix(TokenType::If, parse_if_expression);
        parser.register_prefix(TokenType::Function, parse_fn_literal);

        parser.register_infix(TokenType::Plus, parse_infix_expression);
        parser.register_infix(TokenType::Minus, parse_infix_expression);
        parser.register_infix(TokenType::Slash, parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, parse_infix_expression);
        parser.register_infix(TokenType::Equal, parse_infix_expression);
        parser.register_infix(TokenType::NotEqual, parse_infix_expression);
        parser.register_infix(TokenType::LessThan, parse_infix_expression);
        parser.register_infix(TokenType::GreaterThan, parse_infix_expression);
        parser.register_infix(TokenType::Lparen, parse_call_expression);

        parser
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        //println!("parse_program");
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
        //println!("parse_statement");
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
        //println!("parse_expression_statement");
        if self.current_token.is_none() {
            return None;
        }
        let mut statement = ExpressionStatement::new(self.current_clone());
        statement.expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(statement)
    }

    // page 70
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        //println!("parse_expression");
        let token = self.current_clone();
        let mut left_exp = match self.prefix_fns.get(&token.token_type) {
            Some(prefix_fn) => prefix_fn(self),
            None => {
                self.no_prefix_parse_fn_error(token);
                return None;
            }
        };
        while !self.peek_token_is(&TokenType::Semicolon)
            && (precedence as usize) < (self.peek_precedence() as usize)
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

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut block = BlockStatement::new(self.current_clone());
        self.next_token();

        while !self.current_token_is(&TokenType::Rbrace) && self.current_token.is_some() {
            let statement = self.parse_statement();
            if statement.is_some() {
                block.statements.push(statement.unwrap());
            }
            self.next_token();
        }

        Some(block)
    }

    fn no_prefix_parse_fn_error(&mut self, token: Token) {
        let msg = format!("no prefix parse function found for {}", token);
        self.errors.push(msg);
    }

    fn peek_precedence(&self) -> Precedence {
        let peek = match &self.peek_token {
            Some(token) => token,
            None => return Precedence::Lowest,
        };
        //println!("peek_precedence: current: {}, peek: {}", self.current_clone(), peek);
        match self.precedences.get(&peek.token_type) {
            Some(precedence) => precedence.clone(),
            None => Precedence::Lowest,
        }
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

    pub fn errors(&self) -> Vec<String> {
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
        let mut statement = ReturnStatement::new(self.current_clone());

        self.next_token();

        statement.return_value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if self.current_token.is_none() {
            return None;
        }
        let mut statement = LetStatement::new(self.current_clone());

        if !self.expect_peek(&TokenType::Ident) {
            return None;
        }

        let token = self.current_clone();
        if TokenType::Ident == token.token_type {
            //println!("ident literal is {}", literal);
            statement.name = Some(Identifier::new(token));
            if !self.expect_peek(&TokenType::Assign) {
                return None;
            }

            self.next_token();

            statement.value = self.parse_expression(Precedence::Lowest);

            if self.peek_token_is(&TokenType::Semicolon) {
                self.next_token();
            }
            Some(statement)
        } else {
            None
        }
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers: Vec<Identifier> = Vec::new();

        if self.peek_token_is(&TokenType::Rparen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Identifier::new(self.current_clone());
        identifiers.push(ident);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            let ident = Identifier::new(self.current_clone());
            identifiers.push(ident);
        }

        if !self.expect_peek(&TokenType::Rparen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args = Vec::new();

        if self.peek_token_is(&TokenType::Rparen) {
            self.next_token();
            return Some(args);
        }
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest);
        if expression.is_some() {
            args.push(expression.unwrap());
        }
        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            let expression = self.parse_expression(Precedence::Lowest);
            if expression.is_some() {
                args.push(expression.unwrap());
            }
        }
        if !self.expect_peek(&TokenType::Rparen) {
            return None;
        }

        Some(args)
    }


    fn peek_token_is(&self, token: &TokenType) -> bool {
        if self.peek_token.is_none() {
            return false;
        }
        token == &self.peek_token.as_ref().unwrap().token_type
    }

    fn current_token_is(&self, token: &TokenType) -> bool {
        if self.current_token.is_none() {
            return false;
        }
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
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::ast::NodeInterface;
    use pretty_assertions::assert_eq;

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

    fn test_integer_literal(statement: &ExpressionStatement, value: isize) {
        if statement.expression.is_none() {
            panic!("statement.expression.is_none");
        }
        let expression = statement.expression.as_ref().unwrap();

        if let Expression::Int(n) = expression {
            assert_eq!(*n, value);
        } else {
            panic!("expected ExpressionType::Int, got {} instead", expression);
        }
        assert_eq!(statement.token_literal(), "5");
    }

    fn test_identifier(statement: &ExpressionStatement, value: &str) {
        if statement.expression.is_none() {
            panic!("statement.expression.is_none");
        }
        let expression = statement.expression.as_ref().unwrap();
        if let Expression::Identifier(ident) = expression {
            assert_eq!(ident.token_literal(), value);
        } else {
            panic!(
                "expected Expression::Identifier, got {} instead",
                expression
            );
        }
    }

    // page 80
    //fn test_literal_expression(expresssion: &Expression, expected: &Expression) {
    // use assert_eq!(expression, expected);

    fn test_infix_expression(statement: &StatementType, left: isize, operator: &str, right: isize) {
        if let StatementType::Expression(expression_statement) = &statement {
            if expression_statement.expression.is_none() {
                panic!("statement.expression.is_none");
            }

            let expression = expression_statement.expression.as_ref().unwrap();
            let token_type = TokenType::type_from_str(operator).unwrap();
            let token = Token::new(token_type.clone(), &token_type.literal());

            let mut infix = InfixExpression::new(token).with_left(Expression::Int(left));
            infix.right = Some(Box::new(Expression::Int(right)));

            assert_eq!(expression, &Expression::Infix(infix));
        } else {
            panic!(
                "expected StatementType::Expression, got {} instead",
                statement
            );
        }
    }

    #[test]
    fn test_let_statement_1() {
        let input = "let x = 5;";
        let ident = "x";
        let value = 5;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if let Some(err) = error_string {
            println!("{}", err);
        }
        println!("{}", program);
        assert_eq!(program.statements.len(), 1);

        let expected = StatementType::Let(
            LetStatement::new(Token::new(TokenType::Let, "let"))
                .with_value(Expression::Int(value))
                .with_name(Identifier::new(Token::new(TokenType::Ident, ident)))
        );
        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_let_statement_2() {
        let input = "let y = true;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if let Some(err) = error_string {
            println!("{}", err);
        }
        println!("{}", program);
        assert_eq!(program.statements.len(), 1);

        let expected = StatementType::Let(
            LetStatement::new(Token::new(TokenType::Let, "let"))
                .with_name(Identifier::new(Token::new(TokenType::Ident, "y")))
                .with_value(Expression::Boolean(BooleanExpression::new(Token::new(TokenType::True, "true"))))
        );
        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_let_statement_3() {
        let input = "let foobar = y;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if let Some(err) = error_string {
            println!("{}", err);
        }
        println!("{}", program);
        assert_eq!(program.statements.len(), 1);

        let expected = StatementType::Let(
            LetStatement::new(Token::new(TokenType::Let, "let"))
                .with_name(Identifier::new(Token::new(TokenType::Ident, "foobar")))
                .with_value(Expression::Identifier(Identifier::new(Token::new(TokenType::Ident, "y"))))
        );
        assert_eq!(program.statements[0], expected);
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
        let expected_idents = vec!["x", "y", "foobar"];
        let expected_values = vec![5, 10, 838383];

        // so this needs a test_let_statement helper function in this module
        // and there are more tests in the source code for the book
        for i in 0..program.statements.len() {
            if let StatementType::Let(s) = &program.statements[i] {
                assert_eq!(s.token_literal(), "let");
                let expression = Expression::Int(expected_values[i]);
                assert_eq!(*s.value.as_ref().unwrap(), expression);
                assert_eq!(s.name.as_ref().unwrap().token_literal(), expected_idents[i]);
            } else {
                panic!("expected StatementType::Let, got {} instead", program.statements[i]);
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
            test_identifier(expression_statement, "foobar");
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
            test_integer_literal(expression_statement, 5);
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
        let token_types = vec![TokenType::Bang, TokenType::Minus];
        let rhs = vec![5, 15];

        for i in 0..inputs.len() {
            let lexer = Lexer::new(inputs[i]);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let _error_string = check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];

            let expected = StatementType::Expression(
                ExpressionStatement::new(Token::new(token_types[i], operators[i])).with_expression(
                    Expression::Prefix(
                        PrefixExpression::new(Token::new(token_types[i], operators[i]))
                            .with_right(Expression::Int(rhs[i])),
                    ),
                ),
            );

            assert_eq!(*statement, expected);
        }
    }

    struct Data<T> {
        input: String,
        left_value: T,
        operator: String,
        right_value: T,
    }
    impl<T> Data<T> {
        pub fn new(input: &str, left_value: T, operator: &str, right_value: T) -> Self {
            Self {
                input: input.to_string(),
                left_value,
                operator: operator.to_string(),
                right_value,
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let data = vec![
            Data::new("5 + 5;", 5, "+", 5),
            Data::new("5 - 5;", 5, "-", 5),
            Data::new("5 * 5;", 5, "*", 5),
            Data::new("5 / 5;", 5, "/", 5),
            Data::new("5 > 5;", 5, ">", 5),
            Data::new("5 < 5;", 5, "<", 5),
            Data::new("5 == 5;", 5, "==", 5),
            Data::new("5 != 5;", 5, "!=", 5),
        ];
        let token_type = vec![
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Asterisk,
            TokenType::Slash,
            TokenType::GreaterThan,
            TokenType::LessThan,
            TokenType::Equal,
            TokenType::NotEqual,
        ];

        for i in 0..data.len() {
            let input = &data[i].input;
            let left = data[i].left_value;
            let operator = &data[i].operator;
            let right = data[i].right_value;

            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let _error_string = check_parser_errors(&parser);

            println!("{}", program);
            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];

            let expected = StatementType::Expression(
                ExpressionStatement::new(Token::new(TokenType::Int, &format!("{}", left)))
                    .with_expression(Expression::Infix(
                        InfixExpression::new(Token::new(token_type[i], operator))
                            .with_left(Expression::Int(left))
                            .with_right(Expression::Int(right)),
                    )),
            );
            assert_eq!(*statement, expected);

            //test_infix_expression(statement, left, operator, right);
        }
    }

    #[test]
    fn test_more_parsing_infix_expressions() {
        let data = vec![
            Data::new("true == true", true, "==", true),
            Data::new("true != false", true, "!=", false),
            Data::new("false == false", false, "==", false),
        ];

        for i in 0..data.len() {
            let input = &data[i].input;
            let left_token = if data[i].left_value {
                Token::new(TokenType::True, "true")
            } else {
                Token::new(TokenType::False, "false")
            };
            let operator = match data[i].operator.as_str() {
                "==" => Token::new(TokenType::Equal, "=="),
                "!=" => Token::new(TokenType::NotEqual, "!="),
                _ => unreachable!(),
            };
            let right_token = if data[i].right_value {
                Token::new(TokenType::True, "true")
            } else {
                Token::new(TokenType::False, "false")
            };

            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let _error_string = check_parser_errors(&parser);

            println!("{}", program);
            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];

            let expected = StatementType::Expression(
                ExpressionStatement::new(left_token.clone()).with_expression(Expression::Infix(
                    InfixExpression::new(operator)
                        .with_left(Expression::Boolean(BooleanExpression::new(left_token)))
                        .with_right(Expression::Boolean(BooleanExpression::new(right_token))),
                )),
            );

            assert_eq!(*statement, expected);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = vec![
            "-a * b",
            "!-a",
            "a + b + c",
            "a + b - c",
            "a * b * c",
            "a * b / c",
            "a + b / c",
            "a + b * c + d / e - f",
            "3 + 4; -5 * 5",
            "5 > 4 == 3 < 4",
            "5 < 4 != 3 > 4",
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "true",
            "false",
            "3 > 5 == false",
            "3 < 5 == true",
            "1 + (2 + 3) + 4",
            "(5 + 5) * 2",
            "2 / (5 + 5)",
            "(5 + 5) * 2 * (5 + 5)",
            "-(5 + 5)",
            "!(true == true)",
            "a + add(b * c) + d",
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a + b + c * d / f + g)",
        ];

        let expecteds = vec![
            "((-a) * b)",
            "(!(-a))",
            "((a + b) + c)",
            "((a + b) - c)",
            "((a * b) * c)",
            "((a * b) / c)",
            "(a + (b / c))",
            "(((a + (b * c)) + (d / e)) - f)",
            "(3 + 4)((-5) * 5)",
            "((5 > 4) == (3 < 4))",
            "((5 < 4) != (3 > 4))",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            "true",
            "false",
            "((3 > 5) == false)",
            "((3 < 5) == true)",
            "((1 + (2 + 3)) + 4)",
            "((5 + 5) * 2)",
            "(2 / (5 + 5))",
            "(((5 + 5) * 2) * (5 + 5))",
            "(-(5 + 5))",
            "(!(true == true))",
            "((a + add((b * c))) + d)",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            "add((((a + b) + ((c * d) / f)) + g))",
        ];

        for (input, expected) in inputs.into_iter().zip(expecteds) {
            //println!("testing: {} == {}", input, expected);
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let _error_string = check_parser_errors(&parser);
            assert_eq!(expected, format!("{}", program));
        }
    }

    #[test]
    fn test_boolean_expression() {
        let inputs = vec!["true;", "false;"];
        let expecteds = vec![true, false];

        for (input, expected) in inputs.into_iter().zip(expecteds) {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let _error_string = check_parser_errors(&parser);
            assert_eq!(program.statements.len(), 1);
            let statement = &program.statements[0];
            if let StatementType::Expression(e) = statement {
                if e.expression.is_none() {
                    panic!("e.expression.is_none");
                }
                let expression = e.expression.as_ref().unwrap();
                if let Expression::Boolean(b) = expression {
                    assert_eq!(b.value, expected);
                } else {
                    panic!("expected Expression::Boolean, got {}", expression);
                }
            } else {
                panic!("StatementType is not an Expression, got {}", statement);
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if error_string.is_some() {
            println!("{}", error_string.unwrap());
        }
        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];

        let expected = StatementType::Expression(
            ExpressionStatement::new(Token::new(TokenType::If, "if")).with_expression(
                Expression::If(
                    IfExpression::new(Token::new(TokenType::If, "if"))
                        .with_condition(Expression::Infix(
                            InfixExpression::new(Token::new(TokenType::LessThan, "<"))
                                .with_left(Expression::Identifier(Identifier::new(Token::new(
                                    TokenType::Ident,
                                    "x",
                                ))))
                                .with_right(Expression::Identifier(Identifier::new(Token::new(
                                    TokenType::Ident,
                                    "y",
                                )))),
                        ))
                        .with_consequence(
                            BlockStatement::new(Token::new(TokenType::Lbrace, "{"))
                                .with_statements(vec![StatementType::Expression(
                                    ExpressionStatement::new(Token::new(TokenType::Ident, "x"))
                                        .with_expression(Expression::Identifier(Identifier::new(
                                            Token::new(TokenType::Ident, "x"),
                                        ))),
                                )]),
                        ),
                ),
            ),
        );

        assert_eq!(*statement, expected);
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { jam } else { butter }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if error_string.is_some() {
            println!("{}", error_string.unwrap());
        }
        assert_eq!(program.statements.len(), 1);
        let statement = &program.statements[0];

        let expected = StatementType::Expression(
            ExpressionStatement::new(Token::new(TokenType::If, "if")).with_expression(
                Expression::If(
                    IfExpression::new(Token::new(TokenType::If, "if"))
                        .with_condition(Expression::Infix(
                            InfixExpression::new(Token::new(TokenType::LessThan, "<"))
                                .with_left(Expression::Identifier(Identifier::new(Token::new(
                                    TokenType::Ident,
                                    "x",
                                ))))
                                .with_right(Expression::Identifier(Identifier::new(Token::new(
                                    TokenType::Ident,
                                    "y",
                                )))),
                        ))
                        .with_consequence(
                            BlockStatement::new(Token::new(TokenType::Lbrace, "{"))
                                .with_statements(vec![StatementType::Expression(
                                    ExpressionStatement::new(Token::new(TokenType::Ident, "jam"))
                                        .with_expression(Expression::Identifier(Identifier::new(
                                            Token::new(TokenType::Ident, "jam"),
                                        ))),
                                )]),
                        )
                        .with_alternative(
                            BlockStatement::new(Token::new(TokenType::Lbrace, "{"))
                                .with_statements(vec![StatementType::Expression(
                                    ExpressionStatement::new(Token::new(
                                        TokenType::Ident,
                                        "butter",
                                    ))
                                    .with_expression(
                                        Expression::Identifier(Identifier::new(Token::new(
                                            TokenType::Ident,
                                            "butter",
                                        ))),
                                    ),
                                )]),
                        ),
                ),
            ),
        );

        assert_eq!(*statement, expected);
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if error_string.is_some() {
            println!("{}", error_string.unwrap());
        }
        assert_eq!(program.statements.len(), 1);
        let statement = &program.statements[0];

        let expected = StatementType::Expression(
            ExpressionStatement::new(Token::new(TokenType::Function, "fn")).with_expression(
                Expression::FunctionLiteral(
                    FunctionLiteralExpression::new(Token::new(TokenType::Function, "fn"))
                        .with_parameters(vec![
                            Identifier::new(Token::new(TokenType::Ident, "x")),
                            Identifier::new(Token::new(TokenType::Ident, "y")),
                        ])
                        .with_body(
                            BlockStatement::new(Token::new(TokenType::Lbrace, "{"))
                                .with_statements(vec![StatementType::Expression(
                                    ExpressionStatement::new(Token::new(TokenType::Ident, "x"))
                                        .with_expression(Expression::Infix(
                                            InfixExpression::new(Token::new(TokenType::Plus, "+"))
                                                .with_left(Expression::Identifier(Identifier::new(
                                                    Token::new(TokenType::Ident, "x"),
                                                )))
                                                .with_right(Expression::Identifier(
                                                    Identifier::new(Token::new(
                                                        TokenType::Ident,
                                                        "y",
                                                    )),
                                                )),
                                        )),
                                )]),
                        ),
                ),
            ),
        );

        assert_eq!(*statement, expected);
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for test in tests.into_iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let error_string = check_parser_errors(&parser);
            if error_string.is_some() {
                println!("{}", error_string.unwrap());
            }
            assert_eq!(program.statements.len(), 1);
            let mut expected_parameters = Vec::new();
            for parameter in test.1.iter() {
                expected_parameters.push(Identifier::new(Token::new(TokenType::Ident, parameter)));
            }
            let expected = StatementType::Expression(
                ExpressionStatement::new(Token::new(TokenType::Function, "fn"))
                    .with_expression(Expression::FunctionLiteral(
                        FunctionLiteralExpression::new(Token::new(TokenType::Function, "fn"))
                            .with_parameters(expected_parameters)
                            .with_body(BlockStatement::new(Token::new(TokenType::Lbrace, "{")))
                    )),
            );

            let statement = &program.statements[0];
            assert_eq!(*statement, expected);
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let error_string = check_parser_errors(&parser);
        if error_string.is_some() {
            println!("{}", error_string.unwrap());
        }
        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        let expected = StatementType::Expression(ExpressionStatement::new(Token::new(TokenType::Ident, "add"))
            .with_expression(Expression::Call(CallExpression::new(Token::new(TokenType::Lparen, "(")).with_function(
                Expression::Identifier(Identifier::new(Token::new(TokenType::Ident, "add")))
            ).with_arguments(
                    vec![
                        Expression::Int(1),
                        Expression::Infix(
                            InfixExpression::new(Token::new(TokenType::Asterisk, "*"))
                                .with_left(Expression::Int(2))
                                .with_right(Expression::Int(3))
                        ),
                        Expression::Infix(
                            InfixExpression::new(Token::new(TokenType::Plus, "+"))
                                .with_left(Expression::Int(4))
                                .with_right(Expression::Int(5))
                        ),
                    ]
                ))
            )
        );

        assert_eq!(*statement, expected);
    }

}
