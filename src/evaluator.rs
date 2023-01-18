use crate::{
    ast::{Expression, StatementType},
    object::Object,
    token::TokenType,
};

pub struct Evaluator;

impl Evaluator {
    fn eval_statement(&mut self, node: &StatementType) -> Option<Object> {
        match node {
            StatementType::Let(_) => todo!(),
            StatementType::Return(_) => todo!(),
            StatementType::Expression(statement) => self.eval_expression(
                statement
                    .expression
                    .as_ref()
                    .expect("will have to deal with this later"),
            ),
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> Option<Object> {
        match &expression {
            Expression::Int(n) => Some(Object::Integer(*n)),
            Expression::Boolean(b) => Some(Object::Boolean(b.value)),
            Expression::Prefix(prefix) => {
                let right = self.eval_expression(&prefix.right.as_ref().unwrap());
                self.eval_prefix_expression(prefix.token.token_type, right.unwrap())
            }
            Expression::Infix(infix) => {
                let left = self
                    .eval_expression(
                        infix
                            .left
                            .as_ref()
                            .expect("will have to deal with this later"),
                    )
                    .expect("will have to deal with this later");
                let right = self
                    .eval_expression(
                        infix
                            .right
                            .as_ref()
                            .expect("will have to deal with this later"),
                    )
                    .expect("will have to deal with this later");
                self.eval_infix_expression(infix.token.token_type, left, right)
            }
            /*
            Expression::Identifier(_) => None,
            Expression::Return => todo!(),
            Expression::Assign => todo!(),
            Expression::If(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::Call(_) => todo!(),*/
            _ => None,
        }
    }

    fn eval_infix_expression(
        &self,
        token: TokenType,
        left: Object,
        right: Object,
    ) -> Option<Object> {
        match (left, right) {
            (Object::Integer(a), Object::Integer(b)) => self.eval_integer_infix_expression(token, a, b),
            (_, _) => None
        }
    }

    fn eval_integer_infix_expression(&self, token: TokenType, left: isize, right: isize) -> Option<Object> {
        match token {
            TokenType::Plus => Some(Object::Integer(left + right)),
            TokenType::Minus => Some(Object::Integer(left - right)),
            TokenType::Slash => Some(Object::Integer(left / right)),
            TokenType::Asterisk => Some(Object::Integer(left * right)),
            _ => None,
        }
    }

    fn eval_prefix_expression(&self, token: TokenType, right: Object) -> Option<Object> {
        match token {
            TokenType::Bang => self.eval_bang_operator_expression(right),
            TokenType::Minus => self.eval_minus_prefix_operator_expression(right),
            _ => None,
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Option<Object> {
        match right {
            Object::Boolean(b) => Some(Object::Boolean(!b)),
            Object::Null => Some(Object::Boolean(true)),
            _ => Some(Object::Boolean(false)),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Option<Object> {
        match right {
            Object::Integer(n) => Some(Object::Integer(-n)),
            _ => None,
        }
    }

    pub fn eval_statements(&mut self, statements: &Vec<StatementType>) -> Option<Object> {
        let mut result = Some(Object::Null);
        for statement in statements {
            result = self.eval_statement(statement);
        }
        result
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};
    use pretty_assertions::assert_eq;

    fn test_eval(input: &str) -> Option<Object> {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();
        let mut evaluator = Evaluator;
        evaluator.eval_statements(&program.unwrap().statements)
    }

    fn test_integer_object(obj: Object, expected: isize) {
        match obj {
            Object::Integer(n) => assert_eq!(n, expected),
            object => panic!("expected integer, got {}", object),
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        match obj {
            Object::Boolean(b) => assert_eq!(b, expected),
            object => panic!("expected integer, got {}", object),
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let inputs = vec!["true", "false"];
        let expected = vec![true, false];

        for (input, expected) in inputs.iter().zip(expected) {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated.unwrap(), expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            test_boolean_object(evaluated.unwrap(), test.1);
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for test in tests {
            let evaluated = test_eval(test.0);
            test_integer_object(evaluated.unwrap(), test.1);
        }
    }
}
