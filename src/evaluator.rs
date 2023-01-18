use crate::{ast::{Expression, StatementType}, object::Object, token::TokenType};

pub struct Evaluator;

impl Evaluator {
    fn eval_statement(&mut self, node: &StatementType) -> Option<Object> {
        match node {
            StatementType::Let(_) => todo!(),
            StatementType::Return(_) => todo!(),
            StatementType::Expression(statement) => {
                self.eval_expression(statement.expression.as_ref().expect("will have to deal with this later"))
            }
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
            /*
            Expression::Identifier(_) => None,
            Expression::Infix(_) => todo!(),
            Expression::Return => todo!(),
            Expression::Assign => todo!(),
            Expression::If(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::Call(_) => todo!(),*/
            _ => None,
        }
    }

    fn eval_prefix_expression(&self, token: TokenType, right: Object) -> Option<Object> {
        match token {
            TokenType::Bang => self.eval_bang_operator_expression(right),
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
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut evaluator = Evaluator;
        let what = evaluator.eval_statements(&program.unwrap().statements);
        what
    }

    fn test_integer_object(obj: Object, expected: usize) {
        match obj {
            Object::Integer(n) => assert_eq!(n, expected),
            object => panic!("expected integer, got {}", object),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let inputs = vec!["5", "10"];
        let expected = vec![5, 10];

        for (input, expected) in inputs.iter().zip(expected) {
            let evaluated = test_eval(input);
            test_integer_object(evaluated.unwrap(), expected);
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
}
