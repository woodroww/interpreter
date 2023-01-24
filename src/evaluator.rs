use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockStatement, Expression, Program, StatementType, Identifier},
    environment::Environment,
    object::{FunctionObject, Object, BuiltinObject},
    token::TokenType, builtins::Builtins,
};

pub struct Evaluator {
    builtins: Builtins,
}

impl Evaluator {

    pub fn new() -> Self {
        Self { builtins: Builtins::new() }
    }

    fn eval_statement(
        &mut self,
        node: &StatementType,
        env: Rc<RefCell<Environment>>,
    ) -> Option<Object> {
        match node {
            StatementType::Let(let_statement) => {
                let value =
                    self.eval_expression(&let_statement.value.as_ref().unwrap(), env.clone());
                let value = match value {
                    Some(value) => {
                        if let Object::Error(_) = value {
                            return Some(value);
                        } else {
                            value
                        }
                    }
                    None => todo!(),
                };
                Some(env.borrow_mut().set(
                    let_statement.name.as_ref().unwrap().token.literal.clone(),
                    value,
                ))
            }
            StatementType::Return(return_statement) => {
                let value = self
                    .eval_expression(
                        return_statement
                            .return_value
                            .as_ref()
                            .expect("do we have to have a return value, I assume"),
                        env,
                    )
                    .expect("this to evalutate to something");
                if let Object::Error(_) = value {
                    return Some(value);
                }
                Some(Object::Return(Box::new(value)))
            }
            StatementType::Expression(statement) => self.eval_expression(
                statement
                    .expression
                    .as_ref()
                    .expect("will have to deal with this later"),
                env,
            ),
            StatementType::Block(block_statement) => {
                self.eval_block_statement(&block_statement, env)
            }
        }
    }

    fn eval_expression(
        &mut self,
        expression: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Option<Object> {
        match &expression {
            Expression::Int(n) => Some(Object::Integer(*n)),
            Expression::Boolean(b) => Some(Object::Boolean(b.value)),
            Expression::Prefix(prefix) => {
                let right = self.eval_expression(&prefix.right.as_ref().unwrap(), env);
                match &right {
                    Some(right) => {
                        if let Object::Error(_) = right {
                            return Some(right.clone());
                        }
                    }
                    None => todo!(),
                }

                self.eval_prefix_expression(prefix.token.token_type, right.unwrap())
            }
            Expression::Infix(infix) => {
                let left = self.eval_expression(
                    infix
                        .left
                        .as_ref()
                        .expect("will have to deal with this later"),
                    env.clone(),
                );
                let left = match left {
                    Some(left) => {
                        if let Object::Error(_) = left {
                            return Some(left.clone());
                        } else {
                            left
                        }
                    }
                    None => todo!(),
                };

                let right = self.eval_expression(
                    infix
                        .right
                        .as_ref()
                        .expect("will have to deal with this later"),
                    env,
                );
                let right = match right {
                    Some(right) => {
                        if let Object::Error(_) = right {
                            return Some(right.clone());
                        } else {
                            right
                        }
                    }
                    None => todo!(),
                };

                self.eval_infix_expression(infix.token.token_type, left, right)
            }
            Expression::If(if_expression) => {
                let condition = self.eval_expression(
                    &*if_expression
                        .condition
                        .as_ref()
                        .expect("do we have to have a condition?"),
                    env.clone(),
                );
                let condition = match condition {
                    Some(condition) => {
                        if let Object::Error(_) = condition {
                            return Some(condition.clone());
                        } else {
                            condition
                        }
                    }
                    None => todo!(),
                };
                if is_truthy(condition) {
                    if let Some(consequence) = &if_expression.consequence {
                        self.eval_statement(
                            &StatementType::Block(*consequence.clone()),
                            env.clone(),
                        )
                    } else {
                        Some(Object::Null)
                    }
                } else if let Some(alternative) = &if_expression.alternative {
                    self.eval_statement(&StatementType::Block(*alternative.clone()), env)
                } else {
                    Some(Object::Null)
                }
            }
            Expression::Identifier(ident) => {
                self.eval_identifier(ident, env)
            }
            Expression::FunctionLiteral(function_literal) => {
                let obj = FunctionObject {
                    parameters: function_literal.parameters.clone(),
                    body: function_literal.body.clone(),
                    env: Rc::clone(&env),
                };
                Some(Object::Function(obj))
            }
            Expression::Call(call_expression) => {
                let function = self
                    .eval_expression(&call_expression.function.as_ref().unwrap(), Rc::clone(&env))
                    .unwrap();
                if let Object::Error(_) = function {
                    return Some(function);
                }
                let args = self.eval_expressions(&call_expression.arguments, env);
                if args.len() == 1 {
                    if let Object::Error(_) = args[0] {
                        return Some(args[0].clone());
                    }
                }
                match &function {
                    Object::Function(_) | Object::Builtin(_) => self.apply_function(function, args),
                    _ => Some(Object::Error(format!("not a function: {}", function))),
                }
            }
            Expression::String(s) => {
                Some(Object::String(s.value.clone()))
            }
            Expression::Return => todo!(),
            Expression::Assign => todo!(),
            Expression::ArrayLiteral(_) => todo!(),
            Expression::IndexExpression(_) => todo!(),
        }
    }

    fn apply_function(&mut self, obj: Object, args: Vec<Object>) -> Option<Object> {
        match obj {
            Object::Function(function) => {
                let extended_env = self.extend_function_env(&function, &args);
                let evaluated = self.eval_block_statement(
                    function.body.as_ref().unwrap(),
                    Rc::new(RefCell::new(extended_env)),
                );
                Some(self.unwrap_return_value(evaluated.unwrap()))
            }
            Object::Builtin(builtin_obj) => {
                (builtin_obj.function)(args)
            }
            _ => {
                Some(Object::Error(format!("not a function {}", obj)))
            }
        }
    }

    fn extend_function_env(
        &mut self,
        function: &FunctionObject,
        args: &Vec<Object>,
    ) -> Environment {
        let mut env = Environment::new().with_outer(function.env.clone());
        for (param, arg) in function.parameters.iter().zip(args) {
            env.set(param.token.literal.clone(), arg.clone());
        }
        env
    }

    fn unwrap_return_value(&self, obj: Object) -> Object {
        if let Object::Return(value) = obj {
            *value
        } else {
            obj
        }
    }

    fn eval_identifier(&mut self, ident: &Identifier, env: Rc<RefCell<Environment>>) -> Option<Object> {
        let ident = ident.token.literal.clone();
        let value = env.borrow().get(&ident).clone();
        match value {
            Some(obj) => {
                if let Object::Error(_) = obj {
                    return Some(Object::new_error(&format!(
                        "identifier not found: {}",
                        ident
                    )));
                } else {
                    Some(obj)
                }
            }
            None => {
                let function = self.builtins.get(&ident);
                match function {
                    Some(function) => {
                        Some(Object::Builtin(BuiltinObject::new(*function)))
                    }
                    None => {
                        Some(Object::new_error(&format!(
                            "identifier not found: {}",
                            ident
                        )))
                    }
                }

            }
        }
    }

    fn eval_expressions(
        &mut self,
        args: &Vec<Expression>,
        env: Rc<RefCell<Environment>>,
    ) -> Vec<Object> {
        let mut result = Vec::new();
        for expression in args {
            let evaluated = self.eval_expression(expression, env.clone()).unwrap();
            if let Object::Error(_) = evaluated {
                return vec![evaluated];
            }
            result.push(evaluated);
        }
        result
    }

    fn eval_infix_expression(
        &self,
        operator: TokenType,
        left: Object,
        right: Object,
    ) -> Option<Object> {
        match (&left, &right) {
            (Object::Integer(a), Object::Integer(b)) => {
                return self.eval_integer_infix_expression(operator, *a, *b);
            }
            (Object::String(a), Object::String(b)) => {
                return self.eval_string_infix_expression(operator, a, b);
            }
            (_, _) => {}
        }
        match operator {
            TokenType::Equal => return Some(Object::Boolean(left == right)),
            TokenType::NotEqual => return Some(Object::Boolean(left != right)),
            _ => {}
        }
        if std::mem::discriminant(&left) != std::mem::discriminant(&right) {
            Some(Object::new_error(&format!(
                "type mismatch: {} {} {}",
                left, operator, right
            )))
        } else {
            Some(Object::new_error(&format!(
                "unknown operator: {} {} {}",
                left, operator, right
            )))
        }
    }

    fn eval_integer_infix_expression(
        &self,
        token: TokenType,
        left: isize,
        right: isize,
    ) -> Option<Object> {
        match token {
            TokenType::Plus => Some(Object::Integer(left + right)),
            TokenType::Minus => Some(Object::Integer(left - right)),
            TokenType::Slash => Some(Object::Integer(left / right)),
            TokenType::Asterisk => Some(Object::Integer(left * right)),
            TokenType::LessThan => Some(Object::Boolean(left < right)),
            TokenType::GreaterThan => Some(Object::Boolean(left > right)),
            TokenType::Equal => Some(Object::Boolean(left == right)),
            TokenType::NotEqual => Some(Object::Boolean(left != right)),
            _ => Some(Object::new_error(&format!(
                "unknown operator {} {} {}",
                left, token, right
            ))),
        }
    }

    fn eval_string_infix_expression(
        &self,
        token: TokenType,
        left: &str,
        right: &str,
    ) -> Option<Object> {
        if token != TokenType::Plus {
            Some(Object::Error(format!("unknown operator: {} {} {}", left, token, right)))
        } else {
            let result = left.to_owned() + right;
            Some(Object::String(result))
        }
    }

    fn eval_prefix_expression(&self, token: TokenType, right: Object) -> Option<Object> {
        match token {
            TokenType::Bang => self.eval_bang_operator_expression(right),
            TokenType::Minus => self.eval_minus_prefix_operator_expression(right),
            _ => Some(Object::new_error(&format!(
                "unknown operator {}{}",
                token, right
            ))),
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
            _ => Some(Object::new_error(&format!("unknown operator -{}", right))),
        }
    }

    pub fn eval_block_statement(
        &mut self,
        block: &BlockStatement,
        env: Rc<RefCell<Environment>>,
    ) -> Option<Object> {
        let mut result = Some(Object::Null);
        for statement in &block.statements {
            result = self.eval_statement(&statement, env.clone());
            if let Some(Object::Return(_)) = result {
                return Some(result.unwrap().clone());
            }
            if let Some(Object::Error(_)) = result {
                return Some(result.unwrap().clone());
            }
        }
        result
    }

    pub fn eval_program(
        &mut self,
        program: &Program,
        env: Rc<RefCell<Environment>>,
    ) -> Option<Object> {
        let mut result = Some(Object::Null);
        for statement in &program.statements {
            result = self.eval_statement(&statement, env.clone());
            match &result {
                Some(obj) => match obj {
                    Object::Return(return_value) => return Some(*return_value.clone()),
                    Object::Error(_err) => return result,
                    _ => {}
                },
                None => {}
            }
        }
        result
    }
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Integer(_) => true,
        Object::Boolean(b) => b,
        Object::Null => false,
        _ => false,
        //Object::Return(_) => todo!(),
        //Object::Error(_) => todo!(),
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
        let mut evaluator = Evaluator::new();
        let env = Environment::new();
        evaluator.eval_program(&program.unwrap(), Rc::new(RefCell::new(env)))
    }

    fn test_integer_object(obj: Object, expected: isize) {
        match obj {
            Object::Integer(n) => assert_eq!(n, expected),
            _ => panic!("expected integer, got {}", obj),
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
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated.unwrap(), *expected);
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

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            match evaluated {
                Some(obj) => match test.1 {
                    Some(n) => test_integer_object(obj, n),
                    None => {
                        assert_eq!(obj, Object::Null);
                        println!("got evaluated to Object::Null");
                    }
                },
                None => {
                    unreachable!();
                    //assert_eq!(None, test.1);
                    //println!("got evaluated to None");
                }
            };
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 2 * 5; 9;", 10),
            ("return 10; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
    if (10 > 1) {
        return 10;
    }
    return 1;
}",
                10,
            ),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            match evaluated {
                Some(obj) => test_integer_object(obj, test.1),
                None => panic!("didn't evaluate to anything"),
            }
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}
",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
        ];

        for test in tests {
            let evaluated = test_eval(test.0);
            match evaluated {
                Some(obj) => {
                    if let Object::Error(err) = obj {
                        println!("{}", err);
                    } else {
                        panic!("didn't get error");
                    }
                }
                None => panic!("didn't evaluate to anything"),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for test in tests {
            test_integer_object(test_eval(test.0).unwrap(), test.1);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input).unwrap();

        if let Object::Function(function) = evaluated {
            if function.parameters.len() != 1 {
                panic!(
                    "function has wrong parameters. Parameters {:?}",
                    function.parameters
                );
            }

            if format!("{}", function.parameters[0]) != "x" {
                panic!("parameter is not 'x'. got {}", function.parameters[0]);
            }

            let expected_body = "(x + 2)";

            let function_body = format!("{}", function.body.unwrap());
            if function_body != expected_body {
                panic!("body is not {}. got {}", expected_body, function_body);
            }
        } else {
            panic!("object is not Function. got {}", evaluated);
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for test in tests {
            test_integer_object(test_eval(test.0).unwrap(), test.1);
        }
    }

    #[test]
    fn test_closures() {
        let input = "
let newAdder = fn(x) {
    fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);";

        test_integer_object(test_eval(input).unwrap(), 4);
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";
        let evaluated = test_eval(input).unwrap();
        if let Object::String(s) = evaluated {
            assert_eq!("Hello World!", s);
        } else {
            panic!("object is not String. got {}", evaluated);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;
        let evaluated = test_eval(input).unwrap();

        if let Object::String(s) = evaluated {
            assert_eq!("Hello World!", s);
        } else {
            panic!("object is not String. got {}", evaluated);
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, 0),
            (r#"len("four")"#, 4),
            (r#"len("hello world")"#, 11),
        ];
        let error_tests = vec![
            (r#"len(1)"#, "argument to `len` not supported, got INTEGER"),
            (r#"len("one", "two")"#, "wrong number of arguments. got 2, expected 1"),
        ];

        for test in tests {
            let evaluated = test_eval(test.0).unwrap();
            match evaluated {
                Object::Integer(_n) => test_integer_object(evaluated, test.1),
                _ => panic!("not an integer, got '{}'", evaluated)
            }
        }
        for test in error_tests {
            let evaluated = test_eval(test.0).unwrap();
            match evaluated {
                Object::Error(err) => {
                    if err != test.1 {
                        panic!("wrong error message. expected '{}', got '{}'", test.1, err);
                    }
                },
                _ => panic!("not a error object. got {}", evaluated),
            }
        }
    }
}
