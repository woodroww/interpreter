use crate::{code::Instructions, object::Object, ast::{Program, StatementType, InfixExpression, ExpressionStatement, Expression}};

struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    /*



pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {

pub struct Program {
    pub statements: Vec<StatementType>,

Lexer iterates over chars to produce Tokens.
Parser iterates over Tokens to produce a Program of StatementType(s).
Compiler takes a program and generates bytecode.

pub enum StatementType 
    Let(LetStatement),
        pub struct LetStatement {
            pub token: Token,
            pub name: Option<Identifier>,
            pub value: Option<Expression>,
        }
    Return(ReturnStatement),
        pub struct ReturnStatement {
            pub token: Token,
            pub return_value: Option<Expression>,
        }
    Expression(ExpressionStatement),
        pub struct ExpressionStatement {
            pub token: Token,
            pub expression: Option<Expression>,
        }
            pub enum Expression {
                Identifier(Identifier),
                Prefix(PrefixExpression),
                Infix(InfixExpression),
                Int(isize),
                Return,
                Assign,
                Boolean(BooleanExpression),
                If(IfExpression),
                FunctionLiteral(FunctionLiteralExpression),
                Call(CallExpression),
                String(StringLiteral),
                ArrayLiteral(ArrayLiteral),
                IndexExpression(IndexExpression),
                Hash(HashLiteral),
            }
    Block(BlockStatement),
        pub struct BlockStatement {
            pub token: Token,
            pub statements: Vec<StatementType>,
        }

pub struct Instructions(pub Vec<u8>);
struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}
struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}
Compiler
    pub fn compile(&self, program: Program) -> Result<(), ()> {
    pub fn bytecode(&self) -> Bytecode {



        */

    pub fn compile(&self, program: Program) -> Result<(), ()> {
        for statement in program.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn compile_statement(&self, statement: StatementType) -> Result<(), ()> {
        match statement {
            crate::ast::StatementType::Let(_) => todo!(),
            crate::ast::StatementType::Return(_) => todo!(),
            crate::ast::StatementType::Expression(expresssion_statement) => {
                match expresssion_statement.expression {
                    Some(expression) => self.compile_expression(expression),
                    None => Ok(())
                }
            }
            crate::ast::StatementType::Block(_) => todo!(),
        }
    }

    fn compile_expression(&self, expression: Expression) -> Result<(), ()> {
        match expression {
            Expression::Identifier(_) => todo!(),
            Expression::Prefix(_) => todo!(),
            Expression::Infix(infix) => self.compile_infix_expression(infix),
            Expression::Int(_) => todo!(),
            Expression::Return => todo!(),
            Expression::Assign => todo!(),
            Expression::Boolean(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::String(_) => todo!(),
            Expression::ArrayLiteral(_) => todo!(),
            Expression::IndexExpression(_) => todo!(),
            Expression::Hash(_) => todo!(),
        }
    }

    fn compile_infix_expression(&self, infix: InfixExpression) -> Result<(), ()> {
        match infix.left {
            Some(left) => self.compile_expression(*left)?,
            None => {}
        }
        match infix.right {
            Some(right) => self.compile_expression(*right)?,
            None => {}
        }
        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use crate::{code::Opcode, lexer::Lexer, parser::Parser};

    use super::*;

    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<Object>,
        expected_instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                ],
            }
        ];

        run_compiler_tests(tests);
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {

        for test in tests {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            let compiler = Compiler::new();
            compiler.compile(program).unwrap();
            let bytecode = compiler.bytecode();

            test_instructions(test.expected_instructions, bytecode.instructions);
            test_constants(test.expected_constants, bytecode.constants);
        }
    }

    fn concat_instructions(ins: Vec<Instructions>) -> Instructions {
        // TODO capacity 
        let mut concatted: Vec<u8> = Vec::new();
        for ins in ins {
            concatted.extend(ins.deref());
        }
        Instructions(concatted)
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let concatted = concat_instructions(expected);
        println!("jambones actual:   {:?}", actual.0);
        println!("jambones expected: {:?}", concatted.0);

        if actual.len() != concatted.len() {
            panic!("wrong instructions length.\nwant={:#02x?}\ngot={}",
                concatted.0, actual);
        }
        for ((i, expected), actual) in concatted.iter().enumerate().zip(actual.iter()) {
            if actual != expected {
                panic!("wrong instruction at {}.\nwant={:?}\ngot={:?}",
                    i, concatted.0, actual);
            }
        }
    }

    fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
        if actual.len() != expected.len() {
            eprintln!("wrong number of constants.\nwant={:?}\ngot={:?}",
                expected.len(), actual.len());
            panic!();
        }
        for (i, constant) in expected.iter().enumerate() {
            match constant {
                Object::Integer(value) => {
                    test_integer_object(*value, &actual[i]);
                },
                _ => todo!(),
            }
        }
    }

    fn test_integer_object(expected: isize, obj: &Object) {
        match obj {
            Object::Integer(n) => assert_eq!(n, &expected),
            _ => panic!("expected integer, got {}", obj),
        }
    }

}
