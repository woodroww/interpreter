use crate::{code::{Instructions, Opcode}, object::Object, ast::{Program, StatementType, InfixExpression, ExpressionStatement, Expression}};

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

    // We append the obj to the end of the compilers constants slice and give it its very own
    // identifier by returning its index in the constants slice. This identifier will now be used
    // as the operand for the OpConstant instruction that should cause the VM to load this constant
    // from the constants pool on to the stack.
    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1) as u16
    }

    pub fn compile(&mut self, program: Program) -> Result<(), ()> {
        for statement in program.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, statement: StatementType) -> Result<(), ()> {
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

    fn compile_expression(&mut self, expression: Expression) -> Result<(), ()> {
        match expression {
            Expression::Identifier(_) => todo!(),
            Expression::Prefix(_) => todo!(),
            Expression::Infix(infix) => self.compile_infix_expression(infix),
            Expression::Int(i) => {
                let integer_obj = Object::Integer(i);
                let what = self.add_constant(integer_obj);
                self.emit(Opcode::OpConstant, vec![what]);
                Ok(())
            },
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

    fn compile_infix_expression(&mut self, infix: InfixExpression) -> Result<(), ()> {
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

    fn emit(&mut self, op: Opcode, operands: Vec<u16>) -> u16 {
        let ins = crate::code::make(op, &operands);
        self.add_instruction(ins.unwrap())
    }

    fn add_instruction(&mut self, ins: Instructions) -> u16 {
        let pos_new_instruction = self.instructions.len();
        self.instructions.0.extend(ins.iter());
        // TODO so this datatype I'm not sure what it should be, could it be a usize idk
        pos_new_instruction as u16
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
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
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
            let mut compiler = Compiler::new();
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
        println!("test_constants\n\texpected: {:?}\n\tactual: {:?}", expected, actual);
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
