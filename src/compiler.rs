use crate::{code::Instructions, object::Object, ast::Program};

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

    pub fn compile(&self, program: Program) -> Result<(), ()> {
        /*
        let s = &program.statements[0];
        match s {
            crate::ast::StatementType::Let(_) => todo!(),
            crate::ast::StatementType::Return(_) => todo!(),
            crate::ast::StatementType::Expression(expresssion_statement) => {
                self.compile(expresssion_statement.expression);
            }
            crate::ast::StatementType::Block(_) => todo!(),
        }
        */
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
