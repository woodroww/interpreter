use crate::{
    ast::{Expression, InfixExpression, Program, StatementType},
    code::{Instructions, Opcode},
    object::Object,
    token::TokenType,
};

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
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

    pub fn compile(&mut self, program: Program) -> Result<(), anyhow::Error> {
        for statement in program.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, statement: StatementType) -> Result<(), anyhow::Error> {
        match statement {
            crate::ast::StatementType::Let(_) => todo!(),
            crate::ast::StatementType::Return(_) => todo!(),
            crate::ast::StatementType::Expression(expresssion_statement) => {
                match expresssion_statement.expression {
                    Some(expression) => {
                        self.compile_expression(&expression)?;
                        self.emit(Opcode::OpPop, vec![]);
                        Ok(())
                    }
                    None => Err(anyhow::anyhow!(
                        "Expected an Expression from the ExpressionStatement"
                    )),
                }
            }
            crate::ast::StatementType::Block(_) => todo!(),
        }
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<(), anyhow::Error> {
        match expression {
            Expression::Identifier(_) => todo!(),
            Expression::Prefix(_) => todo!(),
            Expression::Infix(infix) => self.compile_infix_expression(&infix),
            Expression::Int(i) => {
                let integer_obj = Object::Integer(*i);
                let what = self.add_constant(integer_obj);
                self.emit(Opcode::OpConstant, vec![what]);
                Ok(())
            }
            Expression::Return => todo!(),
            Expression::Assign => todo!(),
            Expression::Boolean(boolean) => {
                match boolean.value {
                    true => self.emit(Opcode::OpTrue, vec![]),
                    false => self.emit(Opcode::OpFalse, vec![]),
                };
                Ok(())
            }
            Expression::If(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::String(_) => todo!(),
            Expression::ArrayLiteral(_) => todo!(),
            Expression::IndexExpression(_) => todo!(),
            Expression::Hash(_) => todo!(),
        }
    }

    fn compile_infix_expression(&mut self, infix: &InfixExpression) -> Result<(), anyhow::Error> {
        match TokenType::type_from_str(&infix.operator()) {
            Some(operator) => match operator {
                TokenType::LessThan => {
                    match &infix.right {
                        Some(right) => self.compile_expression(&*right)?,
                        None => {}
                    }
                    match &infix.left {
                        Some(left) => self.compile_expression(&*left)?,
                        None => {}
                    }
                }
                _ => {
                    match &infix.left {
                        Some(left) => self.compile_expression(&*left)?,
                        None => {}
                    }
                    match &infix.right {
                        Some(right) => self.compile_expression(&*right)?,
                        None => {}
                    }
                }
            }
            None => todo!(),
        };

        match TokenType::type_from_str(&infix.operator()) {
            Some(operator) => match operator {
                TokenType::Plus => {
                    self.emit(Opcode::OpAdd, vec![]);
                    Ok(())
                }
                TokenType::Minus => {
                    self.emit(Opcode::OpSub, vec![]);
                    Ok(())
                }
                TokenType::Asterisk => {
                    self.emit(Opcode::OpMul, vec![]);
                    Ok(())
                }
                TokenType::Slash => {
                    self.emit(Opcode::OpDiv, vec![]);
                    Ok(())
                }
                TokenType::GreaterThan => {
                    self.emit(Opcode::OpGreaterThan, vec![]);
                    Ok(())
                }
                TokenType::LessThan => {
                    self.emit(Opcode::OpGreaterThan, vec![]);
                    Ok(())
                }
                TokenType::Equal => {
                    self.emit(Opcode::OpEqual, vec![]);
                    Ok(())
                }
                TokenType::NotEqual => {
                    self.emit(Opcode::OpNotEqual, vec![]);
                    Ok(())
                }

                _ => return Err(anyhow::anyhow!("unknown operator {}", infix.operator())),
            },
            None => todo!(),
        }
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
    use super::*;
    use crate::code::Opcode;
    use std::ops::Deref;

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
                    crate::code::make(Opcode::OpAdd, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "1; 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "1 - 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpSub, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "1 * 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpMul, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "2 / 1".to_string(),
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpDiv, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let bytecode = crate::test_helpers::input_to_bytecode(&test.input);
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
        //println!("jambones actual:   {:?}", actual.0);
        //println!("jambones expected: {:?}", concatted.0);

        if actual.len() != concatted.len() {
            panic!(
                "wrong instructions length.\nwant={}\ngot={}",
                concatted, actual
            );
        }
        for ((i, expected), actual) in concatted.iter().enumerate().zip(actual.iter()) {
            if actual != expected {
                panic!(
                    "wrong instruction at {}.\nwant={}\ngot={}",
                    i, concatted, actual
                );
            }
        }
    }

    fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
        //println!("test_constants\n\texpected: {:?}\n\tactual: {:?}", expected, actual);
        if actual.len() != expected.len() {
            eprintln!(
                "wrong number of constants.\nwant={:?}\ngot={:?}",
                expected.len(),
                actual.len()
            );
            panic!();
        }
        for (i, constant) in expected.iter().enumerate() {
            match constant {
                Object::Integer(value) => {
                    crate::test_helpers::test_integer_object(*value, &actual[i]);
                }
                _ => todo!(),
            }
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "true".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpTrue, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "false".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpFalse, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "1 > 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpGreaterThan, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "1 < 2".to_string(),
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpGreaterThan, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "1 == 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpEqual, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "1 != 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    crate::code::make(Opcode::OpNotEqual, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "true == false".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpTrue, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpFalse, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpEqual, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "true != false".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpTrue, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpFalse, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpNotEqual, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
        ];
        run_compiler_tests(tests);
    }
}
