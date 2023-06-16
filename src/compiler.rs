use crate::{
    ast::{Expression, InfixExpression, Program, StatementType},
    code::{Instructions, Opcode},
    object::Object,
    token::TokenType,
};
use anyhow::anyhow;

pub struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
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
            last_instruction: None,
            previous_instruction: None,
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
    
    fn last_instruction_is_pop(&self) -> bool {
        if let Some(last) = &self.last_instruction {
            last.opcode == Opcode::OpPop
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        let last_position = self.last_instruction.as_ref().unwrap().position;
        self.instructions.0.drain(last_position..);
        self.last_instruction = self.previous_instruction.take();
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        let previous = self.last_instruction.take();
        let last = EmittedInstruction {
            opcode,
            position: position.try_into().unwrap(),
        };
        self.previous_instruction = previous;
        self.last_instruction = Some(last);
    }

    fn emit(&mut self, op: Opcode, operands: Vec<u16>) -> usize {
        let ins = crate::code::make(op, &operands);
        let pos = self.add_instruction(ins.unwrap());
        self.set_last_instruction(op, pos);
        pos
    }

    fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.0.extend(ins.0.into_iter());
        pos_new_instruction
    }

    // replace_instruction and change_operand
    // The underlying assumption here is that we only replace instructions of the same type, with
    // the same non-variable length. If that assumption no longer holds, we’d have to tread far
    // more carefully here and update c.lastInstruction and c.previousInstruction accordingly. You
    // can see how another IR that’s type-safe and independent of the byte-size of encoded
    // instructions comes in handy once the compiler and the instructions it emits grow more
    // complex.

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for (i, ins) in new_instruction.0.into_iter().enumerate() {
            self.instructions.0[pos+i] = ins;
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: u16) {
        let op: Opcode = self.instructions.0[op_pos].try_into().unwrap();
        let new_instruction = crate::code::make(op, &vec![operand]).unwrap();
        self.replace_instruction(op_pos, new_instruction);
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
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
            crate::ast::StatementType::Block(block) => {
                for statement in block.statements {
                    self.compile_statement(statement)?;
                }
                Ok(())
            }
        }
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<(), anyhow::Error> {
        match expression {
            Expression::Identifier(_) => todo!(),
            Expression::Prefix(prefix) => {
                let operator = prefix.token.literal.as_str();
                match &prefix.right {
                    Some(expression) => self.compile_expression(&*expression)?,
                    None => {
                        return Err(anyhow::anyhow!(
                            "No right expression on a prefix expression"
                        ))
                    }
                }
                match operator {
                    "!" => self.emit(Opcode::OpBang, vec![]),
                    "-" => self.emit(Opcode::OpMinus, vec![]),
                    _ => return Err(anyhow::anyhow!("unknown operator {}", operator)),
                };
                Ok(())
            }
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
            Expression::If(if_expression) => {
                match &if_expression.condition {
                    Some(condition) => {
                        self.compile_expression(&condition)?;
                        let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, vec![9999]);
                        match &if_expression.consequence {
                            Some(consequence) => {
                                self.compile_statement(StatementType::Block(*consequence.clone()))?;
                                if self.last_instruction_is_pop() {
                                    self.remove_last_pop();
                                }
                                let after_consequence_pos = self.instructions.len();
                                self.change_operand(jump_not_truthy_pos as usize, after_consequence_pos as u16);
                            }

                            None => {
                                return Err(anyhow!("if expression has no consequence"));
                            }
                        }
                    }
                    None => {
                    }
                }
                Ok(())
            }
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
            },
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

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code::Opcode;
    use std::ops::Deref;
    use pretty_assertions::assert_eq;

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
            CompilerTestCase {
                input: "-1".to_string(),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    crate::code::make(Opcode::OpMinus, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            },
            CompilerTestCase {
                input: "!true".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    crate::code::make(Opcode::OpTrue, &vec![]).unwrap(),
                    crate::code::make(Opcode::OpBang, &vec![]).unwrap(),
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
        if actual.len() != concatted.len() {
            println!(
                "wrong instructions length.\nwant={}\ngot={}",
                concatted, actual
            );
            assert_eq!(format!("{}", concatted), format!("{}", actual));
        }
        for ((i, expected_ins), actual_ins) in concatted.iter().enumerate().zip(actual.iter()) {
            if actual_ins != expected_ins {
                panic!(
                    "wrong instruction at {}.\nwant={}\ngot={}",
                    i, concatted, actual
                );
            }
        }
    }

    fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
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

    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase {
                input: "if (true) { 10 }; 3333;".to_string(),
                expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
                expected_instructions: vec![
                    // 0000
                    crate::code::make(Opcode::OpTrue, &vec![]).unwrap(),
                    // 0001
                    crate::code::make(Opcode::OpJumpNotTruthy, &vec![7]).unwrap(),
                    // 0004
                    crate::code::make(Opcode::OpConstant, &vec![0]).unwrap(),
                    // 0007
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                    // 0008
                    crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
                    // 0011
                    crate::code::make(Opcode::OpPop, &vec![]).unwrap(),
                ],
            }];
        run_compiler_tests(tests);
    }
}
