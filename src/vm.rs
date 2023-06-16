use anyhow::{anyhow, Result};

use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    object::Object,
};

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    // sp    int // Always points to the next value. Top of stack is stack[sp-1]
    // sp: usize,
    last_popped: Option<Object>,
}

const STACK_SIZE: usize = 2048;

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            last_popped: None,
        }
    }

    // fetch-decode-execute
    pub fn run(&mut self) -> Result<(), anyhow::Error> {
        let what = self.instructions.clone();
        let mut iter = what.0.into_iter();
        while let Some(instruction) = iter.next() {
            let op: Opcode = instruction.try_into().unwrap();
            match op {
                Opcode::OpConstant => {
                    let (byte_1, byte_2) = (iter.next().unwrap(), iter.next().unwrap());
                    let value = u16::from_be_bytes([byte_1, byte_2]);
                    let const_index = value;
                    self.push(self.constants[const_index as usize].clone());
                }
                Opcode::OpPop => {
                    self.pop();
                }
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    self.execute_binary_operation(op)?;
                }
                Opcode::OpTrue => {
                    self.push(Object::Boolean(true));
                }
                Opcode::OpFalse => {
                    self.push(Object::Boolean(false));
                }
                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                    self.execute_comparison(op)?;
                }
                Opcode::OpMinus => {
                    self.execute_minus_operator()?;
                }
                Opcode::OpBang => {
                    self.execute_bang_operator()?;
                }
                Opcode::OpJumpNotTruthy => todo!(),
                Opcode::OpJump => todo!(),
            }
        }
        Ok(())
    }

    fn execute_comparison(&mut self, op: Opcode) -> anyhow::Result<()> {
        let right = self.pop().unwrap();
        let left = self.pop().unwrap();

        match (&left, &right) {
            (Object::Integer(l), Object::Integer(r)) => {
                return self.execute_integer_comparison(op, *l, *r);
            }
            _ => {}
        }

        match op {
            Opcode::OpEqual => self.push(self.native_to_boolean_object(right == left)),
            Opcode::OpNotEqual => self.push(self.native_to_boolean_object(right != left)),
            _ => {
                return Err(anyhow!(
                    "unknown operator: {} ({} {})",
                    op,
                    left.type_string(),
                    right.type_string()
                ))
            }
        }
        Ok(())
    }

    fn execute_integer_comparison(
        &mut self,
        op: Opcode,
        left: isize,
        right: isize,
    ) -> anyhow::Result<()> {
        match op {
            Opcode::OpEqual => self.push(self.native_to_boolean_object(right == left)),
            Opcode::OpNotEqual => self.push(self.native_to_boolean_object(right != left)),
            Opcode::OpGreaterThan => self.push(self.native_to_boolean_object(left > right)),
            _ => return Err(anyhow!("unknown operator {}", op)),
        }
        Ok(())
    }

    fn native_to_boolean_object(&self, input: bool) -> Object {
        if input {
            Object::Boolean(true)
        } else {
            Object::Boolean(false)
        }
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> anyhow::Result<()> {
        let right = self.pop().unwrap();
        let left = self.pop().unwrap();
        let right_value = match right {
            Object::Integer(i) => i,
            _ => todo!(),
        };
        let left_value = match left {
            Object::Integer(i) => i,
            _ => todo!(),
        };
        self.execute_binary_integer_operation(op, left_value, right_value)
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: isize,
        right: isize,
    ) -> anyhow::Result<()> {
        let result = match op {
            Opcode::OpAdd => left + right,
            Opcode::OpSub => left - right,
            Opcode::OpMul => left * right,
            Opcode::OpDiv => left / right,
            _ => return Err(anyhow!("unknown integer operator: {}", op)),
        };
        self.push(Object::Integer(result));
        Ok(())
    }

    fn execute_bang_operator(&mut self) -> anyhow::Result<()> {
        match self.pop() {
            Some(operand) => {
                // this here is our VM’s implementation of Monkey’s concept of truthiness
                match operand {
                    Object::Boolean(boolean) => self.push(Object::Boolean(!boolean)),
                    _ => self.push(Object::Boolean(false)),
                }
                Ok(())
            }
            None => Err(anyhow!("nothing on the stack")),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<()> {
        match self.pop() {
            Some(operand) => {
                match operand {
                    Object::Integer(value) => {
                        self.push(Object::Integer(-value));
                        Ok(())
                    }
                    _ => {
                        Err(anyhow!(
                            "unsupported type for negation: {}",
                            operand.type_string()
                        ))
                    }
                }
            }
            None => Err(anyhow!("nothing on the stack")),
        }
    }

    pub fn last_popped_stack_element(&mut self) -> Option<Object> {
        let obj = self.last_popped.clone();
        self.last_popped = None; // is this what we want ?
        obj
    }

    pub fn pop(&mut self) -> Option<Object> {
        match self.stack.pop() {
            Some(obj) => {
                self.last_popped = Some(obj.clone());
                Some(obj)
            }
            None => None,
        }
    }

    pub fn push(&mut self, o: Object) {
        // this is not really necessary unless I switch to an array implementation
        if self.stack.len() >= STACK_SIZE {
            panic!("stack overflow");
        }
        self.stack.push(o);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;
    use crate::test_helpers::*;

    struct VMTestCase {
        input: String,
        expected: Object,
    }

    impl VMTestCase {
        pub fn new_integer_case(input: &str, expected: isize) -> Self {
            Self {
                input: input.to_owned(),
                expected: Object::Integer(expected),
            }
        }
        pub fn new_boolean_case(input: &str, expected: bool) -> Self {
            Self {
                input: input.to_owned(),
                expected: Object::Boolean(expected),
            }
        }
    }

    fn run_vm_tests(tests: Vec<VMTestCase>) {
        for test in tests {
            let bytecode = crate::test_helpers::input_to_bytecode(&test.input);
            let mut vm = VM::new(bytecode);
            vm.run().unwrap();
            let stack_elem = vm.last_popped_stack_element().unwrap();
            test_expected_object(&test.expected, &stack_elem);
        }
    }

    fn test_expected_object(expected: &Object, actual: &Object) {
        match expected {
            Object::Integer(i) => {
                test_integer_object(*i, actual);
            }
            Object::Boolean(b) => {
                test_boolean_object(*b, actual);
            }
            Object::Return(_) => todo!(),
            Object::Error(_) => todo!(),
            Object::Function(_) => todo!(),
            Object::String(_) => todo!(),
            Object::Builtin(_) => todo!(),
            Object::Array(_) => todo!(),
            Object::Hash(_) => todo!(),
            Object::Null => todo!(),
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VMTestCase::new_integer_case("1", 1),
            VMTestCase::new_integer_case("2", 2),
            VMTestCase::new_integer_case("1 + 2", 3),
            VMTestCase::new_integer_case("1 - 2", -1),
            VMTestCase::new_integer_case("1 * 2", 2),
            VMTestCase::new_integer_case("4 / 2", 2),
            VMTestCase::new_integer_case("50 / 2 * 2 + 10 - 5", 55),
            VMTestCase::new_integer_case("5 + 5 + 5 + 5 - 10", 10),
            VMTestCase::new_integer_case("2 * 2 * 2 * 2 * 2", 32),
            VMTestCase::new_integer_case("5 * 2 + 10", 20),
            VMTestCase::new_integer_case("5 + 2 * 10", 25),
            VMTestCase::new_integer_case("5 * (2 + 10)", 60),
            VMTestCase::new_integer_case("-5", -5),
            VMTestCase::new_integer_case("-10", -10),
            VMTestCase::new_integer_case("-50 + 100 + -50", 0),
            VMTestCase::new_integer_case("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VMTestCase::new_boolean_case("true", true),
            VMTestCase::new_boolean_case("true", true),
            VMTestCase::new_boolean_case("false", false),
            VMTestCase::new_boolean_case("1 < 2", true),
            VMTestCase::new_boolean_case("1 > 2", false),
            VMTestCase::new_boolean_case("1 < 1", false),
            VMTestCase::new_boolean_case("1 > 1", false),
            VMTestCase::new_boolean_case("1 == 1", true),
            VMTestCase::new_boolean_case("1 != 1", false),
            VMTestCase::new_boolean_case("1 == 2", false),
            VMTestCase::new_boolean_case("1 != 2", true),
            VMTestCase::new_boolean_case("true == true", true),
            VMTestCase::new_boolean_case("false == false", true),
            VMTestCase::new_boolean_case("true == false", false),
            VMTestCase::new_boolean_case("true != false", true),
            VMTestCase::new_boolean_case("false != true", true),
            VMTestCase::new_boolean_case("(1 < 2) == true", true),
            VMTestCase::new_boolean_case("(1 < 2) == false", false),
            VMTestCase::new_boolean_case("(1 > 2) == true", false),
            VMTestCase::new_boolean_case("(1 > 2) == false", true),
            VMTestCase::new_boolean_case("!true", false),
            VMTestCase::new_boolean_case("!false", true),
            VMTestCase::new_boolean_case("!5", false),
            VMTestCase::new_boolean_case("!!true", true),
            VMTestCase::new_boolean_case("!!false", false),
            VMTestCase::new_boolean_case("!!5", true),
        ];
        run_vm_tests(tests);
    }
}
