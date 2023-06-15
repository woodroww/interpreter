use anyhow::anyhow;

use crate::{code::{Instructions, Opcode}, compiler::Bytecode, object::Object};

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
            }
        }
        Ok(())
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

    fn execute_binary_integer_operation(&mut self, op: Opcode, left: isize, right: isize) -> anyhow::Result<()> {
        let result = match op {
            Opcode::OpAdd => left + right,
            Opcode::OpSub => left - right,
            Opcode::OpMul => left * right,
            Opcode::OpDiv => left / right,
            _ => return Err(anyhow!("unknown integer operator: {}", op))
        };
        self.push(Object::Integer(result));
        Ok(())
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
            None => {
                None
            }
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
            },
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
            VMTestCase {
                input: "1".to_string(),
                expected: Object::Integer(1),
            },
            VMTestCase {
                input: "2".to_string(),
                expected: Object::Integer(2),
            },
            VMTestCase {
                input: "1 + 2".to_string(),
                expected: Object::Integer(3),
            },
            VMTestCase {
                input: "1 - 2".to_string(),
                expected: Object::Integer(-1),
            },
            VMTestCase {
                input: "1 * 2".to_string(),
                expected: Object::Integer(2),
            },
            VMTestCase {
                input: "4 / 2".to_string(),
                expected: Object::Integer(2),
            },
            VMTestCase {
                input: "50 / 2 * 2 + 10 - 5".to_string(),
                expected: Object::Integer(55),
            },
            VMTestCase {
                input: "5 + 5 + 5 + 5 - 10".to_string(),
                expected: Object::Integer(10),
            },
            VMTestCase {
                input: "2 * 2 * 2 * 2 * 2".to_string(),
                expected: Object::Integer(32),
            },
            VMTestCase {
                input: "5 * 2 + 10".to_string(),
                expected: Object::Integer(20),
            },
            VMTestCase {
                input: "5 + 2 * 10".to_string(),
                expected: Object::Integer(25),
            },
            VMTestCase {
                input: "5 * (2 + 10)".to_string(),
                expected: Object::Integer(60),
            },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VMTestCase { input: "true".to_string(), expected: Object::Boolean(true) },
            VMTestCase { input: "false".to_string(), expected: Object::Boolean(false) }
        ];
        run_vm_tests(tests);
    }
}
