use crate::{code::{Instructions, Opcode}, compiler::Bytecode, object::Object};

struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    // sp    int // Always points to the next value. Top of stack is stack[sp-1]
    // sp: usize,
}

const STACK_SIZE: usize = 2048;

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }

    // fetch-decode-execute
    pub fn run(&mut self) {
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
            }
        }
    }

    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.iter().last()
    }
    
    pub fn push(&mut self, o: Object) {
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
            let err = vm.run();
            let stack_elem = vm.stack_top().unwrap();
            test_expected_object(&test.expected, &stack_elem);
        }
    }

    fn test_expected_object(expected: &Object, actual: &Object) {
        match expected {
            Object::Integer(i) => {
                test_integer_object(*i, actual);
            }
            Object::Boolean(_) => todo!(),
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
        /*
    tests := []vmTestCase{
        {"1", 1},
        {"2", 2},
        {"1 + 2", 2}, // FIXME
    }
        */
        let tests = vec![VMTestCase {
            input: "1".to_string(),
            expected: Object::Integer(1),
        }];
        run_vm_tests(tests);
    }
}
