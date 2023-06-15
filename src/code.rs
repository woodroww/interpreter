use std::{collections::HashMap, ops::Deref};
use once_cell::sync::Lazy;

#[derive(Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn from(array: Vec<u8>) -> Self {
        Self(array)
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();
        let mut i = 0;

        while i < self.0.len() {
            let instruction = *self.0.iter().nth(i).unwrap();
            //println!("Display instruction {}", instruction);
            let def = lookup_definition(instruction.try_into().unwrap()).unwrap();
            let (left, right) = self.0.split_at(i+1);
            //println!("Display left {:?} right {:?}", left, right);
            let (operands, read) = read_operands(def, Instructions::from(right.to_vec()));
            let ins_string = format_instruction(def, operands);
            let s = format!("{:0>4} {}\n", i, ins_string);
            //println!("s {}", s);
            output.push_str(&s);
            i += 1 + read as usize;
        }
        write!(f, "{}", output)
    }
}

fn format_instruction(def: &Definition, operands: Vec<u16>) -> String {
    let operand_count = def.operand_widths.len();

    if operands.len() != operand_count {
        format!("ERROR: operand len {} does not match defined {}\n", operands.len(), operand_count)
    } else {
        match operand_count {
            0 => {
                format!("{}", def.name)
            }
            1 => {
                format!("{} {}", def.name, operands[0])
            }
            _ => {
                format!("ERROR: unhandled operand_count for {}", def.name)
            }
        }
    }
}

#[derive(Copy, Hash, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(u8)]
pub enum Opcode {
    OpConstant = 1,
    OpAdd = 2,
    OpPop = 3, // used after every expression, to clean the stack
    OpSub = 4,
    OpMul = 5,
    OpDiv = 6,
    OpTrue = 7,
    OpFalse = 8,
    OpEqual = 9,
    OpNotEqual = 10,
    OpGreaterThan = 11,
}

// these two (above and below) need to match

impl TryFrom<u8> for Opcode {
    type Error = String;
	fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Opcode::OpConstant),
            2 => Ok(Opcode::OpAdd),
            3 => Ok(Opcode::OpPop),
            4 => Ok(Opcode::OpSub),
            5 => Ok(Opcode::OpMul),
            6 => Ok(Opcode::OpDiv),
            7 => Ok(Opcode::OpTrue),
            8 => Ok(Opcode::OpFalse),
            9 => Ok(Opcode::OpEqual),
            10 => Ok(Opcode::OpNotEqual),
            11 => Ok(Opcode::OpGreaterThan),
            _ => Err(format!("invalid Opcode value {}", value)),
        }
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::OpConstant => write!(f, "OpConstant"),
            Opcode::OpAdd => write!(f, "OpAdd"),
            Opcode::OpPop => write!(f, "OpPop"),
            Opcode::OpSub => write!(f, "OpSub"),
            Opcode::OpMul => write!(f, "OpMul"),
            Opcode::OpDiv => write!(f, "OpDiv"),
            Opcode::OpTrue => write!(f, "OpTrue"),
            Opcode::OpFalse => write!(f, "OpFalse"),
            Opcode::OpEqual => write!(f, "OpEqual"),
            Opcode::OpNotEqual => write!(f, "OpNotEqual"),
            Opcode::OpGreaterThan => write!(f, "OpGreaterThan"),
        }
    }
}

// each Opcode enumeration needs a unique code that fits in one byte
// pg 29
pub struct Definition {
    name: String,
    operand_widths: Vec<u8>,
}

static DEFINITIONS: Lazy<HashMap<Opcode, Definition>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert(
        Opcode::OpConstant,
        Definition {
            name: "OpConstant".to_string(),
            operand_widths: vec![2], // one operand of 2 bytes
        },
    );
    map.insert(
        Opcode::OpAdd,
        Definition {
            name: "OpAdd".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpPop,
        Definition {
            name: "OpPop".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpSub,
        Definition {
            name: "OpSub".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpMul,
        Definition {
            name: "OpMul".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpDiv,
        Definition {
            name: "OpDiv".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpTrue,
        Definition {
            name: "OpTrue".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpFalse,
        Definition {
            name: "OpFalse".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpEqual,
        Definition {
            name: "OpEqual".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpNotEqual,
        Definition {
            name: "OpNotEqual".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map.insert(
        Opcode::OpGreaterThan,
        Definition {
            name: "OpGreaterThan".to_string(),
            operand_widths: vec![], // no operands
        },
    );
    map
});

pub fn lookup_definition(op: Opcode) -> Option<&'static Definition> {
    DEFINITIONS.get(&op)
}

pub fn make(op: Opcode, operands: &Vec<u16>) -> Option<Instructions> {
    let def = match DEFINITIONS.get(&op) {
        Some(def) => def,
        None => return None,
    };
    
    let mut instruction_len = 1;
    for w in &def.operand_widths {
        instruction_len += w;
    }

    let mut instruction = Vec::with_capacity(instruction_len as usize);
    instruction.push(op as u8);

    for (operand_idx, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => {
                let operand_bytes = operands[operand_idx].to_be_bytes();
                for byte in operand_bytes {
                    instruction.push(byte);
                }
            }
            _ => {}
        }
    }
    Some(Instructions(instruction))
}

pub fn read_operands(def: &Definition, ins: Instructions) -> (Vec<u16>, u16) {
    //println!("read_operands ins: {:?}", ins.0);
    //println!("read_operands definition: {}, {:?}", def.name, def.operand_widths);
    let mut operands: Vec<u16> = Vec::with_capacity(def.operand_widths.len());
    let mut offset: u16 = 0;

    for (i, width) in def.operand_widths.iter().enumerate() {
        let mut iter = ins.iter().skip(offset.into());
        match width {
            2 => {
                let (byte_1, byte_2) = (iter.next().unwrap(), iter.next().unwrap());
                //println!("byte_1: {}, byte_2: {}", byte_1, byte_2);
                let value = u16::from_be_bytes([*byte_1, *byte_2]);
                //println!("value from_be_bytes {}", value); 
                operands.push(value);
            }
            _ => {
                panic!();
            }
        }
        offset += *width as u16;
    }

    (operands, offset)
}


#[cfg(test)]
mod tests {
	use super::*;

    struct MakeData {
        op: Opcode,
        operands: Vec<u16>,
        expected: Vec<u8>,
    }

    #[test]
    fn test_make() {
        let tests = vec![
            MakeData {
                op: Opcode::OpConstant,
                operands: vec![65534],
                expected: vec![Opcode::OpConstant as u8, 255, 254],
            },
            MakeData {
                op: Opcode::OpAdd,
                operands: vec![],
                expected: vec![Opcode::OpAdd as u8],
            },
        ];

        for test in tests {
            let instructions = make(test.op, &test.operands).unwrap();
            if instructions.len() != test.expected.len() {
                eprintln!("instruction has wrong length. want={}, got={}",
                    test.expected.len(), instructions.len());
                assert!(false);
            }
            for (i, b) in test.expected.iter().enumerate() {
                if instructions[i] != *b {
                    eprintln!("wrong byte at pos {}. want={}, got={}",
                        i, b, instructions[i]);
                    assert!(false);
                }
            }
        }
	}

    struct ReadOperandData {
        op: Opcode,
        operands: Vec<u16>,
        bytes_read: u16,
    }

    #[test]
    fn test_read_operands() {
        let tests = vec![
            ReadOperandData { op: Opcode::OpConstant, operands: vec![65535], bytes_read: 2 }
        ];

        for test in tests {
            let instructions = make(test.op, &test.operands).unwrap();
            let def: &Definition = lookup_definition(test.op).unwrap();
            println!("jam {}, {:?}", def.name, def.operand_widths);

            let (_left, right) = instructions.split_at(1);
            println!("jam2 right {:?}", right);
            let (operands_read, n) = read_operands(def, Instructions::from(right.to_vec()));
            if n != test.bytes_read {
                panic!("n wrong. want={}, got={}", test.bytes_read, n);
            }

            println!("jam3 {:?}", operands_read);
            println!("jam4 {}", n);

            for (expected, actual) in test.operands.iter().zip(operands_read) {
                assert_eq!(*expected, actual);
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            crate::code::make(Opcode::OpConstant, &vec![1]).unwrap(),
            crate::code::make(Opcode::OpConstant, &vec![2]).unwrap(),
            crate::code::make(Opcode::OpConstant, &vec![65535]).unwrap(),
        ];

        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
"#;

        let mut concatted: Vec<u8> = Vec::new();
        for ins in instructions {

            println!("instruction {:?}", ins.0);
            concatted.extend(ins.deref());
        }

        let actual = format!("{}", Instructions::from(concatted));
        println!("expected {}", expected);
        println!("actual {}", actual);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_instructions_string_2() {
        let instructions = vec![
            crate::code::make(Opcode::OpAdd, &vec![]).unwrap(),
            crate::code::make(Opcode::OpConstant, &vec![2]).unwrap(),
            crate::code::make(Opcode::OpConstant, &vec![65535]).unwrap(),
        ];

        let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
"#;

        let mut concatted: Vec<u8> = Vec::new();
        for ins in instructions {

            println!("instruction {:?}", ins.0);
            concatted.extend(ins.deref());
        }

        let actual = format!("{}", Instructions::from(concatted));
        println!("expected {}", expected);
        println!("actual {}", actual);

        assert_eq!(expected, actual);
    }

}

