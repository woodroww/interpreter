use std::collections::HashMap;
use once_cell::sync::Lazy;

/*
type Instructions []byte
type Opcode byte
*/
// pg 28
type Instructions = Vec<Opcode>;

// pg 29
/*
    const (
        OpConstant Opcode = iota
    )
*/
#[derive(Copy, Hash, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(u8)]
pub enum Opcode {
    OpConstant = 1,
}

// these two 1's need to match idk (its like double code)

impl TryFrom<u8> for Opcode {
    type Error = &'static str;
	fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Opcode::OpConstant),
            _ => Err("invalid value for Opcode")
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
    map
});

/*
var definitions = map[Opcode]*Definition{
    OpConstant: {"OpConstant", []int{2}},
}

func Lookup(op byte) (*Definition, error) {
    def, ok := definitions[Opcode(op)]
    if !ok {
        return nil, fmt.Errorf("opcode %d undefined", op)
    }
    return def, nil
}
*/

fn make(op: Opcode, operands: &Vec<u16>) -> Option<Vec<u8>> {
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

    Some(instruction)
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
        ];

        for test in tests {
            let instruction = make(test.op, &test.operands).unwrap();
            println!("instruction {:?}", instruction);
            if instruction.len() != test.expected.len() {
                eprintln!("instruction has wrong length. want={}, got={}",
                    test.expected.len(), instruction.len());
                assert!(false);
            }
            for (i, b) in test.expected.iter().enumerate() {
                if instruction[i] != *b {
                    eprintln!("wrong byte at pos {}. want={}, got={}",
                        i, b, instruction[i]);
                    assert!(false);
                }
            }
        }
	}
}

/*
func TestMake(t *testing.T) {
    tests := []struct {
        op
        Opcode
        operands []int
        expected []byte
    }{
        {OpConstant, []int{65534}, []byte{byte(OpConstant), 255, 254}},
    }
    for _, tt := range tests {
        instruction := Make(tt.op, tt.operands...)

        if len(instruction) != len(tt.expected) {
            t.Errorf("instruction has wrong length. want=%d, got=%d",
                len(tt.expected), len(instruction))
        }

        for i, b := range tt.expected {
            if instruction[i] != tt.expected[i] {
                t.Errorf("wrong byte at pos %d. want=%d, got=%d",
                    i, b, instruction[i])
            }
        }
    }
}
*/
