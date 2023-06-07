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

fn make(op: Opcode, operands: &Vec<u8>) -> Option<Vec<u8>> {
    let def = match DEFINITIONS.get(&op) {
        Some(def) => def,
        None => return None,
    };
    
    let mut instruction_len = 1;
    for w in &def.operand_widths {
        instruction_len += w;
    }

    let mut instruction = Vec::with_capacity(instruction_len as usize);
    instruction[0] = op as u8;


    let op_idx = 0;
    for width in &def.operand_widths {
        match width {
            2 => {
               // binary.BigEndian.PutUint16(instruction[offset:], uint16(o))
               operands[op_id]
               operand_byte.to_be();

            }
            _ => {}
        }

    }

    for (i, operand_byte) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
    }

    Some(instruction)
}

/*
func Make(op Opcode, operands ...int) []byte {
    def, ok := definitions[op]
    if !ok {
        return []byte{}
    }

    instructionLen := 1
    for _, w := range def.OperandWidths {
        instructionLen += w
    }

    // make is a go function that creates a slice, a dynamiclly-sized array
    // this makes an array of bytes that is instructionLen long
    instruction := make([]byte, instructionLen)
    instruction[0] = byte(op)

    offset := 1
    for i, o := range operands {
        width := def.OperandWidths[i]
        switch width {
        case 2:
            binary.BigEndian.PutUint16(instruction[offset:], uint16(o))
        }
        offset += width
    }
    return instruction
}
*/

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
