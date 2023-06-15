use crate::{compiler::{Bytecode, Compiler}, lexer::Lexer, parser::Parser, ast::Program, object::Object};

pub fn input_to_bytecode(input: &str) -> Bytecode {
    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    let mut compiler = Compiler::new();
    compiler.compile(program).unwrap();
    compiler.bytecode()
}

pub fn parse(input: &str) -> Option<Program> {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    p.parse_program()
}

pub fn test_integer_object(expected: isize, obj: &Object) {
    match obj {
        Object::Integer(n) => assert_eq!(n, &expected),
        _ => panic!("expected integer, got {}", obj),
    }
}

pub fn test_boolean_object(expected: bool, obj: &Object) {
    match obj {
        Object::Boolean(boolean) => assert_eq!(boolean, &expected),
        _ => panic!("expected boolean, got {}", obj),
    }
}

