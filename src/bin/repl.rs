use std::io;
use std::io::Write;
use interpreter::{lexer::Lexer, parser::Parser};
use interpreter::evaluator::*;

const PROMPT: &'static str = ">> ";
const MONKEY_FACE: &'static str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;


fn main() -> io::Result<()> {
    start()?;
    Ok(())
}


fn start() -> io::Result<()> {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush()?;
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        if buffer.len() == 0 {
            return Ok(());
        }
        let mut parser = Parser::new(Lexer::new(&buffer));
        let program = parser.parse_program();
        if program.is_none() {
            println!("parse_program returned None");
            continue;
        }
        let program = program.unwrap();

        let errors = parser.errors();
        if errors.len() != 0 {
            print_parse_errors(errors);
            continue;
        }

        let mut evaluator = Evaluator;
        let evaluated = evaluator.eval_statements(&program.statements);
        if evaluated.is_some() {
            println!("{}", evaluated.unwrap());
        }

        //println!("{}", program);
    }
}

fn print_parse_errors(errors: Vec<String>) {
    let mut error_string = if errors.len() == 1 {
        format!("parser has {} error\n", errors.len())
    } else {
        format!("parser has {} errors\n", errors.len())
    };
    for (i, e) in errors.iter().enumerate() {
        error_string.push_str(&format!("parser error: {}: {}\n", i, e));
    }
    println!("{}{}", MONKEY_FACE, error_string);
}

/*

x * y / 2 + 3 * 8 - 123
// ((((x * y) / 2) + (3 * 8)) - 123)

let x = 1 * 2 * 3 * 4 * 5
// let x = ((((1 * 2) * 3) * 4) * 5);

true == false
// (true == false)

let x 12 * 3;
// error
*/
