use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;
use interpreter::{lexer::Lexer, parser::Parser};
use interpreter::evaluator::*;
use interpreter::environment::Environment;

/*
“Dear NAME_OF_FRIEND, remember when I said that someday I’ll be someone and do something great
people will remember me for? Well, today’s the day. My Monkey interpreter works and it supports
functions, higher-order functions, closures and integers and arithmetic and long story short: I’ve
never been happier in my life!”

Excerpt From
Writing An Interpreter In Go
Thorsten Ball
https://itunes.apple.com/WebObjects/MZStore.woa/wa/viewBook?id=0
This material may be protected by copyright.
*/

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
    let mut evaluator = Evaluator;
    let env = Rc::new(RefCell::new(Environment::new()));
    println!("{}", MONKEY_FACE);
    loop {
        print!("{}", PROMPT);
        io::stdout().flush()?;
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        if buffer.len() == 0 {
            return Ok(());
        }
        let mut parser = Parser::new(Lexer::new(&buffer));
        let program = match parser.parse_program() {
            Some(program) => program,
            None => {
                println!("parse_program returned None");
                continue;
            }
        };

        let errors = parser.errors();
        if errors.len() != 0 {
            print_parse_errors(errors);
            continue;
        }

        // is this cloning a problem or is the rc released each loop ?
        let evaluated = evaluator.eval_program(&program, env.clone());
        if evaluated.is_some() {
            println!("{}", evaluated.unwrap());
        }
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

/* -------------------------------------------------------------------------------------
// Things to try in the repl
// -------------------------------------------------------------------------------------

x * y / 2 + 3 * 8 - 123
((((x * y) / 2) + (3 * 8)) - 123)

let x = 1 * 2 * 3 * 4 * 5
let x = ((((1 * 2) * 3) * 4) * 5);

true == false
(true == false)

let x 12 * 3;
error

// -------------------------------------------------------------------------------------
// Closures
// -------------------------------------------------------------------------------------

let newAdder = fn(x) { fn(y) { x + y } };
let addTwo = newAdder(2);
addTwo(3);
5
let addThree = newAdder(3);
addThree(10);

// -------------------------------------------------------------------------------------
// Pass functions
// -------------------------------------------------------------------------------------

let add = fn(a, b) { a + b };
let sub = fn(a, b) { a - b };
let applyFunc = fn(a, b, func) { func(a, b) };
applyFunc(2, 2, add);
4
applyFunc(10, 2, sub);
8

*/
