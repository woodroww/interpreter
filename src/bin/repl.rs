use std::io;
use interpreter::{lexer::Lexer, token::Token};

//const PROMPT: &'static str = ">> ";

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    //println!("{}", PROMPT);
    loop {
        io::stdin().read_line(&mut buffer)?;
        let tokens = Lexer::new(&buffer).collect::<Vec<Token>>();
        println!("{:?}", tokens);
    }
}
