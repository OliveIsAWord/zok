#![forbid(unsafe_code, missing_debug_implementations)]
#![feature(box_patterns, box_syntax, pattern)]

mod executor;
mod lexer;
mod parser;

use executor::{evaluate, Value};
use lexer::lex;
use parser::parse_expr;
use std::error::Error;
use std::io::{self, Write};

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut source = String::new();
        io::stdin().read_line(&mut source).unwrap();
        let output = match eval(&source) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("Error: {e}");
                continue;
            }
        };
        println!("Output: {output}");
    }
}

fn eval(source: &str) -> Result<Value, Box<dyn Error>> {
    let tokens = lex(source)?;
    println!("Tokens: {tokens:?}");
    let ast = parse_expr(&tokens, true)?;
    println!("Ast: {ast:?}");
    Ok(evaluate(&ast)?)
}
