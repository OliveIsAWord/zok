#![forbid(unsafe_code, missing_debug_implementations)]
#![feature(box_patterns, box_syntax, pattern)]

mod executor;
mod lexer;
mod parser;

use executor::evaluate;
use lexer::lex;
use parser::parse_expr;
use std::io::{self, Write};

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut source = String::new();
        io::stdin().read_line(&mut source).unwrap();
        eval(&source);
    }
}

fn eval(source: &str) {
    //let source = "(3 4 (+ 5.0) + 6) + (+ 2 + 3)";
    let tokens = match lex(source) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("lexing error: {e:?}");
            return;
        }
    };
    println!("Tokens: {:?}", tokens);
    let ast = match parse_expr(&tokens, true) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("parsing error: {e:?}");
            return;
        }
    };
    println!("Ast: {:?}", ast);
    let output = match evaluate(&ast) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("parsing error: {e:?}");
            return;
        }
    };
    println!("Output: {}", output);
}
