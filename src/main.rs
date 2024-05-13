#![recursion_limit = "1024"]
#![forbid(unsafe_code)]

mod analyzer;
mod environment;
mod error;
mod interpreter;
mod lox_lib;
mod nodes;
mod parse;
mod print_ast;
mod scanner;
mod token;

use analyzer::Analyse;
use environment::Scope;
use error::{Error, Throw};
use interpreter::Interpret;
use lox_lib::LoxType;
use parse::Parser;
// use print_ast::{Print, PrintAST, PrintRPN};
use scanner::Scanner;
use std::{
    env::args,
    fs,
    io::{stdin, stdout, Write},
    process::exit,
};

#[macro_export]
macro_rules! input {
    ($($expr:expr)?) => {
        {
            let mut input = String::new();
            $(print!($expr);)?
            stdout().flush().expect("Failed to flush output buffer");
            stdin()
                .read_line(&mut input)
                .expect("Failed to read user input");
            input.trim().to_string()
        }
    };
}

fn run<'a>(
    file_name: &'a str,
    program: &'a str,
    environment: &'a mut Scope,
) -> Result<Option<LoxType>, Vec<Error>> {
    // println!("\x1b[38;2;0;0;255mscanning...\x1b[0m");

    let mut scanner = Scanner::new(file_name, program.to_string());
    let tokens = scanner.scan_tokens()?;

    // println!("\x1b[38;2;0;0;255mparsing...\x1b[0m");

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // println!("\x1b[38;2;0;0;255manalyzing...\x1b[0m");

    ast.analyse(&None, false, environment)?;

    // println!("\x1b[38;2;0;0;255mrunning...\x1b[0m\n");

    let result = match ast.interpret(environment) {
        Ok(ok) => ok,
        Err(err) => return Err(vec![err]),
    };

    // println!("\n\x1b[38;2;0;0;255mdone!\x1b[0m\n");

    Ok(result)
}

fn run_file(path: &String) {
    let contents = fs::read_to_string(path).expect("Failed to read from file");
    if let Err(e) = run(path, &contents, &mut Scope::new()) {
        for err in e {
            err.throw();
        }
        exit(65)
    };
}

fn run_prompt() {
    let mut input = input!("> ");
    let mut environment = Scope::new();
    while input != "exit" {
        match run(&String::from("stdin"), &input, &mut environment) {
            Ok(res) => {
                if let Some(res) = res {
                    println!("{res}")
                }
            }
            Err(e) => {
                for err in e {
                    err.throw();
                }
            }
        }
        input = input!("> ");
    }
}

fn main() {
    let mut args = args();
    match args.len() {
        3.. => {
            eprintln!("Usage: {} [path: Path]", args.next().unwrap());
            exit(64);
        }
        2 => {
            run_file(&args.nth(1).unwrap());
        }
        _ => loop {
            let input = input!("Run interactive prompt or from file (prompt / file): ");
            if input == "prompt" {
                run_prompt();
            } else if input == "file" {
                run_file(&input!("File path: ").trim().to_string());
            } else if input != "exit" {
                eprintln!("Invalid input, please try again");
                continue;
            }
            break;
        },
    }
}
