#![recursion_limit = "1024"]
#![forbid(unsafe_code)]

mod analyzer;
mod compiler;
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
use compiler::{CompilePython, Language};
use environment::Scope;
use error::{Error, Throw};
use interpreter::Interpret;
use lox_lib::LoxType;
use nodes::Program;
use parse::Parser;
// use print_ast::{Print, PrintAST, PrintRPN};
use scanner::Scanner;
use std::{
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

fn get_ast(file_name: &str, program: &str, environment: &mut Scope) -> Result<Program, Vec<Error>> {
    // println!("\x1b[38;2;0;0;255mscanning...\x1b[0m");

    let mut scanner = Scanner::new(file_name, program.to_string());
    let tokens = scanner.scan_tokens()?;

    // println!("\x1b[38;2;0;0;255mparsing...\x1b[0m");

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // println!("\x1b[38;2;0;0;255manalyzing...\x1b[0m");

    ast.analyse(&None, false, environment)?;

    Ok(ast)
}

fn compile_program(
    file_name: &str,
    program: &str,
    environment: &mut Scope,
    language: Language,
) -> Result<String, Vec<Error>> {
    let ast = get_ast(file_name, program, environment)?;

    // println!("\x1b[38;2;0;0;255compiling...\x1b[0m\n");

    Ok(match language {
        Language::Python => ast.compile_python(0),
    })
}

fn run_program(
    file_name: &str,
    program: &str,
    environment: &mut Scope,
) -> Result<Option<LoxType>, Vec<Error>> {
    let ast = get_ast(file_name, program, environment)?;

    // println!("\x1b[38;2;0;0;255mrunning...\x1b[0m\n");

    let result = match ast.interpret(environment) {
        Ok(ok) => ok,
        Err(err) => return Err(vec![err]),
    };

    // println!("\n\x1b[38;2;0;0;255mdone!\x1b[0m\n");

    Ok(result)
}

fn compile_file(path: &str, language: Language) {
    let contents = fs::read_to_string(path).expect("Failed to read from file");
    match compile_program(path, &contents, &mut Scope::new(), language) {
        Ok(code) => println!("{code}"),
        Err(e) => {
            for err in e {
                err.throw();
            }
            exit(64);
        }
    }
}

fn run_file(path: &str) {
    let contents = fs::read_to_string(path).expect("Failed to read from file");
    if let Err(e) = run_program(path, &contents, &mut Scope::new()) {
        for err in e {
            err.throw();
        }
        exit(64)
    };
}

fn run_prompt() {
    let mut input = input!("> ");
    let mut environment = Scope::new();
    while input != "exit" {
        match run_program("stdin", &input, &mut environment) {
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

fn compile_prompt(language: Language) {
    let mut input = input!("> ");
    let mut environment = Scope::new();
    while input != "exit" {
        match compile_program("stdin", &input, &mut environment, language) {
            Ok(code) => println!("{code}"),
            Err(e) => {
                for err in e {
                    err.throw();
                }
            }
        }
        input = input!("> ");
    }
}

/// Compile and interpret lox programs
#[derive(Debug, clap::Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// path of the lox file
    path: Option<String>,
    /// language to compile to
    #[arg(short = 'c', long = "compile")]
    language: Option<Language>,
}

fn main() {
    let args = <Args as clap::Parser>::parse();
    match (args.path, args.language) {
        (None, None) => run_prompt(),
        (None, Some(language)) => compile_prompt(language),
        (Some(file_name), None) => run_file(&file_name),
        (Some(file_name), Some(language)) => compile_file(&file_name, language),
    }
}
