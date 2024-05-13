use crate::{lox_lib::LoxType, token::Token};
use std::fmt::Display;

macro_rules! errors {
    ($($name:ident $(($($args:tt)*))? $(:$super_type:ident)?),* $(,)?) => {
        #[derive(Clone, Debug)]
        pub enum ErrorType {
            $(
                $name $(($($args)*))?
            ),*
        }

        impl Display for ErrorType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        ErrorType::$name $( ( errors!(@underscores $($args)*) ) )? => write!(f, errors!(@to_string $name $(($super_type))?)),
                    )*
                }
            }
        }
    };
    (@underscores , $($args:tt)*) => {
        _, errors!(@underscores $($args)*)
    };
    (@underscores $tt:tt $($args:tt)*) => {
        errors!(@underscores $($args)*)
    };
    (@underscores) => {
        _
    };
    (@to_string $name:ident ($super_type:ident)) => {
        stringify!($super_type)
    };
    (@to_string $name:ident) => {
        stringify!($name)
    };
}

#[derive(Clone, Debug)]
pub struct Error {
    super_type: ErrorType,
    file: String,
    first_line: usize,
    last_line: usize,
    begin: usize,
    end: usize,
    lines: Vec<(usize, String)>,
    msg: String,
    error_type: &'static str,
}

impl Error {
    pub fn new(
        super_type: ErrorType,
        tokens: &[Token],
        msg: &str,
        error_type: &'static str,
    ) -> Self {
        let mut lines = vec![];
        let mut last_line = tokens[0].line + 1;
        for token in tokens {
            if token.line != last_line {
                last_line = token.line;
                lines.push((last_line, token.line_contents.clone()));
            }
        }

        Self {
            super_type,
            file: tokens[0].file.clone(),
            first_line: tokens[0].line,
            last_line: tokens[tokens.len() - 1].line,
            begin: tokens[0].begin,
            end: tokens.last().unwrap().end,
            lines,
            msg: msg.to_string(),
            error_type,
        }
    }

    pub fn get_type(&self) -> &ErrorType {
        &self.super_type
    }
}

impl Throw for Error {
    fn throw(&self) {
        let spaces = String::from(" ").repeat(self.last_line.to_string().len());
        let space_count = spaces.len();

        eprintln!("\x1b[38;2;255;0;0m{}\x1b[0m", self.super_type);

        eprintln!(
            "\x1b[38;2;255;0;0;1m{}Error\x1b[39m: {}\x1b[0m",
            self.error_type, self.msg
        );

        eprintln!(
            "\x1b[38;2;49;222;219m  -->\x1b[0m {}:{}:{}",
            self.file, self.first_line, self.begin
        );

        eprintln!("\x1b[38;2;49;222;219m{spaces} |\x1b[0m");

        if self.first_line == self.last_line {
            eprintln!(
                "\x1b[38;2;49;222;219m{} |\x1b[0m {}",
                self.first_line, self.lines[0].1
            );
            eprintln!(
                "\x1b[38;2;49;222;219m{spaces} |{}\x1b[38;2;255;0;0m{}\x1b[0m",
                String::from(" ").repeat(self.begin),
                String::from("^").repeat(self.end - self.begin + 1)
            );
        } else {
            eprintln!(
                "\x1b[38;2;49;222;219m{}{} |\x1b[0m   {}",
                self.first_line,
                String::from(" ").repeat(space_count - self.first_line.to_string().len()),
                self.lines[0].1
            );
            eprintln!(
                "\x1b[38;2;49;222;219m{spaces} |\x1b[38;2;255;0;0m  {}^\x1b[0m",
                String::from("_").repeat(self.begin)
            );
            for (line_number, line) in self.lines.iter().skip(1) {
                eprintln!(
                    "\x1b[38;2;49;222;219m{}{} |\x1b[38;2;255;0;0m |\x1b[0m {line}",
                    line_number,
                    String::from(" ").repeat(space_count - line_number.to_string().len())
                );
            }
            eprintln!(
                "\x1b[38;2;49;222;219m{spaces} |\x1b[38;2;255;0;0m |{}^\x1b[0m",
                String::from("_").repeat(self.begin)
            );
        }
    }
}

pub trait Throw {
    fn throw(&self);
}

errors!(
    Scan,
    Parse,
    RunTime,
    Break: RunTime,
    Continue: RunTime,
    Return(Option<LoxType>): RunTime,
    Analyze,
);
