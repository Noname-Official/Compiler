use clap::ValueEnum;
use strum_macros::{EnumIter, EnumMessage};

use crate::{
    lox_lib::LoxType,
    nodes::{
        ArgDecl, Arguments, Assignment, Block, Comparison, ConstDecl, Equality, ExprStmt,
        Expression, Factor, ForStmt, FuncCall, FuncCall2, FuncDecl, IfStmt, LogicAnd, LogicOr,
        Primary, PrintStmt, Program, Statement, Term, Unary, VarDecl, WhileStmt,
    },
    token::token_type::{KeyWord, Literal, OneChar, OneTwoChar, Special, TokenType},
};

#[derive(Debug, EnumIter, EnumMessage, Clone, Copy, ValueEnum)]
#[strum(serialize_all = "lowercase")]
pub enum Language {
    Python,
}

pub trait CompilePython {
    fn compile_python(&self, indent: usize) -> String;
}

impl CompilePython for Program {
    fn compile_python(&self, indent: usize) -> String {
        let Program::Program(_, (stmts, _)) = self;
        let mut res = String::new();
        for (stmt,) in stmts {
            res += &("\t".repeat(indent) + &stmt.compile_python(indent) + "\n");
        }
        res
    }
}

impl CompilePython for Statement {
    fn compile_python(&self, indent: usize) -> String {
        match self {
            Statement::ExprStmt(_, (expr,)) => expr.compile_python(indent),
            Statement::PrintStmt(_, (stmt,)) => stmt.compile_python(indent),
            Statement::VarDecl(_, (stmt,)) => stmt.compile_python(indent),
            Statement::FuncDecl(_, (_, name, decl)) => {
                String::from("def ") + &name.lexeme + &decl.compile_python(indent)
            }
            Statement::ClassDecl(_, (_, name, _, _fns, _)) => {
                let mut _res = String::from("class ") + &name.lexeme;
                todo!()
            }
            Statement::ConstDecl(_, (stmt,)) => stmt.compile_python(indent),
            Statement::Block(_, (stmt,)) => stmt.compile_python(indent),
            Statement::IfStmt(_, (stmt,)) => stmt.compile_python(indent),
            Statement::WhileStmt(_, (stmt,)) => stmt.compile_python(indent),
            Statement::Break(_, (_, _)) => "break".into(),
            Statement::Continue(_, (_, _)) => "continue".into(),
            Statement::Return(_, (_, expr, _)) => {
                String::from("return")
                    + &match expr {
                        Some((expr,)) => String::from(" ") + &expr.compile_python(indent),
                        None => String::new(),
                    }
            }
            Statement::ForStmt(_, (stmt,)) => stmt.compile_python(indent),
        }
    }
}

impl CompilePython for ExprStmt {
    fn compile_python(&self, indent: usize) -> String {
        let ExprStmt::ExprStmt(_, (expr, _)) = self;
        expr.compile_python(indent)
    }
}

impl CompilePython for PrintStmt {
    fn compile_python(&self, indent: usize) -> String {
        let PrintStmt::PrintStmt(_, (_, expr, _)) = self;
        String::from("print(") + &expr.compile_python(indent) + ")"
    }
}

impl CompilePython for VarDecl {
    fn compile_python(&self, indent: usize) -> String {
        let VarDecl::VarDecl(_, (_, name, expr, _)) = self;
        name.lexeme.clone()
            + " = "
            + &match expr {
                Some((_, expr)) => expr.compile_python(indent),
                None => "None".into(),
            }
    }
}

impl CompilePython for FuncDecl {
    fn compile_python(&self, indent: usize) -> String {
        let FuncDecl::FuncDecl(_, (return_type, _, args, _, block)) = self;
        String::from("(")
            + &args.compile_python(indent)
            + ") -> "
            + &LoxType::try_from(return_type).unwrap().get_python_type()
            + ":\n"
            + &"\t".repeat(indent + 1)
            + &block.compile_python(indent + 1)
    }
}

impl CompilePython for ArgDecl {
    fn compile_python(&self, _indent: usize) -> String {
        let ArgDecl::ArgDecl(_, (args,)) = self;
        match args {
            Some((name, _, arg_type, rest)) => {
                name.lexeme.clone()
                    + ": "
                    + &LoxType::try_from(arg_type).unwrap().get_python_type()
                    + &rest
                        .iter()
                        .fold(String::new(), |acc, (_, name, _, arg_type)| {
                            acc + &name.lexeme
                                + ": "
                                + &LoxType::try_from(arg_type).unwrap().get_python_type()
                        })
            }
            None => String::new(),
        }
    }
}

impl CompilePython for ConstDecl {
    fn compile_python(&self, indent: usize) -> String {
        let ConstDecl::ConstDecl(_, (_, name, _, expr, _)) = self;
        name.lexeme.clone() + " = " + &expr.compile_python(indent)
    }
}

impl CompilePython for Block {
    fn compile_python(&self, indent: usize) -> String {
        let Block::Block(_, (_, stmts, _)) = self;
        let mut stmts = stmts.iter();
        let mut res = match stmts.next() {
            Some((stmt,)) => stmt.compile_python(indent),
            None => return String::from("pass"),
        };
        for (stmt,) in stmts {
            res += &(String::from("\n") + &"\t".repeat(indent) + &stmt.compile_python(indent));
        }
        res
    }
}

impl CompilePython for IfStmt {
    fn compile_python(&self, indent: usize) -> String {
        let IfStmt::IfStmt(_, (_, _, expr, _, body, elifs, else_block)) = self;
        String::from("if ")
            + &expr.compile_python(indent)
            + ":\n"
            + &"\t".repeat(indent + 1)
            + &body.compile_python(indent + 1)
            + &elifs
                .iter()
                .fold(String::new(), |acc, (_, _, expr, _, body)| {
                    acc + "\n"
                        + &"\t".repeat(indent)
                        + "elif "
                        + &expr.compile_python(indent)
                        + ":\n"
                        + &"\t".repeat(indent + 1)
                        + &body.compile_python(indent + 1)
                })
            + &match else_block {
                Some((_, block)) => {
                    String::from("\n")
                        + &"\t".repeat(indent)
                        + "else:\n"
                        + &"\t".repeat(indent + 1)
                        + &block.compile_python(indent + 1)
                }
                None => String::new(),
            }
    }
}

impl CompilePython for WhileStmt {
    fn compile_python(&self, indent: usize) -> String {
        let WhileStmt::WhileStmt(_, (_, _, expr, _, body)) = self;
        String::from("while ")
            + &expr.compile_python(indent)
            + ":\n"
            + &"\t".repeat(indent + 1)
            + &body.compile_python(indent + 1)
    }
}

impl CompilePython for Expression {
    fn compile_python(&self, indent: usize) -> String {
        match self {
            Expression::Expression(_, (logic_or,)) => logic_or.compile_python(indent),
            Expression::Assignment(_, (assignment,)) => assignment.compile_python(indent),
        }
    }
}

impl CompilePython for Assignment {
    fn compile_python(&self, indent: usize) -> String {
        let Assignment::Assignment(_, (name, _, value)) = self;
        name.lexeme.clone() + " = " + &value.compile_python(indent)
    }
}

impl CompilePython for LogicOr {
    fn compile_python(&self, indent: usize) -> String {
        let LogicOr::LogicOr(_, (first, rest)) = self;
        first.compile_python(indent)
            + &rest.iter().fold(String::new(), |acc, (_, operand)| {
                acc + " or " + &operand.compile_python(indent)
            })
    }
}

impl CompilePython for LogicAnd {
    fn compile_python(&self, indent: usize) -> String {
        let LogicAnd::LogicAnd(_, (first, rest)) = self;
        first.compile_python(indent)
            + &rest.iter().fold(String::new(), |acc, (_, operand)| {
                acc + " and " + &operand.compile_python(indent)
            })
    }
}

impl CompilePython for Equality {
    fn compile_python(&self, indent: usize) -> String {
        let Equality::Equality(_, (first, rest)) = self;
        first.compile_python(indent)
            + &rest.iter().fold(String::new(), |acc, (operator, operand)| {
                acc + match operator.token_type {
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => " == ",
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => " != ",
                    _ => unreachable!(),
                } + &operand.compile_python(indent)
            })
    }
}

impl CompilePython for Comparison {
    fn compile_python(&self, indent: usize) -> String {
        let Comparison::Comparison(_, (first, rest)) = self;
        first.compile_python(indent)
            + &rest.iter().fold(String::new(), |acc, (operator, operand)| {
                acc + match operator.token_type {
                    TokenType::OneTwoChar(OneTwoChar::LessEqual) => " <= ",
                    TokenType::OneTwoChar(OneTwoChar::Less) => " < ",
                    TokenType::OneTwoChar(OneTwoChar::GreaterEqual) => " >= ",
                    TokenType::OneTwoChar(OneTwoChar::Greater) => " > ",
                    _ => unreachable!(),
                } + &operand.compile_python(indent)
            })
    }
}

impl CompilePython for Term {
    fn compile_python(&self, indent: usize) -> String {
        let Term::Term(_, (first, rest)) = self;
        first.compile_python(indent)
            + &rest.iter().fold(String::new(), |acc, (operator, operand)| {
                acc + match operator.token_type {
                    TokenType::OneChar(OneChar::Plus) => " + ",
                    TokenType::OneChar(OneChar::Minus) => " - ",
                    _ => unreachable!(),
                } + &operand.compile_python(indent)
            })
    }
}

impl CompilePython for Factor {
    fn compile_python(&self, indent: usize) -> String {
        let Factor::Factor(_, (first, rest)) = self;
        first.compile_python(indent)
            + &rest.iter().fold(String::new(), |acc, (operator, operand)| {
                acc + match operator.token_type {
                    TokenType::OneChar(OneChar::Star) => " == ",
                    TokenType::Special(Special::Slash) => " / ",
                    _ => unreachable!(),
                } + &operand.compile_python(indent)
            })
    }
}

impl CompilePython for Unary {
    fn compile_python(&self, indent: usize) -> String {
        let Unary::Unary(_, (operators, operand)) = self;
        operators.iter().fold(String::new(), |acc, (operator,)| {
            acc + match operator.token_type {
                TokenType::OneChar(OneChar::Plus) => "+",
                TokenType::OneChar(OneChar::Minus) => "-",
                TokenType::OneTwoChar(OneTwoChar::Bang) => "not ",
                _ => unreachable!(),
            }
        }) + &operand.compile_python(indent)
    }
}

impl CompilePython for FuncCall {
    fn compile_python(&self, indent: usize) -> String {
        let FuncCall::FuncCall(_, (expr, calls)) = self;
        expr.compile_python(indent)
            + &calls.iter().fold(String::new(), |acc, (call,)| {
                acc + &call.compile_python(indent)
            })
    }
}

impl CompilePython for FuncCall2 {
    fn compile_python(&self, indent: usize) -> String {
        match self {
            FuncCall2::Call(_, (_, args, _)) => {
                String::from("(")
                    + &match args {
                        Some((args,)) => args.compile_python(indent),
                        None => String::new(),
                    }
                    + ")"
            }
            FuncCall2::Property(_, (_, name)) => String::from(".") + &name.lexeme,
        }
    }
}

impl CompilePython for Arguments {
    fn compile_python(&self, indent: usize) -> String {
        let Arguments::Arguments(_, (first, rest)) = self;
        first.compile_python(indent)
            + &rest.iter().fold(String::new(), |acc, (_, arg)| {
                acc + ", " + &arg.compile_python(indent)
            })
    }
}

impl CompilePython for Primary {
    fn compile_python(&self, indent: usize) -> String {
        match self {
            Primary::Literal(_, (token,)) => match &token.token_type {
                TokenType::Literal(Literal::Identifier(value)) => value.clone(),
                TokenType::Literal(Literal::String(value)) => String::from("\"") + &value + "\"",
                TokenType::Literal(Literal::Integer(value)) => value.to_string(),
                TokenType::Literal(Literal::Float(value)) => value.to_string(),
                TokenType::KeyWord(KeyWord::True) => "True".into(),
                TokenType::KeyWord(KeyWord::False) => "False".into(),
                TokenType::KeyWord(KeyWord::Null) => "None".into(),
                _ => unreachable!(),
            },
            Primary::Func(_, (_, _decl)) => todo!(),
            Primary::Parenthesized(_, (_, expr, _)) => {
                String::from("(") + &expr.compile_python(indent) + ")"
            }
        }
    }
}

impl CompilePython for ForStmt {
    fn compile_python(&self, indent: usize) -> String {
        match self {
            ForStmt::VarDecl(_, (_, _, var_decl, condition, _, increment, _, block)) => {
                var_decl.compile_python(indent)
                    + "\n"
                    + &"\t".repeat(indent)
                    + "while "
                    + &match condition {
                        Some((condition,)) => condition.compile_python(indent),
                        None => "True".into(),
                    }
                    + ":\n"
                    + &"\t".repeat(indent + 1)
                    + &block.compile_python(indent + 1)
                    + &match increment {
                        Some((increment,)) => {
                            String::from("\n")
                                + &"\t".repeat(indent + 1)
                                + &increment.compile_python(indent + 1)
                        }
                        None => String::new(),
                    }
            }
            ForStmt::Expression(_, (_, _, start_expr, condition, _, increment, _, block)) => {
                start_expr.compile_python(indent)
                    + "\n"
                    + &"\t".repeat(indent)
                    + "while "
                    + &match condition {
                        Some((condition,)) => condition.compile_python(indent),
                        None => "True".into(),
                    }
                    + ":\n"
                    + &"\t".repeat(indent + 1)
                    + &block.compile_python(indent + 1)
                    + &match increment {
                        Some((increment,)) => {
                            String::from("\n")
                                + &"\t".repeat(indent + 1)
                                + &increment.compile_python(indent + 1)
                        }
                        None => String::new(),
                    }
            }
            ForStmt::None(_, (_, _, _, condition, _, increment, _, block)) => {
                String::from("while ")
                    + &match condition {
                        Some((condition,)) => condition.compile_python(indent),
                        None => "True".into(),
                    }
                    + ":\n"
                    + &"\t".repeat(indent + 1)
                    + &block.compile_python(indent + 1)
                    + &match increment {
                        Some((increment,)) => {
                            String::from("\n")
                                + &"\t".repeat(indent + 1)
                                + &increment.compile_python(indent + 1)
                        }
                        None => String::new(),
                    }
            }
        }
    }
}
