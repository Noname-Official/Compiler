use crate::nodes::{
    ArgDecl, Arguments, Assignment, Block, Comparison, ConstDecl, Equality, ExprStmt, Expression,
    Factor, ForStmt, FuncCall, FuncCall2, FuncDecl, IfStmt, LogicAnd, LogicOr, Primary, PrintStmt,
    Program, Statement, Term, Unary, VarDecl, WhileStmt,
};

pub trait PrintAST {
    fn print_ast(&self, indent: usize) -> String;
}

const INDENT: &str = "    ";

impl PrintAST for Program {
    fn print_ast(&self, indent: usize) -> String {
        let Program::Program(_, (statements, _)) = self;
        let mut result = String::from("Program(\n");
        for (stmt,) in statements {
            result += &stmt.print_ast(indent + 1);
            result.push('\n');
        }
        result.push_str(&INDENT.repeat(indent));
        result.push(')');
        INDENT.repeat(indent) + &result
    }
}

impl PrintAST for Statement {
    fn print_ast(&self, indent: usize) -> String {
        INDENT.repeat(indent)
            + "Statement(\n"
            + &match self {
                Statement::ExprStmt(_, (expr,)) => expr.print_ast(indent + 1),
                Statement::PrintStmt(_, (printstmt,)) => printstmt.print_ast(indent + 1),
                Statement::VarDecl(_, (var_decl,)) => var_decl.print_ast(indent + 1),
                Statement::IfStmt(_, (if_stmt,)) => if_stmt.print_ast(indent + 1),
                Statement::WhileStmt(_, (while_stmt,)) => while_stmt.print_ast(indent + 1),
                Statement::ForStmt(_, (for_stmt,)) => for_stmt.print_ast(indent + 1),
                Statement::Break(_, (break_kw, _)) => break_kw.lexeme.clone(),
                Statement::Continue(_, (continue_kw, _)) => continue_kw.lexeme.clone(),
                Statement::Return(_, (return_kw, expr, _)) => {
                    return_kw.lexeme.clone()
                        + &match expr {
                            Some((expr,)) => String::from("\n") + &expr.print_ast(indent + 1),
                            None => String::new(),
                        }
                }
                Statement::ConstDecl(_, (const_decl,)) => const_decl.print_ast(indent + 1),
                Statement::Block(_, (block,)) => block.print_ast(indent + 1),
                Statement::FuncDecl(_, (_, name, func_decl)) => {
                    INDENT.repeat(indent + 1)
                        + "func "
                        + &name.lexeme
                        + "\n"
                        + &func_decl.print_ast(indent + 1)
                }
                Statement::ClassDecl(_, (_, name, _, functions, _)) => {
                    INDENT.repeat(indent + 1)
                        + "class "
                        + &name.lexeme
                        + &functions.iter().fold(String::new(), |acc, (_, function)| {
                            acc + "\n" + &function.print_ast(indent + 1)
                        })
                }
            }
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for Block {
    fn print_ast(&self, indent: usize) -> String {
        let Block::Block(_, (left, stmts, right)) = self;
        let mut res = INDENT.repeat(indent) + &left.lexeme.to_string() + "\n";
        for (stmt,) in stmts {
            res.push_str(&stmt.print_ast(indent + 1));
            res += "\n";
        }
        res.push_str(&INDENT.repeat(indent));
        res.push_str(&right.lexeme.to_string());
        res
    }
}

impl PrintAST for ConstDecl {
    fn print_ast(&self, indent: usize) -> String {
        let ConstDecl::ConstDecl(_, (_, name, _, expr, _)) = self;
        INDENT.repeat(indent)
            + "ConstDecl(\n"
            + &INDENT.repeat(indent + 1)
            + &name.lexeme.to_string()
            + "\n"
            + &expr.print_ast(indent + 1)
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for ForStmt {
    fn print_ast(&self, indent: usize) -> String {
        INDENT.repeat(indent)
            + "ForStmt(\n"
            + &match self {
                ForStmt::VarDecl(_, (_, _, var_decl, cond, _, repeated, _, stmt)) => {
                    var_decl.print_ast(indent + 1)
                        + "\n"
                        + &match cond {
                            Some((cond,)) => cond.print_ast(indent + 1),
                            None => String::from(";"),
                        }
                        + "\n"
                        + &match repeated {
                            Some((repeated,)) => repeated.print_ast(indent + 1),
                            None => String::from(";"),
                        }
                        + "\n"
                        + &stmt.print_ast(indent + 1)
                }
                ForStmt::Expression(_, (_, _, expr, cond, _, repeated, _, stmt)) => {
                    expr.print_ast(indent + 1)
                        + "\n"
                        + &match cond {
                            Some((cond,)) => cond.print_ast(indent + 1),
                            None => String::from("';"),
                        }
                        + "\n"
                        + &match repeated {
                            Some((repeated,)) => repeated.print_ast(indent + 1),
                            None => String::from(";"),
                        }
                        + "\n"
                        + &stmt.print_ast(indent + 1)
                }
                ForStmt::None(_, (_, _, semi, cond, _, repeated, _, stmt)) => {
                    semi.lexeme.clone()
                        + "\n"
                        + &match cond {
                            Some((cond,)) => cond.print_ast(indent + 1),
                            None => String::from(";"),
                        }
                        + "\n"
                        + &match repeated {
                            Some((repeated,)) => repeated.print_ast(indent + 1),
                            None => String::from(";"),
                        }
                        + "\n"
                        + &stmt.print_ast(indent + 1)
                }
            }
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for WhileStmt {
    fn print_ast(&self, indent: usize) -> String {
        let WhileStmt::WhileStmt(_, (_, _, expr, _, stmt)) = self;
        INDENT.repeat(indent)
            + "WhileStmt(\n"
            + &expr.print_ast(indent + 1)
            + "\n"
            + &stmt.print_ast(indent + 1)
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for IfStmt {
    fn print_ast(&self, indent: usize) -> String {
        let IfStmt::IfStmt(_, (if_kw, _, expr, _, stmt, elifs, else_)) = self;
        let mut res = INDENT.repeat(indent)
            + &if_kw.lexeme.to_string()
            + "\n"
            + &expr.print_ast(indent + 1)
            + "\n"
            + &stmt.print_ast(indent + 1);
        for (elif, _, expr, _, stmt) in elifs {
            res += &(String::from("\n")
                + &INDENT.repeat(indent)
                + &elif.lexeme.to_string()
                + "\n"
                + &expr.print_ast(indent + 1)
                + "\n"
                + &stmt.print_ast(indent + 1));
        }
        if let Some((else_kw, stmt)) = else_ {
            res += &(String::from("\n")
                + &INDENT.repeat(indent)
                + &else_kw.lexeme.to_string()
                + "\n"
                + &stmt.print_ast(indent + 1));
        }
        res
    }
}

impl PrintAST for ExprStmt {
    fn print_ast(&self, indent: usize) -> String {
        let ExprStmt::ExprStmt(_, (expr, _)) = self;
        INDENT.repeat(indent)
            + "exprstmt(\n"
            + &expr.print_ast(indent + 1)
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for PrintStmt {
    fn print_ast(&self, indent: usize) -> String {
        let PrintStmt::PrintStmt(_, (_, expr, _)) = self;
        INDENT.repeat(indent)
            + "printstmt(\n"
            + &expr.print_ast(indent + 1)
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for VarDecl {
    fn print_ast(&self, indent: usize) -> String {
        let VarDecl::VarDecl(_, (_, name, expr, _)) = self;
        let expr = match expr {
            Some((_, expr)) => String::from("\n") + &expr.print_ast(indent + 1),
            None => String::new(),
        };
        INDENT.repeat(indent)
            + "VarDecl(\n"
            + &INDENT.repeat(indent + 1)
            + &name.lexeme.to_string()
            + &expr
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for Expression {
    fn print_ast(&self, indent: usize) -> String {
        let res = match self {
            Expression::Expression(_, (equality,)) => equality.print_ast(indent + 1),
            Expression::Assignment(_, (assignment,)) => assignment.print_ast(indent + 1),
        };
        INDENT.repeat(indent) + "expression(\n" + &res + "\n" + &INDENT.repeat(indent) + ")"
    }
}

impl PrintAST for LogicOr {
    fn print_ast(&self, indent: usize) -> String {
        let LogicOr::LogicOr(_, (left, args)) = self;
        let mut result = INDENT.repeat(indent) + "LogicOr(\n" + &left.print_ast(indent + 1);
        for (or, right) in args {
            result += &(String::from("\n")
                + &INDENT.repeat(indent + 1)
                + &or.lexeme.to_string()
                + "\n"
                + &right.print_ast(indent + 1));
        }
        result += &(String::from("\n") + &INDENT.repeat(indent) + ")");
        result
    }
}

impl PrintAST for LogicAnd {
    fn print_ast(&self, indent: usize) -> String {
        let LogicAnd::LogicAnd(_, (left, args)) = self;
        let mut result = INDENT.repeat(indent) + "LogicAnd(\n" + &left.print_ast(indent + 1);
        for (and, right) in args {
            result += &(String::from("\n")
                + &INDENT.repeat(indent + 1)
                + &and.lexeme.to_string()
                + "\n"
                + &right.print_ast(indent + 1));
        }
        result += &(String::from("\n") + &INDENT.repeat(indent) + ")");
        result
    }
}

impl PrintAST for Assignment {
    fn print_ast(&self, indent: usize) -> String {
        let Assignment::Assignment(_, (name, _, expr)) = self;
        INDENT.repeat(indent)
            + "assignment(\n"
            + &INDENT.repeat(indent + 1)
            + &name.lexeme.to_string()
            + "\n"
            + &expr.print_ast(indent + 1)
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for Equality {
    fn print_ast(&self, indent: usize) -> String {
        let Equality::Equality(_, (comparison, args)) = self;
        let mut result = String::from("Equality(\n");
        result.push_str(&comparison.print_ast(indent + 1));
        for arg in args {
            result.push('\n');
            result.push_str(&INDENT.repeat(indent + 1));
            result.push_str(&arg.0.lexeme.to_string());
            result.push('\n');
            result.push_str(&arg.1.print_ast(indent + 1));
        }
        result.push('\n');
        result.push_str(&INDENT.repeat(indent));
        result.push(')');
        INDENT.repeat(indent) + &result
    }
}

impl PrintAST for Comparison {
    fn print_ast(&self, indent: usize) -> String {
        let Comparison::Comparison(_, (term, args)) = self;
        let mut result = String::from("Comparison(\n");
        result.push_str(&term.print_ast(indent + 1));
        for arg in args {
            result.push('\n');
            result.push_str(&INDENT.repeat(indent + 1));
            result.push_str(&arg.0.lexeme.to_string());
            result.push('\n');
            result.push_str(&arg.1.print_ast(indent + 1));
        }
        result.push('\n');
        result.push_str(&INDENT.repeat(indent));
        result.push(')');
        INDENT.repeat(indent) + &result
    }
}

impl PrintAST for Term {
    fn print_ast(&self, indent: usize) -> String {
        let Term::Term(_, (factor, args)) = self;
        let mut result = String::from("Term(\n");
        result.push_str(&factor.print_ast(indent + 1));
        for arg in args {
            result.push('\n');
            result.push_str(&INDENT.repeat(indent + 1));
            result.push_str(&arg.0.lexeme.to_string());
            result.push('\n');
            result.push_str(&arg.1.print_ast(indent + 1));
        }
        result.push('\n');
        result.push_str(&INDENT.repeat(indent));
        result.push(')');
        INDENT.repeat(indent) + &result
    }
}

impl PrintAST for Factor {
    fn print_ast(&self, indent: usize) -> String {
        let Factor::Factor(_, (unary, args)) = self;
        let mut result = String::from("Factor(\n");
        result.push_str(&unary.print_ast(indent + 1));
        for arg in args {
            result.push('\n');
            result.push_str(&INDENT.repeat(indent + 1));
            result.push_str(&arg.0.lexeme.to_string());
            result.push('\n');
            result.push_str(&arg.1.print_ast(indent + 1));
        }
        result.push('\n');
        result.push_str(&INDENT.repeat(indent));
        result.push(')');
        INDENT.repeat(indent) + &result
    }
}

impl PrintAST for Unary {
    fn print_ast(&self, indent: usize) -> String {
        let Unary::Unary(_, (args, func_call)) = self;
        let mut result = String::from("Unary(\n") + &INDENT.repeat(indent + 1);
        for arg in args {
            result.push_str(&arg.0.lexeme.to_string());
        }
        result.push('\n');
        result.push_str(&func_call.print_ast(indent + 1));
        result.push('\n');
        result.push_str(&INDENT.repeat(indent));
        result.push(')');
        INDENT.repeat(indent) + &result
    }
}

impl PrintAST for FuncCall {
    fn print_ast(&self, indent: usize) -> String {
        let FuncCall::FuncCall(_, (primary, calls)) = self;
        INDENT.repeat(indent)
            + "FuncCall(\n"
            + &primary.print_ast(indent + 1)
            + &calls.iter().fold(String::new(), |state, (call,)| {
                state + "\n" + &INDENT.repeat(indent + 1) + &call.print_ast(indent + 1)
            })
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for FuncCall2 {
    fn print_ast(&self, indent: usize) -> String {
        match self {
            FuncCall2::Call(_, (left, args, right)) => {
                left.lexeme.clone()
                    + &match args {
                        Some((args,)) => {
                            String::from("\n")
                                + &args.print_ast(indent + 1)
                                + "\n"
                                + &INDENT.repeat(indent + 1)
                        }
                        None => String::new(),
                    }
                    + &right.lexeme
            }
            FuncCall2::Property(_, (dot, property)) => dot.lexeme.clone() + &property.lexeme,
        }
    }
}

impl PrintAST for Arguments {
    fn print_ast(&self, indent: usize) -> String {
        let Arguments::Arguments(_, (expr, exprs)) = self;
        INDENT.repeat(indent)
            + "Arguments(\n"
            + &expr.print_ast(indent + 1)
            + &exprs.iter().fold(String::new(), |state, (_, expr)| {
                state + "\n" + &expr.print_ast(indent + 1)
            })
    }
}

impl PrintAST for Primary {
    fn print_ast(&self, indent: usize) -> String {
        if let Primary::Func(_, (_, func)) = self {
            func.print_ast(indent)
        } else {
            INDENT.repeat(indent)
                + &match self {
                    Primary::Literal(_, (token,)) => token.lexeme.clone().to_string(),
                    Primary::Parenthesized(_, (_, expr, _)) => {
                        let mut result = String::from("(\n");
                        result.push_str(&expr.print_ast(indent + 1));
                        result.push('\n');
                        result.push_str(&INDENT.repeat(indent));
                        result.push(')');
                        result
                    }
                    Primary::Func(_, _) => unreachable!(),
                }
        }
    }
}

impl PrintAST for FuncDecl {
    fn print_ast(&self, indent: usize) -> String {
        let FuncDecl::FuncDecl(_, (return_type, _, params, _, block)) = self;
        INDENT.repeat(indent)
            + "FuncDecl(\n"
            + &INDENT.repeat(indent + 1)
            + &return_type.lexeme
            + "\n"
            + &params.print_ast(indent + 1)
            + "\n"
            + &block.print_ast(indent + 1)
            + "\n"
            + &INDENT.repeat(indent)
            + ")"
    }
}

impl PrintAST for ArgDecl {
    fn print_ast(&self, indent: usize) -> String {
        let ArgDecl::ArgDecl(_, (params,)) = self;
        INDENT.repeat(indent)
            + "ArgDecl("
            + &match params {
                Some((param, _, param_type, params)) => {
                    String::from("\n")
                        + &INDENT.repeat(indent + 1)
                        + &param.lexeme
                        + ": "
                        + &param_type.lexeme
                        + "\n"
                        + &INDENT.repeat(indent)
                        + &params
                            .iter()
                            .fold(String::new(), |state, (_, param, _, param_type)| {
                                state
                                    + INDENT
                                    + &param.lexeme
                                    + ": "
                                    + &param_type.lexeme
                                    + "\n"
                                    + &INDENT.repeat(indent)
                            })
                }
                None => String::new(),
            }
            + ")"
    }
}

pub trait PrintRPN {
    fn print_rpn(&self) -> String;
}

impl PrintRPN for Program {
    fn print_rpn(&self) -> String {
        let Program::Program(_, (stmts, _)) = self;
        let mut result = String::new();
        for (stmt,) in stmts {
            result.push_str(&stmt.print_rpn());
            result.push('\n');
        }
        result
    }
}

impl PrintRPN for Statement {
    fn print_rpn(&self) -> String {
        match self {
            Statement::ExprStmt(_, (expr,)) => expr.print_rpn(),
            Statement::PrintStmt(_, (expr,)) => expr.print_rpn(),
            Statement::VarDecl(_, (var_decl,)) => var_decl.print_rpn(),
            Statement::Block(_, (block,)) => block.print_rpn(),
            Statement::IfStmt(_, (if_stmt,)) => if_stmt.print_rpn(),
            Statement::WhileStmt(_, (while_stmt,)) => while_stmt.print_rpn(),
            Statement::ForStmt(_, (for_stmt,)) => for_stmt.print_rpn(),
            Statement::Break(_, (break_kw, _)) => break_kw.lexeme.clone(),
            Statement::ConstDecl(_, (const_decl,)) => const_decl.print_rpn(),
            Statement::Continue(_, (continue_kw, _)) => continue_kw.lexeme.clone(),
            Statement::Return(_, (return_kw, expr, _)) => {
                return_kw.lexeme.clone()
                    + " "
                    + &match expr {
                        Some((expr,)) => expr.print_rpn(),
                        None => String::new(),
                    }
            }
            Statement::FuncDecl(_, (func, name, func_decl)) => {
                func.lexeme.clone() + " " + &name.lexeme + " " + &func_decl.print_rpn()
            }
            Statement::ClassDecl(_, (func_kw, name, left_brace, funcs, right_brace)) => {
                func_kw.lexeme.clone()
                    + " "
                    + &name.lexeme
                    + " "
                    + &left_brace.lexeme
                    + &funcs.iter().fold(String::new(), |acc, (name, func)| {
                        acc + &name.lexeme + " " + &func.print_rpn()
                    })
                    + &right_brace.lexeme
            }
        }
    }
}

impl PrintRPN for Block {
    fn print_rpn(&self) -> String {
        let Block::Block(_, (_, stmts, _)) = self;
        let mut res = String::from("(\n");
        for (stmt,) in stmts {
            res.push_str(&stmt.print_rpn());
            res.push('\n');
        }
        res.push(')');
        res
    }
}

impl PrintRPN for ConstDecl {
    fn print_rpn(&self) -> String {
        let ConstDecl::ConstDecl(_, (const_kw, name, eq, expr, _)) = self;
        const_kw.lexeme.clone() + &name.lexeme + &eq.lexeme.clone() + &expr.print_rpn()
    }
}

impl PrintRPN for ForStmt {
    fn print_rpn(&self) -> String {
        match self {
            ForStmt::VarDecl(_, (for_kw, _, var_decl, cond, _, repeated, _, stmt)) => {
                for_kw.lexeme.clone()
                    + " "
                    + &var_decl.print_rpn()
                    + "\n"
                    + &match cond {
                        Some((cond,)) => cond.print_rpn(),
                        None => String::from(";"),
                    }
                    + "\n"
                    + &match repeated {
                        Some((repeated,)) => repeated.print_rpn(),
                        None => String::from(";"),
                    }
                    + "\n"
                    + &stmt.print_rpn()
            }
            ForStmt::Expression(_, (for_kw, _, expr, cond, _, repeated, _, stmt)) => {
                for_kw.lexeme.clone()
                    + " "
                    + &expr.print_rpn()
                    + "\n"
                    + &match cond {
                        Some((cond,)) => cond.print_rpn(),
                        None => String::from(";"),
                    }
                    + "\n"
                    + &match repeated {
                        Some((repeated,)) => repeated.print_rpn(),
                        None => String::from(";"),
                    }
                    + "\n"
                    + &stmt.print_rpn()
            }
            ForStmt::None(_, (for_kw, _, semi, cond, _, repeated, _, stmt)) => {
                for_kw.lexeme.clone()
                    + " "
                    + &semi.lexeme.to_string()
                    + "\n"
                    + &match cond {
                        Some((cond,)) => cond.print_rpn(),
                        None => String::from(";"),
                    }
                    + "\n"
                    + &match repeated {
                        Some((repeated,)) => repeated.print_rpn(),
                        None => String::from(";"),
                    }
                    + "\n"
                    + &stmt.print_rpn()
            }
        }
    }
}

impl PrintRPN for WhileStmt {
    fn print_rpn(&self) -> String {
        let WhileStmt::WhileStmt(_, (while_kw, _, expr, _, stmt)) = self;
        while_kw.lexeme.clone() + &expr.print_rpn() + "\n" + &stmt.print_rpn()
    }
}

impl PrintRPN for IfStmt {
    fn print_rpn(&self) -> String {
        let IfStmt::IfStmt(_, (if_kw, _, expr, _, stmt, elifs, else_)) = self;
        let mut res = if_kw.lexeme.clone() + " " + &expr.print_rpn() + "\n" + &stmt.print_rpn();
        for (elif, _, expr, _, stmt) in elifs {
            res += &(String::from("\n")
                + &elif.lexeme.to_string()
                + " "
                + &expr.print_rpn()
                + "\n"
                + &stmt.print_rpn());
        }
        if let Some((else_kw, stmt)) = else_ {
            res += &(String::from("\n") + &else_kw.lexeme.to_string() + " " + &stmt.print_rpn());
        }
        res
    }
}

impl PrintRPN for ExprStmt {
    fn print_rpn(&self) -> String {
        let ExprStmt::ExprStmt(_, (expr, _)) = self;
        expr.print_rpn()
    }
}

impl PrintRPN for PrintStmt {
    fn print_rpn(&self) -> String {
        let PrintStmt::PrintStmt(_, (print_token, expr, _)) = self;
        print_token.lexeme.clone() + " " + &expr.print_rpn()
    }
}

impl PrintRPN for VarDecl {
    fn print_rpn(&self) -> String {
        let VarDecl::VarDecl(_, (var, name, expr, _)) = self;
        let (eq, expr) = match expr {
            Some((eq, expr)) => (eq.lexeme.clone().to_string(), expr.print_rpn()),
            None => (String::new(), String::new()),
        };
        var.lexeme.clone() + " " + &name.lexeme.to_string() + &eq + &expr
    }
}

impl PrintRPN for Expression {
    fn print_rpn(&self) -> String {
        match self {
            Expression::Expression(_, (equality,)) => equality.print_rpn(),
            Expression::Assignment(_, (assignment,)) => assignment.print_rpn(),
        }
    }
}

impl PrintRPN for LogicOr {
    fn print_rpn(&self) -> String {
        let LogicOr::LogicOr(_, (left, args)) = self;
        let mut res = left.print_rpn();
        for (or, right) in args {
            res += &(right.print_rpn() + &or.lexeme.to_string());
        }
        res
    }
}

impl PrintRPN for LogicAnd {
    fn print_rpn(&self) -> String {
        let LogicAnd::LogicAnd(_, (left, args)) = self;
        let mut res = left.print_rpn();
        for (and, right) in args {
            res += &(right.print_rpn() + &and.lexeme.to_string());
        }
        res
    }
}

impl PrintRPN for Assignment {
    fn print_rpn(&self) -> String {
        let Assignment::Assignment(_, (name, eq, expr)) = self;
        name.lexeme.clone() + &eq.lexeme.to_string() + &expr.print_rpn()
    }
}

impl PrintRPN for Equality {
    fn print_rpn(&self) -> String {
        let Equality::Equality(_, (comparison, args)) = self;
        let mut result = comparison.print_rpn();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.1.print_rpn());
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
        }
        result
    }
}

impl PrintRPN for Comparison {
    fn print_rpn(&self) -> String {
        let Comparison::Comparison(_, (term, args)) = self;
        let mut result = term.print_rpn();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.1.print_rpn());
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
        }
        result
    }
}

impl PrintRPN for Term {
    fn print_rpn(&self) -> String {
        let Term::Term(_, (factor, args)) = self;
        let mut result = factor.print_rpn();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.1.print_rpn());
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
        }
        result
    }
}

impl PrintRPN for Factor {
    fn print_rpn(&self) -> String {
        let Factor::Factor(_, (unary, args)) = self;
        let mut result = unary.print_rpn();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.1.print_rpn());
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
        }
        result
    }
}

impl PrintRPN for Unary {
    fn print_rpn(&self) -> String {
        let Unary::Unary(_, (args, func_call)) = self;
        let mut result = func_call.print_rpn();
        for token in args {
            result.push(' ');
            result.push_str(&token.0.lexeme.to_string());
            result.push_str("unary")
        }
        result
    }
}

impl PrintRPN for FuncCall {
    fn print_rpn(&self) -> String {
        let FuncCall::FuncCall(_, (primary, calls)) = self;
        primary.print_rpn()
            + &calls
                .iter()
                .fold(String::new(), |state, (call,)| state + &call.print_rpn())
    }
}

impl PrintRPN for FuncCall2 {
    fn print_rpn(&self) -> String {
        match self {
            FuncCall2::Call(_, (left, args, right)) => {
                left.lexeme.clone()
                    + &match args {
                        Some((args,)) => args.print_rpn(),
                        None => String::new(),
                    }
                    + &right.lexeme
            }
            FuncCall2::Property(_, (dot, property)) => dot.lexeme.clone() + &property.lexeme,
        }
    }
}

impl PrintRPN for Arguments {
    fn print_rpn(&self) -> String {
        let Arguments::Arguments(_, (expr, exprs)) = self;
        expr.print_rpn()
            + &exprs.iter().fold(String::new(), |state, (comma, expr)| {
                state + &comma.lexeme + &expr.print_rpn()
            })
    }
}

impl PrintRPN for Primary {
    fn print_rpn(&self) -> String {
        match self {
            Primary::Literal(_, (literal,)) => literal.lexeme.clone().to_string(),
            Primary::Parenthesized(_, (_, expr, _)) => expr.print_rpn(),
            Primary::Func(_, (_, func)) => func.print_rpn(),
        }
    }
}

impl PrintRPN for FuncDecl {
    fn print_rpn(&self) -> String {
        let FuncDecl::FuncDecl(_, (return_type, left, params, right, block)) = self;
        return_type.lexeme.clone()
            + &left.lexeme
            + &params.print_rpn()
            + &right.lexeme
            + &block.print_rpn()
    }
}

impl PrintRPN for ArgDecl {
    fn print_rpn(&self) -> String {
        let ArgDecl::ArgDecl(_, (args,)) = self;
        match args {
            Some((arg, _, arg_type, args)) => {
                arg.lexeme.clone()
                    + ": "
                    + &arg_type.lexeme
                    + &args
                        .iter()
                        .fold(String::new(), |state, (comma, arg, _, arg_type)| {
                            state + &comma.lexeme + &arg.lexeme + ": " + &arg_type.lexeme
                        })
            }
            None => String::new(),
        }
    }
}

pub trait Print {
    fn print(&self) -> String;
}

impl Print for Program {
    fn print(&self) -> String {
        let Program::Program(_, (stmts, _)) = self;
        let mut result = String::new();
        for (stmt,) in stmts {
            result.push_str(&stmt.print());
            result.push('\n');
        }
        result
    }
}

impl Print for Statement {
    fn print(&self) -> String {
        match self {
            Statement::ExprStmt(_, (expr,)) => expr.print(),
            Statement::PrintStmt(_, (expr,)) => expr.print(),
            Statement::VarDecl(_, (var_decl,)) => var_decl.print(),
            Statement::Block(_, (block,)) => block.print(),
            Statement::IfStmt(_, (if_stmt,)) => if_stmt.print(),
            Statement::WhileStmt(_, (while_stmt,)) => while_stmt.print(),
            Statement::ForStmt(_, (for_stmt,)) => for_stmt.print(),
            Statement::Break(_, (break_kw, semi)) => break_kw.lexeme.clone() + &semi.lexeme,
            Statement::ConstDecl(_, (const_decl,)) => const_decl.print(),
            Statement::Continue(_, (continue_kw, semi)) => {
                continue_kw.lexeme.clone() + &semi.lexeme
            }
            Statement::Return(_, (return_kw, expr, semi)) => {
                return_kw.lexeme.clone()
                    + &match expr {
                        Some((expr,)) => String::from(" ") + &expr.print(),
                        None => String::new(),
                    }
                    + &semi.lexeme
            }
            Statement::FuncDecl(_, (func, name, func_decl)) => {
                func.lexeme.clone() + " " + &name.lexeme + " " + &func_decl.print()
            }
            Statement::ClassDecl(_, (class_kw, name, left_brace, funcs, right_brace)) => {
                class_kw.lexeme.clone()
                    + " "
                    + &name.lexeme
                    + " "
                    + &left_brace.lexeme
                    + &funcs.iter().fold(String::new(), |acc, (name, func)| {
                        acc + &name.lexeme + " " + &func.print()
                    })
                    + &right_brace.lexeme
            }
        }
    }
}

impl Print for Block {
    fn print(&self) -> String {
        let Block::Block(_, (left, stmts, right)) = self;
        let mut res: String = left.lexeme.clone().to_string();
        for (stmt,) in stmts {
            res.push_str(&stmt.print());
        }
        res.push_str(&right.lexeme.to_string());
        res
    }
}

impl Print for ConstDecl {
    fn print(&self) -> String {
        let ConstDecl::ConstDecl(_, (const_kw, name, eq, expr, semi)) = self;
        const_kw.lexeme.clone()
            + " "
            + &name.lexeme.to_string()
            + &eq.lexeme.to_string()
            + &expr.print()
            + &semi.lexeme.to_string()
    }
}

impl Print for ForStmt {
    fn print(&self) -> String {
        match self {
            ForStmt::VarDecl(_, (for_kw, left, var_decl, cond, semi, repeated, right, stmt)) => {
                for_kw.lexeme.clone()
                    + &left.lexeme.to_string()
                    + &var_decl.print()
                    + &match cond {
                        Some((cond,)) => cond.print(),
                        None => String::new(),
                    }
                    + &semi.lexeme.to_string()
                    + &match repeated {
                        Some((repeated,)) => repeated.print(),
                        None => String::new(),
                    }
                    + &right.lexeme.to_string()
                    + &stmt.print()
            }
            ForStmt::Expression(_, (for_kw, left, expr, cond, semi, repeated, right, stmt)) => {
                for_kw.lexeme.clone()
                    + &left.lexeme.to_string()
                    + &expr.print()
                    + &match cond {
                        Some((cond,)) => cond.print(),
                        None => String::new(),
                    }
                    + &semi.lexeme.to_string()
                    + &match repeated {
                        Some((repeated,)) => repeated.print(),
                        None => String::new(),
                    }
                    + &right.lexeme.to_string()
                    + &stmt.print()
            }
            ForStmt::None(_, (for_kw, left, semi1, cond, semi2, repeated, right, stmt)) => {
                for_kw.lexeme.clone()
                    + &left.lexeme.to_string()
                    + &semi1.lexeme.to_string()
                    + &match cond {
                        Some((cond,)) => cond.print(),
                        None => String::new(),
                    }
                    + &semi2.lexeme.to_string()
                    + &match repeated {
                        Some((repeated,)) => repeated.print(),
                        None => String::new(),
                    }
                    + &right.lexeme.to_string()
                    + &stmt.print()
            }
        }
    }
}

impl Print for WhileStmt {
    fn print(&self) -> String {
        let WhileStmt::WhileStmt(_, (while_kw, left, expr, right, stmt)) = self;
        while_kw.lexeme.clone()
            + &left.lexeme.to_string()
            + &expr.print()
            + &right.lexeme.to_string()
            + &stmt.print()
    }
}

impl Print for IfStmt {
    fn print(&self) -> String {
        let IfStmt::IfStmt(_, (if_kw, left, expr, right, stmt, elifs, else_)) = self;
        let mut res = if_kw.lexeme.clone()
            + &left.lexeme.to_string()
            + &expr.print()
            + &right.lexeme.to_string()
            + &stmt.print();
        for (elif, left, expr, right, stmt) in elifs {
            res += &(String::from(" ")
                + &elif.lexeme.to_string()
                + &left.lexeme.to_string()
                + &expr.print()
                + &right.lexeme.to_string()
                + &stmt.print());
        }
        if let Some((else_kw, stmt)) = else_ {
            res += &(String::from(" ") + &else_kw.lexeme.to_string() + " " + &stmt.print());
        }
        res
    }
}

impl Print for ExprStmt {
    fn print(&self) -> String {
        let ExprStmt::ExprStmt(_, (expr, semi)) = self;
        expr.print() + &semi.lexeme.to_string()
    }
}

impl Print for PrintStmt {
    fn print(&self) -> String {
        let PrintStmt::PrintStmt(_, (print_token, expr, semi)) = self;
        print_token.lexeme.clone() + " " + &expr.print() + &semi.lexeme.to_string()
    }
}

impl Print for VarDecl {
    fn print(&self) -> String {
        let VarDecl::VarDecl(_, (var, name, expr, semi)) = self;
        let (eq, expr) = match expr {
            Some((eq, expr)) => (eq.lexeme.clone().to_string(), expr.print()),
            None => (String::new(), String::new()),
        };
        var.lexeme.clone() + " " + &name.lexeme.to_string() + &eq + &expr + &semi.lexeme.to_string()
    }
}

impl Print for Expression {
    fn print(&self) -> String {
        match self {
            Expression::Expression(_, (expr,)) => expr.print(),
            Expression::Assignment(_, (assignment,)) => assignment.print(),
        }
    }
}

impl Print for LogicOr {
    fn print(&self) -> String {
        let LogicOr::LogicOr(_, (left, args)) = self;
        let mut res = left.print();
        for (or, right) in args {
            res += &(or.lexeme.clone() + &right.print())
        }
        res
    }
}

impl Print for LogicAnd {
    fn print(&self) -> String {
        let LogicAnd::LogicAnd(_, (left, args)) = self;
        let mut res = left.print();
        for (and, right) in args {
            res += &(and.lexeme.clone() + &right.print())
        }
        res
    }
}

impl Print for Assignment {
    fn print(&self) -> String {
        let Assignment::Assignment(_, (name, eq, expr)) = self;
        name.lexeme.clone() + &eq.lexeme.to_string() + &expr.print()
    }
}

impl Print for Equality {
    fn print(&self) -> String {
        let Equality::Equality(_, (comparison, args)) = self;
        let mut result = comparison.print();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
            result.push(' ');
            result.push_str(&arg.1.print());
        }
        result
    }
}

impl Print for Comparison {
    fn print(&self) -> String {
        let Comparison::Comparison(_, (term, args)) = self;
        let mut result = term.print();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
            result.push(' ');
            result.push_str(&arg.1.print());
        }
        result
    }
}

impl Print for Term {
    fn print(&self) -> String {
        let Term::Term(_, (factor, args)) = self;
        let mut result = factor.print();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
            result.push(' ');
            result.push_str(&arg.1.print());
        }
        result
    }
}

impl Print for Factor {
    fn print(&self) -> String {
        let Factor::Factor(_, (unary, args)) = self;
        let mut result = unary.print();
        for arg in args {
            result.push(' ');
            result.push_str(&arg.0.lexeme.to_string());
            result.push(' ');
            result.push_str(&arg.1.print());
        }
        result
    }
}

impl Print for Unary {
    fn print(&self) -> String {
        let Unary::Unary(_, (args, func_call)) = self;
        let mut result = String::new();
        for arg in args {
            result.push_str(&arg.0.lexeme.to_string());
        }
        result.push_str(&func_call.print());
        result
    }
}

impl Print for FuncCall {
    fn print(&self) -> String {
        let FuncCall::FuncCall(_, (primary, calls)) = self;
        primary.print()
            + &calls
                .iter()
                .fold(String::new(), |state, (call,)| state + &call.print())
    }
}

impl Print for FuncCall2 {
    fn print(&self) -> String {
        match self {
            FuncCall2::Call(_, (left, args, right)) => {
                left.lexeme.clone()
                    + &match args {
                        Some((args,)) => args.print(),
                        None => String::new(),
                    }
                    + &right.lexeme
            }
            FuncCall2::Property(_, (dot, property)) => dot.lexeme.clone() + &property.lexeme,
        }
    }
}

impl Print for Arguments {
    fn print(&self) -> String {
        let Arguments::Arguments(_, (expr, exprs)) = self;
        expr.print()
            + &exprs.iter().fold(String::new(), |state, (comma, expr)| {
                state + &comma.lexeme + &expr.print()
            })
    }
}

impl Print for Primary {
    fn print(&self) -> String {
        match self {
            Primary::Literal(_, (token,)) => token.lexeme.clone().to_string(),
            Primary::Parenthesized(_, (_, expr, _)) => String::from("(") + &expr.print() + ")",
            Primary::Func(_, (func, func_decl)) => func.lexeme.clone() + &func_decl.print(),
        }
    }
}

impl Print for FuncDecl {
    fn print(&self) -> String {
        let FuncDecl::FuncDecl(_, (return_type, left, args, right, block)) = self;
        return_type.lexeme.clone() + &left.lexeme + &args.print() + &right.lexeme + &block.print()
    }
}

impl Print for ArgDecl {
    fn print(&self) -> String {
        let ArgDecl::ArgDecl(_, (args,)) = self;
        match args {
            Some((arg, _, arg_type, args)) => {
                arg.lexeme.clone()
                    + ": "
                    + &arg_type.lexeme
                    + &args
                        .iter()
                        .fold(String::new(), |state, (comma, arg, _, arg_type)| {
                            state + &comma.lexeme + &arg.lexeme + ": " + &arg_type.lexeme
                        })
            }
            None => String::new(),
        }
    }
}
