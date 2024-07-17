use strum::EnumMessage;

use crate::{
    environment::Scope,
    error::{Error, ErrorType::Analyze},
    lox_lib::{LoxClass, LoxFunction, LoxType},
    nodes::{
        ArgDecl, Arguments, Assignment, Block, Comparison, ConstDecl, Equality, ExprStmt,
        Expression, Factor, ForStmt, FuncCall, FuncCall2, FuncDecl, GetTokens, IfStmt, LogicAnd,
        LogicOr, Primary, PrintStmt, Program, Statement, Term, Unary, VarDecl, WhileStmt,
    },
    token::{
        token_type::{KeyWord, Literal, OneChar, OneTwoChar, TokenType},
        Token,
    },
};
use std::{collections::HashSet, vec};

pub trait Analyse {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        scope: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>>;
}

impl Analyse for Program {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Program::Program(_, (stmts, _)) = self;
        let mut errs = vec![];
        for err in stmts
            .iter()
            .filter_map(|(x,)| x.analyse(func, in_loop, env).err())
        {
            errs.extend(err);
        }
        Scope::clear();
        if errs.is_empty() {
            Ok(None)
        } else {
            Err(errs)
        }
    }
}

impl Analyse for Statement {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        match self {
            Statement::ExprStmt(_, (expr,)) => expr.analyse(func, in_loop, env),
            Statement::PrintStmt(_, (print_stmt,)) => print_stmt.analyse(func, in_loop, env),
            Statement::VarDecl(_, (var_decl,)) => var_decl.analyse(func, in_loop, env),
            Statement::FuncDecl(tokens, (_, name, func_decl)) => {
                let FuncDecl::FuncDecl(_, (return_type, _, args, _, block)) = func_decl.as_ref();
                let ArgDecl::ArgDecl(_, (args,)) = args.as_ref();
                let args = match args {
                    Some((arg, _, _, args)) => {
                        let mut tokens = vec![arg.clone()];
                        tokens.extend(args.iter().map(|(_, x, _, _)| x.clone()));
                        tokens
                    }
                    None => vec![],
                };
                let mut errs = vec![];
                let return_type = match LoxType::try_from(return_type) {
                    Ok(return_type) => return_type,
                    Err(e) => {
                        errs.push(e);
                        LoxType::Null
                    }
                };
                if let Err(e) = env.define(
                    name.lexeme.clone(),
                    Some(LoxType::Function(LoxFunction::UserDefined {
                        name: name.lexeme.clone(),
                        args: args.clone(),
                        return_type: Box::new(return_type.clone()),
                        environment: *env,
                        block: block.clone(),
                    })),
                    tokens,
                ) {
                    errs.push(e);
                }
                let func = match func_decl.analyse(&None, false, env) {
                    Ok(func) => func.unwrap(),
                    Err(e) => {
                        errs.extend(e);
                        LoxType::Function(LoxFunction::UserDefined {
                            name: String::new(),
                            args,
                            return_type: Box::new(return_type),
                            environment: *env,
                            block: block.clone(),
                        })
                    }
                };
                if let Err(e) = env.define(name.lexeme.clone(), Some(func), self.get_tokens()) {
                    errs.push(e);
                }
                if !errs.is_empty() {
                    return Err(errs);
                }
                Ok(None)
            }
            Statement::ClassDecl(tokens, (_, name, _, funcs, _)) => {
                let mut errs = vec![];
                if let Err(e) = env.define(
                    name.lexeme.clone(),
                    Some(LoxType::Class(LoxClass::UserDefined {
                        name: name.lexeme.clone(),
                    })),
                    tokens,
                ) {
                    errs.push(e);
                }

                for (_, function) in funcs {
                    match function.analyse(func, in_loop, env) {
                        Ok(_) => {}
                        Err(e) => errs.extend(e),
                    }
                }

                if !errs.is_empty() {
                    Err(errs)
                } else {
                    Ok(None)
                }
            }
            Statement::ConstDecl(_, (decl,)) => decl.analyse(func, in_loop, env),
            Statement::Block(_, (stmts,)) => stmts.analyse(func, in_loop, env),
            Statement::IfStmt(_, (stmt,)) => stmt.analyse(func, in_loop, env),
            Statement::WhileStmt(_, (stmt,)) => stmt.analyse(func, in_loop, env),
            Statement::Break(_, (break_kw, semi)) => {
                if !in_loop {
                    Err(vec![Error::new(
                        Analyze,
                        &vec![break_kw.clone(), semi.clone()],
                        "Can't use the 'break' statement outside a loop",
                        "Syntax",
                    )])
                } else {
                    Ok(None)
                }
            }
            Statement::Continue(_, (continue_kw, semi)) => {
                if !in_loop {
                    Err(vec![Error::new(
                        Analyze,
                        &vec![continue_kw.clone(), semi.clone()],
                        "Can't use the 'continue' statement outside a loop",
                        "Syntax",
                    )])
                } else {
                    Ok(None)
                }
            }
            Statement::Return(_, (return_kw, expr, semi)) => {
                let mut tokens = vec![return_kw.clone()];
                match expr {
                    Some((expr,)) => tokens.extend(expr.get_tokens().clone()),
                    None => (),
                };
                tokens.push(semi.clone());
                match func {
                    Some(return_type) => match (return_type, expr) {
                        (LoxType::Null, None) => Ok(None),
                        (LoxType::Null, Some(_)) => Err(vec![Error::new(
                            Analyze,
                            &tokens,
                            "Can't return anything from a function that returns 'void'",
                            "Syntax",
                        )]),
                        (_, None) => Err(vec![Error::new(
                            Analyze,
                            &tokens,
                            format!("Expected type '{}', found 'void'", return_type.get_type())
                                .as_str(),
                            "Syntax",
                        )]),
                        (_, Some((expr,))) => cast(
                            return_type,
                            &expr.analyse(func, in_loop, env)?.unwrap(),
                            &tokens,
                        )
                        .map(|_| Some(return_type.clone()))
                        .map_err(|x| vec![x]),
                    },
                    None => Err(vec![Error::new(
                        Analyze,
                        &tokens,
                        "Can't use 'return' statement outside a function",
                        "Syntax",
                    )]),
                }
            }
            Statement::ForStmt(_, (for_stmt,)) => for_stmt.analyse(func, in_loop, env),
        }
    }
}

impl Analyse for ForStmt {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        _in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        match self {
            ForStmt::VarDecl(_, (_, _, var_decl, cond, _, inc, _, block)) => {
                let mut errs = vec![];
                if let Err(e) = var_decl.analyse(func, true, env) {
                    errs.extend(e);
                }
                if let Some((cond,)) = cond {
                    if let Err(e) = cond.analyse(func, true, env) {
                        errs.extend(e);
                    }
                }
                if let Some((inc,)) = inc {
                    if let Err(e) = inc.analyse(func, true, env) {
                        errs.extend(e);
                    }
                }
                if let Err(e) = block.analyse(func, true, env) {
                    errs.extend(e);
                }

                if !errs.is_empty() {
                    Err(errs)
                } else {
                    Ok(None)
                }
            }
            ForStmt::Expression(_, (_, _, expr_stmt, cond, _, inc, _, block)) => {
                let mut errs = vec![];
                if let Err(e) = expr_stmt.analyse(func, true, env) {
                    errs.extend(e);
                }
                if let Some((cond,)) = cond {
                    if let Err(e) = cond.analyse(func, true, env) {
                        errs.extend(e);
                    }
                }
                if let Some((inc,)) = inc {
                    if let Err(e) = inc.analyse(func, true, env) {
                        errs.extend(e);
                    }
                }
                if let Err(e) = block.analyse(func, true, env) {
                    errs.extend(e);
                }

                if !errs.is_empty() {
                    Err(errs)
                } else {
                    Ok(None)
                }
            }
            ForStmt::None(_, (_, _, _, cond, _, inc, _, block)) => {
                let mut errs = vec![];
                if let Some((cond,)) = cond {
                    if let Err(e) = cond.analyse(func, true, env) {
                        errs.extend(e);
                    }
                }
                if let Some((inc,)) = inc {
                    if let Err(e) = inc.analyse(func, true, env) {
                        errs.extend(e);
                    }
                }
                if let Err(e) = block.analyse(func, true, env) {
                    errs.extend(e);
                }

                if !errs.is_empty() {
                    Err(errs)
                } else {
                    Ok(None)
                }
            }
        }
    }
}

impl Analyse for PrintStmt {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let PrintStmt::PrintStmt(_, (print_kw, expr, semi)) = self;
        let mut tokens = vec![print_kw.clone()];
        tokens.extend(expr.get_tokens().clone());
        tokens.push(semi.clone());
        cast(
            &LoxType::String(Default::default()),
            &expr.analyse(func, in_loop, env)?.unwrap(),
            &tokens,
        )
        .map_err(|x| vec![x])?;
        Ok(None)
    }
}

impl Analyse for ConstDecl {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let ConstDecl::ConstDecl(_, (_, name, _, expr, _)) = self;
        let mut errs = vec![];
        let value = expr
            .analyse(func, in_loop, env)
            .unwrap_or_else(|e| {
                errs.extend(e);
                Some(LoxType::Null)
            })
            .unwrap();
        env.define_const(name.lexeme.clone(), value, &[name.clone()])
            .map_err(|x| vec![x])?;
        Ok(None)
    }
}

impl Analyse for IfStmt {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let IfStmt::IfStmt(_, (_, _, expr, _, block, elifs, else_clause)) = self;
        let mut errs = vec![];
        if let Err(e) = expr.analyse(func, in_loop, env) {
            errs.extend(e);
        }
        if let Err(e) = block.analyse(func, in_loop, env) {
            errs.extend(e);
        }
        for (_, _, expr, _, block) in elifs {
            if let Err(e) = expr.analyse(func, in_loop, env) {
                errs.extend(e);
            }
            if let Err(e) = block.analyse(func, in_loop, env) {
                errs.extend(e);
            }
        }
        if let Some((_, block)) = else_clause {
            if let Err(e) = block.analyse(func, in_loop, env) {
                errs.extend(e);
            }
        }
        if !errs.is_empty() {
            Err(errs)
        } else {
            Ok(None)
        }
    }
}

impl Analyse for WhileStmt {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let WhileStmt::WhileStmt(_, (_, _, expr, _, block)) = self;
        let mut errs = vec![];
        if let Err(e) = expr.analyse(func, in_loop, env) {
            errs.extend(e);
        }
        if let Err(e) = block.analyse(func, true, env) {
            errs.extend(e);
        }
        if !errs.is_empty() {
            Err(errs)
        } else {
            Ok(None)
        }
    }
}

impl Analyse for VarDecl {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let VarDecl::VarDecl(_, (_, name, expr, _)) = self;
        let mut errs = vec![];
        let value = match expr {
            Some((_, expr)) => expr.analyse(func, in_loop, env).unwrap_or_else(|e| {
                errs.extend(e);
                Some(LoxType::Null)
            }),
            None => None,
        };
        env.define(name.lexeme.clone(), value, &[name.clone()])
            .map_err(|e| {
                errs.push(e);
                errs
            })?;
        Ok(None)
    }
}

impl Analyse for ExprStmt {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let ExprStmt::ExprStmt(_, (expr, _)) = self;
        expr.analyse(func, in_loop, env)
    }
}

impl Analyse for Expression {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        match self {
            Expression::Expression(_, (expr,)) => expr.analyse(func, in_loop, env),
            Expression::Assignment(_, (assignment,)) => assignment.analyse(func, in_loop, env),
        }
    }
}

impl Analyse for LogicOr {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let LogicOr::LogicOr(_, (expr, args)) = self;
        let mut res = expr.analyse(func, in_loop, env).map(|x| x.unwrap());
        let mut tokens = expr.get_tokens().clone();
        for (op, arg) in args {
            match (
                &mut res,
                arg.analyse(func, in_loop, env).map(|x| x.unwrap()),
            ) {
                (Ok(res_type), Ok(expr)) => {
                    res = type_check(res_type, op, &expr, &tokens, arg.get_tokens())
                        .map_err(|x| vec![x])
                }
                (Ok(_), Err(e)) => res = Err(e),
                (Err(_), Ok(_)) => {}
                (Err(ref mut e), Err(expr)) => e.extend(expr),
            };
            tokens.push(op.clone());
            tokens.extend(arg.get_tokens().clone());
        }
        res.map(Some)
    }
}

impl Analyse for LogicAnd {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let LogicAnd::LogicAnd(_, (expr, args)) = self;
        let mut res = expr.analyse(func, in_loop, env).map(|x| x.unwrap());
        let mut tokens = expr.get_tokens().clone();
        for (op, arg) in args {
            match (
                &mut res,
                arg.analyse(func, in_loop, env).map(|x| x.unwrap()),
            ) {
                (Ok(res_type), Ok(expr)) => {
                    res = type_check(res_type, op, &expr, &tokens, arg.get_tokens())
                        .map_err(|x| vec![x])
                }
                (Ok(_), Err(e)) => res = Err(e),
                (Err(_), Ok(_)) => {}
                (Err(ref mut e), Err(expr)) => e.extend(expr),
            };
            tokens.push(op.clone());
            tokens.extend(arg.get_tokens().clone());
        }
        res.map(Some)
    }
}

impl Analyse for Equality {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Equality::Equality(_, (expr, args)) = self;
        let mut res = expr.analyse(func, in_loop, env).map(|x| x.unwrap());
        let mut tokens = expr.get_tokens().clone();
        for (op, arg) in args {
            match (
                &mut res,
                arg.analyse(func, in_loop, env).map(|x| x.unwrap()),
            ) {
                (Ok(res_type), Ok(expr)) => {
                    res = type_check(res_type, op, &expr, &tokens, arg.get_tokens())
                        .map_err(|x| vec![x])
                }
                (Ok(_), Err(e)) => res = Err(e),
                (Err(_), Ok(_)) => {}
                (Err(ref mut e), Err(expr)) => e.extend(expr),
            };
            tokens.push(op.clone());
            tokens.extend(arg.get_tokens().clone());
        }
        res.map(Some)
    }
}

impl Analyse for Comparison {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Comparison::Comparison(_, (expr, args)) = self;
        let mut res = expr.analyse(func, in_loop, env).map(|x| x.unwrap());
        let mut tokens = expr.get_tokens().clone();
        for (op, arg) in args {
            match (
                &mut res,
                arg.analyse(func, in_loop, env).map(|x| x.unwrap()),
            ) {
                (Ok(res_type), Ok(expr)) => {
                    res = type_check(res_type, op, &expr, &tokens, arg.get_tokens())
                        .map_err(|x| vec![x])
                }
                (Ok(_), Err(e)) => res = Err(e),
                (Err(_), Ok(_)) => {}
                (Err(ref mut e), Err(expr)) => e.extend(expr),
            };
            tokens.push(op.clone());
            tokens.extend(arg.get_tokens().clone());
        }
        res.map(Some)
    }
}

impl Analyse for Term {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Term::Term(_, (expr, args)) = self;
        let mut res = expr.analyse(func, in_loop, env).map(|x| x.unwrap());
        let mut tokens = expr.get_tokens().clone();
        for (op, arg) in args {
            match (
                &mut res,
                arg.analyse(func, in_loop, env).map(|x| x.unwrap()),
            ) {
                (Ok(res_type), Ok(expr)) => {
                    res = type_check(res_type, op, &expr, &tokens, arg.get_tokens())
                        .map_err(|x| vec![x])
                }
                (Ok(_), Err(e)) => res = Err(e),
                (Err(_), Ok(_)) => {}
                (Err(ref mut e), Err(expr)) => e.extend(expr),
            };
            tokens.push(op.clone());
            tokens.extend(arg.get_tokens().clone());
        }
        res.map(Some)
    }
}

impl Analyse for Factor {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Factor::Factor(_, (expr, args)) = self;
        let mut res = expr.analyse(func, in_loop, env).map(|x| x.unwrap());
        let mut tokens = expr.get_tokens().clone();
        for (op, arg) in args {
            match (
                &mut res,
                arg.analyse(func, in_loop, env).map(|x| x.unwrap()),
            ) {
                (Ok(res_type), Ok(expr)) => {
                    res = type_check(res_type, op, &expr, &tokens, arg.get_tokens())
                        .map_err(|x| vec![x])
                }
                (Ok(_), Err(e)) => res = Err(e),
                (Err(_), Ok(_)) => {}
                (Err(ref mut e), Err(expr)) => e.extend(expr),
            };
            tokens.push(op.clone());
            tokens.extend(arg.get_tokens().clone());
        }
        res.map(Some)
    }
}

impl Analyse for Unary {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Unary::Unary(_, (args, func_call)) = self;
        let res = func_call.analyse(func, in_loop, env)?.unwrap();
        if !args.is_empty() {
            if args
                .last()
                .unwrap()
                .0
                .eq_type(&TokenType::OneTwoChar(OneTwoChar::Bang))
            {
                cast(&LoxType::Bool(Default::default()), &res, self.get_tokens())
                    .map_err(|e| vec![e])?;
            } else if !is_number(&res) {
                return Err(vec![Error::new(
                    Analyze,
                    self.get_tokens(),
                    format!("Can't use unary operator on a '{}'", res.get_type()).as_str(),
                    "Type",
                )]);
            }
            match args[0].0.token_type {
                TokenType::OneChar(_) => {
                    if args
                        .iter()
                        .any(|(arg,)| arg.eq_type(&TokenType::OneTwoChar(OneTwoChar::Bang)))
                    {
                        Ok(Some(LoxType::Integer(Default::default())))
                    } else {
                        Ok(Some(res))
                    }
                } // Plus and minus
                TokenType::OneTwoChar(_) => Ok(Some(LoxType::Bool(Default::default()))), // bang
                _ => unreachable!(),
            }
        } else {
            Ok(Some(res))
        }
    }
}

impl Analyse for FuncCall {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let FuncCall::FuncCall(_, (primary, calls)) = self;
        let mut res = primary.analyse(func, in_loop, env)?.unwrap();
        let mut tokens = primary.get_tokens().clone();
        let mut errs = vec![];
        for (call,) in calls {
            match call.as_ref() {
                FuncCall2::Call(_, (left, args, right)) => {
                    if let Some((arguments,)) = args {
                        match arguments.as_ref() {
                            Arguments::Arguments(_, (expr, exprs)) => {
                                if let Err(e) = expr.analyse(func, in_loop, env) {
                                    errs.extend(e);
                                }
                                for (_, expr) in exprs {
                                    if let Err(e) = expr.analyse(func, in_loop, env) {
                                        errs.extend(e);
                                    }
                                }
                            }
                        }
                    }
                    if let LoxType::Function(func) = &res {
                        res = match func {
                            LoxFunction::BuildIn { return_type, .. } => (*return_type).clone(),
                            LoxFunction::UserDefined { return_type, .. } => {
                                return_type.as_ref().clone()
                            }
                        };
                    } else {
                        errs.push(Error::new(
                            Analyze,
                            &tokens,
                            format!("Can't call a '{}'", res.get_type()).as_str(),
                            "Type",
                        ));
                        return Err(errs);
                    }
                    tokens.push(left.clone());
                    match args {
                        Some((args,)) => tokens.extend(args.get_tokens().clone()),
                        None => (),
                    };
                    tokens.push(right.clone());
                }
                FuncCall2::Property(_, (_, property)) => {
                    if let LoxType::Instance(instance) = &res {
                        res = instance
                            .get(property)
                            .ok_or(vec![Error::new(
                                Analyze,
                                self.get_tokens(),
                                "Undefined property",
                                "Undefined",
                            )])?
                            .clone();
                    } else {
                        errs.push(Error::new(
                            Analyze,
                            &tokens,
                            &format!("Can't get a property of a '{}'", res.get_type()),
                            "Type",
                        ))
                    }
                }
            }
        }
        if !errs.is_empty() {
            Err(errs)
        } else {
            Ok(Some(res))
        }
    }
}

impl Analyse for Primary {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        match self {
            Primary::Literal(_, (literal,)) => match &literal.token_type {
                TokenType::Literal(lit) => match lit {
                    Literal::Identifier(_) => {
                        env.define_dist(literal.clone()).map_err(|x| vec![x])?;
                        Ok(Some(env.get_by(literal).map_err(|x| vec![x])?.clone()))
                    }
                    Literal::String(_) => Ok(Some(LoxType::String(Default::default()))),
                    Literal::Integer(_) => Ok(Some(LoxType::Integer(Default::default()))),
                    Literal::Float(_) => Ok(Some(LoxType::Float(Default::default()))),
                },
                TokenType::KeyWord(kw) => match kw {
                    KeyWord::False | KeyWord::True => Ok(Some(LoxType::Bool(Default::default()))),
                    KeyWord::Null => Ok(Some(LoxType::Null)),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            Primary::Func(_, (_, func_decl)) => func_decl.analyse(func, in_loop, env),
            Primary::Parenthesized(_, (_, expr, _)) => expr.analyse(func, in_loop, env),
        }
    }
}

impl Analyse for Assignment {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Assignment::Assignment(_, (name, _, expr)) = self;
        let mut errs = vec![];
        match env.define_dist(name.clone()) {
            Ok(_) => {
                let res = expr.analyse(func, in_loop, env)?.unwrap();
                match env.get_by(name).map_err(|e| vec![e]) {
                    Ok(var_type) => {
                        if var_type.get_type() != res.get_type() {
                            return err(
                                self.get_tokens(),
                                &format!(
                                    "Expected type '{}' got '{}' instead",
                                    var_type.get_type(),
                                    res.get_type()
                                ),
                            )
                            .map_err(|e| vec![e]);
                        }
                        cast(&var_type, &res, self.get_tokens()).map_err(|e| vec![e])?;
                        Ok(Some(var_type.clone()))
                    }
                    Err(_) => {
                        env.assign_by(name, res.clone()).map_err(|e| vec![e])?;
                        Ok(Some(res))
                    }
                }
            }
            Err(e) => {
                errs.push(e);
                if let Err(e) = expr.analyse(func, in_loop, env) {
                    errs.extend(e)
                }
                Err(errs)
            }
        }
    }
}

impl Analyse for FuncDecl {
    fn analyse(
        &self,
        _function: &Option<LoxType>,
        _in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let FuncDecl::FuncDecl(_, (return_type, _, arg_decl, _, block)) = self;
        let mut errs = vec![];
        let mut env = env.enter_scope();
        if let Err(e) = arg_decl.analyse(&None, false, &mut env) {
            errs.extend(e);
        }
        let return_type = LoxType::try_from(return_type).unwrap_or_else(|x| {
            errs.push(x);
            LoxType::Null
        });
        if let Err(e) = block.analyse(&Some(return_type.clone()), false, &mut env) {
            errs.extend(e);
        }
        if !errs.is_empty() {
            return Err(errs);
        }
        let ArgDecl::ArgDecl(_, (args,)) = arg_decl.as_ref();
        Ok(Some(LoxType::Function(LoxFunction::UserDefined {
            name: String::new(),
            args: match args {
                Some((arg, _, _, args)) => {
                    let mut tokens = vec![arg.clone()];
                    tokens.extend(args.iter().map(|(_, x, _, _)| x.clone()));
                    tokens
                }
                None => vec![],
            },
            return_type: Box::new(return_type),
            environment: env,
            block: block.clone(),
        })))
    }
}

impl Analyse for ArgDecl {
    fn analyse(
        &self,
        _func: &Option<LoxType>,
        _in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let ArgDecl::ArgDecl(_, (args,)) = self;
        if args.is_none() {
            return Ok(None);
        }
        let mut names = HashSet::<String>::new();
        let (first, _, first_type, args) = args.clone().unwrap();
        names.insert(first.lexeme.clone());
        let mut errs = vec![];
        if let Err(e) = env.define(
            first.lexeme.clone(),
            Some(LoxType::try_from(&first_type).unwrap_or_else(|e| {
                errs.push(e);
                LoxType::Null
            })),
            &[first],
        ) {
            errs.push(e);
        }
        for (_, arg, _, type_token) in args {
            if !names.insert(arg.lexeme.clone()) {
                errs.push(Error::new(
                    Analyze,
                    &[arg.clone()],
                    &format!("'{}' already is a parameter", arg.lexeme),
                    "DuplicateParameter",
                ))
            }
            if let Err(e) = env.define(
                arg.lexeme.clone(),
                Some(LoxType::try_from(&type_token).unwrap_or_else(|e| {
                    errs.push(e);
                    LoxType::Null
                })),
                &[arg],
            ) {
                errs.push(e);
            }
        }
        if !errs.is_empty() {
            return Err(errs);
        }
        Ok(None)
    }
}

impl Analyse for Block {
    fn analyse(
        &self,
        func: &Option<LoxType>,
        in_loop: bool,
        env: &mut Scope,
    ) -> Result<Option<LoxType>, Vec<Error>> {
        let Block::Block(_, (_, stmts, _)) = self;
        let mut errs = vec![];
        let mut env = env.enter_scope();
        for err in stmts
            .iter()
            .filter_map(|(x,)| x.analyse(func, in_loop, &mut env).err())
        {
            errs.extend(err);
        }
        if errs.is_empty() {
            Ok(None)
        } else {
            Err(errs)
        }
    }
}

fn type_check(
    left: &LoxType,
    op: &Token,
    right: &LoxType,
    left_tokens: &[Token],
    right_tokens: &[Token],
) -> Result<LoxType, Error> {
    let mut all_tokens = left_tokens.to_vec();
    all_tokens.push(op.clone());
    all_tokens.extend(right_tokens.to_owned());
    match op.token_type {
        TokenType::OneChar(_) => {
            if let TokenType::OneChar(OneChar::Plus) = op.token_type {
                if matches!(left, LoxType::String(_)) && matches!(right, LoxType::String(_)) {
                    Ok(LoxType::String(Default::default()))
                } else {
                    cast_to_numbers(left, right, left_tokens, right_tokens)
                }
            } else {
                cast_to_numbers(left, right, left_tokens, right_tokens)
            }
        }
        TokenType::OneTwoChar(op) => match op {
            OneTwoChar::BangEqual | OneTwoChar::EqualEqual => {
                coerce(left, right, &all_tokens).map(|_| LoxType::Bool(Default::default()))
            }
            OneTwoChar::Greater
            | OneTwoChar::GreaterEqual
            | OneTwoChar::Less
            | OneTwoChar::LessEqual => {
                cast_to_numbers(left, right, left_tokens, right_tokens)?;
                Ok(LoxType::Bool(Default::default()))
            }
            _ => unreachable!(),
        },
        TokenType::Literal(_) => unreachable!(),
        TokenType::KeyWord(_) => {
            // "and" and "or" so always cast both expressions to booleans
            cast(&LoxType::Bool(Default::default()), left, left_tokens)?;
            cast(&LoxType::Bool(Default::default()), right, right_tokens)?;
            Ok(LoxType::Bool(Default::default()))
        }
        TokenType::Special(_) => cast_to_numbers(left, right, left_tokens, right_tokens), // division
    }
}

fn err<'a, T>(tokens: &'a [Token], msg: &'a str) -> Result<T, Error> {
    Err(Error::new(Analyze, tokens, msg, "Type"))
}

fn coerce<'a>(left: &'a LoxType, right: &'a LoxType, tokens: &'a [Token]) -> Result<(), Error> {
    match left {
        LoxType::Integer(_) => match right {
            LoxType::Integer(_) | LoxType::Float(_) => Ok(()),
            _ => err(
                tokens,
                &(String::from("Can't operate on int and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Float(_) => match right {
            LoxType::Integer(_) | LoxType::Float(_) => Ok(()),
            _ => err(
                tokens,
                &(String::from("Can't operate on float and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::String(_) => {
            if let LoxType::String(_) = right {
                Ok(())
            } else {
                err(
                    tokens,
                    &(String::from("Can't operate on string and ") + right.get_serializations()[0]),
                )
            }
        }
        LoxType::Bool(_) => match right {
            LoxType::Bool(_) => Ok(()),
            _ => err(
                tokens,
                &(String::from("Can't operate on bool and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Null => match right {
            LoxType::Null => Ok(()),
            _ => err(
                tokens,
                &(String::from("Can't operate on null and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Function(_) => match right {
            LoxType::Function(_) => Ok(()),
            _ => err(
                tokens,
                &(String::from("Can't operate on function and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Class(_) => match right {
            LoxType::Class(_) => Ok(()),
            _ => err(
                tokens,
                &(String::from("Can't operate on class and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Instance(_) => match right {
            LoxType::Instance(_) => Ok(()),
            _ => err(
                tokens,
                &(String::from("Can't operate on object and ") + right.get_serializations()[0]),
            ),
        },
    }
}

fn cast<'a>(target: &'a LoxType, lt: &'a LoxType, tokens: &'a [Token]) -> Result<(), Error> {
    match (target, lt) {
        (LoxType::Integer(_), LoxType::Integer(_))
        | (LoxType::Integer(_), LoxType::Float(_))
        | (LoxType::Float(_), LoxType::Integer(_))
        | (LoxType::Float(_), LoxType::Float(_))
        | (LoxType::String(_), _)
        | (LoxType::Bool(_), _)
        | (LoxType::Function(_), LoxType::Function(_))
        | (LoxType::Class(_), LoxType::Class(_))
        | (LoxType::Null, _) => Ok(()),
        _ => Err(Error::new(
            Analyze,
            tokens,
            format!(
                "Can't cast a '{}' to a '{}'",
                lt.get_type(),
                target.get_type()
            )
            .as_str(),
            "Cast",
        )),
    }
}

fn cast_to_numbers<'a>(
    left: &'a LoxType,
    right: &'a LoxType,
    left_tokens: &'a [Token],
    right_tokens: &'a [Token],
) -> Result<LoxType, Error> {
    if !is_number(left) {
        return Err(Error::new(
            Analyze,
            left_tokens,
            format!("Can't cast a '{}' to a number", left.get_type()).as_str(),
            "Cast",
        ));
    }

    match right {
        LoxType::Integer(_) => Ok(left.clone()),
        LoxType::Float(_) => Ok(right.clone()),
        _ => Err(Error::new(
            Analyze,
            right_tokens,
            format!("Can't cast a '{}' to a number", right.get_type()).as_str(),
            "Cast",
        )),
    }
}

fn is_number(res: &LoxType) -> bool {
    matches!(res, LoxType::Integer(_) | LoxType::Float(_))
}
