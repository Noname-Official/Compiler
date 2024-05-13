use crate::{
    environment::Scope,
    error::{
        Error,
        ErrorType::{Break, Continue, Return, RunTime},
    },
    lox_lib::{LoxCallable, LoxFunction, LoxType},
    nodes::{
        ArgDecl, Arguments, Assignment, Block, Comparison, ConstDecl, Equality, ExprStmt,
        Expression, Factor, ForStmt, FuncCall, FuncCall2, FuncDecl, GetTokens, IfStmt, LogicAnd,
        LogicOr, Primary, PrintStmt, Program, Statement, Term, Unary, VarDecl, WhileStmt,
    },
    token::{
        token_type::{KeyWord, Literal, OneChar, OneTwoChar, Special, TokenType},
        Token,
    },
};
use strum::EnumMessage;

fn function_returned_nothing_error<T>(tokens: &[Token]) -> Result<T, Error> {
    Err(Error::new(
        RunTime,
        tokens,
        "Function returned nothing",
        "Null",
    ))
}

pub trait Interpret {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error>;
}

impl Interpret for Program {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Program::Program(_, (stmts, _)) = self;
        let mut res = None;
        for (stmt,) in stmts {
            res = stmt.interpret(environment)?;
        }
        Scope::clear();
        Ok(res)
    }
}

impl Interpret for Statement {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        match self {
            Statement::ExprStmt(_, (expr_stmt,)) => expr_stmt.interpret(environment),
            Statement::PrintStmt(_, (print_stmt,)) => print_stmt.interpret(environment),
            Statement::VarDecl(_, (var_decl,)) => var_decl.interpret(environment),
            Statement::Block(_, (block,)) => block.interpret(environment),
            Statement::IfStmt(_, (if_stmt,)) => if_stmt.interpret(environment),
            Statement::WhileStmt(_, (while_stmt,)) => while_stmt.interpret(environment),
            Statement::ForStmt(_, (for_stmt,)) => for_stmt.interpret(environment),
            Statement::Break(_, (break_kw, semi)) => Err(Error::new(
                Break,
                &vec![break_kw.clone(), semi.clone()],
                "Can't use 'break' outside loop",
                "Syntax",
            )),
            Statement::ConstDecl(_, (const_decl,)) => const_decl.interpret(environment),
            Statement::Continue(_, (continue_kw, semi)) => Err(Error::new(
                Continue,
                &vec![continue_kw.clone(), semi.clone()],
                "Can't use 'continue' outside loop",
                "Syntax",
            )),
            Statement::Return(_, (_, expr, _)) => Err(Error::new(
                Return(match expr {
                    Some((expr,)) => match expr.interpret(environment)? {
                        Some(v) => Some(v),
                        None => return function_returned_nothing_error(self.get_tokens()),
                    },
                    None => None,
                }),
                self.get_tokens(),
                "Can't use 'return' outside function",
                "Syntax",
            )),
            Statement::FuncDecl(_, (_, name, func_decl)) => {
                let FuncDecl::FuncDecl(_, (return_type, _, arg_decl, _, block)) =
                    func_decl.as_ref();
                let ArgDecl::ArgDecl(_, (args,)) = arg_decl.as_ref();
                // let drop_guard = environment.enter_scope();
                // let env = environment.clone();
                // drop(drop_guard);
                environment.define(
                    name.lexeme.clone(),
                    Some(LoxType::Function(LoxFunction::UserDefined {
                        name: name.lexeme.clone(),
                        block: block.clone(),
                        return_type: Box::new(LoxType::try_from(return_type)?),
                        args: match args {
                            Some((arg, _, _, args)) => {
                                let mut tokens = vec![arg.clone()];
                                tokens.extend(args.iter().map(|(_, x, _, _)| x.clone()));
                                tokens
                            }
                            None => vec![],
                        },
                        environment: *environment,
                    })),
                    &[name.clone()],
                )?;
                Ok(None)
            }
            Statement::ClassDecl(_, (_, name, _, _funcs, _)) => {
                environment.define(
                    name.lexeme.clone(),
                    Some(LoxType::Class(crate::lox_lib::LoxClass::UserDefined {
                        name: name.lexeme.clone(),
                    })),
                    &[name.clone()],
                )?;
                Ok(None)
            }
        }
    }
}

impl Interpret for Block {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Block::Block(_, (_, stmts, _)) = self;
        let mut environment = environment.enter_scope();
        let mut res = None;

        for (stmt,) in stmts {
            res = stmt.interpret(&mut environment)?;
        }

        Ok(res)
    }
}

impl Interpret for ConstDecl {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let ConstDecl::ConstDecl(_, (_, name, _, expr, _)) = self;
        let value = match expr.interpret(environment)? {
            Some(v) => v,
            None => return function_returned_nothing_error(expr.get_tokens()),
        };
        environment.define_const(name.lexeme.clone(), value, &[name.clone()])?;
        Ok(None)
    }
}

impl Interpret for ForStmt {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        match self {
            ForStmt::VarDecl(_, (_, _, var_decl, cond, _, inc, _, stmt)) => {
                var_decl.interpret(environment)?;
                let mut res = None;
                while match cond {
                    Some((cond,)) => is_truthy(match &cond.interpret(environment)? {
                        Some(v) => v,
                        None => return function_returned_nothing_error(cond.get_tokens()),
                    }),
                    None => true,
                } {
                    res = match stmt.interpret(environment) {
                        Ok(res) => res,
                        Err(e) => match e.get_type() {
                            Break => return Ok(res),
                            Continue => None,
                            _ => return Err(e),
                        },
                    };

                    if let Some((repeated,)) = inc {
                        repeated.interpret(environment)?;
                    }
                }
                Ok(res)
            }
            ForStmt::Expression(_, (_, _, expr, cond, _, inc, _, stmt)) => {
                expr.interpret(environment)?;
                let mut res = None;
                while match cond {
                    Some((cond,)) => is_truthy(match &cond.interpret(environment)? {
                        Some(v) => v,
                        None => return function_returned_nothing_error(cond.get_tokens()),
                    }),
                    None => true,
                } {
                    res = match stmt.interpret(environment) {
                        Ok(res) => res,
                        Err(e) => match e.get_type() {
                            Break => return Ok(res),
                            Continue => None,
                            _ => return Err(e),
                        },
                    };

                    if let Some((repeated,)) = inc {
                        repeated.interpret(environment)?;
                    }
                }
                Ok(res)
            }
            ForStmt::None(_, (_, _, _, cond, _, inc, _, stmt)) => {
                let mut res = None;
                while match cond {
                    Some((cond,)) => is_truthy(match &cond.interpret(environment)? {
                        Some(v) => v,
                        None => return function_returned_nothing_error(cond.get_tokens()),
                    }),
                    None => true,
                } {
                    res = match stmt.interpret(environment) {
                        Ok(res) => res,
                        Err(e) => match e.get_type() {
                            Break => return Ok(res),
                            Continue => None,
                            _ => return Err(e),
                        },
                    };

                    if let Some((repeated,)) = inc {
                        repeated.interpret(environment)?;
                    }
                }
                Ok(res)
            }
        }
    }
}

impl Interpret for WhileStmt {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let WhileStmt::WhileStmt(_, (_, _, expr, _, stmt)) = self;
        let mut res = None;
        while is_truthy(match &expr.interpret(environment)? {
            Some(v) => v,
            None => return function_returned_nothing_error(expr.get_tokens()),
        }) {
            res = match stmt.interpret(environment) {
                Ok(res) => res,
                Err(e) => match e.get_type() {
                    Break => return Ok(res),
                    Continue => None,
                    _ => return Err(e),
                },
            };
        }
        Ok(res)
    }
}

impl Interpret for IfStmt {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let IfStmt::IfStmt(_, (_, _, expr, _, stmt, elifs, else_)) = self;
        if is_truthy(match &expr.interpret(environment)? {
            Some(v) => v,
            None => return function_returned_nothing_error(expr.get_tokens()),
        }) {
            return stmt.interpret(environment);
        } else {
            for (_, _, expr, _, stmt) in elifs {
                if is_truthy(match &expr.interpret(environment)? {
                    Some(v) => v,
                    None => return function_returned_nothing_error(expr.get_tokens()),
                }) {
                    return stmt.interpret(environment);
                }
            }

            if let Some((_, stmt)) = else_ {
                return stmt.interpret(environment);
            }
        }

        Ok(None)
    }
}

impl Interpret for ExprStmt {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let ExprStmt::ExprStmt(_, (expr, _)) = self;
        expr.interpret(environment)
    }
}

impl Interpret for PrintStmt {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let PrintStmt::PrintStmt(_, (_, expr, _)) = self;
        println!(
            "{}",
            match expr.interpret(environment)? {
                Some(v) => v,
                None => return function_returned_nothing_error(expr.get_tokens()),
            }
        );
        Ok(None)
    }
}

impl Interpret for VarDecl {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let VarDecl::VarDecl(_, (_, name, expr, _)) = self;
        let value = match expr {
            Some((_, expr)) => Some(match expr.interpret(environment)? {
                Some(v) => v,
                None => return function_returned_nothing_error(expr.get_tokens()),
            }),
            None => None,
        };
        environment.define(name.lexeme.clone(), value, &[name.clone()])?;
        Ok(None)
    }
}

impl Interpret for Expression {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        match self {
            Expression::Expression(_, (expr,)) => expr.interpret(environment),
            Expression::Assignment(_, (assignment,)) => assignment.interpret(environment),
        }
    }
}

impl Interpret for LogicOr {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let LogicOr::LogicOr(tokens, (left, args)) = self;
        let mut value = match left.interpret(environment)? {
            Some(value) => value,
            None => {
                if args.is_empty() {
                    return Ok(None);
                } else {
                    return function_returned_nothing_error(tokens);
                }
            }
        };
        if is_truthy(&value) {
            return Ok(Some(value));
        }
        for (_, right) in args {
            value = right.interpret(environment)?.unwrap();
            if is_truthy(&value) {
                return Ok(Some(value));
            }
        }
        Ok(Some(value))
    }
}

impl Interpret for LogicAnd {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let LogicAnd::LogicAnd(tokens, (left, args)) = self;
        let mut value = match left.interpret(environment)? {
            Some(value) => value,
            None => {
                if args.is_empty() {
                    return Ok(None);
                } else {
                    return function_returned_nothing_error(tokens);
                }
            }
        };
        if !is_truthy(&value) {
            return Ok(Some(value));
        }
        for (_, right) in args {
            value = right.interpret(environment)?.unwrap();
            if !is_truthy(&value) {
                return Ok(Some(value));
            }
        }
        Ok(Some(value))
    }
}

impl Interpret for Assignment {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Assignment::Assignment(_, (name, _, expr)) = self;
        let value = match expr.interpret(environment)? {
            Some(v) => v,
            None => return function_returned_nothing_error(expr.get_tokens()),
        };
        environment.assign_by(name, value.clone())?;
        Ok(Some(value))
    }
}

impl Interpret for Equality {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Equality::Equality(_, (node, args)) = self;
        let mut value = match node.interpret(environment)? {
            Some(value) => value,
            None => {
                if args.is_empty() {
                    return Ok(None);
                } else {
                    return function_returned_nothing_error(node.get_tokens());
                }
            }
        };
        let mut tokens = node.get_tokens().clone();
        for (op, right) in args {
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            let right = right.interpret(environment)?.unwrap();
            let (left, right) = coerce(value, right, &tokens)?;
            value = match (left, right) {
                (LoxType::Integer(l), LoxType::Integer(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => LoxType::Bool(l != r),
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => LoxType::Bool(l == r),
                    _ => unreachable!(),
                },
                (LoxType::Float(l), LoxType::Float(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => LoxType::Bool(l != r),
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => LoxType::Bool(l == r),
                    _ => unreachable!(),
                },
                (LoxType::String(l), LoxType::String(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => LoxType::Bool(l != r),
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => LoxType::Bool(l == r),
                    _ => unreachable!(),
                },
                (LoxType::Bool(l), LoxType::Bool(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => LoxType::Bool(l != r),
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => LoxType::Bool(l == r),
                    _ => unreachable!(),
                },
                (LoxType::Null, LoxType::Null) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => LoxType::Bool(false),
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => LoxType::Bool(true),
                    _ => unreachable!(),
                },
                (LoxType::Function(l), LoxType::Function(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => LoxType::Bool(l == r),
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => LoxType::Bool(l != r),
                    _ => unreachable!(),
                },
                (LoxType::Class(l), LoxType::Class(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::EqualEqual) => LoxType::Bool(l == r),
                    TokenType::OneTwoChar(OneTwoChar::BangEqual) => LoxType::Bool(l != r),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
        }
        Ok(Some(value))
    }
}

impl Interpret for Comparison {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Comparison::Comparison(_, (node, args)) = self;
        let mut value = match node.interpret(environment)? {
            Some(value) => value,
            None => {
                if args.is_empty() {
                    return Ok(None);
                } else {
                    return function_returned_nothing_error(node.get_tokens());
                }
            }
        };
        let mut tokens = node.get_tokens().clone();
        for (op, right) in args {
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            let right = right.interpret(environment)?.unwrap();
            let (left, right) = coerce(value, right, &tokens)?;
            value = match (left, right) {
                (LoxType::Integer(l), LoxType::Integer(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::Less) => LoxType::Bool(l < r),
                    TokenType::OneTwoChar(OneTwoChar::LessEqual) => LoxType::Bool(l <= r),
                    TokenType::OneTwoChar(OneTwoChar::Greater) => LoxType::Bool(l > r),
                    TokenType::OneTwoChar(OneTwoChar::GreaterEqual) => LoxType::Bool(l >= r),
                    _ => unreachable!(),
                },
                (LoxType::Float(l), LoxType::Float(r)) => match op.token_type {
                    TokenType::OneTwoChar(OneTwoChar::Less) => LoxType::Bool(l < r),
                    TokenType::OneTwoChar(OneTwoChar::LessEqual) => LoxType::Bool(l <= r),
                    TokenType::OneTwoChar(OneTwoChar::Greater) => LoxType::Bool(l > r),
                    TokenType::OneTwoChar(OneTwoChar::GreaterEqual) => LoxType::Bool(l >= r),
                    _ => unreachable!(),
                },
                (LoxType::String(_), LoxType::String(_)) => {
                    err(&tokens, "Can't compare string to string")?
                }
                (LoxType::Bool(_), LoxType::Bool(_)) => err(&tokens, "Can't compare bool to bool")?,
                (LoxType::Null, LoxType::Null) => err(&tokens, "Can't compare null to null")?,
                (LoxType::Function(_), LoxType::Function(_)) => {
                    err(&tokens, "Can't compare functions")?
                }
                (LoxType::Class(_), LoxType::Class(_)) => err(&tokens, "Can't compare classes")?,
                _ => unreachable!(),
            };
        }
        Ok(Some(value))
    }
}

impl Interpret for Term {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Term::Term(_, (node, args)) = self;
        let mut value = match node.interpret(environment)? {
            Some(value) => value,
            None => {
                if args.is_empty() {
                    return Ok(None);
                } else {
                    return function_returned_nothing_error(node.get_tokens());
                }
            }
        };
        let mut tokens = node.get_tokens().clone();
        for (op, right) in args {
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            let right = right.interpret(environment)?.unwrap();
            let (left, right) = coerce(value, right, &tokens)?;
            value = match (left, right) {
                (LoxType::Integer(l), LoxType::Integer(r)) => match op.token_type {
                    TokenType::OneChar(OneChar::Plus) => LoxType::Integer(l + r),
                    TokenType::OneChar(OneChar::Minus) => LoxType::Integer(l - r),
                    _ => unreachable!(),
                },
                (LoxType::Float(l), LoxType::Float(r)) => match op.token_type {
                    TokenType::OneChar(OneChar::Plus) => LoxType::Float(l + r),
                    TokenType::OneChar(OneChar::Minus) => LoxType::Float(l - r),
                    _ => unreachable!(),
                },
                (LoxType::String(l), LoxType::String(r)) => match op.token_type {
                    TokenType::OneChar(OneChar::Plus) => LoxType::String(l + &r),
                    TokenType::OneChar(OneChar::Minus) => {
                        err(&tokens, "Can't subtract a string from another string")?
                    }
                    _ => unreachable!(),
                },
                (LoxType::Bool(_), LoxType::Bool(_)) => {
                    err(&tokens, "Can't add or subtruct bool and bool")?
                }
                (LoxType::Null, LoxType::Null) => {
                    err(&tokens, "Can't add or subtract null by null")?
                }
                (LoxType::Function(_), LoxType::Function(_)) => {
                    err(&tokens, "Can't add or subtract functions")?
                }
                (LoxType::Class(_), LoxType::Class(_)) => {
                    err(&tokens, "Can't add or subtract functions")?
                }
                _ => unreachable!(),
            };
        }
        Ok(Some(value))
    }
}

impl Interpret for Factor {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Factor::Factor(_, (node, args)) = self;
        let mut value = match node.interpret(environment)? {
            Some(value) => value,
            None => {
                if args.is_empty() {
                    return Ok(None);
                } else {
                    return function_returned_nothing_error(node.get_tokens());
                }
            }
        };
        let mut tokens = node.get_tokens().clone();
        for (op, right) in args {
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            let right = right.interpret(environment)?.unwrap();
            let (left, right) = coerce(value, right, &tokens)?;
            value = match (left, right) {
                (LoxType::Integer(l), LoxType::Integer(r)) => match op.token_type {
                    TokenType::OneChar(OneChar::Star) => LoxType::Integer(l * r),
                    TokenType::Special(Special::Slash) => LoxType::Integer(l / r),
                    _ => unreachable!(),
                },
                (LoxType::Float(l), LoxType::Float(r)) => match op.token_type {
                    TokenType::OneChar(OneChar::Star) => LoxType::Float(l * r),
                    TokenType::Special(Special::Slash) => LoxType::Float(l / r),
                    _ => unreachable!(),
                },
                (LoxType::String(_), LoxType::String(_)) => {
                    err(&tokens, "Can't multiply or divide a string by a string")?
                }
                (LoxType::Bool(_), LoxType::Bool(_)) => {
                    err(&tokens, "Can't multiply or divide a bool by a bool")?
                }
                (LoxType::Null, LoxType::Null) => {
                    err(&tokens, "Can't multiply or divide null by null")?
                }
                (LoxType::Function(_), LoxType::Function(_)) => {
                    err(&tokens, "Can't multiply or divide functions")?
                }
                (LoxType::Class(_), LoxType::Class(_)) => {
                    err(&tokens, "Can't multiply or divide classes")?
                }
                _ => unreachable!(),
            };
        }
        Ok(Some(value))
    }
}

impl Interpret for Unary {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let Unary::Unary(tokens, (args, node)) = self;
        let mut value = match node.interpret(environment)? {
            Some(value) => value,
            None => {
                if args.is_empty() {
                    return Ok(None);
                } else {
                    return function_returned_nothing_error(node.get_tokens());
                }
            }
        };
        for arg in args {
            match arg.0.token_type.clone() {
                TokenType::OneChar(OneChar::Minus) => {
                    is_number(&value, tokens, "Can't negate a {}")?;
                    match value {
                        LoxType::Integer(int) => value = LoxType::Integer(-int),
                        LoxType::Float(f) => value = LoxType::Float(-f),
                        _ => unreachable!(),
                    }
                }
                TokenType::OneChar(OneChar::Plus) => {
                    is_number(&value, tokens, "Can't use unary operator on a {}")?
                }
                TokenType::OneTwoChar(OneTwoChar::Bang) => {
                    value = LoxType::Bool(!is_truthy(&value));
                }
                _ => unreachable!(),
            }
        }
        Ok(Some(value))
    }
}

impl Interpret for FuncCall {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let FuncCall::FuncCall(_, (primary, calls)) = self;
        let mut tokens = primary.get_tokens().clone();
        let mut res = primary.interpret(environment)?;
        for (func_call,) in calls {
            tokens.extend(func_call.get_tokens().clone());
            match func_call.as_ref() {
                FuncCall2::Call(_, (_, args, _)) => {
                    if let Some(LoxType::Function(ref mut func)) = res {
                        let args = match args {
                            Some((args,)) => match args.as_ref() {
                                Arguments::Arguments(_, (expr, exprs)) => {
                                    let mut args = vec![expr.interpret(environment)?.unwrap()];
                                    for (_, expr) in exprs {
                                        args.push(expr.interpret(environment)?.unwrap());
                                    }
                                    args
                                }
                            },
                            None => vec![],
                        };
                        res = match func.call(args, &tokens) {
                            Ok(result) => result,
                            Err(e) => match e.get_type() {
                                Return(result) => result.clone(),
                                _ => return Err(e),
                            },
                        };
                    } else if let Some(LoxType::Class(ref mut class)) = res {
                        res = match class.call(vec![], &tokens) {
                            Ok(result) => result,
                            Err(e) => match e.get_type() {
                                Return(result) => result.clone(),
                                _ => return Err(e),
                            },
                        }
                    } else {
                        return Err(Error::new(
                            RunTime,
                            &tokens,
                            res.map_or(String::from("Function returned nothing"), |res| {
                                format!("Can't call a {}", res.get_serializations()[0])
                            })
                            .as_str(),
                            "Type",
                        ));
                    }
                }
                FuncCall2::Property(_, (_, ident)) => todo!(),
            }
        }
        Ok(res)
    }
}

impl Interpret for Primary {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        match self {
            Primary::Literal(_, (lit,)) => match lit.token_type.clone() {
                TokenType::Literal(Literal::String(str)) => Ok(Some(LoxType::String(str))),
                TokenType::Literal(Literal::Integer(int)) => Ok(Some(LoxType::Integer(int))),
                TokenType::Literal(Literal::Float(float)) => Ok(Some(LoxType::Float(float))),
                TokenType::KeyWord(KeyWord::Null) => Ok(Some(LoxType::Null)),
                TokenType::KeyWord(KeyWord::False) => Ok(Some(LoxType::Bool(false))),
                TokenType::KeyWord(KeyWord::True) => Ok(Some(LoxType::Bool(true))),
                TokenType::Literal(Literal::Identifier(_)) => {
                    Ok(Some(environment.get_by(lit)?.clone()))
                }
                _ => unreachable!(),
            },
            Primary::Parenthesized(_, (_, expr, _)) => expr.interpret(environment),
            Primary::Func(_, (_, func)) => func.interpret(environment),
        }
    }
}

impl Interpret for FuncDecl {
    fn interpret(&self, environment: &mut Scope) -> Result<Option<LoxType>, Error> {
        let FuncDecl::FuncDecl(_, (return_type, _, arg_decl, _, block)) = self;
        let environment = environment.enter_scope();
        // let env = environment.clone();
        // drop(dropguard);
        Ok(Some(LoxType::Function(LoxFunction::UserDefined {
            name: String::from("lambda"),
            block: block.clone(),
            return_type: Box::new(LoxType::try_from(return_type)?),
            args: match arg_decl.as_ref() {
                ArgDecl::ArgDecl(_, (args,)) => match args {
                    Some((arg, _, _, args)) => {
                        let mut tokens = vec![arg.clone()];
                        tokens.extend(args.iter().map(|(_, x, _, _)| x.clone()));
                        tokens
                    }
                    None => vec![],
                },
            },
            environment,
        })))
    }
}

fn coerce(left: LoxType, right: LoxType, tokens: &[Token]) -> Result<(LoxType, LoxType), Error> {
    match left {
        LoxType::Integer(l) => match right {
            LoxType::Integer(_) => Ok((left, right)),
            LoxType::Float(_) => Ok((LoxType::Float(l as f64), right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on int and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Float(_) => match right {
            LoxType::Integer(i) => Ok((left, LoxType::Float(i as f64))),
            LoxType::Float(_) => Ok((left, right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on float and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::String(_) => match right {
            LoxType::String(_) => Ok((left, right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on string and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Bool(l) => match right {
            LoxType::Bool(_) => Ok((left, right)),
            LoxType::String(_) => Ok((LoxType::String(l.to_string()), right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on bool and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Null => match right {
            LoxType::Null => Ok((left, right)),
            LoxType::String(_) => Ok((LoxType::String(String::from("null")), right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on null and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Function(ref l) => match right {
            LoxType::String(_) => Ok((LoxType::String(l.to_string()), right)),
            LoxType::Function(_) => Ok((left, right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on function and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Class(ref l) => match right {
            LoxType::String(_) => Ok((LoxType::String(l.to_string()), right)),
            LoxType::Class(_) => Ok((left, right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on class and ") + right.get_serializations()[0]),
            ),
        },
        LoxType::Instance(ref l) => match right {
            LoxType::String(_) => Ok((LoxType::String(l.to_string()), right)),
            LoxType::Instance(_) => Ok((left, right)),
            _ => err(
                tokens,
                &(String::from("Can't operate on instance and ") + right.get_serializations()[0]),
            ),
        },
    }
}

fn err<'a, T>(tokens: &'a [Token], msg: &'a str) -> Result<T, Error> {
    Err(Error::new(RunTime, tokens, msg, "Type"))
}

fn is_number<'a>(lt: &'a LoxType, tokens: &'a [Token], msg: &'a str) -> Result<(), Error> {
    match lt {
        LoxType::String(_) => err(tokens, &msg.replacen("{}", "string", 1)),
        LoxType::Bool(_) => err(tokens, &msg.replacen("{}", "bool", 1)),
        LoxType::Null => err(tokens, &msg.replacen("{}", "null", 1)),
        LoxType::Function(_) => err(tokens, &msg.replacen("{}", "function", 1)),
        LoxType::Class(_) => err(tokens, &msg.replacen("{}", "class", 1)),
        _ => Ok(()),
    }
}

fn is_truthy(lt: &LoxType) -> bool {
    match lt {
        LoxType::Integer(i) => *i != 0,
        LoxType::Float(f) => *f != 0.0,
        LoxType::String(s) => !s.is_empty(),
        LoxType::Bool(b) => *b,
        LoxType::Null => false,
        LoxType::Function(_) => true,
        LoxType::Class(_) => true,
        LoxType::Instance(_) => true,
    }
}
