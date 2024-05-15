use crate::{
    environment::Scope,
    error::{
        Error,
        ErrorType::{Analyze, RunTime},
    },
    input,
    interpreter::Interpret,
    nodes::Block,
    token::Token,
};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    io::{stdin, stdout, Write},
    time::SystemTime,
};

pub const TIME: LoxFunction = LoxFunction::BuildIn {
    name: "time",
    arg_count: 0,
    return_type: &LoxType::Float(0.),
    func: |_, _| {
        Ok(Some(LoxType::Float(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        )))
    },
};
pub const INPUT: LoxFunction = LoxFunction::BuildIn {
    name: "input",
    arg_count: 1,
    return_type: &LoxType::String(String::new()),
    func: |_, args| {
        let prompt = &args[0];
        cast(&LoxType::String(Default::default()), prompt, &[])?; // TODO: insert tokens
        let prompt = match prompt {
            LoxType::String(prompt) => prompt,
            _ => unreachable!(),
        };
        Ok(Some(LoxType::String(input!("{prompt}"))))
    },
};
pub const INT: LoxFunction = LoxFunction::BuildIn {
    name: "int",
    arg_count: 1,
    return_type: &LoxType::Integer(0),
    func: |_, args| {
        Ok(Some(match &args[0] {
            LoxType::Integer(i) => LoxType::Integer(*i),
            LoxType::Float(f) => LoxType::Integer(*f as i64),
            LoxType::String(s) => LoxType::Integer(s.parse::<i64>().map_err(|_| {
                Error::new(
                    RunTime,
                    &[], // TODO: insert tokens
                    format!("Failed to cast string '{s}' to int").as_str(),
                    "Cast",
                )
            })?),
            LoxType::Bool(b) => LoxType::Integer(*b as i64),
            LoxType::Function(_) => Err(Error::new(
                RunTime,
                &[],
                "Can't cast a function to int",
                "Cast",
            ))?,
            LoxType::Class(_) => Err(Error::new(
                RunTime,
                &[],
                "Can't cast a class to int",
                "Cast",
            ))?,
            LoxType::Null => Err(Error::new(RunTime, &[], "Can't cast null to int", "Cast"))?,
            LoxType::Instance(_) => Err(Error::new(
                RunTime,
                &[],
                "Can't cast an object to int",
                "Cast",
            ))?,
        }))
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxClass {
    BuildIn { name: String },
    UserDefined { name: String },
}

impl Default for LoxClass {
    fn default() -> Self {
        Self::BuildIn {
            name: Default::default(),
        }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxClass::BuildIn { name, .. } => {
                write!(f, "<build in class {name}>")
            }
            LoxClass::UserDefined { name, .. } => write!(f, "<class {name}>"),
        }
    }
}

impl LoxCallable for LoxClass {
    fn call(&mut self, _args: Vec<LoxType>, _tokens: &[Token]) -> Result<Option<LoxType>, Error> {
        Ok(Some(LoxType::Instance(LoxInstance::new(self))))
    }

    fn arity(&self) -> usize {
        0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxInstance {
    class: LoxClass,
    fields: HashMap<Token, LoxType>,
}

impl LoxInstance {
    pub fn new(class: &LoxClass) -> Self {
        Self {
            class: class.clone(),
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, token: &Token) -> Option<&LoxType> {
        self.fields.get(token)
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "'{}' instace",
            match &self.class {
                LoxClass::BuildIn { name } => name,
                LoxClass::UserDefined { name, .. } => name,
            }
        )
    }
}

pub trait LoxCallable {
    fn call(&mut self, args: Vec<LoxType>, tokens: &[Token]) -> Result<Option<LoxType>, Error>;
    fn arity(&self) -> usize;
}

#[derive(Clone)]
pub enum LoxFunction {
    BuildIn {
        name: &'static str,
        arg_count: u32,
        return_type: &'static LoxType,
        func: for<'b> fn(&'b mut Scope, Vec<LoxType>) -> Result<Option<LoxType>, Error>,
    },
    UserDefined {
        name: String,
        args: Vec<Token>,
        return_type: Box<LoxType>,
        environment: Scope,
        block: Box<Block>,
    },
}

// TODO: why is this needed?
impl Default for LoxFunction {
    fn default() -> Self {
        Self::BuildIn {
            name: "",
            arg_count: 0,
            return_type: &LoxType::Null,
            func: |_, _| Ok(None),
        }
    }
}

impl Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxFunction::BuildIn {
                name, arg_count, ..
            } => {
                write!(
                    f,
                    "LoxCallable(BuildIn{{name: {name}, arg_count: {arg_count}}})"
                )
            }
            LoxFunction::UserDefined { name, args, .. } => write!(
                f,
                "LoxCallable(UserDefined{{name: {name}, args: {:?}, block: ...)}}",
                args
            ),
        }
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                LoxFunction::BuildIn { name, .. },
                LoxFunction::BuildIn {
                    name: other_name, ..
                },
            ) => name == other_name,
            (
                LoxFunction::UserDefined { name, args, .. },
                LoxFunction::UserDefined {
                    name: other_name,
                    args: other_args,
                    ..
                },
            ) => name == other_name && args.iter().eq(other_args),
            _ => false,
        }
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxFunction::BuildIn { name, .. } => {
                write!(f, "<build in func {name}>")
            }
            LoxFunction::UserDefined { name, .. } => write!(f, "<func {name}>"),
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&mut self, args: Vec<LoxType>, tokens: &[Token]) -> Result<Option<LoxType>, Error> {
        match self {
            LoxFunction::BuildIn { func, .. } => func(&mut Scope::new(), args),
            LoxFunction::UserDefined {
                block,
                args: arg_names,
                environment,
                ..
            } => {
                if args.len() != arg_names.len() {
                    return Err(Error::new(
                        RunTime,
                        tokens,
                        format!(
                            "Expected {} arguments but found {}",
                            arg_names.len(),
                            args.len()
                        )
                        .as_str(),
                        "Type",
                    ));
                }
                let mut scope = environment.enter_scope();
                for (arg_name, value) in arg_names.iter().zip(args) {
                    scope.define(arg_name.lexeme.clone(), Some(value), &[arg_name.clone()])?;
                }
                block.interpret(&mut scope)
            }
        }
    }

    fn arity(&self) -> usize {
        match self {
            LoxFunction::BuildIn { arg_count, .. } => *arg_count as usize,
            LoxFunction::UserDefined { args, .. } => args.len(),
        }
    }
}

#[derive(strum_macros::EnumMessage, PartialEq, Debug, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum LoxType {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Function(LoxFunction),
    Class(LoxClass),
    Instance(LoxInstance),
    Null,
}

impl Display for LoxType {
    fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxType::Integer(i) => write!(w, "{i}"),
            LoxType::Float(f) => write!(w, "{f:?}"),
            LoxType::String(s) => write!(w, "{s}"),
            LoxType::Bool(b) => write!(w, "{b}"),
            LoxType::Null => write!(w, "null"),
            LoxType::Function(func) => write!(w, "{func}"),
            LoxType::Class(class) => write!(w, "{class}"),
            LoxType::Instance(inst) => write!(w, "{inst}"),
        }
    }
}

impl LoxType {
    pub fn get_type(&self) -> String {
        match self {
            LoxType::Integer(_) => "int".into(),
            LoxType::Float(_) => "float".into(),
            LoxType::String(_) => "str".into(),
            LoxType::Bool(_) => "bool".into(),
            LoxType::Function(_) => "func".into(),
            LoxType::Class(_) => "class".into(),
            LoxType::Instance(inst) => match &inst.class {
                LoxClass::BuildIn { name } => name.clone(),
                LoxClass::UserDefined { name, .. } => name.clone(),
            },
            LoxType::Null => "null".into(),
        }
    }

    pub fn get_python_type(&self) -> String {
        match self {
            LoxType::Integer(_) => "int",
            LoxType::Float(_) => "float",
            LoxType::String(_) => "str",
            LoxType::Bool(_) => "bool",
            LoxType::Function(_) => "typing.Callable",
            LoxType::Class(_) => "typing.Type",
            LoxType::Instance(_) => "object",
            LoxType::Null => "None",
        }
        .into()
    }
}

impl TryFrom<&Token> for LoxType {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.lexeme.as_str() {
            "int" => Ok(Self::Integer(Default::default())),
            "float" => Ok(Self::Float(Default::default())),
            "void" => Ok(Self::Null),
            "str" => Ok(Self::String(Default::default())),
            "bool" => Ok(Self::Bool(Default::default())),
            "Function" => Ok(Self::Function(Default::default())),
            "class" => Ok(Self::Class(Default::default())),
            _ => Err(Error::new(
                Analyze,
                &[token.clone()],
                format!("'{}' is not a type", token.lexeme).as_str(),
                "Syntax",
            )),
        }
    }
}

fn cast(target: &LoxType, lt: &LoxType, tokens: &[Token]) -> Result<LoxType, Error> {
    match (target, lt) {
        (LoxType::Integer(_), LoxType::Integer(i)) => Ok(LoxType::Integer(*i)),
        (LoxType::Integer(_), LoxType::Float(f)) => Ok(LoxType::Integer(*f as i64)),

        (LoxType::Float(_), LoxType::Integer(i)) => Ok(LoxType::Float(*i as f64)),
        (LoxType::Float(_), LoxType::Float(f)) => Ok(LoxType::Float(*f)),

        (LoxType::String(_), LoxType::Integer(i)) => Ok(LoxType::String(i.to_string())),
        (LoxType::String(_), LoxType::Float(f)) => Ok(LoxType::String(f.to_string())),
        (LoxType::String(_), LoxType::Bool(b)) => Ok(LoxType::String(b.to_string())),
        (LoxType::String(_), LoxType::Function(f)) => Ok(LoxType::String(f.to_string())),
        (LoxType::String(_), LoxType::Null) => Ok(LoxType::String(String::from("null"))),
        (LoxType::String(_), LoxType::String(s)) => Ok(LoxType::String(s.clone())),

        (LoxType::Bool(_), _) => Ok(LoxType::Bool(is_truthy(lt))),

        (LoxType::Function(_), LoxType::Function(f)) => Ok(LoxType::Function(f.clone())),

        (LoxType::Class(_), LoxType::Class(c)) => Ok(LoxType::Class(c.clone())),

        (LoxType::Null, _) => Ok(LoxType::Null),

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
