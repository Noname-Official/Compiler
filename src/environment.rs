use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
};

use crate::{
    error::{Error, ErrorType::Analyze},
    lox_lib::{LoxFunction, LoxType},
    lox_lib::{INPUT, INT, TIME},
    token::Token,
};

const BUILD_IN_NAMES: [&str; 3] = [
    match TIME {
        LoxFunction::BuildIn { name, .. } => name,
        _ => unreachable!(),
    },
    "input",
    match INT {
        LoxFunction::BuildIn { name, .. } => name,
        _ => unreachable!(),
    },
];
const BUILD_IN_FUNCS: [LoxType; 3] = [
    LoxType::Function(TIME),
    LoxType::Function(INPUT),
    LoxType::Function(INT),
];

thread_local! {
// lazy_static::lazy_static! {
    static SCOPES: RefCell<Vec<_Scope>> = const { RefCell::new(Vec::new()) };
    static DISTS: RefCell<Dists> = RefCell::new(Dists::new());

// static SCOPES: RefCell<Vec<_Scope>> = Vec::new().into();
// static DISTS: RefCell<Dists> = Dists::new().into();
}

#[derive(Clone, Debug)]
struct _Scope {
    id: usize,
    parent_id: Option<usize>,
    values: HashMap<String, Option<LoxType>>,
    consts: HashMap<String, LoxType>,
}

#[derive(Clone, Copy)]
pub struct Scope {
    id: usize,
}

macro_rules! impl_functions {
    ($($vis:vis fn $ident:ident ( &self $(, $v:ident: $t:ty)* $(,)? )$( -> $ret_type:ty)?;)*) => {
        $(
            $vis fn $ident(&mut self $(, $v: $t)*)$(->$ret_type)? {
                #[allow(unused_mut)]
                let mut scope = SCOPES.with_borrow(|scopes| scopes[self.id].clone());
                let res = scope.$ident($($v),*);
                SCOPES.with_borrow_mut(|scopes| scopes[self.id] = scope);
                res
            }
        )*
    };
}

impl Scope {
    pub fn new() -> Self {
        let id = _Scope::new();
        Scope { id }
    }

    pub fn clear() {
        SCOPES.set(Vec::new());
        Scope::new();
    }

    pub fn enter_scope(&self) -> Self {
        let scope = Self::new();
        SCOPES.with_borrow_mut(|s| s[scope.id].parent_id = Some(self.id));
        scope
    }

    impl_functions! {
        pub fn assign_by(&self, token: &Token, value: LoxType) -> Result<(), Error>;
        pub fn define_dist(&self, token: Token) -> Result<usize, Error>;
        pub fn get_by(&self, token: &Token) -> Result<LoxType, Error>;
        pub fn define(
            &self,
            name: String,
            value: Option<LoxType>,
            tokens: &[Token],
        ) -> Result<(), Error>;
        pub fn define_const(
            &self,
            name: String,
            value: LoxType,
            tokens: &[Token],
        ) -> Result<(), Error>;
    }
}

impl _Scope {
    #[allow(clippy::new_ret_no_self)]
    fn new() -> usize {
        let scope = Self {
            id: SCOPES.with_borrow(|s| s.len()),
            parent_id: None,
            values: HashMap::new(),
            consts: HashMap::new(),
        };
        let scope_id = scope.id;
        SCOPES.with_borrow_mut(|s| s.push(scope));
        scope_id
    }

    fn get_dist(&self, token: &Token) -> Result<usize, Error> {
        DISTS.with_borrow(|d| {
            d.dists
                .get(token)
                .ok_or(Error::new(
                    Analyze,
                    &[token.clone()],
                    &format!("'{}' doesn't exist", token.lexeme),
                    "Undefined",
                ))
                .cloned()
        })
    }

    fn get_dist_recursive(&self, token: &Token) -> Result<usize, Error> {
        if self.contains(&token.lexeme) {
            Ok(0)
        } else {
            SCOPES.with_borrow(|s| {
                Ok(s[self.parent_id.ok_or(Error::new(
                    Analyze,
                    &[token.clone()],
                    &format!("'{}' doens't exist", token.lexeme),
                    "Undefined",
                ))?]
                .get_dist_recursive(token)?
                    + 1)
            })
        }
    }

    fn define_dist(&mut self, token: Token) -> Result<usize, Error> {
        let dist = self.get_dist_recursive(&token);
        if let Ok(dist) = dist {
            DISTS.with_borrow_mut(|d| d.dists.insert(token.clone(), dist));
        }
        dist.map_err(|_| {
            Error::new(
                Analyze,
                &[token.clone()],
                &format!("Undefined variable '{}'", token.lexeme),
                "Undefined",
            )
        })
    }

    fn get_by(&self, token: &Token) -> Result<LoxType, Error> {
        self.get_at(&token.lexeme, self.get_dist(token)?, &[token.clone()])
    }

    fn get_at(&self, name: &String, dist: usize, tokens: &[Token]) -> Result<LoxType, Error> {
        if dist == 0 {
            self.get(name, tokens)
        } else {
            SCOPES.with_borrow(|s| s[self.parent_id.unwrap()].get_at(name, dist - 1, tokens))
        }
    }

    fn get(&self, name: &String, tokens: &[Token]) -> Result<LoxType, Error> {
        if BUILD_IN_NAMES.contains(&name.as_str()) {
            return Ok(
                BUILD_IN_FUNCS[BUILD_IN_NAMES.iter().position(|&x| x == name).unwrap()].clone(),
            );
        }
        if let Some(value) = self.consts.get(name) {
            return Ok(value.clone());
        }
        match self.values.get(name) {
            Some(value) => match value {
                Some(value) => Ok(value.clone()),
                None => Err(Error::new(
                    Analyze,
                    tokens,
                    format!("Unitialized variable '{name}'").as_str(),
                    "Unitialized",
                )),
            },
            None => Err(Error::new(
                Analyze,
                tokens,
                format!("Undefined variable '{name}'").as_str(),
                "Undefined",
            )),
        }
    }

    fn assign_by(&mut self, token: &Token, value: LoxType) -> Result<(), Error> {
        let dist = self.get_dist(token)?;
        self.assign_at(token.lexeme.clone(), value, dist, &[token.clone()])
    }

    fn assign_at(
        &mut self,
        name: String,
        value: LoxType,
        dist: usize,
        tokens: &[Token],
    ) -> Result<(), Error> {
        if dist == 0 {
            self.assign(name, value, tokens)
        } else {
            let mut scope = SCOPES.with_borrow_mut(|s| s[self.parent_id.unwrap()].clone());
            let res = scope.assign_at(name, value, dist - 1, tokens);
            SCOPES.with_borrow_mut(|s| s[self.parent_id.unwrap()] = scope);
            res
        }
    }

    fn assign(&mut self, name: String, value: LoxType, tokens: &[Token]) -> Result<(), Error> {
        if BUILD_IN_NAMES.contains(&name.as_str()) || self.consts.contains_key(&name) {
            return Err(Error::new(
                Analyze,
                tokens,
                format!("'{name}' already exists").as_str(),
                "Defined",
            ));
        }
        match self.values.entry(name.clone()) {
            Entry::Occupied(mut entry) => {
                entry.insert(Some(value));
                Ok(())
            }
            Entry::Vacant(_) => Err(Error::new(
                Analyze,
                tokens,
                format!("Undefined variable '{name}'").as_str(),
                "Undefined",
            )),
        }
    }

    fn define(
        &mut self,
        name: String,
        value: Option<LoxType>,
        tokens: &[Token],
    ) -> Result<(), Error> {
        if BUILD_IN_NAMES.contains(&name.as_str()) || self.consts.contains_key(&name) {
            return Err(Error::new(
                Analyze,
                tokens,
                format!("'{name}' is already defined as a constant").as_str(),
                "Defined",
            ));
        }
        self.values.insert(name, value);
        Ok(())
    }

    fn define_const(
        &mut self,
        name: String,
        value: LoxType,
        tokens: &[Token],
    ) -> Result<(), Error> {
        if BUILD_IN_NAMES.contains(&name.as_str()) || self.consts.contains_key(&name) {
            return Err(Error::new(
                Analyze,
                tokens,
                format!("'{name}' is already defined as a constant").as_str(),
                "Defined",
            ));
        }
        self.consts.insert(name, value);
        Ok(())
    }

    fn contains(&self, name: &String) -> bool {
        BUILD_IN_NAMES.contains(&name.as_str())
            || self.consts.contains_key(name)
            || self.values.contains_key(name)
    }
}

#[derive(Debug, Clone)]
pub struct Dists {
    pub dists: HashMap<Token, usize>,
}

impl Dists {
    fn new() -> Self {
        Self {
            dists: HashMap::new(),
        }
    }
}
