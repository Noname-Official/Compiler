#![allow(clippy::type_complexity)]

use crate::token::Token;
use cps::cps;

#[allow(unused_macros)]
macro_rules! define_type {
    ($name:ident = $($input:tt)*) => {
        define_type!(@internal_enum $name {} {} $($input)*);
    };


    (
        @internal_enum_type_parenthesized_token_type
        $name:ident
        $type_name:ident
        {$($containers:ident)*}
        {$($call_stack:ident)*}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {$($parenthesized_types:tt)*}
        {$($rest:tt)*}
        | $token_type:ident :: $sub_type:ident
        $($parenthesized:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized_token_type
            $name
            $type_name
            {$($containers)*}
            {$($call_stack)*}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {$($parenthesized_types)*}
            {$($rest)*}
            $($parenthesized)*
        );
    };
    (
        @internal_enum_type_parenthesized_token_type
        $name:ident
        $type_name:ident
        {$($containers:ident)*}
        {$($call_stack:ident)*}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {$($parenthesized_types:tt)*}
        {$($rest:tt)*}
        $($parenthesized:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized
            $name
            $type_name
            {$($containers)*}
            {$($call_stack)*}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {$($parenthesized_types)*}
            {$($rest)*}
            $($parenthesized)*
        );
    };


    (
        @internal_enum_type_parenthesized
        $name:ident
        $type_name:ident
        {$($containers:ident)*}
        {$($call_stack:ident)*}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {{$($nested_parenthesized_types:tt)*} $($parenthesized_types:tt)*}
        {$($rest:tt)*}
        $(,)?
        $token_type:ident :: $sub_type:ident
        $($parenthesized:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized_token_type
            $name
            $type_name
            {$($containers)*}
            {$($call_stack)*}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {{$($nested_parenthesized_types)* Token,} $($parenthesized_types)*}
            {$($rest)*}
            $($parenthesized)*
        );
    };
    (
        @internal_enum_type_parenthesized
        $name:ident
        $type_name:ident
        {$($containers:ident)*}
        {$($call_stack:ident)*}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {{$($nested_parenthesized_types:tt)*} $($parenthesized_types:tt)*}
        {$($rest:tt)*}
        $(,)?
        $boxed_type:ident
        $($parenthesized:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized
            $name
            $type_name
            {$($containers)*}
            {$($call_stack)*}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {{$($nested_parenthesized_types)* Box<$boxed_type>,} $($parenthesized_types)*}
            {$($rest)*}
            $($parenthesized)*
        );
    };
    (
        @internal_enum_type_parenthesized
        $name:ident
        $type_name:ident
        {$($containers:ident)*}
        {$($call_stack:ident)*}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {$($parenthesized_types:tt)*}
        {$($rest:tt)*}
        $(,)?
        ($($nested_parenthesized:tt)*)*
        $($parenthesized:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized
            $name
            $type_name
            {Vec $($containers)*}
            {internal_enum_type_parenthesized $($call_stack)*}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {{} $($parenthesized_types)*}
            {{$($rest)*} $($parenthesized)*}
            $($nested_parenthesized)*
        );
    };
    (
        @internal_enum_type_parenthesized
        $name:ident
        $type_name:ident
        {$($containers:ident)*}
        {$($call_stack:ident)*}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {$($parenthesized_types:tt)*}
        {$($rest:tt)*}
        $(,)?
        ($($nested_parenthesized:tt)*)?
        $($parenthesized:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized
            $name
            $type_name
            {Option $($containers)*}
            {internal_enum_type_parenthesized $($call_stack)*}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {{} $($parenthesized_types)*}
            {{$($rest)*} $($parenthesized)*}
            $($nested_parenthesized)*
        );
    };
    (
        @internal_enum_type_parenthesized
        $name:ident
        $type_name:ident
        {$container:ident $($containers:ident)+}
        {$next_call:ident $($call_stack:ident)+}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {{$($nested_parenthesized_types:tt)*} {$($next_parenthesized_types:tt)*} $($parenthesized_types:tt)*}
        {$($rest:tt)*}
    ) => {
        define_type!(
            @$next_call
            $name
            $type_name
            {$($containers)*}
            {$($call_stack)*}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {{$($next_parenthesized_types)* $container<($($nested_parenthesized_types)*)>,} $($parenthesized_types)*}
            $($rest)*
        );
    };
    (
        @internal_enum_type_parenthesized
        $name:ident
        $type_name:ident
        {$container:ident}
        {$next_call:ident}
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        {{$($parenthesized_types:tt)*}}
        {$($rest:tt)*}
    ) => {
        define_type!(
            @$next_call
            $name
            $type_name
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)* $container<($($parenthesized_types)*)>,}
            $($rest)*
        );
    };


    (
        @internal_enum_type_token_type
        $name:ident
        $type_name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        | $token_type:ident :: $sub_type:ident
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum_type_token_type
            $name
            $type_name
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            $($rest)*
        );
    };
    (
        @internal_enum_type_token_type
        $name:ident
        $type_name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum_type
            $name
            $type_name
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            $($rest)*
        );
    };

    (
        @internal_enum_type
        $name:ident
        $type_name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        $(,)?
        $token_type:ident :: $sub_type:ident
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum_type_token_type
            $name
            $type_name
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)* Token,}
            $($rest)*
        );
    };
    (
        @internal_enum_type
        $name:ident
        $type_name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        $(,)?
        ($($optional:tt)*)?
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized
            $name
            $type_name
            {Option}
            {internal_enum_type}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {{}}
            {$($rest)*}
            $($optional)*
        );
    };
    (
        @internal_enum_type
        $name:ident
        $type_name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        $(,)?
        ($($repeated:tt)*)*
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum_type_parenthesized
            $name
            $type_name
            {Vec}
            {internal_enum_type}
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)*}
            {{}}
            {$($rest)*}
            $($repeated)*
        );
    };
    (
        @internal_enum_type
        $name:ident
        $type_name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        $(,)?
        $boxed_type:ident
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum_type
            $name
            $type_name
            {$($enum_types)*}
            {$($match_arms)*}
            {$($enum_type_args)* Box<$boxed_type>,}
            $($rest)*
        );
    };
    (
        @internal_enum_type
        $name:ident
        $type_name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        {$($enum_type_args:tt)*}
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum
            $name
            {$($enum_types)* $type_name(Vec<Token>, ($($enum_type_args)*)),}
            {$($match_arms)* $name::$type_name(tokens, _) => tokens,}
            $($rest)*
        );
    };

    (
        @internal_enum
        $name:ident
        {$($enum_types:tt)*}
        {$($match_arms:tt)*}
        $(/)?
        $type_name:ident:
        $($rest:tt)*
    ) => {
        define_type!(
            @internal_enum_type
            $name
            $type_name
            {$($enum_types)*}
            {$($match_arms)*}
            {}
            $($rest)*
        );
    };
    (@internal_enum $name:ident {$($enum_types:tt)*} {$($match_arms:tt)*}) => {
        #[derive(Clone)]
        pub enum $name {
            $($enum_types)*
        }

        impl GetTokens for $name {
            fn get_tokens(&self) -> &Vec<Token> {
                match self {
                    $($match_arms)*
                }
            }
        }
    };
}

#[allow(unused_macros)]
macro_rules! define_types {
    (@internal [$($nodes:tt)*] [$($define_type_invocations:tt)*] [$($define_type_args:tt)*] $tt:tt ; $($rest:tt)*) => {
        define_types!(@internal
            [$($nodes)*]
            [$($define_type_invocations)* define_type!($($define_type_args)* $tt);]
            []
            $($rest)*
        );
    };
    (@internal [$($nodes:tt)*] [$($define_type_invocations:tt)*] [] $node:ident = $($rest:tt)*) => {
        define_types!(@internal [$($nodes)* $node] [$($define_type_invocations)*] [$node = ] $($rest)*);
    };
    (@internal [$($nodes:tt)*] [$($define_type_invocations:tt)*] [$($define_type_args:tt)*] $tt:tt $($rest:tt)*) => {
        define_types!(@internal [$($nodes)*] [$($define_type_invocations)*] [$($define_type_args)* $tt] $($rest)*);
    };
    (@internal [$($nodes:tt)*] [$($define_type_invocations:tt)*] []) => {
        $($define_type_invocations)*
    };
    ($($input:tt)*) => {
        define_types!(@internal [] [] [] $($input)*);
    };
}

#[cps]
#[allow(unused_macros)]
macro_rules! define_types_include {
    ($source:literal) =>
    let $($input:tt)* = cps::include!($source) in
    {
        define_types!($($input)*);
    };
}

pub trait GetTokens {
    fn get_tokens(&self) -> &Vec<Token>;
}

// define_types_include!("rules.txt");

#[derive(Clone)]
pub enum Program {
    Program(Vec<Token>, (Vec<(Box<Statement>,)>, Token)),
}

impl GetTokens for Program {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Program::Program(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Statement {
    ExprStmt(Vec<Token>, (Box<ExprStmt>,)),
    PrintStmt(Vec<Token>, (Box<PrintStmt>,)),
    VarDecl(Vec<Token>, (Box<VarDecl>,)),
    FuncDecl(Vec<Token>, (Token, Token, Box<FuncDecl>)),
    ClassDecl(
        Vec<Token>,
        (Token, Token, Token, Vec<(Token, Box<FuncDecl>)>, Token),
    ),
    ConstDecl(Vec<Token>, (Box<ConstDecl>,)),
    Block(Vec<Token>, (Box<Block>,)),
    IfStmt(Vec<Token>, (Box<IfStmt>,)),
    WhileStmt(Vec<Token>, (Box<WhileStmt>,)),
    Break(Vec<Token>, (Token, Token)),
    Continue(Vec<Token>, (Token, Token)),
    Return(Vec<Token>, (Token, Option<(Box<Expression>,)>, Token)),
    ForStmt(Vec<Token>, (Box<ForStmt>,)),
}

impl GetTokens for Statement {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Statement::ExprStmt(tokens, _) => tokens,
            Statement::PrintStmt(tokens, _) => tokens,
            Statement::VarDecl(tokens, _) => tokens,
            Statement::FuncDecl(tokens, _) => tokens,
            Statement::ClassDecl(tokens, _) => tokens,
            Statement::ConstDecl(tokens, _) => tokens,
            Statement::Block(tokens, _) => tokens,
            Statement::IfStmt(tokens, _) => tokens,
            Statement::WhileStmt(tokens, _) => tokens,
            Statement::Break(tokens, _) => tokens,
            Statement::Continue(tokens, _) => tokens,
            Statement::Return(tokens, _) => tokens,
            Statement::ForStmt(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Block {
    Block(Vec<Token>, (Token, Vec<(Box<Statement>,)>, Token)),
}

impl GetTokens for Block {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Block::Block(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum ForStmt {
    VarDecl(
        Vec<Token>,
        (
            Token,
            Token,
            Box<VarDecl>,
            Option<(Box<Expression>,)>,
            Token,
            Option<(Box<Expression>,)>,
            Token,
            Box<Statement>,
        ),
    ),
    Expression(
        Vec<Token>,
        (
            Token,
            Token,
            Box<ExprStmt>,
            Option<(Box<Expression>,)>,
            Token,
            Option<(Box<Expression>,)>,
            Token,
            Box<Statement>,
        ),
    ),
    None(
        Vec<Token>,
        (
            Token,
            Token,
            Token,
            Option<(Box<Expression>,)>,
            Token,
            Option<(Box<Expression>,)>,
            Token,
            Box<Statement>,
        ),
    ),
}

impl GetTokens for ForStmt {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            ForStmt::VarDecl(tokens, _) => tokens,
            ForStmt::Expression(tokens, _) => tokens,
            ForStmt::None(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum WhileStmt {
    WhileStmt(
        Vec<Token>,
        (Token, Token, Box<Expression>, Token, Box<Statement>),
    ),
}

impl GetTokens for WhileStmt {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            WhileStmt::WhileStmt(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum IfStmt {
    IfStmt(
        Vec<Token>,
        (
            Token,
            Token,
            Box<Expression>,
            Token,
            Box<Statement>,
            Vec<(Token, Token, Box<Expression>, Token, Box<Statement>)>,
            Option<(Token, Box<Statement>)>,
        ),
    ),
}

impl GetTokens for IfStmt {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            IfStmt::IfStmt(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum VarDecl {
    VarDecl(
        Vec<Token>,
        (Token, Token, Option<(Token, Box<Expression>)>, Token),
    ),
}

impl GetTokens for VarDecl {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            VarDecl::VarDecl(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum ConstDecl {
    ConstDecl(Vec<Token>, (Token, Token, Token, Box<Expression>, Token)),
}

impl GetTokens for ConstDecl {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            ConstDecl::ConstDecl(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum ExprStmt {
    ExprStmt(Vec<Token>, (Box<Expression>, Token)),
}

impl GetTokens for ExprStmt {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            ExprStmt::ExprStmt(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum PrintStmt {
    PrintStmt(Vec<Token>, (Token, Box<Expression>, Token)),
}

impl GetTokens for PrintStmt {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            PrintStmt::PrintStmt(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Expression {
    Expression(Vec<Token>, (Box<LogicOr>,)),
    Assignment(Vec<Token>, (Box<Assignment>,)),
}

impl GetTokens for Expression {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Expression::Expression(tokens, _) => tokens,
            Expression::Assignment(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum LogicOr {
    LogicOr(Vec<Token>, (Box<LogicAnd>, Vec<(Token, Box<LogicAnd>)>)),
}

impl GetTokens for LogicOr {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            LogicOr::LogicOr(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum LogicAnd {
    LogicAnd(Vec<Token>, (Box<Equality>, Vec<(Token, Box<Equality>)>)),
}

impl GetTokens for LogicAnd {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            LogicAnd::LogicAnd(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Assignment {
    Assignment(Vec<Token>, (Token, Token, Box<Expression>)),
}

impl GetTokens for Assignment {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Assignment::Assignment(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Equality {
    Equality(Vec<Token>, (Box<Comparison>, Vec<(Token, Box<Comparison>)>)),
}

impl GetTokens for Equality {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Equality::Equality(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Comparison {
    Comparison(Vec<Token>, (Box<Term>, Vec<(Token, Box<Term>)>)),
}

impl GetTokens for Comparison {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Comparison::Comparison(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Term {
    Term(Vec<Token>, (Box<Factor>, Vec<(Token, Box<Factor>)>)),
}

impl GetTokens for Term {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Term::Term(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Factor {
    Factor(Vec<Token>, (Box<Unary>, Vec<(Token, Box<Unary>)>)),
}

impl GetTokens for Factor {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Factor::Factor(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Unary {
    Unary(Vec<Token>, (Vec<(Token,)>, Box<FuncCall>)),
}

impl GetTokens for Unary {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Unary::Unary(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum FuncCall {
    FuncCall(Vec<Token>, (Box<Primary>, Vec<(Box<FuncCall2>,)>)),
}

impl GetTokens for FuncCall {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            FuncCall::FuncCall(tokens, _) => tokens,
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Clone)]
pub enum FuncCall2 {
    Call(Vec<Token>, (Token, Option<(Box<Arguments>,)>, Token)),
    Property(Vec<Token>, (Token, Token)),
}

impl GetTokens for FuncCall2 {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            FuncCall2::Call(tokens, _) => tokens,
            FuncCall2::Property(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Primary {
    Literal(Vec<Token>, (Token,)),
    Func(Vec<Token>, (Token, Box<FuncDecl>)),
    Parenthesized(Vec<Token>, (Token, Box<Expression>, Token)),
}

impl GetTokens for Primary {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Primary::Literal(tokens, _) => tokens,
            Primary::Func(tokens, _) => tokens,
            Primary::Parenthesized(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum FuncDecl {
    FuncDecl(Vec<Token>, (Token, Token, Box<ArgDecl>, Token, Box<Block>)),
}

impl GetTokens for FuncDecl {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            FuncDecl::FuncDecl(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum ArgDecl {
    ArgDecl(
        Vec<Token>,
        (Option<(Token, Token, Token, Vec<(Token, Token, Token, Token)>)>,),
    ),
}

impl GetTokens for ArgDecl {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            ArgDecl::ArgDecl(tokens, _) => tokens,
        }
    }
}

#[derive(Clone)]
pub enum Arguments {
    Arguments(Vec<Token>, (Box<Expression>, Vec<(Token, Box<Expression>)>)),
}

impl GetTokens for Arguments {
    fn get_tokens(&self) -> &Vec<Token> {
        match self {
            Arguments::Arguments(tokens, _) => tokens,
        }
    }
}
