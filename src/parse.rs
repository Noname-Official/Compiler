use crate::{
    error::{Error, ErrorType::Parse},
    nodes::{
        ArgDecl, Arguments, Assignment, Block, Comparison, ConstDecl, Equality, ExprStmt,
        Expression, Factor, ForStmt, FuncCall, FuncCall2, FuncDecl, GetTokens, IfStmt, LogicAnd,
        LogicOr, Primary, PrintStmt, Program, Statement, Term, Unary, VarDecl, WhileStmt,
    },
    peek2::{Peek2, ToPeek2},
    token::{
        token_type::{KeyWord, Literal, OneChar, OneTwoChar, Special, TokenType},
        Token,
    },
};

// use cps::cps;
// use paste::paste;

// macro_rules! define_parser_node {
//     ($name:ident = $($input:tt)*) => {
//         define_parser_node!(@internal_node $name {} $($input)*);
//     };
//     (@internal_lits $name:ident $type_name:ident {$($eval:tt)*} {$($eval2:tt)*} $(|)? $lit:literal $($rest:tt)*) => {
//         define_parser_node!(@internal_lits $name $type_name {$($eval)*} {$($eval2)* if self.} $($rest)*);
//     };
//     (@internal_lits $name:ident $type_name:ident {$($eval:tt)*} {$($eval2:tt)*} $($rest:tt)*) => {
//         define_parser_node!(@internal_type $name $type_name {$($eval)*} {$($eval2)*} $($rest)*);
//     };
//     (@internal_grouped_lits
//         $name:ident
//         $type_name:ident
//         {$($eval:tt)*}
//         {$($eval2:tt)*}
//         {$($eval3:tt)*}
//         (
//             |
//             $grouped_lit:literal
//             $($rest:tt)*
//         )
//         $($rest2:tt)*
//     ) => {
//         define_parser_node!(@internal_grouped_lits
//             $name
//             $type_name
//             {$($eval)*}
//             {$($eval2)*}
//             {$($eval3)*}
//             ($($rest)*)
//             $($rest2)*
//         );
//     };
//     (@internal_grouped_lits
//         $name:ident
//         $type_name:ident
//         {$($eval:tt)*}
//         {$($eval2:tt)*}
//         {$($eval3:tt)*}
//         ($($rest:tt)*)
//         $($rest2:tt)*
//     ) => {
//         define_parser_node!(@internal_grouped
//             $name
//             $type_name
//             {$($eval)*}
//             {$($eval2)*}
//             {$($eval3)* Box<Token>,}
//             ($($rest)*)
//             $($rest2)*
//         );
//     };
//     (@internal_grouped
//         $name:ident
//         $type_name:ident
//         {$($eval:tt)*}
//         {$($eval2:tt)*}
//         {$($eval3:tt)*}
//         (
//             $(,)?
//             $grouped_ident:ident
//             $($rest:tt)*
//         )
//         $($rest2:tt)*
//     ) => {
//         define_parser_node!(@internal_grouped
//             $name
//             $type_name
//             {$($eval)*}
//             {$($eval2)*}
//             {$($eval3)* Box<$grouped_ident>,}
//             ($($rest)*)
//             $($rest2)*
//         );
//     };
//     (@internal_grouped
//         $name:ident
//         $type_name:ident
//         {$($eval:tt)*}
//         {$($eval2:tt)*}
//         {$($eval3:tt)*}
//         (
//             $(,)?
//             $grouped_lit:literal
//             $($rest:tt)*
//         )
//         $($rest2:tt)*
//     ) => {
//         define_parser_node!(@internal_grouped_lits
//             $name
//             $type_name
//             {$($eval)*}
//             {$($eval2)*}
//             {$($eval3)*}
//             ($($rest)*)
//             $($rest2)*
//         );
//     };
//     (@internal_grouped $name:ident $type_name:ident {$($eval:tt)*} {$($eval2:tt)*} {$($eval3:tt)*} () $($rest:tt)*) => {
//         define_parser_node!(@internal_type $name $type_name {$($eval)*} {$($eval2)* Vec<($($eval3)*)>,} $($rest)*);
//     };
//     (@internal_type
//         $name:ident
//         $type_name:ident
//         {$($eval:tt)*}
//         {$($eval2:tt)*}
//         $(,)?
//         Token($($types:ident)|+)
//         $($rest:tt)*
//     ) => {
//         define_parser_node!(@internal_lits $name $type_name {$($eval)*} {$($eval2)* Box<Token>,} $($rest)*);
//     };
//     (@internal_type $name:ident $type_name:ident {$($eval:tt)*} {$($eval2:tt)*} $(,)? $ident:ident $($rest:tt)*) => {
//         define_parser_node!(@internal_type $name $type_name {$($eval)*} {$($eval2)* Box<$ident>,} $($rest)*);
//     };
//     (@internal_type
//         $name:ident
//         $type_name:ident
//         {$($eval:tt)*}
//         {$($eval2:tt)*}
//         $(,)?
//         ($($grouped:tt)*)*
//         $($rest:tt)*
//     ) => {
//         define_parser_node!(@internal_grouped $name $type_name {$($eval)*} {$($eval2)*} {} ($($grouped)*) $($rest)*);
//     };
//     (@internal_type
//         $name:ident
//         $type_name:ident
//         {$($eval:tt)*}
//         {$($eval2:tt)*}
//         $(,)?
//         $lit:literal
//         $($rest:tt)*
//     ) => {
//         define_parser_node!(@internal_lits $name $type_name {$($eval)*} {$($eval2)* Box<Token>,} $($rest)*);
//     };
//     (@internal_type $name:ident $type_name:ident {$($eval:tt)*} {$($eval2:tt)*} $($rest:tt)*) => {
//         define_parser_node!(@internal_node $name {$($eval)* $($eval2)*} $($rest)*);
//     };
//     (@internal_node $name:ident {$($eval:tt)*} $(/)? $type_name:ident: $($rest:tt)*) => {
//         define_parser_node!(@internal_type $name $type_name {$($eval)*} {} $($rest)*);
//     };
//     (@internal_node $name:ident {$($eval:tt)*}) => {
//         paste!(
//             fn [<$name:snake>]() -> Result<$name, String> {
//                 $($eval)*
//             }
//         );
//     };
// }

// macro_rules! define_parser {
//     (@internal [$($define_parser_node_invocations:tt)*] [$($define_parser_node_args:tt)*] $tt:tt ; $($rest:tt)*) => {
//         define_parser!(@internal
//             [$($define_parser_node_invocations)* define_parser_node!($($define_parser_node_args)* $tt);]
//             []
//             $($rest)*
//         );
//     };
//     (@internal [$($define_parser_node_invocations:tt)*] [] $node:ident = $($rest:tt)*) => {
//         define_parser!(@internal [$($define_parser_node_invocations)*] [$node = ] $($rest)*);
//     };
//     (@internal [$($define_parser_node_invocations:tt)*] [$($define_parser_node_args:tt)*] $tt:tt $($rest:tt)*) => {
//         define_parser!(@internal [$($define_parser_node_invocations)*] [$($define_parser_node_args)* $tt] $($rest)*);
//     };
//     (@internal [$($define_parser_node_invocations:tt)*] []) => {
//         $($define_parser_node_invocations)*
//     };
//     ($($input:tt)*) => {
//         define_parser!(@internal [] [] $($input)*);
//     };
// }

// #[cps]
// macro_rules! define_parser_include {
//     ($source:literal) =>
//     let ($($input:tt)*) = cps::include!($source) in
//     {
//         define_parser!($($input)*);
//     };
// }

pub struct Parser<T> {
    pub(crate) tokens: T,
    errs: Vec<Error>,
}

impl<T: Iterator<Item = Token>> Parser<Peek2<T>> {
    pub fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.to_peek2(),
            errs: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Program, Vec<Error>> {
        let prog = self.program();
        if !self.errs.is_empty() {
            return Err(self.errs.clone());
        }
        Ok(prog)
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if self
                .advance()
                .eq_type(&TokenType::OneChar(OneChar::Semicolon))
            {
                return;
            }

            if self.match_token_type(vec![
                TokenType::KeyWord(KeyWord::Class),
                TokenType::KeyWord(KeyWord::Const),
                TokenType::KeyWord(KeyWord::Func),
                TokenType::KeyWord(KeyWord::Var),
                TokenType::KeyWord(KeyWord::For),
                TokenType::KeyWord(KeyWord::If),
                TokenType::KeyWord(KeyWord::While),
                TokenType::KeyWord(KeyWord::Print),
                TokenType::KeyWord(KeyWord::Return),
                TokenType::KeyWord(KeyWord::Break),
                TokenType::KeyWord(KeyWord::Continue),
                TokenType::KeyWord(KeyWord::Return),
                TokenType::OneChar(OneChar::LeftBrace),
            ]) {
                return;
            }
        }
    }

    fn match_token_type(&mut self, types: Vec<TokenType>) -> bool {
        for token_type in types {
            if self.check(token_type) {
                return true;
            }
        }
        false
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        self.peek().eq_type(&token_type)
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().eq_type(&TokenType::Special(Special::Eof))
    }

    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn peek2(&mut self) -> &Token {
        if self.is_at_end() {
            return self.peek();
        }
        self.tokens.peek2().unwrap()
    }

    fn check2(&mut self, token_type: TokenType) -> bool {
        self.peek2().eq_type(&token_type)
    }

    fn advance(&mut self) -> Token {
        if self.is_at_end() {
            return self.peek().clone();
        }
        self.tokens.next().unwrap()
    }

    fn consume(
        &mut self,
        token_type: TokenType,
        msg: &'static str,
        error_type: &'static str,
    ) -> Result<Token, Error> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        Err(Error::new(Parse, &[self.peek().clone()], msg, error_type))
    }

    fn program(&mut self) -> Program {
        let mut arg1 = Vec::new();
        let mut tokens: Vec<Token> = Vec::new();
        while !self.is_at_end() {
            if let Ok(stmt) = self.statement() {
                tokens.extend(stmt.get_tokens().clone());
                arg1.push((Box::new(stmt),));
            }
        }
        let eof = match self.consume(
            TokenType::Special(Special::Eof),
            "Failed to find EOF",
            "Syntax",
        ) {
            Ok(eof) => eof,
            Err(e) => {
                self.errs.push(e);
                let last_token = tokens[tokens.len() - 1].clone();
                Token::new(
                    &tokens[0].file,
                    last_token.line_contents,
                    TokenType::Special(Special::Eof),
                    String::new(),
                    last_token.line,
                    last_token.begin,
                    last_token.begin,
                )
            }
        };
        Program::Program(tokens, (arg1, eof))
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        let res = if self.check(TokenType::KeyWord(KeyWord::Print)) {
            self.printstmt().map(|print_stmt| {
                Statement::PrintStmt(print_stmt.get_tokens().clone(), (Box::new(print_stmt),))
            })
        } else if self.check(TokenType::KeyWord(KeyWord::Func))
            && self.check2(TokenType::Literal(Literal::Identifier(Default::default())))
        {
            let func = self.advance();
            let name = self.advance();
            let mut tokens = vec![func.clone(), name.clone()];
            self.func_decl().map(|func_decl| {
                tokens.extend(func_decl.get_tokens().clone());
                Statement::FuncDecl(tokens, (func, name, Box::new(func_decl)))
            })
        } else if self.check(TokenType::KeyWord(KeyWord::Class)) {
            let class_kw = self.advance();
            let name = self.consume(
                TokenType::Literal(Literal::Identifier(Default::default())),
                "Expected name of the class after 'class'",
                "Syntax",
            )?;
            let left_brace = self.consume(
                TokenType::OneChar(OneChar::LeftBrace),
                "Expected '{' after clsas declaration",
                "Syntax",
            )?;
            let mut tokens = vec![class_kw.clone(), name.clone(), left_brace.clone()];
            let mut funcs = vec![];
            while !self.check(TokenType::OneChar(OneChar::RightBrace)) && !self.is_at_end() {
                let name = self.consume(
                    TokenType::Literal(Literal::Identifier(Default::default())),
                    "Expected function name",
                    "Syntax",
                )?;
                let function = self.func_decl()?;
                tokens.push(name.clone());
                tokens.extend(function.get_tokens().clone());
                funcs.push((name, Box::new(function)));
            }
            let right_brace = self.consume(
                TokenType::OneChar(OneChar::RightBrace),
                "Expected '}' after clsas declaration",
                "Syntax",
            )?;
            tokens.push(right_brace.clone());
            Ok(Statement::ClassDecl(
                tokens,
                (class_kw, name, left_brace, funcs, right_brace),
            ))
        } else if self.check(TokenType::KeyWord(KeyWord::Const)) {
            self.const_decl().map(|const_decl| {
                Statement::ConstDecl(const_decl.get_tokens().clone(), (Box::new(const_decl),))
            })
        } else if self.check(TokenType::KeyWord(KeyWord::Continue)) {
            let continue_kw = self.advance();
            self.consume(
                TokenType::OneChar(OneChar::Semicolon),
                "Expected ';'",
                "Syntax",
            )
            .map(|semi| {
                Statement::Continue(vec![continue_kw.clone(), semi.clone()], (continue_kw, semi))
            })
        } else if self.check(TokenType::KeyWord(KeyWord::Break)) {
            let break_kw = self.advance();
            self.consume(
                TokenType::OneChar(OneChar::Semicolon),
                "Expected ';'",
                "Synax",
            )
            .map(|semi| Statement::Break(vec![break_kw.clone(), semi.clone()], (break_kw, semi)))
        } else if self.check(TokenType::KeyWord(KeyWord::Return)) {
            let return_kw = self.advance();
            let mut tokens = vec![return_kw.clone()];
            let expr = if !self.check(TokenType::OneChar(OneChar::Semicolon)) {
                let expr = self.expression()?;
                tokens.extend(expr.get_tokens().clone());
                Some((Box::new(expr),))
            } else {
                None
            };
            let semi = self.consume(
                TokenType::OneChar(OneChar::Semicolon),
                "Expected ';'",
                "Syntax",
            )?;
            tokens.push(semi.clone());
            Ok(Statement::Return(tokens, (return_kw, expr, semi)))
        } else if self.check(TokenType::KeyWord(KeyWord::If)) {
            self.if_stmt().map(|if_stmt| {
                Statement::IfStmt(if_stmt.get_tokens().clone(), (Box::new(if_stmt),))
            })
        } else if self.check(TokenType::KeyWord(KeyWord::Var)) {
            self.var_decl().map(|var_decl| {
                Statement::VarDecl(var_decl.get_tokens().clone(), (Box::new(var_decl),))
            })
        } else if self.check(TokenType::KeyWord(KeyWord::While)) {
            self.while_stmt().map(|while_stmt| {
                Statement::WhileStmt(while_stmt.get_tokens().clone(), (Box::new(while_stmt),))
            })
        } else if self.check(TokenType::KeyWord(KeyWord::For)) {
            self.for_stmt().map(|for_stmt| {
                Statement::ForStmt(for_stmt.get_tokens().clone(), (Box::new(for_stmt),))
            })
        } else if self.check(TokenType::OneChar(OneChar::LeftBrace)) {
            self.block()
                .map(|block| Statement::Block(block.get_tokens().clone(), (Box::new(block),)))
        } else {
            self.expr_stmt().map(|expr_stmt| {
                Statement::ExprStmt(expr_stmt.get_tokens().clone(), (Box::new(expr_stmt),))
            })
        };
        match res {
            Ok(res) => Ok(res),
            Err(e) => {
                self.errs.push(e.clone());
                self.synchronize();
                Err(e)
            }
        }
    }

    fn func_decl(&mut self) -> Result<FuncDecl, Error> {
        let return_type = self.consume(
            TokenType::Literal(Literal::Identifier(Default::default())),
            "Expected return type",
            "Syntax",
        )?;
        let left = self.consume(
            TokenType::OneChar(OneChar::LeftParen),
            "Expected '('",
            "Syntax",
        )?;
        let mut tokens = vec![left.clone()];
        let arg_decl = self.arg_decl()?;
        tokens.extend(arg_decl.get_tokens().clone());
        let right = self.consume(
            TokenType::OneChar(OneChar::RightParen),
            "Expected ')'",
            "Syntax",
        )?;
        tokens.push(right.clone());
        let block = self.block()?;
        tokens.extend(block.get_tokens().clone());
        Ok(FuncDecl::FuncDecl(
            tokens,
            (
                return_type,
                left,
                Box::new(arg_decl),
                right,
                Box::new(block),
            ),
        ))
    }

    fn arg_decl(&mut self) -> Result<ArgDecl, Error> {
        let res = match self.check(TokenType::Literal(Literal::Identifier(Default::default()))) {
            true => {
                let arg = self.advance();
                let colon = self.consume(
                    TokenType::OneChar(OneChar::Colon),
                    "Expected a colon",
                    "Syntax",
                )?;
                let arg_type = self.consume(
                    TokenType::Literal(Literal::Identifier(Default::default())),
                    "Expected a type of the argument",
                    "Syntax",
                )?;
                let mut tokens = vec![arg.clone(), colon.clone(), arg_type.clone()];
                let mut args = vec![];
                while self.check(TokenType::OneChar(OneChar::Comma)) {
                    let comma = self.advance();
                    tokens.push(comma.clone());
                    let arg = self.consume(
                        TokenType::Literal(Literal::Identifier(Default::default())),
                        "Expected argument name",
                        "Syntax",
                    )?;
                    tokens.push(arg.clone());
                    let colon = self.consume(
                        TokenType::OneChar(OneChar::Colon),
                        "Expected a colon",
                        "Syntax",
                    )?;
                    tokens.push(colon.clone());
                    let arg_type = self.consume(
                        TokenType::Literal(Literal::Identifier(Default::default())),
                        "Expected a type of the argument",
                        "Syntax",
                    )?;
                    tokens.push(arg_type.clone());
                    args.push((comma, arg, colon, arg_type));
                }
                (tokens, Some((arg, colon, arg_type, args)))
            }
            false => (vec![], None),
        };
        Ok(ArgDecl::ArgDecl(res.0, (res.1,)))
    }

    fn block(&mut self) -> Result<Block, Error> {
        let left = self.consume(
            TokenType::OneChar(OneChar::LeftBrace),
            "Expected '{'",
            "Syntax",
        )?;
        let mut tokens = vec![left.clone()];
        let mut stmts = Vec::new();
        while !self.check(TokenType::OneChar(OneChar::RightBrace)) && !self.is_at_end() {
            let stmt = self.statement()?;
            tokens.extend(stmt.get_tokens().clone());
            stmts.push((Box::new(stmt),));
        }
        let right = self.consume(
            TokenType::OneChar(OneChar::RightBrace),
            "Expected '}'",
            "Syntax",
        )?;
        tokens.push(right.clone());
        Ok(Block::Block(tokens, (left, stmts, right)))
    }

    fn for_stmt(&mut self) -> Result<ForStmt, Error> {
        let for_kw = self.consume(TokenType::KeyWord(KeyWord::For), "Expected 'for'", "Syntax")?;
        let left = self.consume(
            TokenType::OneChar(OneChar::LeftParen),
            "Expected '('",
            "Syntax",
        )?;
        let mut tokens = vec![for_kw.clone(), left.clone()];
        if self.check(TokenType::KeyWord(KeyWord::Var)) {
            let var_decl = self.var_decl()?;
            tokens.extend(var_decl.get_tokens().clone());
            let condition = if !self.check(TokenType::OneChar(OneChar::Semicolon)) {
                let expr = self.expression()?;
                tokens.extend(expr.get_tokens().clone());
                Some((Box::new(expr),))
            } else {
                None
            };
            let semi = self.consume(
                TokenType::OneChar(OneChar::Semicolon),
                "Expected ';'",
                "Syntax",
            )?;
            tokens.push(semi.clone());
            let repeated = if !self.check(TokenType::OneChar(OneChar::RightParen)) {
                let expr = self.expression()?;
                tokens.extend(expr.get_tokens().clone());
                Some((Box::new(expr),))
            } else {
                None
            };
            let right = self.consume(
                TokenType::OneChar(OneChar::RightParen),
                "Expected ')'",
                "Syntax",
            )?;
            tokens.push(right.clone());
            let stmt = self.statement()?;
            tokens.extend(stmt.get_tokens().clone());
            Ok(ForStmt::VarDecl(
                tokens,
                (
                    for_kw,
                    left,
                    Box::new(var_decl),
                    condition,
                    semi,
                    repeated,
                    right,
                    Box::new(stmt),
                ),
            ))
        } else if self.check(TokenType::OneChar(OneChar::Semicolon)) {
            let semi1 = self.advance();
            tokens.push(semi1.clone());
            let condition = if !self.check(TokenType::OneChar(OneChar::Semicolon)) {
                let expr = self.expression()?;
                tokens.extend(expr.get_tokens().clone());
                Some((Box::new(expr),))
            } else {
                None
            };
            let semi2 = self.consume(
                TokenType::OneChar(OneChar::Semicolon),
                "Expected ';'",
                "Syntax",
            )?;
            tokens.push(semi2.clone());
            let repeated = if !self.check(TokenType::OneChar(OneChar::RightParen)) {
                let expr = self.expression()?;
                tokens.extend(expr.get_tokens().clone());
                Some((Box::new(expr),))
            } else {
                None
            };
            let right = self.consume(
                TokenType::OneChar(OneChar::RightParen),
                "Expected ')'",
                "Syntax",
            )?;
            tokens.push(right.clone());
            let stmt = self.statement()?;
            tokens.extend(stmt.get_tokens().clone());
            Ok(ForStmt::None(
                tokens,
                (
                    for_kw,
                    left,
                    semi1,
                    condition,
                    semi2,
                    repeated,
                    right,
                    Box::new(stmt),
                ),
            ))
        } else {
            let expr = self.expr_stmt()?;
            tokens.extend(expr.get_tokens().clone());
            let condition = if !self.check(TokenType::OneChar(OneChar::Semicolon)) {
                let expr = self.expression()?;
                tokens.extend(expr.get_tokens().clone());
                Some((Box::new(expr),))
            } else {
                None
            };
            let semi = self.consume(
                TokenType::OneChar(OneChar::Semicolon),
                "Expected ';'",
                "Syntax",
            )?;
            tokens.push(semi.clone());
            let repeated = if !self.check(TokenType::OneChar(OneChar::RightParen)) {
                let expr = self.expression()?;
                tokens.extend(expr.get_tokens().clone());
                Some((Box::new(expr),))
            } else {
                None
            };
            let right = self.consume(
                TokenType::OneChar(OneChar::RightParen),
                "Expected ')'",
                "Syntax",
            )?;
            tokens.push(right.clone());
            let stmt = self.statement()?;
            tokens.extend(stmt.get_tokens().clone());
            Ok(ForStmt::Expression(
                tokens,
                (
                    for_kw,
                    left,
                    Box::new(expr),
                    condition,
                    semi,
                    repeated,
                    right,
                    Box::new(stmt),
                ),
            ))
        }
    }

    fn while_stmt(&mut self) -> Result<WhileStmt, Error> {
        let while_kw = self.consume(
            TokenType::KeyWord(KeyWord::While),
            "Expected 'while'",
            "Syntax",
        )?;
        let mut tokens = vec![while_kw.clone()];
        let left = self.consume(
            TokenType::OneChar(OneChar::LeftParen),
            "Expected '('",
            "Syntax",
        )?;
        tokens.push(left.clone());
        let expr = self.expression()?;
        tokens.extend(expr.get_tokens().clone());
        let right = self.consume(
            TokenType::OneChar(OneChar::RightParen),
            "Expected ')'",
            "Syntax",
        )?;
        tokens.push(right.clone());
        let stmt = self.statement()?;
        tokens.extend(stmt.get_tokens().clone());
        Ok(WhileStmt::WhileStmt(
            tokens,
            (while_kw, left, Box::new(expr), right, Box::new(stmt)),
        ))
    }

    fn if_stmt(&mut self) -> Result<IfStmt, Error> {
        let if_kw = self.consume(TokenType::KeyWord(KeyWord::If), "Expected 'if'", "Syntax")?;
        let left = self.consume(
            TokenType::OneChar(OneChar::LeftParen),
            "Expected '('",
            "Syntax",
        )?;
        let mut tokens = vec![if_kw.clone(), left.clone()];
        let expr = self.expression()?;
        tokens.extend(expr.get_tokens().clone());
        let right = self.consume(
            TokenType::OneChar(OneChar::RightParen),
            "Expected ')'",
            "Syntax",
        )?;
        tokens.push(right.clone());
        let stmt = self.statement()?;
        tokens.extend(stmt.get_tokens().clone());
        let mut elifs = Vec::new();
        while self.check(TokenType::KeyWord(KeyWord::Elif)) {
            let elif = self.advance();
            tokens.push(elif.clone());
            let left = self.consume(
                TokenType::OneChar(OneChar::LeftParen),
                "Expected '('",
                "Syntax",
            )?;
            tokens.push(left.clone());
            let expr = self.expression()?;
            tokens.extend(expr.get_tokens().clone());
            let right = self.consume(
                TokenType::OneChar(OneChar::RightParen),
                "Expected ')'",
                "Syntax",
            )?;
            tokens.push(right.clone());
            let stmt = self.statement()?;
            tokens.extend(stmt.get_tokens().clone());
            elifs.push((elif, left, Box::new(expr), right, Box::new(stmt)));
        }
        let else_ = if self.check(TokenType::KeyWord(KeyWord::Else)) {
            let else_kw = self.advance();
            tokens.push(else_kw.clone());
            let stmt = self.statement()?;
            tokens.extend(stmt.get_tokens().clone());
            Some((else_kw, Box::new(stmt)))
        } else {
            None
        };
        Ok(IfStmt::IfStmt(
            tokens,
            (
                if_kw,
                left,
                Box::new(expr),
                right,
                Box::new(stmt),
                elifs,
                else_,
            ),
        ))
    }

    fn expr_stmt(&mut self) -> Result<ExprStmt, Error> {
        let arg1 = self.expression()?;
        let mut tokens = arg1.get_tokens().clone();
        let arg2 = self.consume(
            TokenType::OneChar(OneChar::Semicolon),
            "Semicolon expected",
            "Syntax",
        )?;
        tokens.push(arg2.clone());
        Ok(ExprStmt::ExprStmt(tokens, (Box::new(arg1), arg2)))
    }

    fn printstmt(&mut self) -> Result<PrintStmt, Error> {
        let arg1 = self.consume(
            TokenType::KeyWord(KeyWord::Print),
            "Expected 'print'",
            "Syntax",
        )?;
        let mut tokens = vec![arg1.clone()];
        let arg2 = self.expression()?;
        tokens.extend(arg2.get_tokens().clone());
        let arg3 = self.consume(
            TokenType::OneChar(OneChar::Semicolon),
            "Semicolon expected",
            "Syntax",
        )?;
        tokens.push(arg3.clone());
        Ok(PrintStmt::PrintStmt(tokens, (arg1, Box::new(arg2), arg3)))
    }

    fn const_decl(&mut self) -> Result<ConstDecl, Error> {
        let const_kw = self.consume(
            TokenType::KeyWord(KeyWord::Const),
            "Expected 'const' keyword",
            "Syntax",
        )?;
        let name = self.consume(
            TokenType::Literal(Literal::Identifier(Default::default())),
            "Expected constant name",
            "Syntax",
        )?;
        let eq = self.consume(
            TokenType::OneTwoChar(OneTwoChar::Equal),
            "Expected '='",
            "Syntax",
        )?;
        let mut tokens = vec![const_kw.clone(), name.clone(), eq.clone()];
        let expr = self.expression()?;
        tokens.extend(expr.get_tokens().clone());
        let semi = self.consume(
            TokenType::OneChar(OneChar::Semicolon),
            "Expected ';'",
            "Syntax",
        )?;
        tokens.push(semi.clone());
        Ok(ConstDecl::ConstDecl(
            tokens,
            (const_kw, name, eq, Box::new(expr), semi),
        ))
    }

    fn var_decl(&mut self) -> Result<VarDecl, Error> {
        let var = self.consume(
            TokenType::KeyWord(KeyWord::Var),
            "Expected 'var' keyword",
            "Syntax",
        )?;
        let mut tokens = vec![var.clone()];
        let name = self.consume(
            TokenType::Literal(Literal::Identifier(Default::default())),
            "Expected variable name",
            "Syntax",
        )?;
        tokens.push(name.clone());
        let expr = if self.match_token_type(vec![TokenType::OneTwoChar(OneTwoChar::Equal)]) {
            let eq = self.advance();
            tokens.push(eq.clone());
            let expr = self.expression()?;
            tokens.extend(expr.get_tokens().clone());
            Some((eq, Box::new(expr)))
        } else {
            None
        };
        let semi = self.consume(
            TokenType::OneChar(OneChar::Semicolon),
            "Expected ';'",
            "Syntax",
        )?;
        tokens.push(semi.clone());
        Ok(VarDecl::VarDecl(tokens, (var, name, expr, semi)))
    }

    fn expression(&mut self) -> Result<Expression, Error> {
        if self
            .peek()
            .eq_type(&TokenType::Literal(Literal::Identifier(Default::default())))
            && self
                .peek2()
                .eq_type(&TokenType::OneTwoChar(OneTwoChar::Equal))
        {
            let assignment = self.assignment()?;
            Ok(Expression::Assignment(
                assignment.get_tokens().clone(),
                (Box::new(assignment),),
            ))
        } else {
            let logic_or = self.logic_or()?;
            Ok(Expression::Expression(
                logic_or.get_tokens().clone(),
                (Box::new(logic_or),),
            ))
        }
    }

    fn logic_or(&mut self) -> Result<LogicOr, Error> {
        let left = self.logic_and()?;
        let mut tokens = left.get_tokens().clone();
        let mut args = Vec::new();
        while self.check(TokenType::KeyWord(KeyWord::Or)) {
            let or = self.advance();
            tokens.push(or.clone());
            let right = self.logic_and()?;
            tokens.extend(right.get_tokens().clone());
            args.push((or, Box::new(right)));
        }
        Ok(LogicOr::LogicOr(tokens.clone(), (Box::new(left), args)))
    }

    fn logic_and(&mut self) -> Result<LogicAnd, Error> {
        let left = self.equality()?;
        let mut tokens = left.get_tokens().clone();
        let mut args = Vec::new();
        while self.check(TokenType::KeyWord(KeyWord::And)) {
            let or = self.advance();
            tokens.push(or.clone());
            let right = self.equality()?;
            tokens.extend(right.get_tokens().clone());
            args.push((or, Box::new(right)));
        }
        Ok(LogicAnd::LogicAnd(tokens.clone(), (Box::new(left), args)))
    }

    fn assignment(&mut self) -> Result<Assignment, Error> {
        let name = self.consume(
            TokenType::Literal(Literal::Identifier(Default::default())),
            "Expected variable name",
            "Syntax",
        )?;
        let eq = self.consume(
            TokenType::OneTwoChar(OneTwoChar::Equal),
            "Expected '='",
            "Syntax",
        )?;
        let mut tokens = vec![name.clone(), eq.clone()];
        let expr = self.expression()?;
        tokens.extend(expr.get_tokens().clone());
        Ok(Assignment::Assignment(tokens, (name, eq, Box::new(expr))))
    }

    fn equality(&mut self) -> Result<Equality, Error> {
        let arg1 = self.comparison()?;
        let mut tokens = arg1.get_tokens().clone();
        let mut arg2 = Vec::new();
        while self.match_token_type(vec![
            TokenType::OneTwoChar(OneTwoChar::BangEqual),
            TokenType::OneTwoChar(OneTwoChar::EqualEqual),
        ]) {
            let op = self.advance();
            let right = self.comparison()?;
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            arg2.push((op, Box::new(right)));
        }
        Ok(Equality::Equality(tokens, (Box::new(arg1), arg2)))
    }
    fn comparison(&mut self) -> Result<Comparison, Error> {
        let arg1 = self.term()?;
        let mut tokens = arg1.get_tokens().clone();
        let mut arg2 = Vec::new();
        while self.match_token_type(vec![
            TokenType::OneTwoChar(OneTwoChar::Less),
            TokenType::OneTwoChar(OneTwoChar::LessEqual),
            TokenType::OneTwoChar(OneTwoChar::Greater),
            TokenType::OneTwoChar(OneTwoChar::GreaterEqual),
        ]) {
            let op = self.advance();
            let right = self.term()?;
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            arg2.push((op, Box::new(right)));
        }
        Ok(Comparison::Comparison(tokens, (Box::new(arg1), arg2)))
    }
    fn term(&mut self) -> Result<Term, Error> {
        let arg1 = self.factor()?;
        let mut tokens = arg1.get_tokens().clone();
        let mut arg2 = Vec::new();
        while self.match_token_type(vec![
            TokenType::OneChar(OneChar::Plus),
            TokenType::OneChar(OneChar::Minus),
        ]) {
            let op = self.advance();
            let right = self.factor()?;
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            arg2.push((op, Box::new(right)));
        }
        Ok(Term::Term(tokens, (Box::new(arg1), arg2)))
    }
    fn factor(&mut self) -> Result<Factor, Error> {
        let arg1 = self.unary()?;
        let mut tokens = arg1.get_tokens().clone().clone();
        let mut arg2 = Vec::new();
        while self.match_token_type(vec![
            TokenType::OneChar(OneChar::Star),
            TokenType::Special(Special::Slash),
        ]) {
            let op = self.advance().clone();
            let right = self.unary()?;
            tokens.push(op.clone());
            tokens.extend(right.get_tokens().clone());
            arg2.push((op, Box::new(right)));
        }
        Ok(Factor::Factor(tokens, (Box::new(arg1), arg2)))
    }
    fn unary(&mut self) -> Result<Unary, Error> {
        let mut arg1 = Vec::new();
        let mut tokens = Vec::new();
        while self.match_token_type(vec![
            TokenType::OneChar(OneChar::Plus),
            TokenType::OneChar(OneChar::Minus),
            TokenType::OneTwoChar(OneTwoChar::Bang),
        ]) {
            let op = self.advance();
            tokens.push(op.clone());
            arg1.push((op,));
        }
        let arg2 = self.func_call()?;
        tokens.extend(arg2.get_tokens().clone());
        Ok(Unary::Unary(tokens, (arg1, Box::new(arg2))))
    }

    fn func_call(&mut self) -> Result<FuncCall, Error> {
        let primary = self.primary()?;
        let tokens = primary.get_tokens().clone();

        let mut calls = vec![];
        while self.match_token_type(vec![
            TokenType::OneChar(OneChar::LeftParen),
            TokenType::OneChar(OneChar::Dot),
        ]) {
            calls.push((Box::new(self.func_call2()?),));
        }

        Ok(FuncCall::FuncCall(tokens, (Box::new(primary), calls)))
    }

    fn func_call2(&mut self) -> Result<FuncCall2, Error> {
        if self.check(TokenType::OneChar(OneChar::LeftParen)) {
            let left = self.advance();
            let mut tokens = vec![left.clone()];
            let mut arguments = None;
            if !self.check(TokenType::OneChar(OneChar::RightParen)) {
                let args = self.arguments()?;
                tokens.extend(args.get_tokens().clone());
                arguments = Some((Box::new(args),));
            }
            let right = self.consume(
                TokenType::OneChar(OneChar::RightParen),
                "Expected ')'",
                "Syntax",
            )?;
            tokens.push(right.clone());

            Ok(FuncCall2::Call(tokens, (left, arguments, right)))
        } else {
            let dot = self.consume(
                TokenType::OneChar(OneChar::Dot),
                "Expected one of '.' and '('",
                "Syntax",
            )?;
            let property = self.consume(
                TokenType::Literal(Literal::Identifier(Default::default())),
                "Expected property",
                "Syntax",
            )?;

            Ok(FuncCall2::Property(
                vec![dot.clone(), property.clone()],
                (dot, property),
            ))
        }
    }

    fn arguments(&mut self) -> Result<Arguments, Error> {
        let arg = self.expression()?;
        let mut tokens = arg.get_tokens().clone();
        let mut args = vec![];
        while self.check(TokenType::OneChar(OneChar::Comma)) {
            let comma = self.advance();
            tokens.push(comma.clone());
            let arg = self.expression()?;
            tokens.extend(arg.get_tokens().clone());
            args.push((comma, Box::new(arg)));
        }
        Ok(Arguments::Arguments(tokens, (Box::new(arg), args)))
    }

    fn primary(&mut self) -> Result<Primary, Error> {
        if self.match_token_type(vec![
            TokenType::KeyWord(KeyWord::False),
            TokenType::KeyWord(KeyWord::True),
            TokenType::KeyWord(KeyWord::Null),
            TokenType::Literal(Literal::Integer(Default::default())),
            TokenType::Literal(Literal::Float(Default::default())),
            TokenType::Literal(Literal::String(Default::default())),
            TokenType::Literal(Literal::Identifier(Default::default())),
        ]) {
            let token = self.advance();
            Ok(Primary::Literal(vec![token.clone()], (token,)))
        } else if self.check(TokenType::OneChar(OneChar::LeftParen)) {
            let left_p = self.advance();
            let expr = self.expression()?;
            let right_p = self.consume(
                TokenType::OneChar(OneChar::RightParen),
                "Expected ')' after expression",
                "Syntax",
            )?;
            let mut tokens = vec![left_p.clone()];
            tokens.extend(expr.get_tokens().clone());
            tokens.push(right_p.clone());
            Ok(Primary::Parenthesized(
                tokens,
                (left_p, Box::new(expr), right_p),
            ))
        } else if self.check(TokenType::KeyWord(KeyWord::Func)) {
            let func = self.advance();
            let mut tokens = vec![func.clone()];
            let func_decl = self.func_decl()?;
            tokens.extend(func_decl.get_tokens().clone());
            Ok(Primary::Func(tokens, (func, Box::new(func_decl))))
        } else {
            Err(Error::new(
                Parse,
                &[self.peek().clone()],
                "Expected expression",
                "Syntax",
            ))
        }
    }
}

// define_parser_include!("rules.txt");
