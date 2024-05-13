use std::{fmt, mem::discriminant};

pub mod token_type {
    use std::{
        fmt,
        hash::{Hash, Hasher},
    };
    use strum_macros::{EnumIter, EnumMessage, EnumString};

    #[derive(Debug, EnumIter, EnumString, EnumMessage, Clone, Copy, PartialEq, Hash, Eq)]
    pub enum OneChar {
        #[strum(serialize = "(")]
        LeftParen,
        #[strum(serialize = ")")]
        RightParen,
        #[strum(serialize = "{")]
        LeftBrace,
        #[strum(serialize = "}")]
        RightBrace,
        #[strum(serialize = ",")]
        Comma,
        #[strum(serialize = ".")]
        Dot,
        #[strum(serialize = "-")]
        Minus,
        #[strum(serialize = "+")]
        Plus,
        #[strum(serialize = ":")]
        Colon,
        #[strum(serialize = ";")]
        Semicolon,
        #[strum(serialize = "*")]
        Star,
    }

    #[derive(Debug, EnumIter, EnumString, EnumMessage, Clone, Copy, PartialEq, Hash, Eq)]
    pub enum OneTwoChar {
        #[strum(serialize = "!")]
        Bang,
        #[strum(serialize = "!=")]
        BangEqual,
        #[strum(serialize = "=")]
        Equal,
        #[strum(serialize = "==")]
        EqualEqual,
        #[strum(serialize = ">")]
        Greater,
        #[strum(serialize = ">=")]
        GreaterEqual,
        #[strum(serialize = "<")]
        Less,
        #[strum(serialize = "<=")]
        LessEqual,
    }

    #[derive(Debug, EnumIter, EnumMessage, Clone, PartialEq)]
    pub enum Literal {
        Identifier(String),
        #[strum(serialize = "'\"")]
        String(String),
        Integer(i64),
        Float(f64),
    }

    impl Hash for Literal {
        fn hash<H: Hasher>(&self, state: &mut H) {
            match self {
                Literal::Identifier(x) => {
                    state.write_u8(0);
                    x.hash(state)
                }
                Literal::String(x) => {
                    state.write_u8(1);
                    x.hash(state)
                }
                Literal::Integer(x) => {
                    state.write_u8(2);
                    x.hash(state)
                }
                Literal::Float(x) => {
                    state.write_u8(3);
                    x.to_bits().hash(state)
                }
            }
        }
    }

    impl Eq for Literal {}

    #[derive(Debug, EnumIter, EnumString, EnumMessage, Clone, Copy, PartialEq, Hash, Eq)]
    #[strum(serialize_all = "lowercase")]
    pub enum KeyWord {
        And,
        Break,
        Class,
        Continue,
        Else,
        Elif,
        False,
        Func,
        For,
        If,
        Null,
        Or,
        Print,
        Return,
        Super,
        This,
        True,
        Var,
        While,
        Const,
    }

    #[derive(Debug, EnumIter, EnumMessage, Clone, Copy, PartialEq, Hash, Eq)]
    pub enum Special {
        Eof,
        #[strum(serialize = "/")]
        Slash,
        #[strum(serialize = "/*")]
        MultilineComment,
        #[strum(serialize = "//")]
        SingleLineComment,
    }

    #[derive(Debug, Clone, PartialEq, Hash, Eq)]
    pub enum TokenType {
        OneChar(OneChar),
        OneTwoChar(OneTwoChar),
        Literal(Literal),
        KeyWord(KeyWord),
        Special(Special),
    }

    macro_rules! impl_display {
        ($e:ident) => {
            impl fmt::Display for $e {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    write!(f, "{:?}", self)
                }
            }
        };
    }

    impl_display!(TokenType);
    impl_display!(OneChar);
    impl_display!(OneTwoChar);
    impl_display!(Literal);
    impl_display!(KeyWord);
    impl_display!(Special);
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Token {
    pub token_type: token_type::TokenType,
    pub lexeme: String,
    pub file: String,
    pub line: usize,
    pub begin: usize,
    pub end: usize,
    pub line_contents: String,
}

impl<'a> Token {
    pub fn new(
        file: &'a str,
        line_contents: String,
        token_type: token_type::TokenType,
        lexeme: String,
        line: usize,
        begin: usize,
        end: usize,
    ) -> Self {
        Self {
            file: file.into(),
            line_contents,
            token_type,
            lexeme,
            line,
            begin,
            end,
        }
    }

    pub fn eq_type(&self, other: &token_type::TokenType) -> bool {
        if discriminant(&self.token_type) == discriminant(other) {
            match (&self.token_type, other) {
                (token_type::TokenType::OneChar(v), token_type::TokenType::OneChar(other_v)) => {
                    return discriminant(v) == discriminant(other_v);
                }
                (
                    token_type::TokenType::OneTwoChar(v),
                    token_type::TokenType::OneTwoChar(other_v),
                ) => {
                    return discriminant(v) == discriminant(other_v);
                }
                (token_type::TokenType::Literal(v), token_type::TokenType::Literal(other_v)) => {
                    return discriminant(v) == discriminant(other_v);
                }
                (token_type::TokenType::KeyWord(v), token_type::TokenType::KeyWord(other_v)) => {
                    return discriminant(v) == discriminant(other_v);
                }
                (token_type::TokenType::Special(v), token_type::TokenType::Special(other_v)) => {
                    return discriminant(v) == discriminant(other_v);
                }
                _ => unreachable!(),
            }
        }
        false
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[line {:?}] {:?}: {:?}",
            self.line, self.token_type, self.lexeme
        )
    }
}
