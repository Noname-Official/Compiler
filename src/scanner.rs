use crate::{
    error::{Error, ErrorType::Scan},
    token::{
        token_type::{KeyWord, Literal, OneChar, OneTwoChar, Special, TokenType},
        Token,
    },
};
use std::{iter::Peekable, str::FromStr, vec::IntoIter};
use strum::{EnumMessage, IntoEnumIterator};

pub struct Scanner<'a> {
    file: &'a str,
    lines: Vec<String>,
    tokens: Vec<Token>,
    line: usize,
    pos: usize,
    iter: Peekable<IntoIter<char>>,
    current_token: String,
    errs: Vec<Error>,
}

impl<'a> Scanner<'a> {
    pub fn new(file: &'a str, text: String) -> Self {
        let text = text.replace('\t', "    ");
        Self {
            file,
            lines: text.split('\n').map(|x| x.to_string()).collect(),
            tokens: vec![],
            line: 1,
            pos: 0,
            iter: text.chars().collect::<Vec<_>>().into_iter().peekable(),
            current_token: String::new(),
            errs: Vec::new(),
        }
    }

    pub fn scan_tokens(&'a mut self) -> Result<&'a Vec<Token>, Vec<Error>> {
        self.tokens = Vec::new();
        loop {
            let result = self.is_at_end();
            if result {
                break;
            }

            // We are at the beginning of the next lexeme.
            self.current_token = String::new();
            if let Err(e) = self.scan_token() {
                self.errs.push(e)
            }
        }
        let token = Token::new(
            self.file,
            self.lines[self.line - 1].clone(),
            TokenType::Special(Special::Eof),
            Default::default(),
            self.line,
            self.pos + 1,
            self.pos + 1,
        );
        self.tokens.push(token);
        if !self.errs.is_empty() {
            return Err(self.errs.clone());
        }
        Ok(&self.tokens)
    }

    fn is_at_end(&mut self) -> bool {
        return self.iter.peek().is_none();
    }

    fn advance(&mut self) -> char {
        let char = self.iter.next().unwrap_or('\0');
        self.pos += 1;
        if char == '\n' {
            self.pos = 0;
        }
        self.current_token.push(char);
        char
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(
            self.file,
            self.lines[self.line - 1].clone(),
            token_type,
            self.current_token.clone(),
            self.line,
            self.pos - self.current_token.len() + 1,
            self.pos,
        ));
    }

    fn number(&mut self) {
        loop {
            let peeked = self.iter.peek();
            if peeked.is_none() || !peeked.unwrap().is_ascii_digit() {
                break;
            }
            self.advance();
        }

        let peeked = self.iter.peek();
        if peeked.is_some() && *peeked.unwrap() == '.' {
            self.advance();
            loop {
                let peeked = self.iter.peek();
                if peeked.is_none() || !peeked.unwrap().is_ascii_digit() {
                    break;
                }
                self.advance();
            }
            let value: f64 = self.current_token.parse().unwrap();
            self.add_token(TokenType::Literal(Literal::Float(value)))
        } else {
            let value: i64 = self.current_token.parse().unwrap();
            self.add_token(TokenType::Literal(Literal::Integer(value)))
        }
    }

    fn identifier(&mut self) {
        loop {
            let peeked = self.iter.peek();
            if peeked.is_none() || !peeked.unwrap().is_alphanumeric() && *peeked.unwrap() != '_' {
                break;
            }
            self.advance();
        }

        let value = self.current_token.clone();

        if KeyWord::iter()
            .map(|x| x.to_string().to_lowercase())
            .collect::<Vec<_>>()
            .contains(&value)
        {
            self.add_token(TokenType::KeyWord(KeyWord::from_str(&value).unwrap()));
        } else {
            self.add_token(TokenType::Literal(Literal::Identifier(value)));
        }
    }

    fn string(&mut self) -> Result<(), Error> {
        let start_line = self.line;
        let start_pos = self.pos;
        loop {
            let peeked = self.iter.peek();
            if peeked.is_some() && *peeked.unwrap() == '\n' {
                self.line += 1;
            }
            if peeked.is_none()
                || Literal::String(Default::default()).get_serializations()[0]
                    .contains(*peeked.unwrap())
            {
                break;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(Error::new(
                Scan,
                &[Token::new(
                    self.file,
                    self.lines[start_line - 1].clone(),
                    TokenType::Literal(Literal::String(Default::default())),
                    self.current_token.clone(),
                    start_line,
                    start_pos,
                    self.lines[start_line - 1].len(),
                )],
                "Unterminated string.",
                "Syntax",
            ));
        }

        // The closing ".
        self.advance();

        // Trim the surrounding quotes.
        let value = self
            .current_token
            .clone()
            .strip_prefix('"')
            .unwrap()
            .strip_suffix('"')
            .unwrap()
            .to_string();
        self.add_token(TokenType::Literal(Literal::String(value)));
        Ok(())
    }

    fn scan_token(&mut self) -> Result<(), Error> {
        let c = self.advance();
        let peeked = self.iter.peek().cloned();

        if OneChar::iter().any(|x| x.get_serializations()[0].chars().nth(0).unwrap() == c) {
            self.add_token(TokenType::OneChar(
                OneChar::from_str(&c.to_string()).unwrap(),
            ));
        } else if OneTwoChar::iter().any(|x| x.get_serializations()[0].chars().nth(0).unwrap() == c)
        {
            if peeked.is_some()
                && OneTwoChar::iter()
                    .filter(|x| x.get_serializations()[0].len() == 2)
                    .any(|x| x.get_serializations()[0].chars().nth(1).unwrap() == peeked.unwrap())
            {
                self.advance();
                self.add_token(TokenType::OneTwoChar(
                    OneTwoChar::from_str(format!("{}{}", c, peeked.unwrap()).as_str()).unwrap(),
                ));
            } else {
                self.add_token(TokenType::OneTwoChar(
                    OneTwoChar::from_str(&c.to_string()).unwrap(),
                ));
            }
        } else if c.to_string() == Special::Slash.get_serializations()[0] {
            if peeked.is_some()
                && peeked.unwrap()
                    == Special::MultilineComment.get_serializations()[0]
                        .chars()
                        .nth(1)
                        .unwrap()
            {
                loop {
                    let result = self.advance();
                    if result == '\n' {
                        self.line += 1;
                    }
                    let peeked = self.iter.peek();
                    if peeked.is_none() || result == '*' && *peeked.unwrap() == '/' {
                        self.advance();
                        break;
                    }
                }
            } else if peeked.is_some()
                && peeked.unwrap()
                    == Special::SingleLineComment.get_serializations()[0]
                        .chars()
                        .nth(1)
                        .unwrap()
            {
                loop {
                    let peeked = self.iter.peek();
                    if peeked.is_none() || *peeked.unwrap() == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else {
                self.add_token(TokenType::Special(Special::Slash));
            }
        } else if " \r\t".contains(c) {
            // ignore whitespace
        } else if c == '\n' {
            self.line += 1;
        } else if Literal::String(Default::default()).get_serializations()[0].contains(c) {
            self.string()?;
        } else if c.is_ascii_digit() {
            self.number();
        } else if c.is_alphabetic() {
            self.identifier();
        } else {
            return Err(Error::new(
                Scan,
                &[Token::new(
                    self.file,
                    self.lines[self.line - 1].clone(),
                    TokenType::OneChar(OneChar::Plus),
                    c.to_string(),
                    self.line,
                    self.pos,
                    self.pos,
                )],
                format!("Unexpected character '{c}'.").as_str(),
                "Syntax",
            ));
        }
        Ok(())
    }
}
