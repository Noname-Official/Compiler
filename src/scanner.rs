use crate::{
    error::{Error, ErrorType::Scan},
    token::{
        token_type::{KeyWord, Literal, OneChar, OneTwoChar, Special, TokenType},
        Token,
    },
};
use std::{char, iter::Peekable, rc::Rc, str::FromStr, sync::RwLock};
use strum::{EnumMessage, IntoEnumIterator};

pub struct Scanner<'a, T: Iterator<Item = char>> {
    file: &'a str,
    cur_line: Rc<RwLock<String>>,
    line: usize,
    pos: usize,
    iter: Peekable<T>,
    current_token: String,
    pub(crate) errs: Vec<Error>,
    finished: bool,
}

impl<'a, T: Iterator<Item = char>> Scanner<'a, T> {
    pub fn new(file: &'a str, text: T) -> Self {
        Self {
            file,
            cur_line: Rc::new(RwLock::new(String::new())),
            line: 1,
            pos: 0,
            iter: text.peekable(),
            current_token: String::new(),
            errs: Vec::new(),
            finished: false,
        }
    }

    fn is_at_end(&mut self) -> bool {
        return self.iter.peek().is_none();
    }

    fn advance(&mut self) -> char {
        let char = self.iter.next().unwrap_or('\0');
        self.pos += 1;
        if char == '\n' {
            self.line += 1;
            self.pos = 0;
            self.cur_line = Rc::new(RwLock::new(String::new()));
        } else {
            self.cur_line.write().unwrap().push(char);
        }
        self.current_token.push(char);
        char
    }

    fn add_token(&mut self, token_type: TokenType) -> Token {
        Token::new(
            self.file,
            Rc::clone(&self.cur_line),
            token_type,
            self.current_token.clone(),
            self.line,
            self.pos - self.current_token.len() + 1,
            self.pos,
        )
    }

    fn number(&mut self) -> Token {
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

    fn identifier(&mut self) -> Token {
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
            self.add_token(TokenType::KeyWord(KeyWord::from_str(&value).unwrap()))
        } else {
            self.add_token(TokenType::Literal(Literal::Identifier(value)))
        }
    }

    fn string(&mut self) -> Result<Token, Error> {
        let start_line_num = self.line;
        let start_line = Rc::clone(&self.cur_line);
        let end = self.cur_line.read().unwrap().len();
        let start_pos = self.pos;
        loop {
            let peeked = self.iter.peek();
            if peeked.is_some() && *peeked.unwrap() == '\n' {
                self.line += 1;
                self.pos = 0;
                self.cur_line = Rc::new(RwLock::new(String::new()));
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
                    start_line,
                    TokenType::Literal(Literal::String(Default::default())),
                    self.current_token.clone(),
                    start_line_num,
                    start_pos,
                    end,
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
        Ok(self.add_token(TokenType::Literal(Literal::String(value))))
    }

    fn scan_token(&mut self) -> Result<Token, Error> {
        loop {
            // We are at the beginning of the next lexeme.
            self.current_token = String::new();
            let c = self.advance();
            let peeked = self.iter.peek().cloned();

            if OneChar::iter().any(|x| x.get_serializations()[0].chars().nth(0).unwrap() == c) {
                return Ok(self.add_token(TokenType::OneChar(
                    OneChar::from_str(&c.to_string()).unwrap(),
                )));
            } else if OneTwoChar::iter()
                .any(|x| x.get_serializations()[0].chars().nth(0).unwrap() == c)
            {
                if peeked.is_some()
                    && OneTwoChar::iter()
                        .filter(|x| x.get_serializations()[0].len() == 2)
                        .any(|x| {
                            x.get_serializations()[0].chars().nth(1).unwrap() == peeked.unwrap()
                        })
                {
                    self.advance();
                    return Ok(self.add_token(TokenType::OneTwoChar(
                        OneTwoChar::from_str(format!("{}{}", c, peeked.unwrap()).as_str()).unwrap(),
                    )));
                } else {
                    return Ok(self.add_token(TokenType::OneTwoChar(
                        OneTwoChar::from_str(&c.to_string()).unwrap(),
                    )));
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
                    return Ok(self.add_token(TokenType::Special(Special::Slash)));
                }
            } else if " \r\t\n".contains(c) {
                // ignore whitespace
            } else if Literal::String(Default::default()).get_serializations()[0].contains(c) {
                return self.string();
            } else if c.is_ascii_digit() {
                return Ok(self.number());
            } else if c.is_alphabetic() {
                return Ok(self.identifier());
            } else {
                return Err(Error::new(
                    Scan,
                    &[Token::new(
                        self.file,
                        Rc::clone(&self.cur_line),
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
        }
    }
}

impl<T: Iterator<Item = char>> Iterator for Scanner<'_, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let result = self.is_at_end();
            if result {
                break;
            }

            match self.scan_token() {
                Ok(token) => return Some(token),
                Err(e) => self.errs.push(e),
            }
        }
        if !self.finished {
            let token = Token::new(
                self.file,
                Rc::clone(&self.cur_line),
                TokenType::Special(Special::Eof),
                Default::default(),
                self.line,
                self.pos + 1,
                self.pos + 1,
            );
            self.finished = true;
            return Some(token);
        }
        // TODO: handle errors
        // if !self.errs.is_empty() {
        //     return Err(self.errs.clone());
        // }
        None
    }
}

mod tests {}
