pub mod position;
pub mod reader;
pub mod token;

use std::collections::HashMap;

use self::position::Position;
use self::reader::Reader;
use self::token::{FloatSuffix, IntSuffix, IntBase, Token, TokenKind};
use crate::error::{Msg, MsgWithPos};
use crate::unwrap_err;


pub struct Lexer {
    reader: Reader,
    keywords: HashMap<&'static str, TokenKind>,
}

impl Lexer {
    pub fn from_str(code: &str) -> Lexer {
        let reader = Reader::from_string(code);
        Lexer::new(reader)
    }

    pub fn new(reader: Reader) -> Lexer {
        let keywords = keywords_in_map();

        Lexer {
            reader: reader,
            keywords: keywords,
        }
    }

    pub fn filename(&self) -> &str {
        self.reader.filename()
    }

    pub fn read_token(&mut self) -> Result<Token, MsgWithPos> {
        loop {
            self.skip_white();

            let pos = self.reader.pos();
            let ch = self.cur();

            if let None = ch {
                return Ok(Token::new(TokenKind::End, pos));
            }

            if is_digit(ch) {
                return self.read_number();

            } else if self.is_comment_start() {
                unwrap_err!(self.read_comment());

            } else if self.is_multi_comment_start() {
                unwrap_err!(self.read_multi_comment());

            } else if is_identifier_start(ch) {
                return self.read_identifier();

            } else if is_quote(ch) {
                return self.read_string();

            } else if is_char_quote(ch) {
                return self.read_char_literal();

            } else if is_operator(ch) {
                return self.read_operator();

            } else {
                let ch = ch.unwrap();

                return Err(MsgWithPos::new(pos, Msg::UnknownChar(ch)));
            }
        }
    }

    fn skip_white(&mut self) {
        while is_whitespace(self.cur()) {
            self.read_char();
        }
    }

    fn read_comment(&mut self) -> Result<(), MsgWithPos> {
        while !self.cur().is_none() && !is_newline(self.cur()) {
            self.read_char();
        }

        Ok(())
    }

    fn read_multi_comment(&mut self) -> Result<(), MsgWithPos> {
        let pos = self.reader.pos();

        self.read_char();
        self.read_char();

        while !self.cur().is_none() && !self.is_multi_comment_end() {
            self.read_char();
        }

        if self.cur().is_none() {
            return Err(MsgWithPos::new(pos, Msg::UnclosedComment));
        }

        self.read_char();
        self.read_char();

        Ok(())
    }

    fn read_identifier(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();
        let mut value = String::new();

        while is_identifier(self.cur()) {
            let ch = self.cur().unwrap();
            self.read_char();
            value.push(ch);
        }

        let lookup = self.keywords.get(&value[..]).cloned();
        let mut ttype;

        if let Some(tok_type) = lookup {
            ttype = tok_type;

            if ttype == TokenKind::Try {
                if let Some(ch) = self.cur() {
                    if ch == '!' || ch == '?' {
                        self.read_char();

                        ttype = if ch == '!' {
                            TokenKind::TryForce
                        } else {
                            TokenKind::TryOpt
                        };
                    }
                }
            }

        } else if value == "_" {
            ttype = TokenKind::Underscore;
        } else {
            ttype = TokenKind::Identifier(value);
        }

        Ok(Token::new(ttype, pos))
    }

    fn read_char_literal(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();

        self.read_char();
        let ch = self.read_escaped_char(pos, Msg::UnclosedChar)?;

        if is_char_quote(self.cur()) {
            self.read_char();

            let ttype = TokenKind::LitChar(ch);
            Ok(Token::new(ttype, pos))
        } else {
            Err(MsgWithPos::new(pos, Msg::UnclosedChar))
        }
    }

    fn read_escaped_char(&mut self, pos: Position, unclosed: Msg) -> Result<char, MsgWithPos> {
        if let Some(ch) = self.cur() {
            self.read_char();

            if ch == '\\' {
                let ch = if let Some(ch) = self.cur() {
                    ch
                } else {
                    return Err(MsgWithPos::new(pos, unclosed));
                };

                self.read_char();

                match ch {
                    '\\' => Ok('\\'),
                    'n' => Ok('\n'),
                    't' => Ok('\t'),
                    'r' => Ok('\r'),
                    '\"' => Ok('\"'),
                    '\'' => Ok('\''),
                    '0' => Ok('\0'),
                    _ => {
                        let msg = Msg::InvalidEscapeSequence(ch);
                        Err(MsgWithPos::new(pos, msg))
                    }
                }

            } else {
                Ok(ch)
            }

        } else {
            Err(MsgWithPos::new(pos, unclosed))
        }
    }

    fn read_string(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();
        let mut value = String::new();

        self.read_char();

        while !self.cur().is_none() && !is_quote(self.cur()) {
            let ch = self.read_escaped_char(pos, Msg::UnclosedString)?;
            value.push(ch);
        }

        if is_quote(self.cur()) {
            self.read_char();

            let ttype = TokenKind::String(value);
            Ok(Token::new(ttype, pos))

        } else {
            Err(MsgWithPos::new(pos, Msg::UnclosedString))
        }
    }

    fn read_operator(&mut self) -> Result<Token, MsgWithPos> {
        let mut tok = self.build_token(TokenKind::End);
        let ch = self.cur().unwrap();
        self.read_char();

        let nch = self.cur().unwrap_or('x');
        let nnch = self.next().unwrap_or('x');

        tok.kind = match ch {
            '+' => TokenKind::Add,
            '-' => {
                if nch == '>' {
                    self.read_char();
                    TokenKind::Arrow
                } else {
                    TokenKind::Sub
                }
            }

            '*' => TokenKind::Mul,
            '/' => TokenKind::Div,
            '%' => TokenKind::Mod,

            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,

            '|' => {
                if nch == '|' {
                    self.read_char();
                    TokenKind::Or
                } else {
                    TokenKind::BitOr
                }
            }

            '&' => {
                if nch == '&' {
                    self.read_char();
                    TokenKind::And
                } else {
                    TokenKind::BitAnd
                }
            }

            '^' => TokenKind::Caret,
            '~' => TokenKind::Tilde,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            ':' => {
                if nch == ':' {
                    self.read_char();
                    TokenKind::Sep
                } else {
                    TokenKind::Colon
                }
            }
            '.' => TokenKind::Dot,
            '=' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenKind::EqEqEq
                    } else {
                        TokenKind::EqEq
                    }
                } else {
                    TokenKind::Eq
                }
            }

            '<' => {
                match nch {
                    '=' => {
                        self.read_char();
                        TokenKind::Le
                    }

                    '<' => {
                        self.read_char();
                        TokenKind::LtLt
                    }

                    _ => TokenKind::Lt,
                }
            }

            '>' => {
                match nch {
                    '=' => {
                        self.read_char();
                        TokenKind::Ge
                    }

                    '>' => {
                        self.read_char();

                        if nnch == '>' {
                            self.read_char();
                            TokenKind::GtGtGt
                        } else {
                            TokenKind::GtGt
                        }
                    }

                    _ => TokenKind::Gt,
                }
            }

            '!' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenKind::NeEqEq
                    } else {
                        TokenKind::Ne
                    }
                } else {
                    TokenKind::Not
                }
            }

            _ => {
                return Err(MsgWithPos::new(tok.position, Msg::UnknownChar(ch)));
            }
        };

        Ok(tok)
    }

    fn read_number(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();
        let mut value = String::new();

        let base = if self.cur() == Some('0') {
            let next = self.next();

            match next {
                Some('x') => {
                    self.read_char();
                    self.read_char();

                    IntBase::Hex
                }

                Some('b') => {
                    self.read_char();
                    self.read_char();

                    IntBase::Bin
                }

                _ => IntBase::Dec,
            }
        } else {
            IntBase::Dec
        };

        self.read_digits(&mut value, base);

        if base == IntBase::Dec && self.cur() == Some('.') && is_digit(self.next()) {
            self.read_char();
            value.push('.');

            self.read_digits(&mut value, IntBase::Dec);

            if self.cur() == Some('e') || self.cur() == Some('E') {
                value.push(self.cur().unwrap());
                self.read_char();

                if self.cur() == Some('+') || self.cur() == Some('-') {
                    value.push(self.cur().unwrap());
                    self.read_char();
                }

                self.read_digits(&mut value, IntBase::Dec);
            }

            let suffix = match self.cur() {
                Some('D') => {
                    self.read_char();
                    FloatSuffix::Double
                }

                Some('F') => {
                    self.read_char();
                    FloatSuffix::Float
                }

                _ => FloatSuffix::Double,
            };

            let ttype = TokenKind::LitFloat(value, suffix);
            return Ok(Token::new(ttype, pos));
        }

        let suffix = match self.cur() {
            Some('L') => {
                self.read_char();
                IntSuffix::Long
            }

            Some('Y') => {
                self.read_char();
                IntSuffix::Byte
            }

            Some('D') if base == IntBase::Dec => {
                self.read_char();

                let ttype = TokenKind::LitFloat(value, FloatSuffix::Double);
                return Ok(Token::new(ttype, pos));
            }

            Some('F') if base == IntBase::Dec => {
                self.read_char();

                let ttype = TokenKind::LitFloat(value, FloatSuffix::Float);
                return Ok(Token::new(ttype, pos));
            }

            _ => IntSuffix::Int,
        };

        let ttype = TokenKind::LitInt(value, base, suffix);
        Ok(Token::new(ttype, pos))
    }

    fn read_digits(&mut self, buffer: &mut String, base: IntBase) {
        while is_digit_or_underscore(self.cur(), base) {
            let ch = self.cur().unwrap();
            self.read_char();
            buffer.push(ch);
        }
    }

    fn read_char(&mut self) {
        self.reader.advance();
    }

    fn cur(&self) -> Option<char> {
        self.reader.cur()
    }

    fn next(&self) -> Option<char> {
        self.reader.next()
    }

    fn build_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.reader.pos())
    }

    fn is_comment_start(&self) -> bool {
        self.cur() == Some('/') && self.next() == Some('/')
    }

    fn is_multi_comment_start(&self) -> bool {
        self.cur() == Some('/') && self.next() == Some('*')
    }

    fn is_multi_comment_end(&self) -> bool {
        self.cur() == Some('*') && self.next() == Some('/')
    }
}

fn is_digit(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_digit(10)).unwrap_or(false)
}

fn is_digit_or_underscore(ch: Option<char>, base: IntBase) -> bool {
    ch.map(|ch| ch.is_digit(base.num()) || ch == '_')
        .unwrap_or(false)
}

fn is_whitespace(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_whitespace()).unwrap_or(false)
}

fn is_newline(ch: Option<char>) -> bool {
    ch == Some('\n')
}

fn is_quote(ch: Option<char>) -> bool {
    ch == Some('\"')
}

fn is_char_quote(ch: Option<char>) -> bool {
    ch == Some('\'')
}

fn is_operator(ch: Option<char>) -> bool {
    ch.map(|ch| "^+-*/%&|,=!~;:.()[]{}<>".contains(ch))
        .unwrap_or(false)
}

fn is_identifier_start(ch: Option<char>) -> bool {
    match ch {
        Some(ch) => (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_',
        _ => false,
    }
}

fn is_identifier(ch: Option<char>) -> bool {
    is_identifier_start(ch) || is_digit(ch)
}

fn keywords_in_map() -> HashMap<&'static str, TokenKind> {
    let mut keywords = HashMap::new();

    keywords.insert("class", TokenKind::Class);
    keywords.insert("self", TokenKind::This);
    keywords.insert("Self", TokenKind::CapitalThis);
    keywords.insert("super", TokenKind::Super);
    keywords.insert("func", TokenKind::Fun);
    keywords.insert("let", TokenKind::Let);
    keywords.insert("var", TokenKind::Var);
    keywords.insert("while", TokenKind::While);
    keywords.insert("if", TokenKind::If);
    keywords.insert("else", TokenKind::Else);
    keywords.insert("for", TokenKind::For);
    keywords.insert("in", TokenKind::In);
    keywords.insert("impl", TokenKind::Impl);
    keywords.insert("loop", TokenKind::Loop);
    keywords.insert("break", TokenKind::Break);
    keywords.insert("continue", TokenKind::Continue);
    keywords.insert("return", TokenKind::Return);
    keywords.insert("true", TokenKind::True);
    keywords.insert("false", TokenKind::False);
    keywords.insert("nil", TokenKind::Nil);
    keywords.insert("enum", TokenKind::Enum);
    keywords.insert("type", TokenKind::Type);
    keywords.insert("alias", TokenKind::Alias);
    keywords.insert("struct", TokenKind::Struct);
    keywords.insert("trait", TokenKind::Trait);
    keywords.insert("throws", TokenKind::Throws);
    keywords.insert("throw", TokenKind::Throw);
    keywords.insert("try", TokenKind::Try);
    keywords.insert("do", TokenKind::Do);
    keywords.insert("catch", TokenKind::Catch);
    keywords.insert("finally", TokenKind::Finally);
    keywords.insert("abstract", TokenKind::Abstract);
    keywords.insert("open", TokenKind::Open);
    keywords.insert("override", TokenKind::Override);
    keywords.insert("defer", TokenKind::Defer);
    keywords.insert("final", TokenKind::Final);
    keywords.insert("is", TokenKind::Is);
    keywords.insert("as", TokenKind::As);
    keywords.insert("internal", TokenKind::Internal);
    keywords.insert("init", TokenKind::Init);
    keywords.insert("public", TokenKind::Pub);
    keywords.insert("static", TokenKind::Static);
    keywords.insert("spawn", TokenKind::Spawn);
    keywords.insert("const", TokenKind::Const);

    keywords
}
