use crate::msg::{Msg, MsgWithPos};
use crate::reader::Reader;
use crate::token::*;

use std::collections::HashMap;
pub struct Lexer
{
    reader: Reader,
    keywords: HashMap<&'static str, TokenKind>,
}
use hmap::hmap;

impl Lexer
{
    pub fn from_str(code: &str) -> Lexer
    {
        let reader = Reader::from_string(code);
        Lexer::new(reader)
    }

    pub fn new(reader: Reader) -> Lexer
    {
        let keywords = hmap!(
            "self" => TokenKind::This,
            "function" => TokenKind::Fun,
            "let" => TokenKind::Let,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "in" => TokenKind::In,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            "match" => TokenKind::Match,
            "continue" => TokenKind::Continue,
            "const" => TokenKind::Const,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,
            "type" => TokenKind::Type,
            "throw" => TokenKind::Throw,
            "do" => TokenKind::Do,
            "import" => TokenKind::Import,
            "internal" => TokenKind::Internal,
            "class" => TokenKind::Class,
            "implements" => TokenKind::Implements,
            "new" => TokenKind::New,
            "include" => TokenKind::Include
        );

        Lexer { reader: reader,
                keywords: keywords }
    }

    pub fn filename(&self) -> &str
    {
        self.reader.filename()
    }

    fn read_multi_comment(&mut self) -> Result<(), MsgWithPos>
    {
        let pos = self.reader.pos();

        self.read_char();
        self.read_char();

        while !self.cur().is_none() && !self.is_multi_comment_end()
        {
            self.read_char();
        }

        if self.cur().is_none()
        {
            return Err(MsgWithPos::new(pos, Msg::UnclosedComment));
        }

        self.read_char();
        self.read_char();

        Ok(())
    }

    pub fn read_token(&mut self) -> Result<Token, MsgWithPos>
    {
        loop
        {
            self.skip_white();

            let pos = self.reader.pos();
            let ch = self.cur();

            if let None = ch
            {
                return Ok(Token::new(TokenKind::End, pos));
            }

            if is_digit(ch)
            {
                return self.read_number();
            }
            else if self.is_comment_start()
            {
                self.read_comment()?;
            }
            else if self.is_multi_comment_start()
            {
                self.read_multi_comment()?;
            }
            else if is_identifier_start(ch)
            {
                return self.read_identifier();
            }
            else if is_quote(ch)
            {
                return self.read_string();
            }
            else if is_char_quote(ch)
            {
                return self.read_char_literal();
            }
            else if is_operator(ch)
            {
                return self.read_operator();
            }
            else
            {
                let ch = ch.unwrap();

                return Err(MsgWithPos::new(pos, Msg::UnknownChar(ch)));
            }
        }
    }

    fn skip_white(&mut self)
    {
        while is_whitespace(self.cur())
        {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> Result<Token, MsgWithPos>
    {
        let pos = self.reader.pos();
        let mut value = String::new();

        while is_identifier(self.cur())
        {
            let ch = self.cur().unwrap();
            self.read_char();
            value.push(ch);
        }

        let lookup = self.keywords.get(&value[..]).cloned();
        let mut ttype;

        if let Some(tok_type) = lookup
        {
            ttype = tok_type;
        }
        else if value == "_"
        {
            ttype = TokenKind::Underscore;
        }
        else
        {
            ttype = TokenKind::Identifier(value);
        }

        Ok(Token::new(ttype, pos))
    }

    fn read_char_literal(&mut self) -> Result<Token, MsgWithPos>
    {
        let pos = self.reader.pos();

        self.read_char();
        let ch = self.read_escaped_char(pos, Msg::UnclosedChar)?;

        if is_char_quote(self.cur())
        {
            self.read_char();

            let ttype = TokenKind::LitChar(ch);
            Ok(Token::new(ttype, pos))
        }
        else
        {
            Err(MsgWithPos::new(pos, Msg::UnclosedChar))
        }
    }

    fn read_escaped_char(&mut self, pos: Position, unclosed: Msg) -> Result<char, MsgWithPos>
    {
        if let Some(ch) = self.cur()
        {
            self.read_char();

            if ch == '\\'
            {
                let ch = if let Some(ch) = self.cur()
                {
                    ch
                }
                else
                {
                    return Err(MsgWithPos::new(pos, unclosed));
                };

                self.read_char();

                match ch
                {
                    '\\' => Ok('\\'),
                    'n' => Ok('\n'),
                    't' => Ok('\t'),
                    'r' => Ok('\r'),
                    '\"' => Ok('\"'),
                    '\'' => Ok('\''),
                    '0' => Ok('\0'),
                    _ =>
                    {
                        let msg = Msg::InvalidEscapeSequence(ch);
                        Err(MsgWithPos::new(pos, msg))
                    }
                }
            }
            else
            {
                Ok(ch)
            }
        }
        else
        {
            Err(MsgWithPos::new(pos, unclosed))
        }
    }

    fn read_string(&mut self) -> Result<Token, MsgWithPos>
    {
        let pos = self.reader.pos();
        let mut value = String::new();

        self.read_char();

        while !self.cur().is_none() && !is_quote(self.cur())
        {
            let ch = self.read_escaped_char(pos, Msg::UnclosedString)?;
            value.push(ch);
        }

        if is_quote(self.cur())
        {
            self.read_char();

            let ttype = TokenKind::String(value);
            Ok(Token::new(ttype, pos))
        }
        else
        {
            Err(MsgWithPos::new(pos, Msg::UnclosedString))
        }
    }

    fn read_operator(&mut self) -> Result<Token, MsgWithPos>
    {
        let mut tok = self.build_token(TokenKind::End);
        let ch = self.cur().unwrap();
        self.read_char();

        let nch = self.cur().unwrap_or('x');

        tok.kind = match ch
        {
            '+' => TokenKind::Add,
            '-' =>
            {
                if nch == '>'
                {
                    self.read_char();
                    TokenKind::Arrow
                }
                else
                {
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

            '|' =>
            {
                if nch == '|'
                {
                    self.read_char();
                    TokenKind::Or
                }
                else
                {
                    TokenKind::BitOr
                }
            }

            '&' =>
            {
                if nch == '&'
                {
                    self.read_char();
                    TokenKind::And
                }
                else
                {
                    TokenKind::BitAnd
                }
            }

            '^' => TokenKind::Caret,
            '~' => TokenKind::Tilde,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            ':' =>
            {
                if nch == ':'
                {
                    self.read_char();
                    TokenKind::Sep
                }
                else
                {
                    TokenKind::Colon
                }
            }
            '.' => TokenKind::Dot,
            '=' =>
            {
                if nch == '='
                {
                    self.read_char();
                    TokenKind::EqEq
                }
                else
                {
                    TokenKind::Eq
                }
            }

            '<' => match nch
            {
                '=' =>
                {
                    self.read_char();
                    TokenKind::Le
                }

                '<' =>
                {
                    self.read_char();
                    TokenKind::LtLt
                }

                _ => TokenKind::Lt,
            },

            '>' => match nch
            {
                '=' =>
                {
                    self.read_char();
                    TokenKind::Ge
                }

                '>' =>
                {
                    self.read_char();

                    TokenKind::GtGt
                }

                _ => TokenKind::Gt,
            },

            '!' =>
            {
                if nch == '='
                {
                    self.read_char();
                    TokenKind::Ne
                }
                else
                {
                    TokenKind::Not
                }
            }

            _ =>
            {
                return Err(MsgWithPos::new(tok.position, Msg::UnknownChar(ch)));
            }
        };

        Ok(tok)
    }

    fn read_comment(&mut self) -> Result<(), MsgWithPos>
    {
        while !self.cur().is_none() && !is_newline(self.cur())
        {
            self.read_char();
        }

        Ok(())
    }

    fn read_digits(&mut self, buffer: &mut String, base: IntBase)
    {
        while is_digit_or_underscore(self.cur(), base)
        {
            let ch = self.cur().unwrap();
            self.read_char();
            buffer.push(ch);
        }
    }

    fn read_char(&mut self)
    {
        self.reader.advance();
    }

    fn cur(&self) -> Option<char>
    {
        self.reader.cur()
    }

    fn next(&self) -> Option<char>
    {
        self.reader.next()
    }

    fn build_token(&self, kind: TokenKind) -> Token
    {
        Token::new(kind, self.reader.pos())
    }

    fn is_comment_start(&self) -> bool
    {
        self.cur() == Some('/') && self.next() == Some('/')
    }

    fn is_multi_comment_start(&self) -> bool
    {
        self.cur() == Some('/') && self.next() == Some('*')
    }

    fn is_multi_comment_end(&self) -> bool
    {
        self.cur() == Some('*') && self.next() == Some('/')
    }

    fn read_number(&mut self) -> Result<Token, MsgWithPos>
    {
        let pos = self.reader.pos();
        let mut value = String::new();

        let base = if self.cur() == Some('0')
        {
            let next = self.next();

            match next
            {
                Some('x') =>
                {
                    self.read_char();
                    self.read_char();

                    IntBase::Hex
                }

                Some('b') =>
                {
                    self.read_char();
                    self.read_char();

                    IntBase::Bin
                }

                _ => IntBase::Dec,
            }
        }
        else
        {
            IntBase::Dec
        };

        self.read_digits(&mut value, base);

        if base == IntBase::Dec && self.cur() == Some('.') && is_digit(self.next())
        {
            self.read_char();
            value.push('.');

            self.read_digits(&mut value, IntBase::Dec);

            if self.cur() == Some('e') || self.cur() == Some('E')
            {
                value.push(self.cur().unwrap());
                self.read_char();

                if self.cur() == Some('+') || self.cur() == Some('-')
                {
                    value.push(self.cur().unwrap());
                    self.read_char();
                }

                self.read_digits(&mut value, IntBase::Dec);
            }

            let ttype = TokenKind::LitFloat(value);
            return Ok(Token::new(ttype, pos));
        }

        let ttype = TokenKind::LitInt(value, base, IntSuffix::Int);
        Ok(Token::new(ttype, pos))
    }
}

fn is_digit(ch: Option<char>) -> bool
{
    ch.map(|ch| ch.is_digit(10)).unwrap_or(false)
}

fn is_digit_or_underscore(ch: Option<char>, base: IntBase) -> bool
{
    ch.map(|ch| ch.is_digit(base.num()) || ch == '_')
      .unwrap_or(false)
}

fn is_whitespace(ch: Option<char>) -> bool
{
    ch.map(|ch| ch.is_whitespace()).unwrap_or(false)
}

fn is_newline(ch: Option<char>) -> bool
{
    ch == Some('\n')
}

fn is_quote(ch: Option<char>) -> bool
{
    ch == Some('\"')
}

fn is_char_quote(ch: Option<char>) -> bool
{
    ch == Some('\'')
}

fn is_operator(ch: Option<char>) -> bool
{
    ch.map(|ch| "^+-*/%&|,=!~;:.()[]{}<>".contains(ch))
      .unwrap_or(false)
}

fn is_identifier_start(ch: Option<char>) -> bool
{
    match ch
    {
        Some(ch) => (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_',
        _ => false,
    }
}

fn is_identifier(ch: Option<char>) -> bool
{
    is_identifier_start(ch) || is_digit(ch)
}
