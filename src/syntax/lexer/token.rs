use std::fmt;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind {
    String(String),
    LitChar(char),
    LitInt(String, IntBase, IntSuffix),
    LitFloat(String, FloatSuffix),
    Identifier(String),
    End,

    LQuote,
    RQuote,

    // Keywords
    Fun,
    Let,
    Var,
    While,
    If,
    Else,
    Loop,
    Break,
    Continue,
    Return,
    True,
    False,
    Null,
    Pub,
    Static,
    Inline,
    Import,
    Extern,
    Link,
    Enum,
    Type,
    Alias,
    Struct,
    Const,
    ConstExpr,
    SizeOf,
    Underscore,
    Defer,

    // Operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Not,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Dot,
    Colon,
    Sep, // ::
    Arrow,
    Tilde,
    BitOr,
    BitAnd,
    Caret,
    And,
    Or,
    Internal,

    Eq,
    EqEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    EqEqEq,
    NeEqEq,
    Is,
    As,
    DotDotDot,
    GtGt,
    GtGtGt,
    LtLt,
}

impl TokenKind {
    pub fn name(&self) -> &str {
        match *self {
            TokenKind::String(_) => "string",
            TokenKind::LitInt(_, _, suffix) => match suffix {
                IntSuffix::Byte => "byte number",
                IntSuffix::Int => "int number",
                IntSuffix::Long => "long number",
                IntSuffix::UByte => "unsigned byte number",
                IntSuffix::UInt => "unsigned int number",
                IntSuffix::ULong => "unsigned long number",
            },
            TokenKind::DotDotDot => "...",

            TokenKind::LitChar(_) => "char",

            TokenKind::LitFloat(_, suffix) => match suffix {
                FloatSuffix::Float => "float number",
                FloatSuffix::Double => "double number",
            },
            TokenKind::Import => "import",
            TokenKind::Identifier(_) => "identifier",
            TokenKind::End => "<<EOF>>",

            TokenKind::LQuote => "<",
            TokenKind::RQuote => ">",

            TokenKind::Fun => "fun",
            TokenKind::Let => "let",
            TokenKind::Var => "var",
            TokenKind::While => "while",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Loop => "loop",

            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Return => "return",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Null => "null",
            TokenKind::Pub => "pub",
            TokenKind::Static => "static",
            TokenKind::Inline => "inline",
            TokenKind::Extern => "extern",
            TokenKind::Link => "link",
            TokenKind::Enum => "enum",
            TokenKind::Type => "type",
            TokenKind::Alias => "alias",
            TokenKind::Struct => "struct",
            TokenKind::Const => "const",
            TokenKind::SizeOf => "sizeof",
            TokenKind::ConstExpr => "constexpr",
            TokenKind::Underscore => "_",
            TokenKind::Defer => "defer",

            // Operators
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Mod => "%",
            TokenKind::Not => "!",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Dot => ".",
            TokenKind::Colon => ":",
            TokenKind::Sep => "::",
            TokenKind::Arrow => "=>",
            TokenKind::Tilde => "~",
            TokenKind::BitOr => "|",
            TokenKind::BitAnd => "&",
            TokenKind::Caret => "^",
            TokenKind::And => "&&",
            TokenKind::Or => "||",
            TokenKind::Internal => "internal",

            TokenKind::Eq => "=",
            TokenKind::EqEq => "==",
            TokenKind::Ne => "!=",
            TokenKind::Lt => "<",
            TokenKind::Le => "<=",
            TokenKind::Gt => ">",
            TokenKind::Ge => ">=",

            TokenKind::GtGt => ">>",
            TokenKind::GtGtGt => ">>>",
            TokenKind::LtLt => "<<",

            TokenKind::EqEqEq => "===",
            TokenKind::NeEqEq => "!==",
            TokenKind::Is => "is",
            TokenKind::As => "as",
        }
    }
}

use crate::syntax::position::Position;

#[derive(PartialEq, Eq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

impl Token {
    pub fn new(tok: TokenKind, pos: Position) -> Token {
        Token {
            kind: tok,
            position: pos,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == TokenKind::End
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn name(&self) -> String {
        match self.kind {
            TokenKind::LitInt(ref val, _, suffix) => {
                let suffix = match suffix {
                    IntSuffix::Byte => "B",
                    IntSuffix::Int => "",
                    IntSuffix::Long => "L",
                    IntSuffix::UByte => "UB",
                    IntSuffix::UInt => "UI",
                    IntSuffix::ULong => "UL",
                };

                format!("{}{}", val, suffix)
            }

            TokenKind::String(ref val) => format!("\"{}\"", &val),
            TokenKind::Identifier(ref val) => val.clone(),

            _ => self.kind.name().into(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntBase {
    Bin,
    Dec,
    Hex,
}

impl IntBase {
    pub fn num(self) -> u32 {
        match self {
            IntBase::Bin => 2,
            IntBase::Dec => 10,
            IntBase::Hex => 16,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum IntSuffix {
    Int,
    Long,
    Byte,
    ULong,
    UInt,
    UByte,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FloatSuffix {
    Float,
    Double,
}
