use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn new(x: u32, y: u32) -> Self {
        Self { line: x, column: y }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}:{})", self.line, self.column)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind {
    String(String),
    LitChar(char),
    LitInt(String, IntBase, IntSuffix),
    LitFloat(String),
    Identifier(String),
    End,

    LQuote,
    RQuote,

    // Keywords
    Include,
    Class,
    Implements,
    This,
    Match,
    Fun,
    New,
    Let,
    Var,
    While,
    If,
    Else,
    Loop,
    For,
    In,
    Break,
    Continue,
    Return,
    True,
    False,
    Nil,
    Throw,
    Do,

    Type,
    Const,

    Underscore,
    Import,

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

    GtGt,
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
            },

            TokenKind::LitChar(_) => "char",

            TokenKind::LitFloat(_) => "float number",

            TokenKind::Identifier(_) => "identifier",
            TokenKind::End => "<<EOF>>",

            TokenKind::LQuote => "<",
            TokenKind::RQuote => ">",

            // Keywords
            TokenKind::Class => "class",
            TokenKind::Implements => "implements",
            TokenKind::This => "self",
            TokenKind::Fun => "function",
            TokenKind::Let => "let",
            TokenKind::Var => "var",
            TokenKind::While => "while",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Loop => "loop",
            TokenKind::For => "for",
            TokenKind::In => "in",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Return => "return",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Nil => "nil",
            TokenKind::Throw => "throw",
            TokenKind::Match => "match",
            TokenKind::Do => "do",
            TokenKind::Type => "type",
            TokenKind::Const => "const",
            TokenKind::New => "new",
            TokenKind::Underscore => "_",

            TokenKind::Import => "import",
            TokenKind::Include => "include",
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
            TokenKind::LtLt => "<<",
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum IntSuffix {
    Int,
    Long,
    Byte,
}

#[derive(PartialEq, Eq, Debug, Clone)]
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
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
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
