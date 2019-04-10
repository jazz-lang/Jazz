#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub enum Keyword {
  Var,
  If,
  Else,
  Function,
  Try,
  Catch,
  Type,
  Match,
  Then,
  When,
  While,
  Exception,
  Rec,
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum Const {
  Int(i64),
  Float(f64),
  Char(char),
  Bool(bool),
  Str(String),
  Ident(String),
  Constr(String),
  Module(Vec<String>, Box<Const>),
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum TokenKind {
  End,
  Semicolon,
  Dot,
  Comma,
  Quote,

  Arrow,
  Vertical,
  StreamOpen,
  StreamClose,
  Const(Const),
  Var,
  If,
  Else,
  Function,
  Try,
  Catch,
  Type,
  Match,
  Then,
  When,
  While,
  Exception,
  Rec,
  Break,
  Continue,
  Open,
  Underscore,
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
  LQuote,
  RQuote,
  RBracket,
  LBrace,
  RBrace,
  Return,
  Colon,
  Sep, // ::

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
  GtGtGt,
  LtLt,
  Comment(String),
  CommentLine(String),
  Unknown(String),
  Whitespace(String),
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct Token {
  pub pos: Position,
  pub kind: TokenKind,
}
impl Token {
  pub fn new(tt: TokenKind, pos: Position) -> Token {
    Token { kind: tt, pos }
  }
  pub fn is_eof(&self) -> bool {
    self.kind == TokenKind::End
  }

  pub fn is(&self, kind: TokenKind) -> bool {
    self.kind == kind
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct Position {
  pub line: u32,
  pub column: u32,
}

use std::fmt;

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

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
  pub pos: Position,
  pub decl: ExprDecl,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprDecl {
  Assign(Box<Expr>, Box<Expr>),
  Const(Const),
  Block(Vec<Box<Expr>>),
  Paren(Box<Expr>),
  Field(Box<Expr>, String),
  Call(Box<Expr>, Vec<Box<Expr>>),
  Array(Box<Expr>, Box<Expr>),
  Tuple(Vec<Box<Expr>>),
  Vars(Vec<(String, Option<Box<Expr>>)>),
  For(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
  ForIn(String, Box<Expr>, Box<Expr>),
  While(Box<Expr>, Box<Expr>),
  If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
  Try(Box<Expr>, String, Box<Expr>),
  Function(bool, Option<String>, Vec<String>, Box<Expr>),
  Binop(String, Box<Expr>, Box<Expr>),
  Return(Option<Box<Expr>>),
  Break(Option<Box<Expr>>),
  Var(bool, String, Option<Box<Expr>>),
  Continue,
  Next(Box<Expr>, Box<Expr>),
  Object(Vec<(String, Box<Expr>)>),
  Label(String),
  /// Enum
  ///
  /// Example
  /// ```rust
  /// enum expr = {
  ///   val(.) // 1 value
  ///   binop(...) // 3 values
  ///   unop(..)
  ///
  /// }
  /// ```
  Enum(
    String,
    Vec<(
      String, // field name
      usize,  // value count
    )>,
  ),

  Record(String, Vec<String>),

  Switch(Box<Expr>, Vec<(Box<Expr>, Box<Expr>)>, Option<Box<Expr>>),
  Unop(String, Box<Expr>),
  Throw(String),
  Include(String),
  Yield(Box<Expr>),
  Jazz(String),
}

impl TokenKind {
  pub fn name(&self) -> &str {
    match *self {
      TokenKind::Return => "return",
      TokenKind::Const(Const::Str(_)) => "string",
      TokenKind::Const(Const::Int(_)) => "int number",

      TokenKind::Const(Const::Char(_)) => "char",

      TokenKind::Const(Const::Float(_)) => "float number",

      TokenKind::Const(Const::Ident(_)) => "identifier",

      TokenKind::End => "<<EOF>>",
      TokenKind::Quote => "`",
      TokenKind::Vertical => "|",
      TokenKind::StreamOpen => "|<",
      TokenKind::StreamClose => ">|",
      TokenKind::Then => "then",
      TokenKind::Try => "try",
      TokenKind::Catch => "catch",
      TokenKind::When => "when",
      TokenKind::Rec => "rec",
      TokenKind::Exception => "exception",
      TokenKind::Open => "open",
      TokenKind::Internal => "internal",
      TokenKind::Comment(_) => "comment",
      TokenKind::CommentLine(_) => "comment line",
      TokenKind::Whitespace(_) => "whitespace",
      TokenKind::Unknown(_) => "unknown",
      TokenKind::Const(_) => "constant",
      TokenKind::LQuote => "<",
      TokenKind::RQuote => ">",

      // Keywords
      TokenKind::Function => "function",

      TokenKind::Var => "var",
      TokenKind::While => "while",
      TokenKind::If => "if",
      TokenKind::Else => "else",

      TokenKind::Break => "break",
      TokenKind::Continue => "continue",

      TokenKind::Const(Const::Bool(true)) => "true",
      TokenKind::Const(Const::Bool(false)) => "false",

      TokenKind::Match => "match",

      TokenKind::Type => "type",

      TokenKind::Underscore => "_",

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
      TokenKind::Arrow => "->",
      TokenKind::Tilde => "~",
      TokenKind::BitOr => "|",
      TokenKind::BitAnd => "&",
      TokenKind::Caret => "^",
      TokenKind::And => "&&",
      TokenKind::Or => "||",

      TokenKind::Eq => "=",
      TokenKind::EqEq => "==",
      TokenKind::Ne => "!=",
      TokenKind::Lt => "<",
      TokenKind::Le => "<=",
      TokenKind::Gt => ">",
      TokenKind::Ge => ">=",
      TokenKind::GtGtGt => ">>>",
      TokenKind::GtGt => ">>",
      TokenKind::LtLt => "<<",
    }
  }
}
