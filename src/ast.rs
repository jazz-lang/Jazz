use crate::lexer::position::Position;
use crate::lexer::token::*;

pub type AST = Vec<Elem>;

#[derive(Clone, Debug)]
pub enum Elem {
    Func(Function),
    Struct(Struct),
    Const(Type, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Block(Vec<Box<Stmt>>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    Var(Name, Option<Type>, Option<Box<Expr>>),
    Let(Name, Option<Type>, Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub pos: Position,
}

impl Stmt {
    pub fn new(kind: StmtKind, pos: Position) -> Stmt {
        Stmt { kind, pos }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Or,
    And,
    BitAnd,
    BitOr,
    BitXor,
    Neq,
    Shl,
    Shr,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Cast(Type),
    Plus,
    Neg,
    Not,
}

pub type Abi = String;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Name,
    pub pos: Position,
    pub is_pub: bool,
    pub is_static: bool,
    pub external: bool,
    pub abi: Option<Abi>,
    pub params: Vec<Param>,
    pub block: Option<Box<Stmt>>,

    pub return_ty: Type,

    /// Const functions calculated on compile time and instead of inserting code for calling this functions inserted return value
    /// ```kotlin
    /// const func add(x: int,y: int): int {
    ///     return x + y;
    /// }
    ///
    /// var result: int = add(2,3);
    /// ````
    /// generated assembly:
    /// ```assembly
    /// mov eax,5
    /// mov DWORD PTR [rsp - var_offset],eax
    /// ```
    ///
    pub is_const: bool,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: Name,
    pub pos: Position,
    pub typ: Type,
    pub reassignable: bool,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub name: Name,
    pub pos: Position,
    pub typ: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Basic(String),
    Ptr(Box<Type>),
    Ref(Box<Type>),
    Complex(Box<Type>),
}

use std::fmt;

impl fmt::Display for Type {
    fn fmt(&self,f:  &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Basic(n) => write!(f,"{}",n),
            Type::Ptr(n) => write!(f,"*{}",n),
            Type::Ref(n) => write!(f,"&{}",n),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    IntLiteral(i64, IntBase, IntSuffix),
    FloatLiteral(f64, FloatSuffix),
    StrLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    NullLiteral,
    Call(Path, Option<Box<Expr>>, Vec<Box<Expr>>),
    Assign(Box<Expr>, Box<Expr>),
    Field(Box<Expr>, Name),
    Ident(String),
    Dot(Box<Expr>, Box<Expr>),
    StructInit(Path, Vec<StructArg>),
    Array(Box<Expr>, Box<Expr>),
    Conv(Box<Type>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct StructArg {
    pub pos: Position,
    pub name: Name,
    pub expr: Box<Expr>,
}

pub type Name = String;

#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    pub path: Vec<String>,
}

impl Path {
    pub fn new(name: Name) -> Path {
        Path { path: vec![name] }
    }

    pub fn name(&self) -> &Name {
        assert_eq!(1, self.path.len());

        &self.path[0]
    }

    pub fn len(&self) -> usize {
        self.path.len()
    }
}

use std::ops::Index;

impl Index<usize> for Path {
    type Output = Name;

    fn index(&self, idx: usize) -> &Name {
        &self.path[idx]
    }
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Position,
}

impl Expr {
    pub fn new(kind: ExprKind, pos: Position) -> Expr {
        Expr { kind, pos }
    }

    pub fn pos(&self) -> &Position {
        &self.pos
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
}
