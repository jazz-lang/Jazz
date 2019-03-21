use crate::token::Position;

#[derive(Clone, PartialEq)]
pub struct Expr {
    pub pos: Position,
    pub expr: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Assign(Box<Expr>, Box<Expr>),
    BinOp(Box<Expr>, String, Box<Expr>),
    Unop(String, Box<Expr>),
    Access(Box<Expr>, String),
    Ident(String),
    Function(String, Vec<String>, Box<Expr>),
    Class(String, Box<Expr>, Option<Box<Expr>>),
    Lambda(Vec<String>, Box<Expr>),
    Match(Box<Expr>, Vec<(Box<Expr>, Box<Expr>)>, Option<Box<Expr>>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    ConstInt(i64),
    ConstChar(char),
    ConstStr(String),
    New(Box<Expr>),
    ConstFloat(f64),
    Object(Vec<(Box<Expr>, Box<Expr>)>),
    Var(bool, String, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    Block(Vec<Box<Expr>>),
    Return(Option<Box<Expr>>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Nil,
    Break,
    Continue,
    Throw(String),
    ConstBool(bool),
    Array(Vec<Box<Expr>>),
    ArrayIndex(Box<Expr>, Box<Expr>),
    This,
    Import(String),
    Include(String),
}

use std::fmt;

impl Expr {
    pub fn is_access(&self) -> bool {
        if let ExprKind::Access(_, _) = self.expr {
            return true;
        };
        false
    }

    pub fn is_binop(&self) -> bool {
        if let ExprKind::BinOp(_, _, _) = self.expr {
            return true;
        };
        false
    }

    pub fn is_binop_cmp(&self) -> bool {
        if let ExprKind::BinOp(_, ref op, _) = self.expr {
            let op: &str = op;
            match op {
                ">" | "<" | ">=" | "<=" | "==" | "!=" => return true,
                _ => return false,
            }
        }
        false
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.expr)
    }
}
