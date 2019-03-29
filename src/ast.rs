use crate::token::Position;
#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    True,
    False,
    Null,
    This,
    Int(i64),
    Float(f64),
    Str(String),
    Builtin(String),
    Ident(String),
}

use crate::P;

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub pos: Position,
    pub decl: ExprDecl,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum WhileFlag {
    NormalWhile,
    DoWhile,
}
#[derive(Clone, Debug, PartialEq)]
pub enum ExprDecl {
    Assign(P<Expr>, P<Expr>),
    Const(Constant),
    Block(Vec<P<Expr>>),
    Paren(P<Expr>),
    Field(P<Expr>, String),
    Call(P<Expr>, Vec<P<Expr>>),
    Array(P<Expr>, P<Expr>),
    Vars(Vec<(String, Option<P<Expr>>)>),
    For(P<Expr>, P<Expr>, P<Expr>, P<Expr>),
    ForIn(String, P<Expr>, P<Expr>),
    While(P<Expr>, P<Expr>),
    If(P<Expr>, P<Expr>, Option<P<Expr>>),
    Try(P<Expr>, String, P<Expr>),
    Function(Vec<String>, P<Expr>),
    Binop(String, P<Expr>, P<Expr>),
    Return(Option<P<Expr>>),
    Break(Option<P<Expr>>),
    Var(bool, String, Option<P<Expr>>),
    Continue,
    Next(P<Expr>, P<Expr>),
    Object(Vec<(String, P<Expr>)>),
    Label(String),
    Switch(P<Expr>, Vec<(P<Expr>, P<Expr>)>, Option<P<Expr>>),
    Unop(String, P<Expr>),
    Throw(String),
    Jazz(String),
}

pub const NULL_POS: Position = Position { column: 0, line: 0 };

pub fn make_call(v: P<Expr>, args: Vec<P<Expr>>, pos: Position) -> Expr {
    Expr {
        pos: pos,
        decl: ExprDecl::Call(v, args),
    }
}
pub fn make_ident(i: String, pos: Position) -> Expr {
    Expr {
        pos: pos,
        decl: ExprDecl::Const(Constant::Ident(i)),
    }
}
pub fn make_builtin(b: String, pos: Position) -> Expr {
    Expr {
        pos: pos,
        decl: ExprDecl::Const(Constant::Builtin(b)),
    }
}
pub fn make_int(i: i64, pos: Position) -> Expr {
    Expr {
        pos: pos,
        decl: ExprDecl::Const(Constant::Int(i)),
    }
}
pub fn make_str(s: String, pos: Position) -> Expr {
    Expr {
        pos: pos,
        decl: ExprDecl::Const(Constant::Str(s)),
    }
}
pub fn make_bin(op: String, e1: P<Expr>, e2: P<Expr>, pos: Position) -> Expr {
    Expr {
        pos: pos,
        decl: ExprDecl::Binop(op, e1, e2),
    }
}

impl Expr {
    pub fn iter(&self, mut f: impl FnMut(&P<Expr>)) {
        match &self.decl {
            ExprDecl::Block(el) => {
                for e in el.iter() {
                    f(e);
                }
            }
            ExprDecl::Paren(e) => f(e),
            ExprDecl::Field(e, _) => f(e),
            ExprDecl::Call(e, el) => {
                f(e);
                for x in el.iter() {
                    f(x);
                }
            }
            ExprDecl::Array(e1, e2) => {
                f(e1);
                f(e2);
            }
            ExprDecl::Var(_, _, e) => match e {
                Some(e) => f(e),
                _ => (),
            },
            ExprDecl::While(e1, e2) => {
                f(e1);
                f(e2);
            }
            ExprDecl::If(e1, e2, e3) => {
                f(e1);
                f(e2);
                match e3 {
                    Some(e) => f(e),
                    _ => (),
                }
            }
            ExprDecl::Function(_, e) => f(e),
            ExprDecl::Binop(_, e1, e2) => {
                f(e1);
                f(e2)
            }
            ExprDecl::Return(Some(e)) => f(e),
            ExprDecl::Break(Some(e)) => f(e),
            ExprDecl::Next(e1, e2) => {
                f(e1);
                f(e2);
            }
            _ => (),
        }
    }
}
