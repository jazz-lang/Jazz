use super::*;
use crate::syntax::lexer::token::IntBase;
use fmt::Display;
use std::fmt;

impl Display for Expr
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.kind) }
}

impl Display for ExprKind
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            ExprKind::Int(i, base, _) => match base
            {
                IntBase::Hex => write!(f, "{:x}", i),
                IntBase::Bin => write!(f, "{:b}", i),
                IntBase::Dec => write!(f, "{}", i),
            },
            ExprKind::Float(float, _) => write!(f, "{}", float),
            ExprKind::Field(expr, field) => write!(f, "{}.{}", expr, field),
            ExprKind::Conv(expr, to) => write!(f, "{} as {}", expr, to),
            ExprKind::Deref(expr) => write!(f, "*{}", expr),
            ExprKind::AddressOf(expr) => write!(f, "&{}", expr),
            ExprKind::Assign(to, from) => write!(f, "{} = {}", to, from),
            ExprKind::Bool(b) => write!(f, "{}", b),
            ExprKind::Null => write!(f, "null"),
            ExprKind::Ident(name) => write!(f, "{}", name),
            ExprKind::Str(s) => write!(f, "{:?}", s),
            ExprKind::Struct(path, fields) =>
            {
                write!(f, "{} {{\n", path.name())?;
                for field in fields.iter()
                {
                    write!(f, "\t{}: {}\n", field.name, field.expr)?;
                }
                write!(f, "\n \t}}")
            }
            ExprKind::Binary(op, lhs, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            ExprKind::Unary(op, val) => write!(f, "{}{}", op, val),
            ExprKind::SizeOf(ty) => write!(f, "sizeof({})", ty),
            ExprKind::GetFunc(name) => write!(f, "func &{}", name),
            ExprKind::Char(c) => write!(f, "{:?}", c),
            ExprKind::ArrayIdx(array, idx) => write!(f, "{}[{}]", array, idx),
            ExprKind::Array(_, _) => write!(f, "unimplemented"),
            ExprKind::Call(path, this, args) =>
            {
                if this.is_some()
                {
                    write!(f, "{}.", this.as_ref().unwrap())?;
                }
                write!(f, "{}(", path.name())?;
                for (i, arg) in args.iter().enumerate()
                {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1
                    {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for Stmt
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.kind) }
}

impl Display for StmtKind
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            StmtKind::Continue => write!(f, "continue"),
            StmtKind::Break => write!(f, "break"),
            StmtKind::Block(block) =>
            {
                write!(f, "{{\n")?;
                for stmt in block.iter()
                {
                    write!(f, "\t{}", stmt)?
                }
                write!(f, "\n}}\n")
            }
            StmtKind::Expr(expr) => write!(f, "{}\n", expr),
            StmtKind::If(cond, then, or) =>
            {
                write!(f, "if {} {{\n", cond)?;
                write!(f, "\t{}", then)?;
                write!(f, "}}")?;
                if or.is_some()
                {
                    write!(f, " else ")?;
                    write!(f, " {} \n", or.as_ref().unwrap())?;
                    write!(f, "}}\n")?;
                }
                write!(f, "\n")
            }
            StmtKind::While(cond, body) => write!(f, "while {} \n {{\n {} \n}}", cond, body),
            StmtKind::Loop(body) => write!(f, "{{\n{}\n}}", body),
            StmtKind::Return(ret) =>
            {
                if ret.is_some()
                {
                    write!(f, "return {}", ret.as_ref().unwrap())
                }
                else
                {
                    write!(f, "return")
                }
            }

            StmtKind::Var(name, _, ty, expr) =>
            {
                write!(f, "var {}", name)?;
                if ty.is_some()
                {
                    write!(f, ": {}", ty.as_ref().unwrap())?;
                }
                if expr.is_some()
                {
                    write!(f, " = {}", expr.as_ref().unwrap())?;
                }
                write!(f, "\n")
            }
        }
    }
}

impl Display for Function
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        if self.public
        {
            write!(f, "pub ")?;
        }
        if self.internal
        {
            write!(f, "internal ")?;
        }
        if self.external
        {
            write!(f, "external ")?;
        }
        if self.constant
        {
            write!(f, "constexpr ")?;
        }
        if self.static_
        {
            write!(f, "static ")?;
        }
        write!(f, "func {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate()
        {
            write!(f, "{}: {}", param.0, param.1)?;
            if i != self.params.len() - 1
            {
                write!(f, ",")?;
            }
        }
        write!(f, ") {} ", self.ret)?;
        if self.body.is_some()
        {
            write!(f, "{}", self.body.as_ref().unwrap())?
        }
        write!(f, "\n")
    }
}

impl Display for Struct
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "struct {} {{\n", self.name)?;
        for field in self.fields.iter()
        {
            write!(f, "\t{}: {}\n", field.name, field.data_type)?;
        }
        write!(f, "}}\n")
    }
}

impl Display for Global
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        if self.external
        {
            write!(f, "external ")?;
        }
        write!(f, "var {}: {}", self.name, self.typ)?;
        if self.expr.is_some()
        {
            write!(f, " = {}", self.expr.as_ref().unwrap())?;
        }
        write!(f, "\n")
    }
}

impl Display for Elem
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Elem::Func(fun) => write!(f, "{}", fun),
            Elem::Struct(s) => write!(f, "{}", s),
            Elem::Import(s) => write!(f, "import {}", s),
            Elem::Alias(name, ty) => write!(f, "alias {} = {}", name, ty),
            Elem::ConstExpr { name, expr, .. } => write!(f, "constexpr {} = {}", name, expr),
            Elem::Global(g) => write!(f, "{}", g),
            Elem::Link(l) => write!(f, "link \"{}\" ", l),
            _ => write!(f, ""),
        }
    }
}
