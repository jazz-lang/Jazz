use crate::syntax::lexer::token::{FloatSuffix, IntBase, IntSuffix};
use std::collections::HashMap;
/// Constant value that known at compile-time
///
/// TODO: Complex values such as structs and strings
#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Const
{
    Imm(i64, IntSuffix, IntBase),
    Float(f64, FloatSuffix),
    Bool(bool),
    Void,
    None,
}

impl Const
{
    fn to_kind(&self) -> ExprKind
    {
        match self
        {
            Const::Imm(imm, suffix, base) => ExprKind::Int(*imm, base.clone(), suffix.clone()),
            Const::Float(f, suffix) => ExprKind::Float(*f, suffix.clone()),
            Const::Bool(b) => ExprKind::Bool(*b),
            _ => unreachable!(),
        }
    }
}

impl Const
{
    fn is_none(&self) -> bool
    {
        match self
        {
            Const::None => true,
            _ => false,
        }
    }
}

fn ty_size(ty: &Type) -> Option<usize>
{
    match ty
    {
        Type::Ptr(_) => Some(std::mem::size_of::<*const u8>()),
        Type::Basic(basic) =>
        {
            let name: &str = &str(basic.name).to_string();
            match name
            {
                "u8" => Some(1),
                "u16" => Some(2),
                "u32" => Some(4),
                "u64" => Some(8),
                "usize" => Some(std::mem::size_of::<usize>()),
                "isize" => Some(std::mem::size_of::<isize>()),
                "i8" => Some(1),
                "i16" => Some(2),
                "i32" => Some(4),
                "i64" => Some(8),
                "char" => Some(1),
                _ => None,
            }
        }
        Type::Struct(s) =>
        {
            let mut size = 0;
            for field in s.fields.iter()
            {
                if let Some(s) = ty_size(&field.data_type)
                {
                    size += s;
                }
                else
                {
                    return None;
                }
            }

            Some(size)
        }
        Type::Array(array) =>
        {
            if array.len.is_some()
            {
                if let Some(size) = ty_size(&array.subtype)
                {
                    return Some(size * array.len.unwrap());
                }
                else
                {
                    return None;
                }
            }
            else
            {
                return Some(std::mem::size_of::<*const u8>());
            }
        }
        Type::Func(_) => return Some(std::mem::size_of::<*const u8>()),
        Type::Void(_) => return Some(0),
    }
}

use crate::ast::*;
use crate::syntax::interner::{str, Name};
use crate::Context;

pub struct ConstEval<'a>
{
    /// Variables defined and known in compile-time context
    known_vars: HashMap<Name, Const>,
    ctx: &'a mut Context,
    /// All constant functions
    const_functions: HashMap<Name, Vec<Function>>,
    return_: Option<Const>,
    constexprs: HashMap<Name, Expr>,
}

impl<'a> ConstEval<'a>
{
    pub fn new(ctx: &'a mut Context) -> ConstEval<'a>
    {
        ConstEval {
            ctx: ctx,
            known_vars: HashMap::new(),
            const_functions: HashMap::new(),
            return_: None,
            constexprs: HashMap::new(),
        }
    }

    fn try_get_var(&mut self, name: &Name) -> Const
    {
        if self.constexprs.contains_key(name)
        {
            let cexpr = self.constexprs.get(name).unwrap().clone();
            let val = self.eval(&cexpr);
            return val;
        }
        let var = self.known_vars.get(name).unwrap_or(&Const::None);

        var.clone()
    }
    /// If values of lhs and rhs known at compile time evaluates binary operation
    fn eval_binop(&mut self, op: &str, lhs: &Expr, rhs: &Expr) -> Const
    {
        let c1 = self.eval(&lhs);
        let c2 = self.eval(&rhs);

        if c1.is_none() || c2.is_none()
        {
            return Const::None;
        }

        match op
        {
            "+" => match (c1, c2)
            {
                (Const::Imm(i1, suffix, base), Const::Imm(i2, _, _)) =>
                {
                    Const::Imm(i1 + i2, suffix, base)
                }
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 + f2, s),
                _ => Const::None,
            },
            "-" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 - i2, s, b),
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 - f2, s),
                _ => Const::None,
            },
            "/" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 / i2, s, b),
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 / f2, s),
                _ => Const::None,
            },
            "*" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 * i2, s, b),
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 * f2, s),
                _ => Const::None,
            },
            "%" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 % i2, s, b),
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 % f2, s),
                _ => Const::None,
            },
            "|" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 | i2, s, b),
                _ => Const::None,
            },
            "&" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 & i2, s, b),
                _ => Const::None,
            },
            ">>" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 >> i2, s, b),
                _ => Const::None,
            },
            "<<" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 << i2, s, b),
                _ => Const::None,
            },
            "==" => Const::Bool(c1 == c2),
            "!=" => Const::Bool(c1 != c2),
            ">" => Const::Bool(c1 > c2),
            "<" => Const::Bool(c1 < c2),
            ">=" => Const::Bool(c1 >= c2),
            "<=" => Const::Bool(c1 <= c2),
            "||" => match (c1, c2)
            {
                (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(b1 || b2),
                _ => Const::None,
            },
            "&&" => match (c1, c2)
            {
                (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(b1 && b2),
                _ => Const::None,
            },
            _ => Const::None,
        }
    }
    /// if `to` expression is identifier
    ///  and variable with name of identifier known
    ///  and `from` expression known at compile time perform assign
    fn try_assign(&mut self, to: &Expr, from: &Expr)
    {
        match &to.kind
        {
            ExprKind::Ident(name) =>
            {
                if self.known_vars.contains_key(name)
                {
                    let val = self.eval(from);
                    if !val.is_none()
                    {
                        self.known_vars.insert(*name, val);
                    }
                    else
                    {
                        self.known_vars.remove(name);
                    }
                }
            }
            _ => (),
        }
    }
    /// Evaluate expression
    fn eval(&mut self, expr: &Expr) -> Const
    {
        match &expr.kind
        {
            ExprKind::Conv(expr,to) => {
                let val = self.eval(expr);

                if !val.is_none() {
                    use crate::semantic::{ty_is_any_float,ty_is_any_int};
                    if ty_is_any_int(to) {
                        match val {
                            Const::Imm(i,s,b) => Const::Imm(i,s,b),
                            Const::Float(f,s) => Const::Imm(f as i64,match s {
                                FloatSuffix::Float => IntSuffix::Int,
                                FloatSuffix::Double => IntSuffix::Long
                            },
                            IntBase::Dec
                            ),
                            Const::Bool(b) => Const::Imm(b as i64,IntSuffix::Int,IntBase::Dec),
                            _ => Const::None
                        }
                    } else if ty_is_any_float(to) {
                        match val {
                            Const::Float(f,s) => Const::Float(f,s),
                            Const::Imm(
                                i,
                                s,_
                            ) => Const::Float(
                                i as f64,
                                match s {
                                    IntSuffix::Int => FloatSuffix::Float,
                                    IntSuffix::Long | IntSuffix::ULong => FloatSuffix::Double,
                                    _ => FloatSuffix::Float
                                }
                            ),
                            _ => Const::None
                        }
                    } else {
                        Const::None
                    }
                } else {
                    Const::None
                }
            }
            
            ExprKind::Int(i, b, s) => Const::Imm(*i, *s, *b),
            ExprKind::Float(f, s) => Const::Float(*f, *s),
            ExprKind::Bool(b) => Const::Bool(*b),

            ExprKind::Binary(op, lhs, rhs) => self.eval_binop(op, lhs, rhs),
            ExprKind::Unary(op,expr) => {
                let op: &str = op;
                let val = self.eval(expr);
                if val.is_none() {
                    return Const::None;
                }
                match op {
                    "+" => match val {
                        Const::Imm(i,s,b) => Const::Imm(i,s,b),
                        Const::Float(f,s) => Const::Float(f,s),
                        _ => Const::None
                    }
                    "-" => match val {
                        Const::Imm(i,s,b) => Const::Imm(-i,s,b),
                        Const::Float(f,s) => Const::Float(-f,s),
                        _ => Const::None
                    }
                    "!" => match val {
                        Const::Imm(i,s,b) => Const::Imm(!i,s,b),
                        
                        Const::Bool(b) => Const::Bool(!b),
                        _ => Const::None
                    }
                    _ => Const::None
                }
            }    
            
            ExprKind::Ident(name) => self.try_get_var(name),
            ExprKind::Assign(to, from) =>
            {
                self.try_assign(to, from);
                Const::None
            }
            ExprKind::Call(name, this, args) =>
            {
                if this.is_some()
                {
                    return Const::None; // we don't support constexpr methods yet
                }
                if self.const_functions.contains_key(&name.name())
                {
                    let funcs: Vec<Function> =
                        self.const_functions.get(&name.name()).unwrap().clone();
                    let mut func = None;

                    for fun in funcs.iter()
                    {
                        if args.len() < fun.params.len() || args.len() > fun.params.len()
                        {
                            continue;
                        }
                        let mut params_match = false;
                        if args.len() == 0 && fun.params.len() == 0
                        {
                            params_match = true;
                        }
                        else
                        {
                            for (i, arg) in args.iter().enumerate()
                            {
                                let ty = self.ctx.types.get(&arg.id).unwrap().clone();
                                params_match = ty == *fun.params[i].1;
                            }
                        }
                        if params_match
                        {
                            func = Some(fun.clone());
                            break;
                        }
                    }

                    if func.is_none()
                    {
                        panic!("Const function not found");
                    }
                    else
                    {
                        let func: Function = func.unwrap();
                        let mut params = vec![];
                        for (name, _) in func.params.iter()
                        {
                            params.push(*name);
                        }
                        return self.eval_constfn(&params, func.body.as_ref().unwrap(), args);
                    }
                }

                Const::None
            }
            ExprKind::SizeOf(ty) =>
            {
                if let Some(size) = ty_size(ty)
                {
                    return Const::Imm(size as i64, IntSuffix::Int, IntBase::Dec);
                }
                else
                {
                    return Const::None;
                }
            }

            _ => Const::None,
        }
    }
    /// Evaluate constant function
    fn eval_constfn(&mut self, params: &[Name], body: &Stmt, args: &Vec<Box<Expr>>) -> Const
    {
        let old_vars = self.known_vars.clone();
        self.known_vars.clear();
        self.return_ = None;

        for (i, param) in params.iter().enumerate()
        {
            let val = self.eval(&args[i]);
            if val.is_none()
            {
                return Const::None; // Argument value not known at compile time, return none
            }
            self.known_vars.insert(*param, val);
        }
        let val = self.eval_stmt(body);
        self.known_vars = old_vars;
        if val.is_some()
        {
            return val.unwrap();
        }
        else
        {
            return Const::None;
        }
    }
    /// Evaluate constant
    fn eval_stmt(&mut self, stmt: &Stmt) -> Option<Const>
    {
        match &stmt.kind
        {
            StmtKind::Block(stmts) =>
            {
                for stmt in stmts.iter()
                {
                    let val = self.eval_stmt(stmt);
                    if val.is_some()
                    {
                        return val;
                    }
                }
                return None;
            }
            StmtKind::Expr(expr) =>
            {
                self.eval(expr);
                None
            }
            StmtKind::Return(expr) =>
            {
                if expr.is_some()
                {
                    let val = self.eval(expr.as_ref().unwrap());
                    if val.is_none()
                    {
                        return None;
                    }
                    else
                    {
                        return Some(val);
                    }
                }
                else
                {
                    return Some(Const::Void);
                }
            }
            StmtKind::Var(name, _, _, expr) =>
            {
                if expr.is_none()
                {
                    return None;
                }
                else
                {
                    let val = self.eval(expr.as_ref().unwrap());
                    if val.is_none()
                    {
                        return None;
                    }

                    self.known_vars.insert(*name, val);
                }

                return None;
            }
            /*StmtKind::If(cond,then_body,else_body) =>
            {
                let val = self.eval(cond);
                if val.is_none() {
                    return None;
                }
                if let Const::Bool(true) = val {
                    self.eval_stmt(then_body);
                } else if let Const::Bool(false) = val {
                    if else_body.is_some() {
                        let else_body = else_body.as_ref().unwrap();
                        self.eval_stmt(else_body);
                    }
                }

                return Some(Const::Void);
            }*/
            _ => panic!("Unsupported statement in constant function"),
        }
    }

    /// Evaluates statement in non-const context
    fn eval_normal_stmt(&mut self, s: &Stmt, fid: usize)
    {
        match &s.kind
        {
            StmtKind::Block(block) =>
            {
                for stmt in block.iter()
                {
                    self.eval_normal_stmt(stmt, fid);
                }
            }
            StmtKind::Expr(expr) =>
            {
                let val = self.eval(expr);
                if !val.is_none()
                {
                    if let Elem::Func(func) = &mut self.ctx.file.elems[fid]
                    {
                        func.replace_expr_to(
                            expr.id,
                            Expr {
                                id: expr.id,
                                pos: expr.pos,
                                kind: val.to_kind(),
                            },
                        );
                    }
                }
            }
            StmtKind::If(cond, then, otherwise) =>
            {
                let val = self.eval(cond);
                if !val.is_none()
                {
                    if let Elem::Func(func) = &mut self.ctx.file.elems[fid]
                    {
                        func.replace_expr_to(
                            cond.id,
                            Expr {
                                id: cond.id,
                                pos: cond.pos,
                                kind: val.to_kind(),
                            },
                        );
                    }
                }
                self.eval_normal_stmt(then, fid);
                if otherwise.is_some()
                {
                    self.eval_normal_stmt(otherwise.as_ref().unwrap(), fid);
                }
            }
            StmtKind::While(cond, body) =>
            {
                let val = self.eval(cond);
                if !val.is_none()
                {
                    if let Elem::Func(func) = &mut self.ctx.file.elems[fid]
                    {
                        func.replace_expr_to(
                            cond.id,
                            Expr {
                                id: cond.id,
                                pos: cond.pos,
                                kind: val.to_kind(),
                            },
                        );
                    }
                }
                self.eval_normal_stmt(body, fid);
            }
            StmtKind::Return(expr) =>
            {
                if expr.is_some()
                {
                    let expr = expr.as_ref().unwrap();
                    let val = self.eval(expr);
                    if !val.is_none()
                    {
                        if let Elem::Func(func) = &mut self.ctx.file.elems[fid]
                        {
                            func.replace_expr_to(
                                expr.id,
                                Expr {
                                    id: expr.id,
                                    pos: expr.pos,
                                    kind: val.to_kind(),
                                },
                            );
                        }
                    }
                }
            }
            StmtKind::Var(var, _, _, val) =>
            {
                if val.is_some()
                {
                    let expr = val.as_ref().unwrap();
                    let val = self.eval(expr);
                    if !val.is_none()
                    {
                        if let Elem::Func(func) = &mut self.ctx.file.elems[fid]
                        {
                            func.replace_expr_to(
                                expr.id,
                                Expr {
                                    id: expr.id,
                                    pos: expr.pos,
                                    kind: val.to_kind(),
                                },
                            );
                        }
                        self.known_vars.insert(*var, val);
                    }
                }
            }
            StmtKind::Loop(stmt) => self.eval_normal_stmt(stmt, fid),
            StmtKind::Continue => (),
            StmtKind::Break => (),
        }
    }

    fn opt_func(&mut self, func: &Function, id: usize)
    {
        self.eval_normal_stmt(func.body.as_ref().unwrap(), id);
    }

    pub fn run(&mut self)
    {
        for (i, elem) in self.ctx.file.elems.clone().iter().enumerate()
        {
            match elem
            {
                Elem::Func(func) =>
                {
                    if func.external || func.internal
                    {
                        continue;
                    }
                    if func.constant
                    {
                        if self.const_functions.contains_key(&func.name)
                        {
                            let funcs = self.const_functions.get_mut(&func.name).unwrap();
                            funcs.push(func.clone());
                        }
                        else
                        {
                            self.const_functions.insert(func.name, vec![func.clone()]);
                        }
                    }
                    self.opt_func(func, i);
                }
                Elem::ConstExpr { name, expr, .. } =>
                {
                    self.constexprs.insert(*name, *expr.clone());
                }
                _ => (),
            }
        }
    }
}