use crate::{
    intern,
    syntax::{
        lexer::token::{FloatSuffix, IntBase, IntSuffix},
        position::Position,
    },
};

use crate::{
    ast::*,
    syntax::interner::{str, Name},
    Context,
};
use std::intrinsics::transmute;
use wrc::WRC as Rc;
pub fn rc<T>(v: T) -> Rc<RefCell<T>> { Rc::new(RefCell::new(v)) }

use std::{cell::RefCell, collections::HashMap};

#[derive(Clone, Debug)]
pub enum Const
{
    /// Immediate value or just int value
    Imm(i64, IntSuffix, IntBase),
    /// Float value
    Float(f64, FloatSuffix),
    /// Boolean value
    Bool(bool),
    /// Struct value with fields
    Struct(Name, Vec<(Name, Rc<RefCell<Const>>, NodeId)>),
    /// Just a void value
    Void,
    Str(String),
    Array(
        Rc<RefCell<Vec<Rc<RefCell<Const>>>>>,
        Vec<(NodeId, Position)>,
        Box<Type>,
    ),
    Ret(Rc<RefCell<Const>>),
    /// If evaluator seen this value then evaluation stops
    None,
}

use std::cmp::PartialEq;
impl PartialEq for Const
{
    fn eq(&self, other: &Self) -> bool
    {
        match (self, other)
        {
            (Const::Imm(i, _, _), Const::Imm(i2, _, _)) => i == i2,
            (Const::Imm(i, _, _), Const::Float(f, _)) => *i as f64 == *f,
            (Const::Float(f, _), Const::Imm(i, _, _)) => *f == *i as f64,
            (Const::Float(f, _), Const::Float(f2, _)) => f == f2,
            (Const::Bool(b), Const::Bool(b2)) => b == b2,
            (Const::Struct(s1name, fields1), Const::Struct(s2name, fields2)) =>
            {
                if fields1.len() == 0 && fields2.len() == 0
                {
                    return s1name == s2name;
                }
                else
                {
                    let mut fields_ok = false;
                    for (f1, f2) in fields1.iter().zip(fields2.iter())
                    {
                        fields_ok = f1 == f2;
                    }
                    fields_ok
                }
            }
            _ => false,
        }
    }
}

impl Const
{
    fn is_void(&self) -> bool
    {
        match self
        {
            Const::Void => true,
            _ => false,
        }
    }

    fn is_none(&self) -> bool
    {
        match self
        {
            Const::None => true,
            _ => false,
        }
    }

    /// Translate Const value into Expression
    fn to_kind(&self) -> ExprKind
    {
        match self
        {
            Const::Imm(imm, suffix, base) => ExprKind::Int(*imm, base.clone(), suffix.clone()),
            Const::Float(f, suffix) => ExprKind::Float(*f, suffix.clone()),
            Const::Bool(b) => ExprKind::Bool(*b),
            Const::Struct(name, fields) =>
            {
                let mut args = vec![];
                for (name, constant, id) in fields.iter()
                {
                    args.push(StructArg {
                        id: *id,
                        pos: Position::new(intern(""), 0, 0),
                        name: *name,
                        expr: box Expr {
                            id: NodeId(0),
                            pos: Position::new(intern(""), 0, 0),
                            kind: constant.borrow().to_kind(),
                        },
                    })
                }
                ExprKind::Struct(Path::new(*name), args)
            }
            Const::Array(values, pos_and_id, ty) =>
            {
                let mut exprs = vec![];
                for (i, val) in values.borrow().iter().enumerate()
                {
                    exprs.push(box Expr {
                        id: pos_and_id[i].0,
                        pos: pos_and_id[i].1,
                        kind: val.borrow().to_kind(),
                    });
                }

                ExprKind::Array(ty.clone(), exprs)
            }
            Const::Ret(val) => val.borrow().to_kind(),
            Const::Str(s) => ExprKind::Str(s.to_owned()),
            v => panic!("{:?}", v),
        }
    }
}

pub struct EvalCtx<'a>
{
    pub ctx: &'a mut Context,
    pub const_fns: HashMap<Name, Vec<Function>>,
    pub functions: HashMap<Name, Vec<Function>>,
    pub fns: HashMap<Name, Vec<Function>>,
    pub known_vars: HashMap<Name, Rc<RefCell<Const>>>,
    pub constexprs: HashMap<Name, Expr>,
    pub result: Option<Rc<RefCell<Const>>>,
    cur_func: Option<NodeId>,
}

impl<'a> EvalCtx<'a>
{
    pub fn new(ctx: &'a mut Context) -> EvalCtx<'a>
    {
        EvalCtx {
            ctx,
            const_fns: HashMap::new(),
            functions: HashMap::new(),
            fns: HashMap::new(),
            known_vars: HashMap::new(),
            constexprs: HashMap::new(),
            result: None,
            cur_func: None,
        }
    }

    fn search_for_func_const(
        params: &[Type],
        this: Option<&Type>,
        functions: &[Function],
    ) -> Option<(Function, Vec<Type>)>
    {
        let val = None;

        for function in functions.iter()
        {
            let function: &Function = function;

            let mut params_okay = false;
            let mut not_found = false;
            if function.params.len() > params.len()
            {
                continue;
            }

            if function.params.len() == 0 && params.len() == 0 && this.is_none()
            {
                return Some((function.clone(), vec![]));
            }

            for (index, param) in params.iter().enumerate()
            {
                if index < function.params.len()
                {
                    params_okay = param == &*function.params[index].1;
                }
                else
                {
                    if function.variadic && params_okay
                    {
                        not_found = false;
                        break;
                    }
                    else
                    {
                        params_okay = false;
                        not_found = true;
                        break;
                    }
                }

                if !params_okay
                {
                    not_found = true;
                    break;
                }
            }

            if not_found
            {
                continue;
            }

            if params_okay
            {
                return Some((
                    function.clone(),
                    function
                        .params
                        .iter()
                        .map(|(_, typ)| *typ.clone())
                        .collect(),
                ));
            }
            else
            {
                continue;
            }
        }
        val
    }

    /// If values of lhs and rhs known at compile time evaluates binary
    /// operation
    fn eval_binop(&mut self, op: &str, lhs: &Expr, rhs: &Expr, const_: bool) -> Rc<RefCell<Const>>
    {
        let c1 = self.expr(&lhs, const_);
        let c2 = self.expr(&rhs, const_);

        if c1.borrow().is_none() || c2.borrow().is_none()
        {
            return Rc::new(RefCell::new(Const::None));
        }
        let c1: &Const = &c1.borrow();
        let c2: &Const = &c2.borrow();

        let val = match op
        {
            "+" => match (c1.clone(), c2.clone())
            {
                (Const::Imm(i1, suffix, base), Const::Imm(i2, _, _)) =>
                {
                    Const::Imm(i1.overflowing_add(i2).0, suffix, base)
                }
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 + f2, s),
                _ => Const::None,
            },
            "-" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) =>
                {
                    Const::Imm(i1.overflowing_sub(*i2).0, *s, *b)
                }
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 - f2, *s),
                _ => Const::None,
            },
            "/" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) =>
                {
                    Const::Imm(i1.overflowing_div(*i2).0, *s, *b)
                }
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 / f2, *s),
                _ => Const::None,
            },
            "*" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) =>
                {
                    Const::Imm(i1.overflowing_mul(*i2).0, *s, *b)
                }
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 * f2, *s),
                _ => Const::None,
            },
            "%" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 % i2, *s, *b),
                (Const::Float(f1, s), Const::Float(f2, _)) => Const::Float(f1 % f2, *s),
                _ => Const::None,
            },
            "|" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 | i2, *s, *b),
                _ => Const::None,
            },
            "&" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 & i2, *s, *b),
                _ => Const::None,
            },
            ">>" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 >> i2, *s, *b),
                _ => Const::None,
            },
            "<<" => match (c1, c2)
            {
                (Const::Imm(i1, s, b), Const::Imm(i2, _, _)) => Const::Imm(i1 << i2, *s, *b),
                _ => Const::None,
            },
            /*"==" => Const::Bool(c1 == c2),
            "!=" => Const::Bool(c1 != c2),
            ">" => Const::Bool(c1 > c2),
            "<" => Const::Bool(c1 < c2),
            ">=" => Const::Bool(c1 >= c2),
            "<=" => Const::Bool(c1 <= c2),*/
            "||" => match (c1, c2)
            {
                (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(*b1 || *b2),
                _ => Const::None,
            },
            "&&" => match (c1, c2)
            {
                (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(*b1 && *b2),
                _ => Const::None,
            },
            "<" => match (c1, c2)
            {
                (Const::Imm(i, _s, _b), Const::Imm(i2, _, _)) => Const::Bool(i < i2),
                (Const::Float(f, _), Const::Float(f2, _)) => Const::Bool(f < f2),
                _ => Const::None,
            },
            ">" => match (c1, c2)
            {
                (Const::Imm(i, _s, _b), Const::Imm(i2, _, _)) => Const::Bool(i > i2),
                (Const::Float(f, _), Const::Float(f2, _)) => Const::Bool(f > f2),
                _ => Const::None,
            },
            "==" => match (c1, c2)
            {
                (Const::Imm(i, _s, _b), Const::Imm(i2, _, _)) => Const::Bool(i == i2),
                (Const::Float(f, _), Const::Float(f2, _)) => Const::Bool(f == f2),
                (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(b1 == b2),
                (Const::Str(s), Const::Str(s2)) => Const::Bool(s == s2),
                _ => Const::None,
            },
            "!=" => match (c1, c2)
            {
                (Const::Imm(i, _s, _b), Const::Imm(i2, _, _)) => Const::Bool(i != i2),
                (Const::Float(f, _), Const::Float(f2, _)) => Const::Bool(f != f2),
                (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(b1 != b2),
                _ => Const::None,
            },
            ">=" => match (c1, c2)
            {
                (Const::Imm(i, _s, _b), Const::Imm(i2, _, _)) => Const::Bool(i >= i2),
                (Const::Float(f, _), Const::Float(f2, _)) => Const::Bool(f >= f2),
                _ => Const::None,
            },
            "<=" => match (c1, c2)
            {
                (Const::Imm(i, _s, _b), Const::Imm(i2, _, _)) => Const::Bool(i <= i2),
                (Const::Float(f, _), Const::Float(f2, _)) => Const::Bool(f <= f2),
                _ => Const::None,
            },
            _ => Const::None,
        };

        Rc::new(RefCell::new(val))
    }

    fn try_get_var(&mut self, name: &Name, const_: bool) -> Rc<RefCell<Const>>
    {
        if self.constexprs.contains_key(name)
        {
            let cexpr = self.constexprs.get(name).unwrap().clone();
            let val = self.expr(&cexpr, const_);
            return val;
        }
        let var = self.known_vars.get(name);
        if var.is_none()
        {
            return Rc::new(RefCell::new(Const::None));
        }
        var.unwrap().clone()
    }

    fn try_assign(&mut self, to: &Expr, from: &Expr, const_: bool)
    {
        match &to.kind
        {
            ExprKind::Ident(name) =>
            {
                if self.known_vars.contains_key(name)
                {
                    let val = self.expr(from, const_);

                    if !val.borrow().is_none()
                    {
                        self.known_vars.insert(*name, val);
                    }
                    else
                    {
                        self.known_vars.remove(name);
                    }
                }
            }
            ExprKind::Field(expr, field) =>
            {
                if let ExprKind::Ident(name) = &expr.kind
                {
                    if self.known_vars.contains_key(name)
                    {
                        let val = self.expr(from, const_);
                        if val.borrow().is_none()
                        {
                            return;
                        }
                        let cval = self.known_vars.get(name).unwrap();
                        let cval: &mut Const = &mut cval.borrow_mut();
                        if let Const::Struct(_, fields) = cval
                        {
                            for (name, val_, id) in fields.iter_mut()
                            {
                                if name == field
                                {
                                    *id = from.id;
                                    *val_ = val.clone();
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            _ => (),
        }
    }

    fn expr(&mut self, expr: &Expr, const_: bool) -> Rc<RefCell<Const>>
    {
        if const_
        {
            match &expr.kind
            {
                ExprKind::Int(i, base, suffix) => rc(Const::Imm(*i, *suffix, *base)),
                ExprKind::Float(f, suffix) => rc(Const::Float(*f, *suffix)),
                ExprKind::Binary(op, lhs, rhs) =>
                {
                    let val = self.eval_binop(op, lhs, rhs, const_);
                    if val.borrow().is_none()
                    {
                        return val;
                    }

                    self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);

                    val
                }
                ExprKind::Bool(b) => rc(Const::Bool(*b)),
                ExprKind::CompTime(expr_) =>
                {
                    let val = self.expr(expr_, true);
                    if val.borrow().is_none()
                    {
                        return val;
                    }
                    self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);

                    return val;
                }
                ExprKind::Array(ty, exprs) =>
                {
                    let mut pos_and_id = vec![];
                    let mut values = vec![];

                    for exp in exprs.iter()
                    {
                        let val = self.expr(exp, const_);

                        if val.borrow().is_none()
                        {
                            return val;
                        }
                        //self.maybe_replace_expr(val,exp.id,exp.pos);
                        pos_and_id.push((exp.id, exp.pos));
                        values.push(val);
                    }

                    let val = rc(Const::Array(rc(values), pos_and_id, ty.clone()));

                    return val;
                }
                ExprKind::ArrayIdx(array_e, idx_e) =>
                {
                    let array = self.expr(array_e, const_);
                    let idx = self.expr(idx_e, const_);

                    if array.borrow().is_none()
                    {
                        return array;
                    }
                    if idx.borrow().is_none()
                    {
                        return array;
                    }
                    //self.maybe_replace_expr(&idx.borrow(),idx_e.id,idx_e.pos);
                    //self.maybe_replace_expr(&array.borrow(),array_e.id,array_e.pos);
                    let idx_: &Const = &idx.borrow();
                    let idx = if let Const::Imm(i, _, _) = idx_
                    {
                        *i as usize
                    }
                    else
                    {
                        panic!("Integer expected");
                    };

                    let array: &Const = &array.borrow();
                    if let Const::Array(array, _, _) = array
                    {
                        return array.borrow()[idx].clone();
                    }
                    else
                    {
                        panic!("Array expected");
                    };
                }
                ExprKind::Assign(to, from_) =>
                {
                    let from = self.expr(from_, const_);
                    if from.borrow().is_none()
                    {
                        return from;
                    }

                    self.try_assign(to, from_, const_);

                    return from;
                }
                ExprKind::Field(val, field) =>
                {
                    let val = self.expr(val, const_);
                    let val: &Const = &val.borrow();
                    if val.is_none()
                    {
                        return rc(Const::None);
                    }
                    if let Const::Struct(_, fields) = val
                    {
                        for (name, cval, _) in fields.iter()
                        {
                            if name == field
                            {
                                return cval.clone();
                            }
                        }
                    }

                    return rc(Const::None);
                }
                ExprKind::Ident(name) => self.try_get_var(name, const_),
                ExprKind::Call(name, this, args) =>
                {
                    if this.is_some()
                    {
                        return rc(Const::None); // we don't support constexpr methods yet
                    }

                    if self.const_fns.contains_key(&name.name())
                    {
                        let funcs: Vec<Function> =
                            self.const_fns.get(&name.name()).unwrap().clone();
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
                            for (i, (name, _)) in func.params.iter().enumerate()
                            {
                                params.push((*name, *args[i].clone()));
                            }
                            let val = self.eval(&func, &params, const_);
                            if val.borrow().is_none()
                            {
                                return val;
                            }
                            self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);

                            return val;
                        }
                    }
                    else if self.functions.contains_key(&name.name()) && const_
                    {
                        let funcs: Vec<Function> =
                            self.functions.get(&name.name()).unwrap().clone();
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
                            panic!("function not found");
                        }
                        else
                        {
                            let func: Function = func.unwrap();
                            let mut params = vec![];
                            for (i, (name, _)) in func.params.iter().enumerate()
                            {
                                params.push((*name, *args[i].clone()));
                            }
                            let val = self.eval(&func, &params, const_);
                            self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);
                            return val;
                        }
                    }
                    else
                    {
                    }

                    return rc(Const::None);
                }
                _ => return rc(Const::None),
            }
        }
        else
        {
            if let ExprKind::CompTime(expr_) = &expr.kind
            {
                let val = self.expr(expr_, true);
                if val.borrow().is_none()
                {
                    return val;
                }
                self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);
                return val;
            }
            rc(Const::None)
        }
    }

    fn maybe_replace_expr(&mut self, to: &Const, id: NodeId, pos: Position)
    {
        if let Some(idx) = self.cur_func
        {
            if let Some(f) = self.ctx.get_func_mut(idx)
            {
                f.replace_expr_to(
                    id,
                    Expr {
                        id,
                        pos,
                        kind: to.to_kind(),
                    },
                )
            }
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt, const_: bool) -> Option<Rc<RefCell<Const>>>
    {
        if const_
        {
            match &stmt.kind
            {
                StmtKind::CompTime(s) => self.eval_stmt(s, true),
                StmtKind::Block(stmts) =>
                {
                    let mut last = None;
                    for stmt in stmts.iter()
                    {
                        let val = self.eval_stmt(stmt, const_);
                        last = val;

                        if last.is_some()
                        {
                            let last: &Const = &last.as_ref().unwrap().borrow();
                            if let Const::Ret(_) = last
                            {
                                break;
                            }
                        }
                    }
                    return last;
                }
                StmtKind::Expr(expr) =>
                {
                    let val = self.expr(expr, const_);
                    if val.borrow().is_none()
                    {
                        return None;
                    }
                    self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);
                    return Some(val);
                }
                StmtKind::Return(expr) =>
                {
                    if expr.is_some()
                    {
                        let expr = expr.as_ref().unwrap();
                        let val = self.expr(expr, const_);
                        if val.borrow().is_none()
                        {
                            return None;
                        }
                        self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);
                        if val.borrow().is_none()
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
                        return Some(rc(Const::Ret(Rc::new(RefCell::new(Const::Void)))));
                    }
                }
                StmtKind::Var(name, _, ty, expr) =>
                {
                    if expr.is_none()
                    {
                        return None;
                    }
                    else
                    {
                        let expr = expr.as_ref().unwrap();
                        let val = self.expr(expr, const_);
                        self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);
                        if val.borrow().is_none()
                        {
                            return None;
                        }

                        self.known_vars.insert(*name, val);
                    }

                    return Some(rc(Const::Void));
                }
                StmtKind::If(cond, then_body, else_body) =>
                {
                    let val = self.expr(cond, const_);

                    let val: &Const = &val.borrow();
                    self.maybe_replace_expr(val, cond.id, cond.pos);
                    if val.is_none()
                    {
                        return None;
                    }

                    if let Const::Bool(true) = val
                    {
                        return self.eval_stmt(then_body, const_);
                    }
                    else if let Const::Bool(false) = val
                    {
                        if else_body.is_some()
                        {
                            let else_body = else_body.as_ref().unwrap();
                            return self.eval_stmt(else_body, const_);
                        }
                        else
                        {
                            return Some(Rc::new(RefCell::new(Const::Void)));
                        }
                    }
                    return Some(Rc::new(RefCell::new(Const::Void)));
                }

                StmtKind::While(cond_, body) =>
                {
                    let cond = self.expr(cond_, const_);

                    let mut cond: Const = cond.borrow().clone();
                    self.maybe_replace_expr(&cond, cond_.id, cond_.pos);
                    while let Const::Bool(true) = cond
                    {
                        let val = &self.eval_stmt(body, const_);

                        if val.is_none()
                        {
                            return None;
                        }
                        else if val.as_ref().unwrap().borrow().is_none()
                        {
                            return None;
                        }
                        let tmp = self.expr(cond_, const_);
                        if tmp.borrow().is_none()
                        {
                            return None;
                        }
                        else
                        {
                            cond = tmp.borrow().clone();
                            self.maybe_replace_expr(&cond, cond_.id, cond_.pos);
                        }
                    }
                    return Some(Rc::new(RefCell::new(Const::Void)));
                }

                _ => return None,
            }
        }
        else
        {
            match &stmt.kind {
                StmtKind::CompTime(s) => return self.eval_stmt(s,true),
                StmtKind::Block(b) => {
                    for s in b.iter() {self.eval_stmt(s,false);}
                    return Some(rc(Const::Void))
                }
                StmtKind::Expr(e) => {
                    if let ExprKind::CompTime(s) = &e.kind { return Some(self.expr(s,true))}
                    else {
                        return None;
                    }
                }
                _ => None
            }

        }
    }

    fn eval(&mut self, f: &Function, params: &Vec<(Name, Expr)>, const_: bool)
        -> Rc<RefCell<Const>>
    {
        let old_vars = self.known_vars.clone();
        //self.known_vars.clear();
        self.cur_func = Some(f.id);

        let mut new_vars = HashMap::new();
        for (name, expr) in params.iter()
        {
            let val = self.expr(expr, const_);

            if val.borrow().is_none()
            {
                return rc(Const::None); // Argument value not known at compile time, return none
            }

            self.maybe_replace_expr(&val.borrow(), expr.id, expr.pos);
            new_vars.insert(*name, val);
        }

        self.known_vars = new_vars;

        let val = self.eval_stmt(f.body.as_ref().unwrap(), const_);

        self.known_vars = old_vars;
        if val.is_some()
        {
            let val: &Const = &val.as_ref().unwrap().borrow();
            if let Const::Ret(val) = val
            {
                return val.clone();
            }
            else
            {
                return rc(val.clone());
            }
        }
        else
        {
            return rc(Const::None);
        }
    }

    pub fn run(&mut self)
    {
        for elem in self.ctx.file.elems.iter()
        {
            match elem
            {
                Elem::Func(f) =>
                {
                    if f.constant
                    {
                        if let Some(funs) = self.const_fns.get_mut(&f.name)
                        {
                            funs.push(f.clone());
                        }
                        else
                        {
                            let funs = vec![f.clone()];
                            self.const_fns.insert(f.name, funs);
                        }
                    }
                    else if !f.external && !f.internal
                    {
                        if let Some(funs) = self.functions.get_mut(&f.name)
                        {
                            funs.push(f.clone());
                        }
                        else
                        {
                            let funs = vec![f.clone()];
                            self.functions.insert(f.name, funs);
                        }
                    }
                }
                _ => (),
            }
        }

        if let Some(fun) = self.functions.get(&intern("main"))
        {
            let main_fun = fun[0].clone();

            self.eval(&main_fun, &vec![], false);
        }
    }
}
