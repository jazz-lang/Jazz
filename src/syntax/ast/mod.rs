use std::{fmt, ops::Index};

pub mod display;

use super::lexer::token::{FloatSuffix, IntBase, IntSuffix};

pub struct File
{
    pub root: String,
    pub src: String,
    pub path: String,
    pub elems: Vec<Elem>,
}

impl File
{
    pub fn functions(&self) -> Vec<&Function>
    {
        let mut funs = vec![];
        for elem in self.elems.iter()
        {
            if let Elem::Func(f) = elem
            {
                funs.push(f);
            }
        }

        funs
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash, PartialOrd)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "#{}", self.0) }
}
#[derive(Clone, Debug)]
pub enum Elem
{
    Func(Function),
    Struct(Struct),
    Const(Const),
    Enum, // todo
    Global(Global),
    Link(Name),
    Import(String),
    ConstExpr
    {
        id: NodeId,
        pos: Position,
        name: Name,
        expr: Box<Expr>,
    },
    Alias(Name, Type),
}

impl PartialEq for Elem
{
    fn eq(&self, other: &Self) -> bool
    {
        match (self, other)
        {
            (Elem::Func(f), Elem::Func(f2)) => f == f2,
            (Elem::Struct(s), Elem::Struct(s2)) => s.name == s2.name,
            (Elem::ConstExpr { name, .. }, Elem::ConstExpr { name: name2, .. }) => name == name2,
            (Elem::Alias(name, _), Elem::Alias(name2, _)) => name == name2,
            (Elem::Const(c), Elem::Const(c2)) => c.name == c2.name,
            (Elem::Global(g), Elem::Global(g2)) => g.name == g2.name,
            (Elem::Import(s), Elem::Import(s2)) => s == s2,
            (Elem::Link(l), Elem::Link(l2)) => l == l2,

            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Global
{
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub external: bool,
    pub public: bool,
    pub reassignable: bool,

    pub typ: Box<Type>,
    pub expr: Option<Box<Expr>>,
}

impl PartialEq for Global
{
    fn eq(&self, other: &Self) -> bool { self.name == other.name }
}

#[derive(Clone, Debug)]
pub struct Const
{
    pub id: NodeId,
    pub pos: Position,
    pub public: bool,
    pub name: Name,
    pub typ: Type,
    pub expr: Box<Expr>,
}

impl PartialEq for Const
{
    fn eq(&self, other: &Self) -> bool { self.name == other.name }
}

#[derive(Clone, Debug)]
pub struct Struct
{
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub public: bool,
    pub fields: Vec<StructField>,
}

impl PartialEq for Struct
{
    fn eq(&self, other: &Self) -> bool { self.name == other.name }
}

#[derive(Clone, Debug)]
pub struct StructField
{
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
}
impl Eq for StructField {}

use std::hash::{Hash, Hasher};

impl Hash for StructField
{
    fn hash<H: Hasher>(&self, h: &mut H)
    {
        self.name.hash(h);
        self.data_type.hash(h);
    }
}

impl PartialEq for StructField
{
    fn eq(&self, other: &Self) -> bool
    {
        (self.name == other.name) && (self.data_type == other.data_type)
    }
}

#[derive(Clone, Debug)]
pub struct StructArg
{
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub expr: Box<Expr>,
}

use super::interner::*;
use crate::syntax::position::Position;

#[derive(Clone, Debug)]
pub enum Type
{
    Basic(TypeBasic),
    Ptr(TypePtr),
    Array(TypeArray),
    Func(TypeFunc),
    Struct(TypeStruct),
    Void(Position),
    Vector(TypeVector),
}

impl Hash for Type
{
    fn hash<H: Hasher>(&self, h: &mut H)
    {
        match self
        {
            Type::Basic(b) => b.name.hash(h),
            Type::Array(a1) =>
            {
                a1.subtype.hash(h);
                a1.len.hash(h)
            }
            Type::Ptr(p) => p.subtype.hash(h),
            Type::Void(_) => 0.hash(h),
            Type::Func(f) =>
            {
                f.params.hash(h);
                f.ret.hash(h)
            }
            Type::Struct(s) =>
            {
                s.fields.hash(h);
                s.name.hash(h);
            }
            Type::Vector(v) =>
            {
                v.subtype.hash(h);
                v.size.hash(h);
            }
        }
    }
}

use std::cmp::{Eq, PartialEq};
impl Eq for Type {}
impl PartialEq for Type
{
    fn eq(&self, other: &Type) -> bool
    {
        match (self, other)
        {
            (Type::Basic(basic), Type::Basic(basic2)) => basic.name == basic2.name,
            (Type::Array(a1), Type::Array(a2)) => (a1.subtype == a2.subtype) && (a1.len == a2.len),
            (Type::Ptr(ptr), Type::Ptr(ptr2)) => ptr.subtype == ptr2.subtype,
            (Type::Struct(s), Type::Struct(s2)) => (s.fields == s2.fields) && (s.name == s2.name),
            (Type::Void(_), Type::Void(_)) => true,
            (Type::Func(f), Type::Func(f2)) => (f.params == f2.params) && (f.ret == f2.ret),
            (Type::Struct(s), Type::Basic(b)) => s.name == b.name,
            (Type::Basic(b), Type::Struct(s)) => s.name == b.name,
            (Type::Vector(v1), Type::Vector(v2)) => v1.subtype == v2.subtype && v1.size == v2.size,
            _ => false,
        }
    }
}
#[derive(Clone, Debug)]
pub struct TypeVector
{
    pub id: NodeId,
    pub pos: Position,
    pub subtype: Box<Type>,
    pub size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypePtr
{
    pub id: NodeId,
    pub pos: Position,
    pub subtype: Box<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeBasic
{
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeArray
{
    pub id: NodeId,
    pub pos: Position,
    pub subtype: Box<Type>,
    pub len: Option<usize>,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeStruct
{
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeFunc
{
    pub id: NodeId,
    pub pos: Position,
    pub params: Vec<Box<Type>>,
    pub ret: Box<Type>,
}

impl Type
{
    pub fn make_ptr(&self) -> Type
    {
        match self
        {
            Type::Basic(b) => Type::create_ptr(b.id, b.pos, box self.clone()),
            Type::Ptr(p) => Type::create_ptr(p.id, p.pos, box self.clone()),
            Type::Struct(s) => Type::create_ptr(s.id, s.pos, box self.clone()),
            Type::Func(f) => Type::create_ptr(f.id, f.pos, box self.clone()),
            Type::Array(a) => Type::create_ptr(a.id, a.pos, box self.clone()),
            Type::Vector(v) => Type::create_ptr(v.id, v.pos, box self.clone()),
            _ => unimplemented!(),
        }
    }

    pub const fn create_basic(id: NodeId, pos: Position, name: Name) -> Type
    {
        Type::Basic(TypeBasic { id, pos, name })
    }

    pub const fn create_struct(
        id: NodeId,
        pos: Position,
        name: Name,
        fields: Vec<StructField>,
    ) -> Type
    {
        Type::Struct(TypeStruct {
            id,
            pos,
            fields,
            name,
        })
    }

    pub const fn create_ptr(id: NodeId, pos: Position, sub: Box<Type>) -> Type
    {
        Type::Ptr(TypePtr {
            id,
            pos,
            subtype: sub,
        })
    }
    pub const fn create_array(id: NodeId, pos: Position, ty: Box<Type>, len: Option<usize>)
        -> Type
    {
        Type::Array(TypeArray {
            id,
            pos,
            subtype: ty,
            len,
        })
    }

    pub const fn create_func(
        id: NodeId,
        pos: Position,
        params: Vec<Box<Type>>,
        ret: Box<Type>,
    ) -> Type
    {
        Type::Func(TypeFunc {
            id,
            pos,
            params,
            ret,
        })
    }

    pub fn to_basic(&self) -> Option<&TypeBasic>
    {
        match self
        {
            Type::Basic(b) => Some(b),
            _ => None,
        }
    }

    pub fn is_vec(&self) -> bool
    {
        match self
        {
            Type::Vector(_) => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool
    {
        match self
        {
            Type::Ptr(_) => true,
            _ => false,
        }
    }

    pub fn to_ptr(&self) -> Option<&TypePtr>
    {
        match self
        {
            Type::Ptr(p) => Some(p),
            _ => None,
        }
    }
    pub fn to_func(&self) -> Option<&TypeFunc>
    {
        match self
        {
            Type::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<&TypeStruct>
    {
        match self
        {
            Type::Struct(s) => Some(s),
            Type::Ptr(s) => s.subtype.to_struct(),
            _ => None,
        }
    }

    pub fn is_struct(&self) -> bool
    {
        match self
        {
            Type::Struct(_) => true,
            Type::Ptr(ptr) => ptr.subtype.is_struct(),
            _ => false,
        }
    }

    pub fn to_vec(&self) -> Option<&TypeVector>
    {
        match self
        {
            Type::Vector(v) => Some(v),
            _ => None,
        }
    }

    pub fn to_array(&self) -> Option<&TypeArray>
    {
        match self
        {
            Type::Array(arr) => Some(arr),
            _ => None,
        }
    }

    pub fn is_void(&self) -> bool
    {
        match self
        {
            Type::Void(_) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool
    {
        match self
        {
            Type::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_basic(&self) -> bool
    {
        match self
        {
            Type::Basic(_) => true,
            _ => false,
        }
    }

    pub fn pos(&self) -> Position
    {
        match self
        {
            Type::Vector(v) => v.pos,
            Type::Array(arr) => arr.pos,
            Type::Basic(b) => b.pos,
            Type::Ptr(p) => p.pos,
            Type::Func(f) => f.pos,
            Type::Void(pos) => *pos,
            Type::Struct(s) => s.pos,
        }
    }

    pub fn id(&self) -> NodeId
    {
        match self
        {
            Type::Array(arr) => arr.id,
            Type::Basic(b) => b.id,
            Type::Ptr(p) => p.id,
            Type::Func(f) => f.id,
            Type::Struct(s) => s.id,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Type::Vector(v) => write!(f, "<{};{}>", v.subtype, v.size),
            Type::Void(_) => write!(f, "void"),
            Type::Ptr(ptr) => write!(f, "*{}", ptr.subtype),
            Type::Array(arr) => write!(
                f,
                "{}[{}]",
                arr.subtype,
                if arr.len.is_some()
                {
                    arr.len.unwrap().to_string()
                }
                else
                {
                    "".to_owned()
                }
            ),
            Type::Basic(basic) => write!(f, "{}", str(basic.name)),
            Type::Struct(struc) =>
            {
                write!(f, "{}(", str(struc.name))?;
                for (i, field) in struc.fields.iter().enumerate()
                {
                    write!(f, "{}", field.data_type)?;
                    if i != struc.fields.len() - 1
                    {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            }
            Type::Func(fun) =>
            {
                write!(f, "(")?;
                for (i, p) in fun.params.iter().enumerate()
                {
                    write!(f, "{}", p)?;
                    if i != fun.params.len() - 1
                    {
                        write!(f, ",")?;
                    }
                }
                write!(f, ") -> {}", fun.ret)
            }
        }
    }
}

/// Function
///
/// ```go
/// func (v ref String) add(other ref String) String {
///     return v + other
/// }
///
/// "Hello, ".add("World!")
/// ```

#[derive(Clone, Debug)]
pub struct Function
{
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,

    /// string attributes that passed to LLVM
    pub attributes: Vec<String>,
    pub variadic: bool,
    pub inline: bool,
    pub external: bool,
    pub constant: bool,
    pub public: bool,
    pub internal: bool,
    pub static_: bool,
    pub params: Vec<(Name, Box<Type>)>,
    pub ret: Box<Type>,
    pub this: Option<(Name, Box<Type>)>,
    pub body: Option<Box<Stmt>>,
    pub ir_temp_id: usize,
}

impl Function
{
    pub fn replace_expr_to(&mut self, id: NodeId, to: Expr)
    {
        fn replace_stmt(s: &mut Stmt, id: NodeId, to: Expr) -> bool
        {
            match &mut s.kind
            {
                StmtKind::Continue => false,
                StmtKind::Break => false,
                StmtKind::Return(expr) =>
                {
                    if expr.is_some()
                    {
                        let expr = expr.as_mut().unwrap();
                        if expr.id == id
                        {
                            expr.kind = to.kind.clone();
                            return true;
                        }
                    }
                    return false;
                }
                StmtKind::Block(block) =>
                {
                    for stmt in block.iter_mut()
                    {
                        if replace_stmt(stmt, id, to.clone())
                        {
                            return true;
                        }
                    }

                    return false;
                }
                StmtKind::Expr(expr) =>
                {
                    if expr.id == id
                    {
                        *expr = box to;
                        return true;
                    }
                    return false;
                }
                StmtKind::If(e, then, other) =>
                {
                    if e.id == id
                    {
                        *e = box to;
                        return true;
                    }
                    if replace_stmt(then, id, to.clone())
                    {
                        return true;
                    }
                    if other.is_some()
                    {
                        let other = other.as_mut().unwrap();
                        if replace_stmt(other, id, to.clone())
                        {
                            return true;
                        }
                        else
                        {
                            return false;
                        }
                    }
                    return false;
                }
                StmtKind::While(expr, then) =>
                {
                    if expr.id == id
                    {
                        *expr = box to;
                        return true;
                    }
                    if replace_stmt(then, id, to.clone())
                    {
                        return true;
                    }
                    else
                    {
                        return false;
                    }
                }
                StmtKind::Loop(body) => replace_stmt(body, id, to.clone()),
                StmtKind::Var(_, _, _, expr) =>
                {
                    if expr.is_some()
                    {
                        let expr = expr.as_mut().unwrap();
                        if expr.id == id
                        {
                            *expr = box to;
                            return true;
                        }
                    }
                    false
                }
            }
        }
        if self.body.is_some()
        {
            let body = self.body.as_mut().unwrap();
            replace_stmt(body, id, to);
        }
    }
}

impl PartialEq for Function
{
    fn eq(&self, other: &Self) -> bool
    {
        let mut params_match = false;
        for (p1, p2) in self.params.iter().zip(&other.params)
        {
            params_match = p1.1 == p2.1;
        }

        self.name == other.name
            && self.ret == other.ret
            && params_match
            && self.this == other.this
            && self.variadic == other.variadic
    }
}

#[derive(Clone, Debug)]
pub struct Expr
{
    pub id: NodeId,
    pub pos: Position,
    pub kind: ExprKind,
}

impl Expr
{
    pub fn is_deref(&self) -> bool
    {
        match &self.kind
        {
            ExprKind::Deref(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self, val: bool) -> bool
    {
        match &self.kind
        {
            ExprKind::Bool(b) => *b == val,
            _ => false,
        }
    }
}

impl Expr
{
    pub fn map<U>(&self, mut f: impl FnMut(&Self) -> U, or_: U) -> Vec<U>
    {
        match &self.kind
        {
            ExprKind::Unary(_, expr) => return vec![f(expr)],
            ExprKind::Binary(_, e1, e2) => return vec![f(e1), f(e2)],
            ExprKind::ArrayIdx(e1, e2) => return vec![f(e1), f(e2)],
            ExprKind::Array(_, exprs) => exprs.iter().map(|e| f(e)).collect(),
            ExprKind::Call(_, e1, e2) =>
            {
                let mut v = vec![];
                if e1.is_some()
                {
                    v.push(f(&e1.clone().unwrap()));
                }
                let _ = e2.iter().map(|e| v.push(f(e)));

                v
            }
            ExprKind::Assign(e1, e2) => vec![f(e1), f(e2)],
            ExprKind::Field(e1, _) => vec![f(e1)],
            ExprKind::Conv(e1, _) => vec![f(e1)],
            ExprKind::Struct(_, fields) => fields.iter().map(|e| f(&e.expr)).collect(),
            _ => vec![or_],
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind
{
    Unary(String, Box<Expr>),
    Binary(String, Box<Expr>, Box<Expr>),
    Char(char),
    Int(i64, IntBase, IntSuffix),
    Float(f64, FloatSuffix),
    Str(String),
    Bool(bool),
    Ident(Name),
    ArrayIdx(Box<Expr>, Box<Expr>),
    Deref(Box<Expr>),
    Array(Box<Type>, Vec<Box<Expr>>),
    GetFunc(Name),
    Null,
    Call(Path, Option<Box<Expr>>, Vec<Box<Expr>>),
    Assign(Box<Expr>, Box<Expr>),
    Field(Box<Expr>, Name),
    Conv(Box<Expr>, Box<Type>),
    Struct(Path, Vec<StructArg>),
    AddressOf(Box<Expr>),
    SizeOf(Box<Type>),
}
#[derive(Clone, Debug)]
pub struct Stmt
{
    pub id: NodeId,
    pub pos: Position,
    pub kind: StmtKind,
}
impl Stmt
{
    pub fn is_if(&self) -> bool { self.kind.is_if() }
}

#[derive(Clone, Debug)]
pub enum StmtKind
{
    Return(Option<Box<Expr>>),
    Block(Vec<Box<Stmt>>),
    Expr(Box<Expr>),
    Loop(Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    Var(Name, bool, Option<Type>, Option<Box<Expr>>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Continue,
    Break,
}

impl StmtKind
{
    pub fn is_if(&self) -> bool
    {
        match self
        {
            StmtKind::If(_, _, _) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Path
{
    pub path: Vec<Name>,
}

impl Path
{
    pub fn new(name: Name) -> Path { Path { path: vec![name] } }

    pub fn name(&self) -> Name
    {
        assert_eq!(1, self.path.len());

        self.path[0]
    }

    pub fn len(&self) -> usize { self.path.len() }

    pub fn is_empty(&self) -> bool { self.len() == 0 }
}

impl Index<usize> for Path
{
    type Output = Name;

    fn index(&self, idx: usize) -> &Name { &self.path[idx] }
}
