use crate::{ast::*, syntax::interner::Name, Context, *};

use std::cell::RefCell;

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct FuncSig
{
    pub name: Name,
    pub params: Vec<Type>,
    pub ret: Box<Type>,
    pub this: Option<Box<Type>>,
    pub variadic: bool,
    pub this_name: Name,
}

pub struct SemCheck<'a>
{
    ctx: &'a mut Context,
    structures: RefCell<HashMap<Name, Struct>>,
    functions: HashMap<FuncSig, Function>,
    signatures: HashMap<Name, Vec<FuncSig>>,
    globals: HashMap<Name, Global>,
    constants: HashMap<Name, Const>,
    vars: Vec<HashMap<Name, Type>>,
    constexprs: HashMap<Name, Box<Expr>>,
    ret: Type,
    types: HashMap<NodeId, Type>,
    aliases: HashMap<Name, Type>,
    imported: HashMap<Name, Elem>,
    imported_funs: HashMap<Name, Vec<Function>>,
    __internal_funs: HashMap<Name, Function>,
}
