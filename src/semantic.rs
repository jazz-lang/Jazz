use super::syntax::interner::Name;
use super::syntax::lexer::token::{FloatSuffix, IntSuffix};
use super::Context;
use super::*;
use crate::ast::*;
use colored::Colorize;
use std::collections::HashSet;

pub struct SemCheck<'a>
{
    ctx: &'a mut Context,
    structures: HashMap<Name, Struct>,
    functions: HashMap<FuncSig, Function>,
    signatures: HashMap<Name, Vec<FuncSig>>,
    globals: HashMap<Name, Global>,
    constants: HashMap<Name, Const>,
    vars: Vec<HashMap<Name, Type>>,
    constexprs: HashMap<Name, Box<Expr>>,
    ret: Type,
    types: HashMap<NodeId, Type>,
    aliases: HashMap<Name, Type>,
    imported: HashMap<Name,Elem>,
    imported_funs: HashMap<Name,Vec<Function>>
}


pub fn ty_is_any_int(ty: &Type) -> bool
{
    match ty
    {
        Type::Basic(basic) =>
        {
            let s: &str = &str(basic.name).to_string();
            match s
            {
                "uchar" | "char" | "u8" | "u16" | "u32" | "u64" | "i64" | "i32" | "i16" | "i8"
                | "isize" | "usize" => true,
                _ => false,
            }
        }

        _ => false,
    }
}

pub fn ty_is_any_float(ty: &Type) -> bool
{
    match ty
    {
        Type::Basic(basic) =>
        {
            let s: &str = &str(basic.name).to_string();
            match s
            {
                "f32" | "f64" => true,
                _ => false,
            }
        }

        _ => false,
    }
}

#[derive(Debug)]
pub enum Error
{
    ConstantExists(String),
    FunctionExists(String, Type),
    StructureExists(String),
    GlobalExists(String),
    VariableAlreadyDefined(String),
}

impl Error
{
    pub fn message(&self) -> String
    {
        match self
        {
            Error::ConstantExists(name) => format!("Constant {} exists", name),
            Error::FunctionExists(name, ty) =>
            {
                format!("Function {} with signature {} already exists", name, ty)
            }
            Error::StructureExists(s) => format!("Structure {} exists", s),
            Error::GlobalExists(s) => format!("Global {} exists", s),
            Error::VariableAlreadyDefined(s) => format!("Variable {} exists", s),
        }
    }
}

#[derive(Debug)]
pub struct ErrorWPos
{
    pub pos: Position,
    pub error: Error,
    pub src: String,
}

use std::fmt;

impl fmt::Display for ErrorWPos
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "error {}: {}", self.pos, self.error.message())
    }
}

impl ErrorWPos
{
    pub const fn new(pos: Position, error: Error, src: String) -> ErrorWPos
    {
        ErrorWPos { pos, error, src }
    }
}

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

impl<'a> SemCheck<'a>
{
    pub fn new(ctx: &'a mut Context) -> SemCheck<'a>
    {
        SemCheck {
            ctx,
            structures: HashMap::new(),
            functions: HashMap::new(),
            globals: HashMap::new(),
            constants: HashMap::new(),
            vars: vec![],
            signatures: HashMap::new(),
            ret: Type::Void(Position::new(intern("<>"), 0, 0)),
            types: HashMap::new(),
            constexprs: HashMap::new(),
            aliases: HashMap::new(),
            imported: HashMap::new(),
            imported_funs: HashMap::new(),
            
        }
    }

    pub fn run(&mut self)
    {
        self.imports();
        let maybe_err = self.declare();
        if maybe_err.is_ok()
        {
            for (_, fun) in self.functions.clone().iter()
            {
                self.ret = self.infer_type(&fun.ret);
                self.vars.clear();
                self.vars.push(HashMap::new());
                if !fun.external
                {
                    for (name, ty) in fun.params.iter()
                    {
                        let ty = self.infer_type(ty);
                        self.vars.last_mut().unwrap().insert(*name, ty);
                    }

                    if fun.this.is_some()
                    {
                        let (name, ty) = fun.this.clone().unwrap();
                        let ty = self.infer_type(&ty);

                        self.vars.last_mut().unwrap().insert(name, ty);
                    }

                    let body = fun.body.clone();
                    let body = body.unwrap();
                    self.tc_stmt(&body);
                }
            }
        }
        else
        {
            eprintln!("{}", maybe_err.unwrap_err());
        }

        for (k, v) in self.types.iter()
        {
            self.ctx.types.insert(k.clone(), v.clone());
        }
    }

    pub fn imports(&mut self)
    {
        let elems = self.ctx.file.elems.clone();

        for elem in elems.iter()
        {
            if let Elem::Import(import) = elem
            {
                let import = if self.ctx.file.root.is_empty()
                {
                    import.to_owned()
                }
                else
                {
                    let path = std::path::Path::new(&self.ctx.file.root).parent().unwrap();
                    format!("{}/{}", path.display(), import)
                };

                let mut file = File {
                    elems: vec![],
                    src: String::new(),
                    path: String::new(),
                    root: import.clone(),
                };
                use crate::syntax::lexer;
                use crate::syntax::parser::Parser;
                use lexer::reader::Reader;
                let reader = Reader::from_file(&import).expect("File not found");
                let mut parser = Parser::new(reader, &mut file);
                parser.parse().expect("Error");

                let mut ctx = Context::new(file);

                let mut sem = SemCheck::new(&mut ctx);
                sem.imports();
                let maybe_err = sem.declare();
                if maybe_err.is_err()
                {
                    eprintln!("{}", maybe_err.err().unwrap());
                    std::process::exit(-1);
                }
                for elem in ctx.file.elems.iter()
                {
                    match elem
                    {
                        Elem::Func(f) =>
                        {
                            
                            if f.public && !f.static_
                            {
                                let funs = self.imported_funs.get(&f.name).clone();
                                if funs.is_none() {
                                    let funs = vec![f.clone()];
                                    self.imported_funs.insert(f.name, funs);
                                    self.ctx.file.elems.push(Elem::Func(f.clone()));
                                } else {
                                    let funs = funs.unwrap();
                                    let mut f_found = false;
                                    for fun in funs.iter() {
                                        let mut params = vec![];
                                        for p in f.params.iter() {
                                            params.push((p.0,Box::new(self.infer_type(&p.1))));
                                        }
                                        if params == fun.params {
                                            f_found = true;
                                            break;
                                        }
                                    }

                                    if !f_found {
                                        let funs = self.imported_funs.get_mut(&f.name).unwrap();
                                        self.ctx.file.elems.push(Elem::Func(f.clone()));
                                        funs.push(f.clone());
                                    }


                                }
                                
                            }
                        }
                        Elem::Link(name) =>
                        {
                            self.ctx.file.elems.push(Elem::Link(*name));
                        }
                        Elem::Const(c) =>
                        {
                            if !self.imported.contains_key(&c.name) {
                                if c.public
                                {
                                    self.imported.insert(c.name,Elem::Const(c.clone()));
                                    self.ctx.file.elems.push(Elem::Const(c.clone()));
                                }
                            }
                            
                        }
                        Elem::Struct(s) =>
                        {   
                            if !self.imported.contains_key(&s.name) {
                                if s.public
                                {
                                    self.imported.insert(s.name, Elem::Struct(s.clone()));
                                    self.ctx.file.elems.push(Elem::Struct(s.clone()));
                                }
                            }
                        }
                        Elem::Global(glob) =>
                        {
                            if !self.imported.contains_key(&glob.name) {
                                if glob.public
                                {
                                    self.imported.insert(glob.name, Elem::Global(glob.clone()));
                                    self.ctx.file.elems.push(Elem::Global(glob.clone()));
                                }
                            }
                        }
                        Elem::ConstExpr {
                            name,
                            expr,
                            id,
                            pos,
                        } =>
                        {
                            if !self.imported.contains_key(name) {
                                let elem = Elem::ConstExpr {
                                    name: *name,
                                    expr: expr.clone(),
                                    id: *id,
                                    pos: *pos,
                                };

                                self.imported.insert(*name, elem.clone());
                                self.ctx.file.elems.push(elem);

                            }
                        }
                        Elem::Alias(name, ty) =>
                        {
                            if !self.imported.contains_key(name) {
                                let elem = Elem::Alias(*name,ty.clone());
                                self.imported.insert(*name,elem.clone());
                                self.ctx.file.elems.push(elem);

                            }
                            
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    pub fn declare(&mut self) -> Result<(), ErrorWPos>
    {
        let src = self.ctx.file.src.clone();
        for elem in self.ctx.file.elems.iter()
        {
            if let Elem::Struct(s) = elem
            {
                if self.structures.contains_key(&s.name)
                {
                    return Err(ErrorWPos::new(
                        s.pos,
                        Error::StructureExists(str(s.name).to_string()),
                        src.clone(),
                    ));
                }
                let _structure = Struct {
                    id: s.id,
                    pos: s.pos,
                    name: s.name,
                    public: s.public,
                    fields: s.fields.clone(),
                };
                self.structures.insert(s.name, s.clone());
            }
        }
        for elem in self.ctx.file.elems.iter()
        {
            match elem
            {
                Elem::ConstExpr { name, expr, .. } =>
                {
                    self.constexprs.insert(*name, expr.clone());
                }
                Elem::Alias(name, ty) =>
                {
                    self.aliases.insert(*name, ty.clone());
                }
                Elem::Const(c) =>
                {
                    if self.constants.contains_key(&c.name)
                    {
                        return Err(ErrorWPos::new(
                            c.pos,
                            Error::ConstantExists(str(c.name).to_string()),
                            src.clone(),
                        ));
                    }
                    self.constants.insert(c.name, c.clone());
                }

                Elem::Func(func) =>
                {
                    let mut params: Vec<Type> =
                        func.params.iter().map(|(_, ty)| *ty.clone()).collect();
                    let mut ret = func.ret.clone();
                    let this = func.this.clone();

                    let (mut this, this_name) = if this.is_some()
                    {
                        (
                            Some(this.clone().unwrap().1.clone()),
                            this.clone().unwrap().0,
                        )
                    }
                    else
                    {
                        (None, intern(""))
                    };
                    let name = func.name;

                    ret = Box::new(self.infer_type(&ret));

                    for p in params.iter_mut()
                    {
                        let ty = self.infer_type(p);
                        *p = ty;
                    }

                    if this.is_some()
                    {
                        this = Some(Box::new(self.infer_type(&this.clone().unwrap())));
                    }

                    let sig = FuncSig {
                        name,
                        params,
                        ret,
                        this_name,
                        this: if this.is_some()
                        {
                            Some(this.unwrap().clone())
                        }
                        else
                        {
                            None
                        },
                        variadic: func.variadic,
                    };

                    if !self.signatures.contains_key(&func.name)
                    {
                        self.signatures.insert(func.name, vec![]);
                    }
                    let signatures = self.signatures.get_mut(&func.name).unwrap();
                    if signatures.contains(&sig)
                    {
                        return Err(ErrorWPos::new(
                            func.pos,
                            Error::FunctionExists(
                                str(func.name).to_string(),
                                Type::create_func(
                                    func.id,
                                    func.pos,
                                    sig.params.iter().map(|ty| Box::new(ty.clone())).collect(),
                                    func.ret.clone(),
                                ),
                            ),
                            src.clone(),
                        ));
                    }
                    signatures.push(sig.clone());
                    if self.functions.contains_key(&sig)
                    {
                        return Err(ErrorWPos::new(
                            func.pos,
                            Error::FunctionExists(
                                str(func.name).to_string(),
                                Type::create_func(
                                    func.id,
                                    func.pos,
                                    sig.params.iter().map(|ty| Box::new(ty.clone())).collect(),
                                    func.ret.clone(),
                                ),
                            ),
                            src.clone(),
                        ));
                        //return Err(ErrorWPos::new(func.pos,Error::FunctionExists(str(func.name).to_string(),Type::create_func(func.id, func.pos, func.params.clone(), func.ret.clone())),src.clone()));
                    }

                    self.functions.insert(sig, func.clone());
                }
                Elem::Global(c) =>
                {
                    if self.globals.contains_key(&c.name)
                    {
                        return Err(ErrorWPos::new(
                            c.pos,
                            Error::GlobalExists(str(c.name).to_string()),
                            src.clone(),
                        ));
                    }

                    let mut c = c.clone();
                    c.typ = Box::new(self.infer_type(&*c.typ));
                    self.globals.insert(c.name, c.clone());
                }

                _ => (), // do nothing
            }
        }

        Ok(())
    }

    /// Infer type,if some struct type declared in context and @ty is basic then return Type::Struct
    pub fn infer_type(&self, ty: &Type) -> Type
    {
        let pos = ty.pos();

        match ty
        {
            Type::Struct(struc) =>
            {
                let id = ty.id();
                let mut fields: Vec<StructField> = vec![];

                for field in struc.fields.iter()
                {
                    fields.push(StructField {
                        name: field.name,
                        data_type: self.infer_type(&field.data_type),
                        id: field.id,
                        pos: field.pos,
                    })
                }

                Type::Struct(TypeStruct {
                    id,
                    pos,
                    fields,
                    name: struc.name,
                })
            }
            Type::Basic(basic) =>
            {
                if let Some(ty) = self.aliases.get(&basic.name)
                {
                    return self.infer_type(&ty);
                }
                let id = ty.id();
                if self.structures.contains_key(&basic.name)
                {
                    let struc = self.structures.get(&basic.name).unwrap();

                    Type::create_struct(id, pos, basic.name, struc.fields.clone())
                }
                else
                {
                    Type::Basic(basic.clone())
                }
            }
            Type::Ptr(t) =>
            {
                let id = ty.id();
                Type::Ptr(TypePtr {
                    id,
                    pos,
                    subtype: Box::new(self.infer_type(&t.subtype)),
                })
            }
            Type::Array(arr) =>
            {
                let id = ty.id();
                Type::Array(TypeArray {
                    len: arr.len,
                    subtype: Box::new(self.infer_type(&arr.subtype)),
                    id,
                    pos,
                })
            }
            Type::Func(tyfun) =>
            {
                let id = ty.id();
                let mut params = vec![];
                for p in tyfun.params.iter()
                {
                    params.push(Box::new(self.infer_type(p)));
                }

                let ret = self.infer_type(&tyfun.ret);
                Type::Func(TypeFunc {
                    id,
                    pos,
                    params,
                    ret: Box::new(ret),
                })
            }
            Type::Void(pos) => Type::Void(*pos),
        }
    }

    pub fn tc_stmt(&mut self, stmt: &Stmt)
    {
        let _id = stmt.id;
        match &stmt.kind
        {
            StmtKind::Continue | StmtKind::Break => (),
            StmtKind::Expr(e) =>
            {
                self.tc_expr(e);
            }
            StmtKind::Return(e) =>
            {
                if e.is_some()
                {
                    let mut t = self.tc_expr(&e.clone().unwrap());
                    t = self.infer_type(&t);

                    if t == self.ret || ty_is_any_int(&t) && ty_is_any_int(&self.ret)
                    {
                        return;
                    }
                    else
                    {
                        error!(format!("Expected {} type,found {}", self.ret, t), stmt.pos);
                    }
                }

                assert!(self.ret.is_void());
            }
            StmtKind::While(e, s) =>
            {
                if e.is_bool(true)
                {
                    warn!("Consider using loop instead of `while true`", stmt.pos);
                }
                self.tc_expr(e);
                self.tc_stmt(s);
            }
            StmtKind::If(cond, then, otherwise) =>
            {
                self.tc_expr(cond);
                self.tc_stmt(then);

                if otherwise.is_some()
                {
                    let otherwise = otherwise.clone().unwrap();
                    self.tc_stmt(&otherwise);
                }
            }
            StmtKind::Var(name, _, ty, init) =>
            {
                if self.vars.last().unwrap().contains_key(name)
                {
                    error!(format!("Variable {} already exists", str(*name)), stmt.pos);
                }
                if init.is_some() && ty.is_none()
                {
                    let init = init.clone().unwrap();
                    let mut t = self.tc_expr(&init);
                    t = self.infer_type(&t);
                    self.vars.last_mut().unwrap().insert(*name, t.clone());
                    self.types.insert(stmt.id, t);
                }
                else if ty.is_some() && init.is_none()
                {
                    self.vars
                        .last_mut()
                        .unwrap()
                        .insert(*name, ty.clone().unwrap());
                    self.types.insert(stmt.id, ty.clone().unwrap());
                }
                else if ty.is_none() && init.is_none()
                {
                    error!("Type annotation required", stmt.pos);
                }
                else
                {
                    let init = init.clone().unwrap();
                    let mut t = self.tc_expr(&init);
                    t = self.infer_type(&t);
                    let mut t2 = ty.clone().unwrap();
                    t2 = self.infer_type(&t2);
                    if ty_is_any_int(&t2) && ty_is_any_int(&t)
                    {
                        self.vars.last_mut().unwrap().insert(*name, t2.clone());
                        self.types.insert(stmt.id, t2);
                    }
                    else
                    {
                        if t2 != t
                        {
                            error!(format!("Expected {}, found {}", t, t2), stmt.pos);
                        }
                        self.vars.last_mut().unwrap().insert(*name, t2.clone());
                        self.types.insert(stmt.id, t2);
                    }
                }
            }
            StmtKind::Block(stmts) =>
            {
                let prev;
                if !self.vars.is_empty()
                {
                    prev = self.vars.last().unwrap().clone();
                }
                else
                {
                    prev = HashMap::new();
                };
                self.vars.push(prev);
                for stmt in stmts.iter()
                {
                    self.tc_stmt(stmt);
                }
                self.vars.pop();
            }
            StmtKind::Loop(stmt) => self.tc_stmt(stmt),
        };
    }

    pub fn tc_expr(&mut self, expr: &Expr) -> Type
    {
        match &expr.kind
        {
            ExprKind::Int(_, _, suffix) =>
            {
                let ty = match suffix
                {
                    IntSuffix::Byte => Type::create_basic(expr.id, expr.pos, intern("i8")),
                    IntSuffix::Int => Type::create_basic(expr.id, expr.pos, intern("i32")),
                    IntSuffix::Long => Type::create_basic(expr.id, expr.pos, intern("i64")),
                    IntSuffix::UByte => Type::create_basic(expr.id, expr.pos, intern("u8")),
                    IntSuffix::UInt => Type::create_basic(expr.id, expr.pos, intern("u32")),
                    IntSuffix::ULong => Type::create_basic(expr.id, expr.pos, intern("u64")),
                };
                self.types.insert(expr.id, ty.clone());
                ty
            }
            ExprKind::Float(_, suffix) =>
            {
                let ty = match suffix
                {
                    FloatSuffix::Float => Type::create_basic(expr.id, expr.pos, intern("f32")),
                    FloatSuffix::Double => Type::create_basic(expr.id, expr.pos, intern("f64")),
                };

                self.types.insert(expr.id, ty.clone());

                ty
            }
            ExprKind::GetFunc(name) =>
            {
                warn!("Matching function args not supported yet so 'func &' might return wrong function",expr.pos);
                for (_, sigs) in self.signatures.iter()
                {
                    for sig in sigs.iter()
                    {
                        if sig.name == *name
                        {
                            let ty = Type::create_func(
                                expr.id,
                                expr.pos,
                                sig.params.iter().map(|t| Box::new(t.clone())).collect(),
                                sig.ret.clone(),
                            );
                            self.types.insert(expr.id, ty.clone());

                            return ty;
                        }
                    }
                }
                error!(format!("Function {} not found", str(*name)), expr.pos);
            }
            ExprKind::Deref(expr_) =>
            {
                let ty = self.tc_expr(expr_);
                let ty = self.infer_type(&ty);
                if let Type::Ptr(ty) = ty
                {
                    self.types.insert(expr.id, *ty.subtype.clone());
                    return *ty.subtype.clone();
                }
                else
                {
                    error!(format!("Dereferencing non-ptr type {}", ty), expr.pos);
                }
            }
            ExprKind::AddressOf(expr_) =>
            {
                let mut ty = self.tc_expr(expr_);
                ty = self.infer_type(&ty);
                self.types.insert(expr.id, ty.clone());
                Type::create_ptr(expr.id, expr.pos, Box::new(ty))
            }
            ExprKind::Call(path, object, args) =>
            {
                let mut params = vec![];
                for arg in args.iter()
                {
                    let ty = self.tc_expr(arg);
                    let ty = self.infer_type(&ty);
                    params.push(ty.clone());
                    self.types.insert(arg.id, ty);
                }
                let objty = if object.is_some()
                {
                    let ty = self.tc_expr(&object.clone().unwrap());
                    Some(Box::new(self.infer_type(&ty)))
                }
                else
                {
                    None
                };
                let sigs = self.signatures.get(&path.name());
                if sigs.is_some()
                {
                    if object.is_some()
                    {
                        let sigs: &Vec<FuncSig> = sigs.unwrap();
                        let mut objty = objty.clone().unwrap();
                        if !objty.is_ptr()
                        {
                            objty =
                                Box::new(Type::create_ptr(objty.id(), objty.pos(), objty.clone()));
                        }
                        for sig in sigs.iter()
                        {
                            if sig.params == params && sig.this == Some(objty.clone())
                            {
                                let ty = *sig.ret.clone();
                                self.types.insert(expr.id, ty.clone());

                                return ty;
                            }
                        }
                    }
                    else
                    {
                        let sigs: &Vec<FuncSig> = sigs.unwrap();
                        for sig in sigs.iter()
                        {
                            /*if params.len() > sig.params.len() {
                                assert!(sig.variadic);
                            }
                            if sig.params == params {
                                return *sig.ret.clone();
                            }*/
                            let this_sig;
                            if params.is_empty() && sig.params.is_empty()
                            {
                                return *sig.ret.clone();
                            }
                            let mut types_good = false;
                            for (i, param) in params.iter().enumerate()
                            {
                                if (params.len() > sig.params.len() && !sig.variadic)
                                    || params.len() < sig.params.len()
                                {
                                    types_good = false;
                                    break;
                                }
                                if i < sig.params.len()
                                {
                                    types_good = param == &sig.params[i];
                                    //if !types_good {types_good = ty_is_any_int(param) && ty_is_any_int(&sig.params[i]);};
                                }
                            }
                            this_sig = if sig.variadic
                            {
                                types_good && sig.variadic
                            }
                            else
                            {
                                types_good
                            };

                            if this_sig
                            {
                                let ty = self.infer_type(&sig.ret);
                                self.types.insert(expr.id, ty.clone());
                                return ty;
                            }
                        }
                    }
                }
                else if self.vars.last().unwrap().contains_key(&path.name())
                {
                    let ty: &Type = self.vars.last().unwrap().get(&path.name()).unwrap();
                    let f = ty.to_func();
                    if f.is_none()
                    {
                        error!("Function type expected", expr.pos);
                    }
                    else
                    {
                        let f = f.unwrap().clone();
                        let mut types_good = false;
                        for (i, p) in params.iter().enumerate()
                        {
                            if i < f.params.len()
                            {
                                types_good = p == &self.infer_type(&f.params[i]);
                            }
                        }

                        if types_good
                        {
                            let ty = self.infer_type(&f.ret);
                            self.types.insert(expr.id, ty.clone());
                            return ty;
                        }
                    }
                }

                let fun_ty = Type::create_func(
                    expr.id,
                    expr.pos,
                    params.iter().map(|t| Box::new(t.clone())).collect(),
                    Box::new(Type::Void(expr.pos)),
                );
                if objty.is_none()
                {
                    for (sig,f) in self.functions.iter() {
                        if f.name == path.name() {
                        
                        for param in sig.params.iter() {
                            print!("{}",param);
                        }
                        }
                        
                    }
                    error!(
                        format!("Function {}{} not found", str(path.name()), fun_ty),
                        expr.pos
                    );
                }
                else
                {
                    
                    error!(
                        format!(
                            "Function ({}) {}{} not found",
                            objty.unwrap(),
                            str(path.name()),
                            fun_ty
                        ),
                        expr.pos
                    );
                }
            }
            ExprKind::Binary(op, e1, e2) =>
            {
                let mut t1 = self.tc_expr(e1);
                let mut t2 = self.tc_expr(e2);
                let op: &str = op;
                t1 = self.infer_type(&t1);
                t2 = self.infer_type(&t2);

                if t1.is_ptr() && ty_is_any_int(&t2) && (op == "+" || op == "-")
                {
                    let ty = *t1.to_ptr().unwrap().subtype.clone();
                    self.types.insert(expr.id, ty.clone());
                    return ty;
                }

                if ty_is_any_int(&t1) && ty_is_any_int(&t2)
                {
                    match op
                    {
                        "<" | ">" | ">=" | "<=" | "!=" | "==" =>
                        {
                            let ty = Type::create_basic(expr.id, expr.pos, intern("bool"));
                            self.types.insert(expr.id, ty.clone());
                            ty
                        }
                        _ =>
                        {
                            self.types.insert(expr.id, t1.clone());
                            t1
                        }
                    }
                }
                else if ty_is_any_float(&t1) && ty_is_any_float(&t2)
                {
                    match op
                    {
                        "<" | ">" | ">=" | "<=" | "!=" | "==" =>
                        {
                            let ty = Type::create_basic(expr.id, expr.pos, intern("bool"));
                            self.types.insert(expr.id, ty.clone());
                            ty
                        }
                        _ =>
                        {
                            self.types.insert(expr.id, t1.clone());
                            t1
                        }
                    }
                }
                else
                {
                    match op
                    {
                        "&&" | "||" =>
                        {
                            let t1 = t1.to_basic();
                            let t2 = t2.to_basic();
                            if t1.is_none() && t2.is_none()
                            {
                                error!("Expected basic type bool", expr.pos);
                            }
                            else
                            {
                                let t1 = t1.unwrap();
                                let t2 = t2.unwrap();

                                if &str(t1.name).to_string() == "bool"
                                    && &str(t2.name).to_string() == "bool"
                                {
                                    let t = Type::create_basic(expr.id, expr.pos, intern("bool"));
                                    self.types.insert(expr.id, t.clone());
                                    t
                                }
                                else
                                {
                                    error!("Expected bool type", expr.pos);
                                }
                            }
                        }
                        _ =>
                        {
                            let name = match op
                            {
                                "+" => "__add__",
                                "-" => "__sub__",
                                "/" => "__div__",
                                "*" => "__mul__",
                                "%" => "__mod__",
                                ">>" => "__shr__",
                                "<<" => "__shl__",
                                ">" => "__gt__",
                                "<" => "__lt__",
                                ">=" => "__gte__",
                                "<=" => "__lte__",
                                "==" => "__eq__",
                                "!=" => "__neq__",
                                "|" => "__bor__",
                                "&" => "__band__",
                                "&&" => "__and__",
                                "||" => "__or__",
                                "^" => "__xor__",
                                _ => unimplemented!(),
                            };

                            for (_, sigs) in self.signatures.iter()
                            {
                                for sig in sigs.iter()
                                {
                                    let id = t1.id();
                                    let pos = t1.pos();
                                    let ptr = Type::create_ptr(id, pos, Box::new(t1.clone()));
                                    if sig.name == intern(name)
                                        && sig.this == Some(Box::new(ptr))
                                        && sig.params == vec![t2.clone()]
                                    {
                                        self.types.insert(expr.id, *sig.ret.clone());

                                        return *sig.ret.clone();
                                    }
                                }
                            }
                            error!(format!("Expected {} found {}", t1, t2), expr.pos);
                        }
                    }
                }
            }

            ExprKind::Assign(to, from) =>
            {
                let mut to = self.tc_expr(to);
                to = self.infer_type(&to);

                let mut from = self.tc_expr(from);
                from = self.infer_type(&from);

                if ty_is_any_int(&to) && ty_is_any_int(&from)
                {
                    return Type::Void(expr.pos);
                }

                if to != from
                {
                    error!(format!("Expected {} type,found {}", to, from), expr.pos);
                }

                Type::Void(expr.pos)
            }

            ExprKind::Conv(e, to) =>
            {
                self.tc_expr(e);
                self.types.insert(expr.id, *to.clone());
                *to.clone()
            }

            ExprKind::Ident(name) =>
            {
                if self.constexprs.contains_key(name)
                {
                    let expr_ = self.constexprs.get(name).unwrap().clone();

                    let ty = self.tc_expr(&expr_);
                    let ty = self.infer_type(&ty);
                    self.types.insert(expr.id, ty.clone());
                    return ty;
                }

                if self.constants.contains_key(name)
                {
                    let ty = self.constants.get(name).unwrap().typ.clone();
                    self.types.insert(expr.id, ty.clone());

                    ty
                }
                else if self.globals.contains_key(name)
                {
                    let expr_ = self.globals.get(name).unwrap().expr.clone();
                    self.vars.push(HashMap::new());
                    if expr_.is_some()
                    {
                        let ty = self.tc_expr(expr_.as_ref().unwrap());
                        let ty = self.infer_type(&ty);
                        self.types.insert(expr_.as_ref().unwrap().id, ty);
                    }
                    self.vars.pop();
                    let ty = *self.globals.get(name).unwrap().typ.clone();
                    let ty = self.infer_type(&ty);
                    self.types.insert(expr.id, ty.clone());
                    ty
                }
                else
                {
                    let ty = self
                        .vars
                        .last()
                        .unwrap()
                        .get(name)
                        .unwrap_or_else(|| {
                            error!(format!("Variable {} not found", str(*name)), expr.pos)
                        })
                        .clone();
                    self.types.insert(expr.id, ty.clone());
                    ty
                }
            }
            ExprKind::Field(expr_, field_name) =>
            {
                let mut ty = self.tc_expr(expr_);
                ty = self.infer_type(&ty);
                if ty.is_struct()
                {
                    let struct_ = ty.to_struct().unwrap();
                    for field in struct_.fields.iter()
                    {
                        let field: &StructField = field;

                        if str(field.name).to_string() == str(field_name.to_owned()).to_string()
                        {
                            let ty = self.infer_type(&field.data_type);
                            self.types.insert(expr.id, ty.clone());
                            return ty;
                        }
                    }
                    error!(format!("Field {} not found", str(*field_name)), expr.pos);
                }
                else
                {
                    error!(format!("Structure type expected,found {}", ty), expr.pos);
                }
            }
            ExprKind::Struct(construct, _) =>
            {
                let name = construct.name();
                let struct_ = self.structures.get(&name).expect("struct not found");
                let ty = self.infer_type(&Type::create_struct(
                    expr.id,
                    expr.pos,
                    name,
                    struct_.fields.clone(),
                ));
                self.types.insert(expr.id, ty.clone());
                ty
            }
            ExprKind::Char(_) =>
            {
                let ty = Type::create_basic(expr.id, expr.pos, intern("char"));
                self.types.insert(expr.id, ty.clone());
                ty
            }
            ExprKind::Null =>
            {
                let ty = Type::create_ptr(
                    expr.id,
                    expr.pos,
                    Box::new(Type::create_basic(expr.id, expr.pos, intern("u8"))),
                );
                self.types.insert(expr.id, ty.clone());
                ty
            }
            ExprKind::Unary(_, expr_) =>
            {
                let t = self.tc_expr(expr_);
                let t = self.infer_type(&t);
                self.types.insert(expr.id, t.clone());
                t
            }
            ExprKind::Str(_) =>
            {
                let ty = Type::create_ptr(
                    expr.id,
                    expr.pos,
                    Box::new(Type::create_basic(expr.id, expr.pos, intern("char"))),
                );
                self.types.insert(expr.id, ty.clone());
                ty
            }
            ExprKind::Bool(_) =>
            {
                let basic = Type::create_basic(expr.id, expr.pos, intern("bool"));
                self.types.insert(expr.id, basic.clone());

                basic
            }
            ExprKind::SizeOf(_) =>
            {
                let basic = Type::create_basic(expr.id, expr.pos, intern("usize"));
                self.types.insert(expr.id, basic.clone());

                basic
            }
            ExprKind::ArrayIdx(array, idx) =>
            {
                let array_type = self.tc_expr(array);
                let index = self.tc_expr(idx);
                let array_type = self.infer_type(&array_type);
                let _ = self.infer_type(&index);

                let result_type = if array_type.is_array()
                {
                    array_type.to_array().unwrap().subtype.clone()
                }
                else if array_type.is_ptr()
                {
                    array_type.to_ptr().unwrap().subtype.clone()
                }
                else
                {
                    error!(
                        format!(
                            "Expected array or pointer,found value with type {}",
                            array_type
                        ),
                        expr.pos
                    );
                };
                let result_type = self.infer_type(&result_type);
                self.types.insert(expr.id, result_type.clone());
                result_type
            }
            ExprKind::Array(_, _) => unimplemented!(),
        }
    }
}
