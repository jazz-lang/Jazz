use jazzvm::hash::hash_bytes;
use jazzvm::opcode::Opcode;
pub fn stack_delta(op: Opcode) -> i32 {
    use Opcode::*;
    match op {
        AccNull | AccTrue | AccFalse | AccThis | AccInt(_) | AccStack(_) | AccGlobal(_)
        | AccEnv(_) | AccField(_) | AccBuiltin(_) | AccIndex(_) | JumpIf(_) | JumpIfNot(_)
        | Jump(_) | Ret(_) | SetGlobal(_) | SetStack(_) | SetEnv(_) | SetThis | Bool | IsNull
        | IsNotNull | Not | Hash | TypeOf | New | AccStack0 | AccStack1 => 0,
        Add | Sub | Mul | Div | Rem | Shl | Shr | UShr | Or | And | Xor | Eq | Neq | Gt | Gte
        | Lt | Lte | PhysCompare => -1,
        AccArray => -1,
        SetField(_) | SetIndex(_) => -1,
        SetArray => -2,
        Push => 1,
        Pop(x) => x as i32,
        Apply(nargs) | Call(nargs) => nargs as i32,
        MakeEnv(size) | MakeArray(size) => size as i32,
        ObjCall(nargs) => -(nargs as i32 + 1),
        Opcode::Last => 0,
        _ => unimplemented!(),
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Global {
    Var(String),
    Func(i32, i32),
    Str(String),
    Float(String),
}
#[derive(Clone, Debug, PartialEq)]
pub enum Access {
    Env(i32),
    Stack(i32),
    Global(i32),
    Field(String),
    Index(i32),
    Array,
    This,
}

use std::collections::HashMap;

pub struct Globals {
    pub globals: HashMap<Global, i32>,
    pub objects: HashMap<String, Vec<i32>>,
    pub functions: Vec<(Vec<Opcode>, Vec<(i32, i32)>, i32, i32)>,
    pub table: Vec<Global>,
}

pub struct Context {
    pub g: Globals,
    pub ops: Vec<Opcode>,
    pub locals: HashMap<String, i32>,
    pub env: HashMap<String, i32>,
    pub stack: i32,
    pub limit: i32,
    pub nenv: i32,
    pub breaks: Vec<i32>,
    pub continues: Vec<i32>,
    pub pos: Vec<(i32, i32)>,
    pub cur_pos: (i32, i32),
    pub cur_file: String,
    pub builtins: HashMap<String, i32>,
}

use crate::ast::*;
use crate::token::Position;
use crate::P;

impl Context {
    pub fn check_stack(&self, stack: i32, p: &str) {
        if self.stack != stack {
            panic!("Stack alignment failure {}", p);
        }
    }
    pub fn pos(&self) -> usize {
        self.ops.len()
    }

    pub fn write(&mut self, op: Opcode) {
        self.stack = self.stack + stack_delta(op.clone());
        self.pos.push(self.cur_pos);
        self.ops.push(op);
    }

    pub fn jmp<'a>(&mut self) -> impl FnOnce(&mut Context) {
        let p = self.pos();
        self.write(Opcode::Jump(0));
        return move |ctx: &mut Context| {
            let p2 = ctx.pos();
            ctx.ops[p] = Opcode::Jump(p2 as u32 - p as u32);
        };
    }
    pub fn cjmp(&mut self, cond: bool) -> impl FnOnce(&mut Context) {
        let p = self.pos();
        self.write(Opcode::Jump(0));
        return move |ctx: &mut Context| {
            let p2 = ctx.pos() as u32;
            ctx.ops[p] = if cond {
                Opcode::JumpIf(p2 - p as u32)
            } else {
                Opcode::JumpIfNot(p2 - p as u32)
            };
        };
    }

    pub fn goto(&mut self, p: u32) {
        self.write(Opcode::Jump(p));
    }
    pub fn global(&mut self, g: &Global) -> i32 {
        return match self.g.globals.get(g).cloned() {
            Some(g) => g.clone(),
            None => {
                let gid = self.g.table.len() as i32;
                self.g.globals.insert(g.clone(), gid);
                self.g.table.push(g.clone());
                gid
            }
        };
    }

    pub fn scan_labels(&mut self, supported: bool, in_block: bool, expr: &P<Expr>) {
        match &expr.decl {
            ExprDecl::Function(args, body) => {
                let nargs = args.len();
                self.stack += nargs as i32;
                self.scan_labels(supported, false, body);
                self.stack -= nargs as i32;
            }
            ExprDecl::Block(exprs) => {
                let old = self.stack;
                for expr in exprs.iter() {
                    self.scan_labels(supported, true, expr);
                }
                self.stack = old;
            }
            ExprDecl::Var(_, _, init) => {
                if !in_block {
                    panic!("Variable declaration must be done in block")
                };
                match init {
                    Some(e) => self.scan_labels(supported, false, e),
                    _ => (),
                };
                self.stack += 1;
            }
            ExprDecl::Label(l) => {
                if !supported {
                    panic!("Label not supported in this part")
                }
            }
            ExprDecl::Assign(e1, e2) => {
                fn is_extended(e: &ExprDecl) -> bool {
                    match e {
                        ExprDecl::Paren(p) => is_extended(&p.decl),
                        ExprDecl::Array(_, _) | ExprDecl::Field(_, _) => true,
                        _ => false,
                    }
                }
                let ext = is_extended(&e1.decl);
                if ext {
                    self.stack += 1;
                }
                self.scan_labels(supported, false, e2);
                self.stack += 1;
                self.scan_labels(supported, false, e1);
                self.stack -= if ext { 2 } else { 1 };
            }
            ExprDecl::Call(e, el) => {
                for ex in el.iter() {
                    self.scan_labels(supported, false, ex);
                }
                self.scan_labels(supported, false, e);
                self.stack -= el.len() as i32;
            }
            ExprDecl::Binop(_, _, _) | ExprDecl::Field(_, _) | ExprDecl::Array(_, _) => {
                expr.iter(move |x| {
                    self.scan_labels(false, false, x);
                });
            }
            _ => {
                expr.iter(move |x| {
                    self.scan_labels(supported, false, x);
                });
            }
        }
    }

    pub fn compile_binop(&mut self, op: &str, e1: &P<Expr>, e2: &P<Expr>) {
        match op {
            "&&" => {
                self.compile(e1);
                let jnext = self.cjmp(false);
                self.compile(e2);
                jnext(self);
            }
            "||" => {
                self.compile(e1);
                let jnext = self.cjmp(true);
                self.compile(e2);
                jnext(self);
            }
            _ => {
                self.compile(e1);
                self.write(Opcode::Push);
                self.compile(e2);
                self.write_op(op);
            }
        }
    }
    pub fn compile_const(&mut self, c: &Constant, p: Position) {
        match c {
            Constant::True => self.write(Opcode::AccTrue),
            Constant::False => self.write(Opcode::AccFalse),
            Constant::Null => self.write(Opcode::AccNull),
            Constant::This => self.write(Opcode::AccThis),
            Constant::Int(n) => self.write(Opcode::AccInt(n.clone())),
            Constant::Float(f) => self.write(Opcode::AccFloat(f.clone())),
            Constant::Str(s) => self.write(Opcode::AccStr(s.clone())),
            Constant::Ident(s) => {
                let s: &str = s;
                let l = self.locals.get(s);
                if l.is_some() {
                    let l = *l.unwrap();
                    if l < self.limit {
                        let e = self.env.get(s);
                        let e = if e.is_none() {
                            let e = self.nenv;
                            self.nenv += 1;
                            self.env.insert(s.to_owned(), e);
                            e
                        } else {
                            *e.unwrap()
                        };
                        self.write(Opcode::AccStack(e as u32));
                    } else {
                        let p = self.stack - 1;
                        if p == 0 {
                            self.write(Opcode::AccStack0);
                        } else if p == 1 {
                            self.write(Opcode::AccStack1);
                        } else if p == 2 {
                            self.write(Opcode::AccStack2);
                        } else {
                            self.write(Opcode::AccStack(p as u32));
                        }
                    }
                } else {
                    let g = self.global(&Global::Var(s.to_owned()));
                    self.write(Opcode::AccGlobal(g as u32));
                }
            }
            _ => unimplemented!(),
        }
    }
    pub fn write_op(&mut self, op: &str) {
        use Opcode::*;
        match op {
            "+" => self.write(Add),
            "-" => self.write(Sub),
            "/" => self.write(Div),
            "*" => self.write(Mul),
            "%" => self.write(Rem),
            "<<" => self.write(Shl),
            ">>" => self.write(Shr),
            "|" => self.write(Or),
            "&" => self.write(And),
            "^" => self.write(Xor),
            "==" => self.write(Eq),
            "!=" => self.write(Neq),
            ">" => self.write(Gt),
            ">=" => self.write(Gte),
            "<" => self.write(Lt),
            "<=" => self.write(Lte),
            _ => panic!("Unknown operation {}", op),
        }
    }

    pub fn compile_access(&mut self, e: &P<Expr>) -> Access {
        match &e.decl {
            ExprDecl::Const(Constant::Ident(name)) => {
                let l = self.locals.get(name);
                let s: &str = name;
                if l.is_some() {
                    let l = *l.unwrap();
                    let e = self.env.get(s);
                    let e = if e.is_none() {
                        let e = self.nenv;
                        self.nenv += 1;
                        self.env.insert(s.to_owned(), e);
                        e
                    } else {
                        *e.unwrap()
                    };
                    return Access::Env(e);
                } else {
                    let g = self.global(&Global::Var(s.to_owned()));
                    return Access::Global(g);
                }
            }
            ExprDecl::Field(e, f) => {
                self.compile(e);
                self.write(Opcode::Push);
                return Access::Field(f.to_owned());
            }
            ExprDecl::Const(Constant::This) => return Access::This,
            _ => unimplemented!(),
        }
    }

    pub fn access_set(&mut self, acc: Access) {
        match acc {
            Access::Env(n) => self.write(Opcode::SetEnv(n as u32)),
            Access::Stack(l) => self.write(Opcode::SetStack(self.stack as u32 - l as u32)),
            Access::Global(g) => self.write(Opcode::SetGlobal(g as u32)),
            Access::Field(f) => {
                let mut h = 0xcbf29ce484222325;
                hash_bytes(&mut h, f.as_bytes());
                self.write(Opcode::SetField(h));
            }
            Access::Index(i) => self.write(Opcode::SetIndex(i as u32)),
            Access::This => self.write(Opcode::SetThis),
            Access::Array => self.write(Opcode::SetArray),
        }
    }

    pub fn compile(&mut self, e: &P<Expr>) {
        match &e.decl {
            ExprDecl::Const(c) => self.compile_const(c, e.pos.clone()),
            ExprDecl::Block(v) => {
                if v.len() == 0 {
                    self.write(Opcode::AccNull);
                } else {
                    let locals = self.locals.clone();
                    let stack = self.stack;
                    for el in v.iter() {
                        self.compile(el);
                    }
                    if stack < self.stack {
                        self.write(Opcode::Pop((self.stack - stack) as u16)); // clear stack from values and locals
                    }
                    self.locals = locals;
                }
            }
            ExprDecl::Paren(e) => self.compile(e),
            ExprDecl::Field(e, f) => {
                self.compile(e);
                let mut h = 0xcbf29ce484222325;
                hash_bytes(&mut h, f.as_bytes());
                self.write(Opcode::AccField(h));
            }
            ExprDecl::Var(_, name, init) => {
                match init {
                    Some(e) => self.compile(e),
                    None => self.write(Opcode::AccNull),
                }

                self.write(Opcode::Push);
                self.locals.insert(name.to_owned(), self.stack);
            }
            ExprDecl::Assign(e1, e2) => {
                let a = self.compile_access(e1);
                self.compile(e2);
                self.access_set(a);
            }
            ExprDecl::Binop(op, e1, e2) => {
                self.compile_binop(op, e1, e2);
            }
            ExprDecl::Return(e) => {
                match e {
                    Some(e) => self.compile(e),
                    None => self.write(Opcode::AccNull),
                }

                let stack = self.stack;
                self.write(Opcode::Ret((self.stack - self.limit) as u16));
                self.stack = stack;
            }
            _ => unimplemented!(),
        }
    }
}

pub fn compile_ast(ast: Vec<P<Expr>>) -> Context {
    let g = Globals {
        globals: HashMap::new(),
        objects: HashMap::new(),
        functions: vec![],
        table: vec![],
    };
    let mut ctx = Context {
        g: g,
        stack: 0,
        limit: -1,
        locals: HashMap::new(),
        ops: vec![],
        env: HashMap::new(),
        nenv: 0,
        pos: Vec::new(),
        cur_pos: (0, 0),
        builtins: HashMap::new(),
        breaks: vec![],
        continues: vec![],
        cur_file: String::from("_"),
    };
    use crate::P;
    let ast = P(Expr {
        pos: Position::new(0, 0),
        decl: ExprDecl::Block(ast.clone()),
    });
    ctx.scan_labels(true, true, &ast);
    ctx.compile(&ast);

    //ctx.write(Opcode::Last);
    ctx
}
