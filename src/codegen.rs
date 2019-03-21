use gc::{Gc, GcCell};
use hashbrown::{HashMap, HashSet};
use waffle::{gc, instructions::Instruction, value::*, vm::VirtualMachine};

use crate::{ast::*, stdlib};

pub struct Compiler<'a> {
    pub globals: HashMap<String, usize>,
    pub vm: &'a mut VirtualMachine,
    pub functions: HashSet<usize>,
    pub module: String,
}

impl<'a> Compiler<'a> {
    pub fn new(vm: &'a mut VirtualMachine, module: String) -> Compiler<'a> {
        Compiler {
            globals: HashMap::new(),
            vm,
            functions: HashSet::new(),
            module,
        }
    }

    fn get_definitions(&mut self) -> HashMap<String, usize> {
        self.globals.clone()
    }

    fn get_functions(&mut self) -> HashSet<usize> {
        self.functions.clone()
    }

    pub fn import(&mut self, fname: &str) {
        use crate::parser::Parser;
        use crate::reader::Reader;
        let reader = Reader::from_file(fname).unwrap();
        let mut ast = vec![];
        use std::path::Path;
        let mname = Path::new(fname)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();

        let mut parser = Parser::new(reader, &mut ast);
        parser.parse().unwrap();
        let mut compiler = Compiler::new(self.vm, mname.clone());
        compiler.compile_ast(ast);
        let defs = compiler.get_definitions();
        let fns = compiler.get_functions();
        let mut object = Object::new(&mname, None, mname.clone());

        for (key, value) in defs.iter() {
            if fns.contains(value) {
                object.insert(Value::String(key.to_owned()), gc!(Value::Function(*value)));
            } else {
                object.insert(Value::String(key.to_owned()), gc!(Value::Object(*value)));
            }
        }

        let idx = self.vm.pool.new_object(object);
        self.globals.insert(mname, idx);
        self.vm.globals.insert(idx, gc!(Value::Object(idx)));
    }

    fn register_stdlib(&mut self) {
        stdlib::system_class(self);
        let f = Function {
            typ: FunctionType::Internal(stdlib::string),
            name: "string".to_owned(),
            module_name: "stdlib".to_owned(),
            export: true,
        };
        let idx = self.vm.pool.new_func(f);
        self.globals.insert("string".into(), idx);
        self.vm
            .globals
            .insert(idx, gc!(waffle::value::Value::Function(idx)));

        let f = Function {
            typ: FunctionType::Internal(stdlib::anew),
            name: "anew".to_owned(),
            module_name: "stdlib".to_owned(),
            export: true,
        };
        let idx = self.vm.pool.new_func(f);
        self.globals.insert("anew".into(), idx);
        self.vm
            .globals
            .insert(idx, gc!(waffle::value::Value::Function(idx)));

        let f = Function {
            typ: FunctionType::Internal(stdlib::print),
            name: "print".to_owned(),
            module_name: "stdlib".to_owned(),
            export: true,
        };
        let idx = self.vm.pool.new_func(f);
        self.globals.insert("print".into(), idx);
        self.vm
            .globals
            .insert(idx, gc!(waffle::value::Value::Function(idx)));
    }

    pub fn compile_ast(&mut self, ast: Vec<Box<Expr>>) {
        self.register_stdlib();
        for expr in ast.iter() {
            match &expr.expr {
                ExprKind::Import(fname) => self.import(fname),
                ExprKind::Include(fname) => {
                    use crate::parser::Parser;
                    use crate::reader::Reader;
                    let reader = Reader::from_file(fname).unwrap();
                    let mut ast = vec![];

                    let mut parser = Parser::new(reader, &mut ast);

                    parser.parse().unwrap();
                    use std::path::Path;
                    let mname = Path::new(fname)
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_owned();

                    let mut compiler = Compiler::new(self.vm, mname);
                    compiler.compile_ast(ast);

                    let defs = compiler.get_definitions();
                    let fns = compiler.get_functions();

                    self.globals.extend(defs);
                    self.functions.extend(fns);
                }
                _ => (),
            }
        }
        for expr in ast.iter() {
            match &expr.expr {
                ExprKind::Import(_fname) => (),
                ExprKind::Include(_fname) => (),
                ExprKind::Function(name, args, body) => {
                    let function = Function {
                        name: name.to_owned(),
                        typ: FunctionType::Native(vec![]),
                        module_name: self.module.clone(),
                        export: true,
                    };
                    let idx = self.vm.pool.new_func(function);
                    self.globals.insert(name.to_owned(), idx);
                    use gc::{Gc, GcCell};
                    self.vm.globals.insert(
                        idx,
                        Gc::new(GcCell::new(waffle::value::Value::Function(idx))),
                    );
                    let mut fbuilder = FunctionBuilder::new(self, args.len());
                    for param in args.iter() {
                        let reg = fbuilder.register_push_temp();
                        fbuilder.new_local(param.to_owned(), reg);
                    }
                    fbuilder.expr(body);

                    let code = fbuilder.finish();
                    if cfg!(debug_assertions) {
                        println!("Disassemble of `{}` function", name);
                        for (idx, op) in code.iter().enumerate() {
                            println!("{:04} {:?}", idx, op);
                        }
                    }

                    let func = self.vm.pool.get_func(idx);
                    func.borrow_mut().typ = FunctionType::Native(code);
                    self.functions.insert(idx);
                }

                ExprKind::Class(name, block, _implements) => {
                    let object = Object::new(name, None, self.module.clone());
                    let oidx = if !self.globals.contains_key(name) {
                        let oidx = self.vm.pool.new_object(object);
                        self.globals.insert(name.to_owned(), oidx);
                        self.vm.globals.insert(oidx, gc!(Value::Object(oidx)));
                        oidx
                    } else {
                        let idx = *self.globals.get(name).unwrap();
                        let obj = self.vm.pool.get_object(idx);
                        *obj.borrow_mut() = object;
                        idx
                    };

                    if let ExprKind::Block(exprs) = &block.expr {
                        for expr in exprs.iter() {
                            match &expr.expr {
                                ExprKind::Function(fname, args, block) => {
                                    let function = Function {
                                        name: fname.to_owned(),
                                        typ: FunctionType::Native(vec![]),
                                        module_name: "main".to_owned(),
                                        export: true,
                                    };
                                    let id = self.vm.pool.new_func(function);
                                    let object = self.vm.pool.get_object(oidx);
                                    ((**object).borrow_mut()).insert(
                                        Value::String(fname.to_owned()),
                                        gc!(Value::Function(id)),
                                    );
                                    let mut fbuilder = FunctionBuilder::new(self, args.len());
                                    for param in args.iter() {
                                        let reg = fbuilder.register_push_temp();
                                        fbuilder.new_local(param.to_owned(), reg);
                                    }
                                    fbuilder.expr(block);

                                    let code = fbuilder.finish();
                                    if cfg!(debug_assertions) {
                                        println!("Disassemble of `{}::{}` function", name, fname);
                                        for (idx, op) in code.iter().enumerate() {
                                            println!("{:04} {:?}", idx, op);
                                        }
                                    }

                                    let func = self.vm.pool.get_func(id);
                                    func.borrow_mut().typ = FunctionType::Native(code);
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                }
                _ => unimplemented!(),
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum UOP {
    Goto(String),
    GotoF(usize, String),
    GotoT(usize, String),
    Op(Instruction),
}

pub const MAX_REGISTERS: usize = 256;

pub struct FunctionBuilder<'a, 'b: 'a> {
    compiler: &'a mut Compiler<'b>,
    locals: HashMap<String, usize>,
    /// This field is required for `break` in `while` (Check `check_labels` documentation)
    ///
    end_labels: Vec<String>,
    /// This field is required for `continue` in `while`
    /// # Example
    /// ```
    /// while i != 100 { // push to check_labels location of this check
    ///     while i < 50 { // push to check_labels location of this check
    ///         i = i + 2        
    ///         continue // use last label from check_labels
    ///     } // pop check_labels
    ///     i = i + 1
    /// }
    /// ```
    check_labels: Vec<String>,
    ins: Vec<UOP>,
    labels: HashMap<String, Option<usize>>,
    pub state: [bool; MAX_REGISTERS],
    pub maxtemps: usize,
    pub ntemps: usize,
    pub nlocals: usize,
    pub skipclear: [bool; MAX_REGISTERS],
    pub registers: Vec<usize>,
    pub context: Vec<Vec<bool>>,
}

impl<'a, 'b: 'a> FunctionBuilder<'a, 'b> {
    pub fn new(cmpl: &'a mut Compiler<'b>, nlocals: usize) -> Self {
        Self {
            compiler: cmpl,
            locals: HashMap::new(),
            end_labels: vec![],
            check_labels: vec![],
            labels: HashMap::new(),
            ins: vec![],
            state: [false; MAX_REGISTERS],
            maxtemps: 0,
            ntemps: 0,
            nlocals,
            registers: Vec::with_capacity(MAX_REGISTERS),
            context: Vec::with_capacity(MAX_REGISTERS),
            skipclear: [false; MAX_REGISTERS],
        }
    }

    pub fn new_local(&mut self, n: String, reg: usize) {
        self.state[reg] = true;
        self.nlocals += 1;
        self.locals.insert(n, reg);
    }

    pub fn get_local(&mut self, n: &str) -> usize {
        if self.locals.contains_key(n) {
            *self.locals.get(n).expect("Unknown local")
        } else {
            panic!("Local `{}` doesn't exists", n);
        }
    }

    pub fn new_empty_label(&mut self) -> String {
        let lab_name = self.labels.len().to_string();
        self.labels.insert(lab_name.clone(), None);
        lab_name
    }

    pub fn label_here(&mut self, label: &str) {
        *self.labels.get_mut(label).unwrap() = Some(self.ins.len());
    }

    pub fn emit(&mut self, ins: Instruction) {
        self.ins.push(UOP::Op(ins));
    }

    pub fn emit_goto(&mut self, to: &str) {
        self.ins.push(UOP::Goto(to.to_owned()));
    }
    pub fn emit_gotof(&mut self, to: &str, r: usize) {
        self.ins.push(UOP::GotoF(r, to.to_owned()));
    }

    pub fn register_new(&mut self) -> usize {
        for i in 0..MAX_REGISTERS {
            if !self.state[i] {
                self.state[i] = true;
                return i;
            }
        }
        println!("No registers available");
        0
    }

    pub fn register_push(&mut self, nreg: usize) -> usize {
        self.registers.push(nreg);
        if self.register_is_temp(nreg) {
            self.ntemps += 1;
        }
        nreg
    }

    pub fn register_first_temp_available(&mut self) -> usize {
        for i in 0..MAX_REGISTERS {
            if !self.state[i] {
                return i;
            }
        }
        0
    }

    pub fn register_push_temp(&mut self) -> usize {
        let value = self.register_new();
        self.registers.push(value);
        if value > self.maxtemps {
            self.maxtemps = value;
            self.ntemps += 1;
        }

        value
    }

    pub fn register_pop(&mut self) -> usize {
        self.register_pop_context_protect(false)
    }

    pub fn register_clear(&mut self, nreg: usize) {
        if nreg >= self.nlocals {
            self.state[nreg] = false;
        }
    }
    pub fn register_is_temp(&self, nreg: usize) -> bool {
        nreg >= self.nlocals
    }

    pub fn register_pop_context_protect(&mut self, protect: bool) -> usize {
        if self.registers.is_empty() {
            panic!("REGISTER ERROR");
        }

        let value = self.registers.pop().unwrap_or_default();

        if protect {
            self.state[value] = true;
        } else if value > self.nlocals {
            self.state[value] = false;
        }

        /*if protect && value >= self.nlocals {
            let ctx = self.context.last_mut().unwrap();
            ctx[value] = true;
        }*/

        value
    }

    fn finish(&mut self) -> Vec<Instruction> {
        self.ins
            .iter()
            .map(|i| match *i {
                UOP::Op(ref op) => op.clone(),
                UOP::Goto(ref lbl) => Instruction::Jump(self.labels.get(lbl).unwrap().unwrap() - 1),
                UOP::GotoF(ref reg, ref lbl) => {
                    Instruction::JumpF(*reg, self.labels.get(lbl).unwrap().unwrap() - 1)
                }
                UOP::GotoT(ref reg, ref lbl) => {
                    Instruction::JumpT(*reg, self.labels.get(lbl).unwrap().unwrap() - 1)
                }
            })
            .collect::<Vec<Instruction>>()
    }

    pub fn expr(&mut self, e: &Expr) {
        match &e.expr {
            ExprKind::ConstInt(i) => {
                let r0 = self.register_push_temp();
                self.emit(Instruction::LoadInt(r0, *i));
            }
            ExprKind::ConstFloat(f) => {
                let r0 = self.register_push_temp();
                self.emit(Instruction::LoadFloat(r0, *f));
            }
            ExprKind::ConstStr(s) => {
                let r0 = self.register_push_temp();
                self.emit(Instruction::LoadString(r0, s.to_owned()));
            }
            ExprKind::ConstBool(b) => {
                let r0 = self.register_push_temp();
                if *b {
                    self.emit(Instruction::LoadTrue(r0));
                } else {
                    self.emit(Instruction::LoadFalse(r0));
                }
            }

            ExprKind::Access(obj, field) => {
                self.expr(obj);
                let ra = self.register_pop_context_protect(true);
                self.emit(Instruction::LoadString(255, field.to_owned()));
                let temp = self.register_push_temp();

                self.emit(Instruction::LoadAt(ra, 255, temp));
                self.register_clear(ra);
            }

            ExprKind::Return(expr) => {
                if expr.is_some() {
                    self.expr(&expr.clone().unwrap());
                    let r = self.register_pop();
                    self.emit(Instruction::Ret(r));
                } else {
                    self.emit(Instruction::Ret0);
                }
            }
            ExprKind::While(cond, expr_do) => {
                let compare = self.new_empty_label();
                let end = self.new_empty_label();
                self.label_here(&compare);
                self.expr(cond);
                let r = self.register_pop();
                self.emit_gotof(&end, r);
                self.expr(expr_do);
                self.emit_goto(&compare);
                self.label_here(&end);
            }

            ExprKind::If(cond, then, or) => {
                let lbl_false = self.new_empty_label();
                self.expr(cond);
                let reg = self.register_pop();
                self.emit_gotof(&lbl_false, reg);
                self.expr(then);
                self.label_here(&lbl_false);
                if or.is_some() {
                    self.expr(&or.clone().unwrap());
                }
            }

            ExprKind::BinOp(lhs, op, rhs) => {
                self.expr(lhs);
                self.expr(rhs);
                let r3 = self.register_pop();
                let r2 = self.register_pop();
                self.register_clear(r3);
                self.register_clear(r2);
                let r1 = self.register_push_temp();
                let op: &str = op;

                let ins = match op {
                    "+" => Instruction::Add(r1, r2, r3),
                    "-" => Instruction::Sub(r1, r2, r3),
                    "*" => Instruction::Mul(r1, r2, r3),
                    "/" => Instruction::Div(r1, r2, r3),
                    ">" => Instruction::Gt(r1, r2, r3),
                    ">=" => Instruction::Gte(r1, r2, r3),
                    "<" => Instruction::Lt(r1, r2, r3),
                    "<=" => Instruction::Lte(r1, r2, r3),
                    "==" => Instruction::Eq(r1, r2, r3),
                    "!=" => Instruction::Neq(r1, r2, r3),
                    _ => unimplemented!(),
                };

                self.emit(ins);
            }
            ExprKind::Ident(name) => {
                if self.compiler.globals.contains_key(name) {
                    let idx = *self.compiler.globals.get(name).unwrap();
                    let r0 = self.register_push_temp();
                    self.emit(Instruction::LoadGlobal(r0, idx));
                } else {
                    if name == "_args" {
                        let r = self.register_push_temp();
                        self.emit(Instruction::LoadArgs(r));
                        return;
                    }
                    let reg = *self
                        .locals
                        .get(name)
                        .unwrap_or_else(|| panic!("Local `{}` not found", name));
                    let r0 = self.register_push_temp();
                    self.emit(Instruction::Move(reg, r0));
                }
            }
            ExprKind::Var(_, name, init) => {
                if init.is_some() {
                    self.expr(&init.clone().unwrap());
                    let r0 = self.register_pop();
                    self.new_local(name.to_owned(), r0);
                } else {
                    let r0 = self.register_new();
                    self.new_local(name.to_owned(), r0);
                }
            }
            ExprKind::This => {
                let r = self.register_push_temp();
                self.emit(Instruction::LoadThis(r));
            }
            ExprKind::Assign(var, to) => {
                self.expr(to);
                let r2 = self.register_pop_context_protect(true);
                match &var.expr {
                    ExprKind::Ident(name) => {
                        if self.locals.contains_key(name) {
                            let r0 = self.get_local(name);
                            // self.expr(to);
                            //let r1 = self.register_pop();
                            if r0 != r2 {
                                self.emit(Instruction::Move(r2, r0));
                            }
                        } else {
                            self.expr(to);
                            let r = self.register_pop();
                            let gidx = self.compiler.globals.get(name).unwrap();
                            self.emit(Instruction::SetGlobal(r, *gidx));
                        }
                    }
                    ExprKind::Access(obj, field) => {
                        let r1 = self.register_push_temp();
                        self.emit(Instruction::LoadString(r1, field.to_owned()));
                        //self.expr(to);
                        //let r2 = self.register_pop();
                        self.expr(obj);
                        let r = self.register_pop();
                        self.emit(Instruction::StoreAt(r, r1, r2));
                        //self.register_clear(r);
                    }
                    _ => unimplemented!(),
                }
            }
            ExprKind::Block(exprs) => {
                for expr in exprs.iter() {
                    self.expr(expr);
                }
            }
            ExprKind::Call(callee, args) => {
                for arg in args.iter().rev() {
                    self.expr(arg);
                    let r = self.register_pop();
                    self.register_clear(r);
                    self.emit(Instruction::Push(r));
                }
                if let ExprKind::Access(obj, field) = &callee.expr {
                    self.expr(obj);

                    let ra = self.register_pop_context_protect(true);
                    self.emit(Instruction::Push(ra));
                    let rb = self.register_push_temp();
                    self.emit(Instruction::LoadString(rb, field.to_owned()));
                    let temp = self.register_push_temp();
                    self.emit(Instruction::LoadAt(ra, rb, temp));
                    self.register_clear(ra);
                    let callee = self.register_pop();
                    let dest = self.register_push_temp();

                    self.emit(Instruction::Call(dest, callee, args.len()));
                } else {
                    self.expr(callee);
                    let r = self.register_pop();
                    self.emit(Instruction::Push(r));
                    let dest = self.register_push_temp();
                    self.emit(Instruction::Call(dest, r, args.len()));
                }
            }
            ExprKind::New(init_class) => {
                if let ExprKind::Call(to_call, args) = &init_class.expr {
                    for arg in args.iter() {
                        self.expr(arg);
                        let r = self.register_pop();
                        self.register_clear(r);
                        self.state[r] = false;
                        self.emit(Instruction::Push(r));
                    }
                    self.expr(to_call);
                    let r = self.register_pop();
                    let dest = self.register_push_temp();
                    self.emit(Instruction::New(dest, r, args.len()));
                } else {
                    panic!("Call expected");
                }
            }

            v => panic!("{:?}", v),
        }
    }
}
