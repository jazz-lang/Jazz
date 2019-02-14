use fxhash::FxHashMap;
use fxhash::FxHashSet;
use waffle::opcodes::Opcode;
use waffle::value::{FuncKind, Function};
use waffle::VirtualMachine;

#[derive(Clone, Debug)]
pub enum UOP
{
    Op(Opcode),
    Goto(String),
}

use crate::ast::*;

pub struct Compiler<'a>
{
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

    pub ins: Vec<UOP>,
    pub vm: &'a mut VirtualMachine,
    pub locals: FxHashSet<String>,
    pub labels: FxHashMap<String, Option<usize>>,
    pub fdefs: FxHashMap<&'static str, usize>,
}

impl<'a> Compiler<'a>
{
    pub fn new(vm: &'a mut VirtualMachine, builtins: FxHashMap<&'static str, usize>)
               -> Compiler<'a>
    {
        Self { end_labels: vec![],
               check_labels: vec![],
               locals: FxHashSet::default(),
               fdefs: builtins,
               ins: vec![],
               vm,
               labels: FxHashMap::default() }
    }

    pub fn compile(&mut self, exprs: Vec<Box<Expr>>, args: Vec<String>)
    {
        for (id, arg) in args.iter().enumerate()
        {
            self.locals.insert(arg.clone());
        }

        for expr in exprs.iter()
        {
            self.expr(expr, false);
        }
    }

    fn new_empty_label(&mut self) -> String
    {
        let lab_name = self.labels.len().to_string();
        self.labels.insert(lab_name.clone(), None);
        lab_name
    }

    pub fn label_here(&mut self, label: &str)
    {
        *self.labels.get_mut(label).unwrap() = Some(self.ins.len());
    }
    pub fn emit(&mut self, op: Opcode)
    {
        self.ins.push(UOP::Op(op));
    }
    pub fn emit_goto(&mut self, lbl: &str)
    {
        self.ins.push(UOP::Goto(lbl.to_owned()));
    }

    pub fn finish(&mut self) -> Vec<Opcode>
    {
        let opcodes =
            self.ins
                .iter()
                .map(|e| match e
                {
                    &UOP::Goto(ref lbl) => Opcode::Jump(self.labels.get(lbl).unwrap().unwrap()),
                    &UOP::Op(ref op) => op.clone(),
                })
                .collect::<Vec<Opcode>>();
        if cfg!(debug_assertions)
        {
            println!("begin");
            for (idx, op) in opcodes.iter().enumerate()
            {
                println!("{:04} {:?}", idx, op);
            }
            println!("end");
        }
        opcodes
    }

    fn expr(&mut self, e: &Box<Expr>, is_fcall: bool)
    {
        match &e.expr
        {
            ExprKind::Assign(to, val) =>
            {
                self.expr(val, false);
                if let ExprKind::Ident(ref name) = to.clone().expr
                {
                    self.emit(Opcode::StoreLocal(name.to_owned()));
                }
                if let ExprKind::Access(obj, field) = to.clone().expr
                {
                    self.expr(val, false);
                    self.emit(Opcode::ConstStr(field.to_owned()));
                    self.expr(&obj, false);
                    self.emit(Opcode::StoreField);
                }
                if let ExprKind::ArrayIndex(arr, idx) = &to.expr
                {
                    self.expr(val, false);
                    self.expr(idx, false);
                    self.expr(arr, false);
                    self.emit(Opcode::StoreField);
                }
            }
            ExprKind::ConstInt(i) => self.emit(Opcode::ConstInt(*i)),
            ExprKind::ConstFloat(f) => self.emit(Opcode::ConstFloat(*f)),
            ExprKind::ConstChar(c) => self.emit(Opcode::ConstStr(c.to_string())),
            ExprKind::ConstStr(s) => self.emit(Opcode::ConstStr(s.to_owned())),
            ExprKind::ConstBool(b) =>
            {
                if *b
                {
                    self.emit(Opcode::ConstTrue)
                }
                else
                {
                    self.emit(Opcode::ConstFalse)
                }
            }
            ExprKind::Ident(name) =>
            {
                let name: &str = name;
                if self.fdefs.contains_key(name)
                {
                    let idx = self.fdefs.get(name).unwrap();
                    self.emit(Opcode::ConstFuncRef(*idx));
                    return;
                }

                self.emit(Opcode::PushLocal(name.to_owned()));
            }
            ExprKind::Var(_, name, init) =>
            {
                if init.is_some()
                {
                    let init = init.clone().unwrap();
                    if let ExprKind::Lambda(args, expr) = init.expr
                    {
                        let func = Function { nargs: args.len() as i32,
                                              args: args.clone(),
                                              is_native: false,
                                              addr: FuncKind::Interpret(vec![]) };
                        let id = self.vm.pool.add_func(func);
                        let s: &str = &name.clone();
                        self.fdefs.insert(unsafe { std::mem::transmute(s) }, id);
                        let locals = self.locals.clone();
                        let builtins = self.fdefs.clone();
                        let mut cmpl = Compiler::new(self.vm, builtins);
                        cmpl.locals = locals;
                        cmpl.compile(vec![expr.clone()], args.clone());
                        let ins = cmpl.finish();
                        if cfg!(debug_assertions)
                        {
                            println!("Lambda code with arguments {:?}", args);
                            for (id, op) in ins.iter().enumerate()
                            {
                                println!("{}: {:?}", id, op);
                            }
                            println!("end");
                        }
                        let func = self.vm.pool.get_func_mut(id);
                        func.addr = FuncKind::Interpret(ins);

                        self.emit(Opcode::ConstFuncRef(id));
                    }
                    else
                    {
                        self.expr(&init, false);
                    }
                }
                else
                {
                    self.emit(Opcode::ConstNull);
                }
                self.emit(Opcode::StoreLocal(name.to_owned()));
                self.locals.insert(name.to_owned());
            }
            ExprKind::Return(e) =>
            {
                if e.is_some()
                {
                    self.expr(&e.clone().unwrap(), false);
                }
                self.emit(Opcode::Ret);
            }
            ExprKind::Access(base, field) =>
            {
                if is_fcall
                {
                    self.expr(base, false);
                }
                self.emit(Opcode::ConstStr(field.to_owned()));
                self.expr(base, false);

                self.emit(Opcode::PushField);
            }
            ExprKind::Match(value, with, or) =>
            {
                let orl = self.new_empty_label();
                let end = self.new_empty_label();
                let check = self.new_empty_label();
                let mut tbl = FxHashMap::default();
                self.emit_goto(&check);
                for (id, (_, then)) in with.iter().enumerate()
                {
                    let label = self.new_empty_label();
                    self.label_here(&label);
                    tbl.insert(id, label);
                    self.expr(then, false);
                    self.emit_goto(&end);
                }

                self.label_here(&orl);
                if or.is_some()
                {
                    self.expr(&or.clone().unwrap(), false);
                    self.emit_goto(&end);
                }
                self.label_here(&check);
                for (id, (cond, _)) in with.iter().enumerate()
                {
                    self.expr(value, false);
                    self.expr(cond, false);
                    self.emit(Opcode::Eq);
                    let label = tbl.get(&id).unwrap();
                    let lbl = self.labels.get(label).unwrap();
                    self.emit(Opcode::JumpNz(lbl.unwrap()));
                }
                if or.is_some()
                {
                    self.emit_goto(&orl);
                }

                self.label_here(&end);
            }
            ExprKind::If(cond, then, else_do) =>
            {
                self.expr(&cond, false);

                let if_true = self.new_empty_label();
                let if_false = self.new_empty_label();
                let check = self.new_empty_label();
                let end = self.new_empty_label();
                self.emit_goto(&check);
                self.label_here(&if_true);
                self.expr(&then, false);
                self.emit_goto(&end);
                self.label_here(&if_false);
                if else_do.is_some()
                {
                    self.expr(&else_do.clone().unwrap(), false);
                }
                else
                {
                    self.emit(Opcode::ConstNull);
                }
                self.emit_goto(&end);

                let l = self.labels.clone();
                self.label_here(&check);
                self.emit(Opcode::JumpNz(l.get(&if_true).unwrap().unwrap()));
                self.emit(Opcode::Jump(l.get(&if_false).unwrap().unwrap()));
                self.label_here(&end);
            }

            ExprKind::While(cond, then) =>
            {
                let while_block = self.new_empty_label();
                let while_end = self.new_empty_label();
                let while_start = self.new_empty_label();

                self.end_labels.push(while_end.clone());
                self.check_labels.push(while_start.clone());
                self.emit_goto(&while_start);
                self.label_here(&while_block);
                self.expr(&then.clone(), false);
                self.label_here(&while_start);
                self.expr(&cond.clone(), false);
                let l = self.labels.clone();
                self.emit(Opcode::JumpNz(l.get(&while_block).unwrap().unwrap()));
                self.label_here(&while_end);
            }
            ExprKind::This => self.emit(Opcode::PushThis),
            ExprKind::BinOp(lhs, op, rhs) =>
            {
                self.expr(rhs, false);
                self.expr(lhs, false);
                let s: &str = op;
                match s
                {
                    "+" => self.emit(Opcode::Add),
                    "-" => self.emit(Opcode::Sub),
                    "*" => self.emit(Opcode::Mul),
                    "/" => self.emit(Opcode::Div),
                    ">>" => self.emit(Opcode::Shr),
                    "<<" => self.emit(Opcode::Shl),
                    "|" | "||" => self.emit(Opcode::Or),
                    "&" | "&&" => self.emit(Opcode::And),
                    ">" => self.emit(Opcode::Gt),
                    ">=" => self.emit(Opcode::Gte),
                    "<" => self.emit(Opcode::Lt),
                    "<=" => self.emit(Opcode::Lte),
                    "==" => self.emit(Opcode::Eq),
                    "!=" => self.emit(Opcode::Neq),
                    "^" => self.emit(Opcode::Xor),
                    _ =>
                    {
                        if self.fdefs.contains_key(s)
                        {

                        }
                        else
                        {
                            panic!("Unknwown binary operation");
                        }
                    }
                }
            }
            ExprKind::Unop(op, value) =>
            {
                self.expr(value, false);
                let s: &str = op;
                match s
                {
                    "-" => self.emit(Opcode::Neg),
                    "!" => self.emit(Opcode::Not),
                    _ => unimplemented!(),
                }
            }
            ExprKind::Nil => self.emit(Opcode::ConstNull),
            ExprKind::Call(func, args) =>
            {
                if let ExprKind::Ident(ref s) = func.expr
                {
                    let s: &str = s;
                    if s == "new"
                    {
                        if args.len() != 0
                        {
                            self.expr(&args[0], false);
                        }
                        self.emit(Opcode::New);
                        return;
                    }
                }
                for arg in args.iter().rev()
                {
                    self.expr(arg, false);
                }
                if !func.is_access()
                {
                    self.emit(Opcode::ConstNull);
                }

                self.expr(func, func.is_access());
                self.emit(Opcode::Call(args.len()));
                return;
            }
            ExprKind::Lambda(args, expr) =>
            {
                let mut func = Function { nargs: args.len() as i32,
                                          is_native: false,
                                          args: args.clone(),
                                          addr: FuncKind::Interpret(vec![]) };
                let locals = self.locals.clone();
                let builtins = self.fdefs.clone();
                let mut cmpl = Compiler::new(self.vm, builtins);
                cmpl.locals = locals;
                cmpl.compile(vec![expr.clone()], args.clone());
                let ins = cmpl.finish();
                if cfg!(debug_assertions)
                {
                    println!("Lambda code with arguments {:?}", args);
                    for op in &ins
                    {
                        println!("{:?}", op);
                    }
                    println!("end");
                }

                func.addr = FuncKind::Interpret(ins);
                let id = self.vm.pool.add_func(func);

                self.emit(Opcode::ConstFuncRef(id));
            }
            ExprKind::Block(e) =>
            {
                for expr in e.iter()
                {
                    self.expr(expr, false);
                }
            }
            ExprKind::ArrayIndex(arr, idx) =>
            {
                self.expr(idx, false);
                self.expr(arr, false);
                self.emit(Opcode::PushField);
            }
            v => panic!("{:?}", v),
        }
    }
}
