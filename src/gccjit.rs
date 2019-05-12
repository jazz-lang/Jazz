use crate::Context as CContext;
use gccjit_rs::block::{BinaryOp, Block, ComparisonOp, UnaryOp};
use gccjit_rs::ctx::{Context, GlobalKind, OutputKind};
use gccjit_rs::field::Field;
use gccjit_rs::function::{Function as CFunction, FunctionType};
use gccjit_rs::lvalue::LValue;
use gccjit_rs::rvalue::{RValue, ToRValue};
use gccjit_rs::structs::Struct;
use gccjit_rs::ty::Type as CType;

use crate::str;
use crate::syntax::ast::{
    Elem, Expr, ExprKind, Function, NodeId, Stmt, StmtKind, StructArg, StructField, Type,
};

use crate::syntax::interner::Name;
use std::collections::HashMap;
use std::collections::VecDeque;
#[derive(Clone)]
pub struct FunctionUnit {
    /// AST Function
    pub f: Function,
    /// GCCJIT Function
    pub c: CFunction,

    pub irname: String,
}
#[derive(Clone)]
pub struct VarInfo {
    pub lval: LValue,
    pub ty: Type,
    pub cty: CType,
}
#[derive(Clone)]
pub struct GccStruct {
    pub ty: Struct,
    pub fields: HashMap<Name, Field>,
    pub types: Vec<Type>,
}

pub struct Codegen<'a> {
    ctx: Context,
    context: &'a CContext,
    continue_blocks: VecDeque<Block>,
    break_blocks: VecDeque<Block>,

    cur_func: Option<CFunction>,
    cur_block: Option<Block>,
    variables: HashMap<Name, VarInfo>,
    globals: HashMap<Name, (VarInfo, Option<Box<Expr>>)>,
    functions: HashMap<Name, Vec<FunctionUnit>>,
    external_functions: HashMap<Name, FunctionUnit>,
    structures: HashMap<Name, GccStruct>,
    constants: HashMap<Name, Expr>,
    block_id: usize,
    fun_id: usize,
    aliases: HashMap<Name, Type>,
    tmp_id: usize,
    terminated: Vec<bool>,
    cur_return: Option<Type>,
}

impl<'a> Codegen<'a> {
    pub fn ty_size(&self, ty: &Type) -> usize {
        match ty {
            Type::Void(_) => 0,
            Type::Basic(basic) => {
                let name: &str = &str(basic.name);
                match name {
                    "u8" => 1,
                    "i8" => 1,
                    "char" => 1,
                    "i16" => 2,
                    "u16" => 2,
                    "i32" => 4,
                    "u32" => 4,
                    "i64" => 8,
                    "u64" => 8,
                    "f32" => 4,
                    "f64" => 8,
                    "bool" => 1,
                    "usize" => 8,
                    s => {
                        let interned = crate::syntax::interner::intern(s);
                        if self.structures.contains_key(&interned) {
                            let structure = self.structures.get(&interned).unwrap().clone();
                            let mut size = 0;
                            for field in structure.types.iter() {
                                size += self.ty_size(field)
                            }

                            return size;
                        } else {
                            if let Some(ty) = self
                                .aliases
                                .get(&crate::syntax::interner::intern(s))
                                .clone()
                            {
                                return self.ty_size(ty);
                            }
                        }
                        panic!("Type not found");
                    }
                }
            }
            Type::Ptr(_ptr) => return 8,
            Type::Func(_tyfunc) => return 8,
            Type::Struct(structure) => {
                let structure = self.structures.get(&structure.name).unwrap();
                let mut size = 0;
                for field in structure.types.iter() {
                    size += self.ty_size(field)
                }

                return size;
            }
            Type::Array(array) => {
                if array.len.is_some() {
                    return self.ty_size(&array.subtype) * array.len.unwrap() as usize;
                } else {
                    return 8; // pointer
                }
            }
        }
    }

    pub fn ty_to_ctype(&self, ty: &Type) -> CType {
        let ctx = self.ctx;
        match ty {
            Type::Void(_) => ctx.new_type::<()>(),
            Type::Basic(basic) => {
                let name: &str = &str(basic.name);
                match name {
                    "u8" => ctx.new_type::<u8>(),
                    "i8" => ctx.new_type::<i8>(),
                    "char" => ctx.new_type::<char>(),
                    "i16" => ctx.new_type::<i16>(),
                    "u16" => ctx.new_type::<u16>(),
                    "i32" => ctx.new_type::<i32>(),
                    "u32" => ctx.new_type::<u32>(),
                    "i64" => ctx.new_type::<i64>(),
                    "u64" => ctx.new_type::<u64>(),
                    "f32" => ctx.new_type::<f32>(),
                    "f64" => ctx.new_type::<f64>(),
                    "bool" => ctx.new_type::<bool>(),
                    "usize" => ctx.new_type::<usize>(),
                    s => {
                        let interned = crate::syntax::interner::intern(s);
                        if self.structures.contains_key(&interned) {
                            let ty = self.structures.get(&interned).unwrap().ty.as_type();

                            return ty;
                        } else {
                            if let Some(ty) = self
                                .aliases
                                .get(&crate::syntax::interner::intern(s))
                                .clone()
                            {
                                return self.ty_to_ctype(&ty);
                            }

                            unreachable!()
                        }
                    }
                }
            }
            Type::Ptr(ptr) => self.ty_to_ctype(&ptr.subtype).make_pointer(),
            Type::Func(tyfunc) => {
                let params = tyfunc
                    .params
                    .iter()
                    .map(|elem| self.ty_to_ctype(elem))
                    .collect::<Vec<_>>();
                ctx.new_function_pointer_type(None, self.ty_to_ctype(&tyfunc.ret), &params, false)
            }
            Type::Struct(struct_) => self.structures.get(&struct_.name).unwrap().ty.as_type(),
            Type::Array(array) => {
                if array.len.is_some() {
                    let len = *array.len.as_ref().unwrap();

                    ctx.new_array_type(None, self.ty_to_ctype(&array.subtype), len as i32)
                } else {
                    self.ty_to_ctype(&array.subtype).make_pointer()
                }
            }
        }
    }
    pub fn new(context: &'a CContext, name: &str) -> Codegen<'a> {
        let ctx = Context::default();

        ctx.set_name(name);
        ctx.set_dump_gimple(context.gimple);
        use gccjit_rs::sys::*;
        unsafe {
            let ptr = gccjit_rs::ctx::context_get_ptr(&ctx);
            gcc_jit_context_set_bool_allow_unreachable_blocks(ptr, true as _);
        }
        Codegen {
            ctx,
            context,
            continue_blocks: VecDeque::new(),
            break_blocks: VecDeque::new(),
            cur_block: None,
            cur_func: None,
            variables: HashMap::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
            external_functions: HashMap::new(),
            structures: HashMap::new(),
            constants: HashMap::new(),
            block_id: 0,
            fun_id: 0,
            aliases: HashMap::new(),
            tmp_id: 0,
            terminated: vec![],

            cur_return: None,
        }
    }

    pub fn find_struct(&self, ty: &Type) -> Option<GccStruct> {
        match ty {
            Type::Basic(basic) => {
                if let Some(s) = self.structures.get(&basic.name) {
                    return Some(s.clone());
                } else if let Some(ty) = self.aliases.get(&basic.name) {
                    return self.find_struct(ty);
                } else {
                    return None;
                }
            }
            Type::Struct(basic) => {
                if let Some(s) = self.structures.get(&basic.name) {
                    return Some(s.clone());
                } else if let Some(ty) = self.aliases.get(&basic.name) {
                    return self.find_struct(ty);
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    pub fn expr_to_lvalue(&mut self, expr: &Expr) -> Option<LValue> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if self.variables.contains_key(name) {
                    let value = self.variables.get(name).unwrap().clone();

                    return Some(value.lval);
                } else if self.globals.contains_key(name) {
                    let value = self.globals.get(name).unwrap().0.lval;

                    return Some(value);
                } else {
                    return None;
                }
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let _op: &str = op;
                let t1 = self.get_id_type(lhs.id);
                let t2 = self.get_id_type(rhs.id);

                if t1.is_ptr() && crate::semantic::ty_is_any_int(&t2) {
                    let array = self.gen_expr(lhs);
                    let idx = self.gen_expr(rhs);
                    return Some(self.ctx.new_array_access(None, array, idx));
                } else {
                    return None;
                }
            }
            ExprKind::ArrayIdx(array, index) => {
                let array = self.gen_expr(array);
                let index = self.gen_expr(index);

                return Some(self.ctx.new_array_access(None, array, index));
            }
            ExprKind::Field(object, name) => {
                let ty: Type = self.get_id_type(object.id).clone();

                if ty.is_ptr() {
                    let ptr = ty.to_ptr().unwrap();
                    if ptr.subtype.is_struct() {
                        let struct_ = self
                            .structures
                            .get(&ptr.subtype.to_struct().unwrap().name)
                            .unwrap()
                            .clone();

                        let cfield = struct_.fields.get(name).expect("Field not found");
                        let lval = self.expr_to_lvalue(object).expect("LValue expected");
                        return Some(lval.to_rvalue().dereference_field(None, *cfield));
                    } else if let Type::Basic(basic) = &*ptr.subtype.clone() {
                        let struct_ = self.structures.get(&basic.name).unwrap().clone();

                        let cfield = struct_.fields.get(name).expect("Field not found");
                        let lval = self.expr_to_lvalue(object).expect("LValue expected");
                        return Some(lval.to_rvalue().dereference_field(None, *cfield));
                    } else {
                        panic!()
                    }
                } else if ty.is_struct() {
                    let struct_: GccStruct = self
                        .structures
                        .get(&ty.to_struct().unwrap().name)
                        .unwrap()
                        .clone();

                    let cfield = struct_.fields.get(name).expect("Field not found");
                    let lval = self.expr_to_lvalue(object).expect("LValue expected");

                    return Some(lval.access_field(None, *cfield));
                } else {
                    let s = self.find_struct(&ty).expect("Struct not found");

                    let cfield = s.fields.get(name).expect("Field not found");
                    let lval = self.expr_to_lvalue(object).expect("LValue expected");

                    return Some(lval.access_field(None, *cfield));
                }
            }
            ExprKind::Deref(expr_) => {
                let val = self.gen_expr(expr_);

                return Some(val.dereference(None));
            }

            _ => return None, // unimplemented or impossible to get lval
        }
    }

    fn get_id_type(&self, id: NodeId) -> Type {
        self.context.types.get(&id).unwrap().clone()
    }

    fn block_name_new(&mut self) -> String {
        let name = format!("L{}", self.block_id);
        self.block_id += 1;
        name
    }

    pub fn gen_stmt(&mut self, stmt: &Stmt, init: bool) {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                let rval = self.gen_expr(expr);
                self.cur_block.unwrap().add_eval(None, rval);
            }
            StmtKind::Block(stmts) => {
                if !init {
                    let old_block = self.cur_block;
                    let block_name = self.block_name_new();
                    let block = self.cur_func.unwrap().new_block(&block_name);
                    self.cur_block = Some(block);

                    for stmt in stmts.iter() {
                        self.gen_stmt(stmt, false);
                    }

                    self.cur_block = old_block;
                } else {
                    for stmt in stmts.iter() {
                        self.gen_stmt(stmt, false);
                    }
                }
            }
            StmtKind::Break => {
                let break_bb = if let Some(block) = self.break_blocks.back() {
                    *block
                } else {
                    panic!("");
                };
                self.cur_block.unwrap().end_with_jump(None, break_bb);
                self.cur_block = Some(break_bb);
                self.terminated.push(true);
            }
            StmtKind::Continue => {
                let continue_bb = if let Some(block) = self.continue_blocks.back() {
                    *block
                } else {
                    panic!("")
                };
                self.cur_block.unwrap().end_with_jump(None, continue_bb);
            }
            StmtKind::Return(expr) => {
                if expr.is_some() {
                    let expr = expr.as_ref().unwrap();
                    let rval = self.gen_expr(expr);

                    self.cur_block.unwrap().end_with_return(None, rval);
                } else {
                    self.cur_block.unwrap().end_with_void_return(None);
                }
                self.terminated.push(true);
            }
            StmtKind::Var(name, _, _, init) => {
                let ty = self.get_id_type(stmt.id).clone();

                let cty = self.ty_to_ctype(&ty);
                let local = self
                    .cur_func
                    .unwrap()
                    .new_local(None, cty, &str(*name).to_string());
                if init.is_some() {
                    let expr = init.as_ref().unwrap();
                    let rval = self.gen_expr(expr);
                    self.cur_block.unwrap().add_assignment(None, local, rval);
                }

                self.variables.insert(
                    *name,
                    VarInfo {
                        cty: cty,
                        lval: local,
                        ty: ty.clone(),
                    },
                );
            }
            StmtKind::If(cond, then, otherwise) => {
                let func: CFunction = self.cur_func.unwrap();
                let block = self.cur_block.unwrap();
                let then_name = self.block_name_new();
                let else_name = self.block_name_new();

                let bb_then = func.new_block(&format!("if_true:{}", then_name));
                let bb_else = func.new_block(&format!("if_false:{}", else_name));
                let bb_merge: Block = if otherwise.is_some() {
                    let merge_name = self.block_name_new();

                    func.new_block(&format!("after:{}", merge_name))
                } else {
                    bb_else
                };

                let expr = self.gen_expr(cond);

                block.end_with_conditional(None, expr, bb_then, bb_else);

                self.cur_block = Some(bb_then);
                self.gen_stmt(then, true);
                if !*self.terminated.last().unwrap_or(&true) {
                    self.cur_block.unwrap().end_with_jump(None, bb_merge);
                }
                if let Some(else_branch) = otherwise {
                    self.cur_block = Some(bb_else);
                    self.gen_stmt(else_branch, true);
                    if !self.terminated.last().unwrap_or(&false) {
                        self.cur_block.unwrap().end_with_jump(None, bb_merge);
                    }
                }

                self.cur_block = Some(bb_merge);
            }
            StmtKind::While(cond, block_) => {
                let func: CFunction = self.cur_func.unwrap();
                let block = self.cur_block.unwrap();
                let loop_cond: Block = func.new_block(self.block_name_new());
                let loop_body: Block = func.new_block(self.block_name_new());
                let after_loop: Block = func.new_block(self.block_name_new());
                self.break_blocks.push_back(after_loop);
                self.continue_blocks.push_back(loop_cond);

                block.end_with_jump(None, loop_cond);
                self.cur_block = Some(loop_cond);
                let val = self.gen_expr(cond);
                self.cur_block
                    .unwrap()
                    .end_with_conditional(None, val, loop_body, after_loop);

                self.cur_block = Some(loop_body);
                self.gen_stmt(block_, true);

                self.cur_block.unwrap().end_with_jump(None, loop_cond);

                self.continue_blocks.pop_back();
                self.break_blocks.pop_back();
                self.cur_block = Some(after_loop);
            }
            StmtKind::Loop(body) => {
                let bb = self.cur_func.unwrap().new_block(self.block_name_new());
                let after = self.cur_func.unwrap().new_block(self.block_name_new());
                self.break_blocks.push_back(after);
                self.continue_blocks.push_back(bb);

                self.cur_block.unwrap().end_with_jump(None, bb);
                self.cur_block = Some(bb);

                self.gen_stmt(body, true);

                self.cur_block.unwrap().end_with_jump(None, bb);

                self.cur_block = Some(after);
            }
        }
    }

    pub fn gen_expr(&mut self, expr: &Expr) -> RValue {
        let val = match &expr.kind {
            ExprKind::ArrayIdx(array, index) => {
                let array = self.gen_expr(array);
                let index = self.gen_expr(index);

                return self.ctx.new_array_access(None, array, index).to_rvalue();
            }
            ExprKind::Ident(name) => {
                if self.constants.contains_key(name) {
                    let constexpr = self.constants.get(name).unwrap().clone();
                    if let Some(lval) = self.expr_to_lvalue(&constexpr) {
                        return lval.to_rvalue();
                    } else {
                        return self.gen_expr(&constexpr);
                    }
                };
                self.expr_to_lvalue(expr).unwrap().to_rvalue()
            }
            ExprKind::Float(f, suffix) => {
                use crate::syntax::lexer::token::FloatSuffix;
                let float: f64 = *f as _;
                match suffix {
                    FloatSuffix::Float => self
                        .ctx
                        .new_rvalue_from_double(self.ctx.new_type::<f32>(), float),
                    FloatSuffix::Double => self
                        .ctx
                        .new_rvalue_from_double(self.ctx.new_type::<f64>(), float),
                }
            }
            ExprKind::Int(i, _, suffix) => {
                use crate::syntax::lexer::token::IntSuffix;
                let int: i64 = *i as _;
                match suffix {
                    IntSuffix::Int => self
                        .ctx
                        .new_rvalue_from_int(self.ctx.new_type::<i32>(), int as i32),
                    IntSuffix::UInt => self
                        .ctx
                        .new_rvalue_from_int(self.ctx.new_type::<u32>(), int as i32),
                    IntSuffix::Byte => self
                        .ctx
                        .new_rvalue_from_int(self.ctx.new_type::<i8>(), int as i32),
                    IntSuffix::UByte => self
                        .ctx
                        .new_rvalue_from_int(self.ctx.new_type::<u8>(), int as i32),
                    IntSuffix::Long => self
                        .ctx
                        .new_rvalue_from_long(self.ctx.new_type::<i64>(), int),
                    IntSuffix::ULong => self
                        .ctx
                        .new_rvalue_from_long(self.ctx.new_type::<u64>(), int),
                }
            }
            ExprKind::Str(s) => self.ctx.new_string_literal(s),
            ExprKind::Deref(expr) => {
                let rvalue = self.gen_expr(expr);
                rvalue.dereference(None).to_rvalue()
            }
            ExprKind::Unary(op, expr) => {
                let op: &str = op;
                let rval = self.gen_expr(expr);
                let ty = rval.get_type();
                match op {
                    "-" => self.ctx.new_unary_op(None, UnaryOp::Minus, ty, rval),
                    "!" => {
                        let ast_ty = self.get_id_type(expr.id);
                        if crate::semantic::ty_is_any_int(&ast_ty) {
                            self.ctx
                                .new_unary_op(None, UnaryOp::BitwiseNegate, ty, rval)
                        } else {
                            self.ctx
                                .new_unary_op(None, UnaryOp::LogicalNegate, ty, rval)
                        }
                    }
                    "+" => rval,
                    _ => unreachable!(),
                }
            }
            ExprKind::Field(expr_, name) => {
                let ast_ty = self.get_id_type(expr_.id);
                let rvalue = self.gen_expr(expr_).clone();

                if ast_ty.is_ptr() {
                    let ptr = ast_ty.to_ptr().unwrap();
                    if ptr.subtype.is_struct() {
                        let struct_: GccStruct = self
                            .structures
                            .get(&ptr.subtype.to_struct().unwrap().name)
                            .unwrap()
                            .clone();

                        let cfield = struct_.fields.get(name).expect("Field not found");
                        let rval = self.gen_expr(expr_);
                        return rval.dereference_field(None, *cfield).to_rvalue();
                    } else {
                        if ptr.subtype.is_basic() {
                            let basic = ptr.subtype.to_basic().unwrap();
                            let struct_: GccStruct =
                                self.structures.get(&basic.name).unwrap().clone();

                            let cfield = struct_.fields.get(name).expect("Field not found");
                            let rval = self.gen_expr(expr_);
                            return rval.dereference_field(None, *cfield).to_rvalue();
                        }
                        panic!();
                    }
                } else if ast_ty.is_struct() {
                    let struct_: GccStruct = self
                        .structures
                        .get(&ast_ty.to_struct().unwrap().name)
                        .unwrap()
                        .clone();

                    let cfield = struct_.fields.get(name).expect("Field not found");
                    rvalue.access_field(None, *cfield)
                } else {
                    let basic = ast_ty.to_basic().unwrap();

                    let cstruct: &GccStruct = self.structures.get(&basic.name).expect("not found");

                    let field = cstruct.fields.get(name).unwrap();

                    rvalue.access_field(None, *field)
                }
            }
            ExprKind::Assign(lval, rval) => {
                let lval = self.expr_to_lvalue(lval).unwrap();
                let rval = self.gen_expr(rval);
                self.cur_block.unwrap().add_assignment(None, lval, rval);

                return self.ctx.new_rvalue_zero(self.ctx.new_type::<i32>()); // todo: something better than this?
            }

            ExprKind::Bool(b) => self
                .ctx
                .new_rvalue_from_int(self.ctx.new_type::<bool>(), *b as i32),
            ExprKind::AddressOf(expr_) => {
                let val = self.expr_to_lvalue(expr_).expect("lvalue expected");

                return val.get_address(None);
            }
            ExprKind::Conv(val, to) => {
                let cty = self.ty_to_ctype(to);
                let rval = self.gen_expr(val);
                return self.ctx.new_cast(None, rval, cty);
            }

            ExprKind::Call(name, this, args) => {
                if this.is_some() {
                    panic!("methods not yet implemented in gccjit backend");
                }

                let param_types = args
                    .iter()
                    .map(|expr| self.get_id_type(expr.id).clone())
                    .collect::<Vec<_>>();

                let var = if let Some(var) = self.variables.get(&name.name()) {
                    var.lval
                } else if let Some(var) = self.variables.get(&name.name()) {
                    var.lval
                } else if let Some(functions) = self.functions.get(&name.name()) {
                    let mut lval = None;
                    let mut params_match = false;
                    let mut not_found = true;
                    for unit in functions.iter() {
                        let unit: &FunctionUnit = unit;
                        for ((i, (_name, param_ty)), arg_ty) in
                            unit.f.params.iter().enumerate().zip(&param_types)
                        {
                            params_match = (*param_ty.clone() == arg_ty.clone()
                                && i < unit.f.params.len())
                                || i > unit.f.params.len();
                        }
                        if params_match {
                            lval = Some(unit.c);
                            not_found = false;
                            break;
                        } else if unit.f.params.len() == 0 && param_types.len() == 0 {
                            lval = Some(unit.c);
                            not_found = false;
                        }
                    }
                    if not_found {
                        panic!("Function not found");
                    }
                    let mut params = vec![];
                    for arg in args.iter() {
                        params.push(self.gen_expr(arg));
                    }
                    return self.ctx.new_call(None, lval.unwrap(), &params);
                } else if self.external_functions.contains_key(&name.name()) {
                    let unit: &FunctionUnit =
                        &self.external_functions.get(&name.name()).unwrap().clone();

                    let mut params = vec![];
                    for (i, arg) in args.iter().enumerate() {
                        let val = self.gen_expr(arg);
                        let val = if i < unit.f.params.len() {
                            let ty: Type = *unit.f.params[i].1.clone();
                            let cty = self.ty_to_ctype(&ty);
                            self.ctx.new_cast(None, val, cty)
                        } else {
                            val
                        };
                        params.push(val);
                    }
                    return self.ctx.new_call(None, unit.c, &params);
                } else {
                    panic!()
                };

                let mut params = vec![];
                for arg in args.iter() {
                    params.push(self.gen_expr(arg));
                }

                self.ctx.new_call_through_ptr(None, var, &params)
            }

            ExprKind::Struct(name, args) => {
                let name = name.name();

                let struct_: GccStruct = self
                    .find_struct(&Type::create_basic(expr.id, expr.pos, name))
                    .expect("Struct not found");
                //let rval: RValue = self.ctx.new_rvalue_zero(struct_.ty.as_type());
                let tmp_ = format!("{}", self.tmp_id);
                self.tmp_id += 1;
                let tmp: LValue =
                    self.cur_func
                        .unwrap()
                        .new_local(None, struct_.ty.as_type(), &tmp_);
                //self.cur_block.unwrap().add_assignment(None, tmp, rval);
                for arg in args.iter() {
                    let arg: &StructArg = arg;
                    let val = self.gen_expr(&arg.expr);
                    self.cur_block.unwrap().add_assignment(
                        None,
                        tmp.access_field(None, *struct_.fields.get(&arg.name).unwrap()),
                        val,
                    );
                }
                tmp.to_rvalue()
            }
            ExprKind::SizeOf(ty) => {
                let size = self.ty_size(ty);
                return self
                    .ctx
                    .new_rvalue_from_int(self.ctx.new_type::<usize>(), size as i32);
            }
            ExprKind::GetFunc(name) => {
                let val = if self.functions.contains_key(name) {
                    let functions: &Vec<FunctionUnit> = self.functions.get(name).unwrap();
                    let mut v = None;
                    for unit in functions.iter() {
                        let unit: &FunctionUnit = unit;

                        if &unit.f.name == name {
                            v = Some(unit.c.get_address(None));
                            break;
                        }
                    }
                    v.expect("Function addr")
                } else if self.external_functions.contains_key(name) {
                    let func = self.external_functions.get(name).unwrap();

                    func.c.get_address(None)
                } else {
                    panic!("Function not found");
                };
                val
            }

            ExprKind::Binary(op, e1, e2) => {
                let t1 = self.get_id_type(e1.id);
                let t2 = self.get_id_type(e2.id);
                use crate::semantic::{ty_is_any_float, ty_is_any_int};
                if op.contains("==")
                    || op.contains("!=")
                    || op.contains(">")
                    || op.contains("<")
                    || op.contains(">")
                    || op.contains(">=")
                    || op.contains("<=")
                {
                    let op: &str = op;
                    let comparison = match op {
                        "==" => ComparisonOp::Equals,
                        ">" => ComparisonOp::GreaterThan,
                        "<" => ComparisonOp::LessThan,
                        ">=" => ComparisonOp::GreaterThanEquals,
                        "<=" => ComparisonOp::LessThanEquals,
                        "!=" => ComparisonOp::NotEquals,
                        _ => unreachable!(),
                    };
                    let cty = self.ty_to_ctype(&t1);
                    let e1 = self.gen_expr(e1);
                    let e2 = self.gen_expr(e2);
                    let e2 = self.ctx.new_cast(None, e2, cty);
                    return self.ctx.new_comparison(None, comparison, e1, e2);
                }

                if t1.is_ptr() && crate::semantic::ty_is_any_int(&t2) {
                    let array = self.gen_expr(e1);
                    let index = self.gen_expr(e2);
                    return self.ctx.new_array_access(None, array, index).to_rvalue();
                } else if ty_is_any_int(&t1) && ty_is_any_int(&t2) {
                    let cty = self.ty_to_ctype(&t1);
                    let op: &str = op;
                    let binary = match op {
                        "+" => BinaryOp::Plus,
                        "-" => BinaryOp::Minus,
                        "*" => BinaryOp::Mult,
                        "/" => BinaryOp::Divide,
                        "%" => BinaryOp::Modulo,
                        "|" => BinaryOp::BitwiseAnd,
                        "&" => BinaryOp::BitwiseAnd,
                        "^" => BinaryOp::BitwiseXor,
                        ">>" => BinaryOp::RShift,
                        "<<" => BinaryOp::LShift,

                        _ => unreachable!(),
                    };
                    let l = self.gen_expr(e1);
                    let r = self.gen_expr(e2);
                    let r = self.ctx.new_cast(None, r, cty);
                    return self.ctx.new_binary_op(None, binary, cty, l, r);
                } else if ty_is_any_float(&t1) && ty_is_any_float(&t2) {
                    let cty = self.ty_to_ctype(&t1);
                    let op: &str = op;
                    let binary = match op {
                        "+" => BinaryOp::Plus,
                        "-" => BinaryOp::Minus,
                        "*" => BinaryOp::Mult,
                        "/" => BinaryOp::Divide,
                        "%" => BinaryOp::Modulo,

                        _ => unreachable!(),
                    };
                    let l = self.gen_expr(e1);
                    let r = self.gen_expr(e2);
                    let r = self.ctx.new_cast(None, r, cty);
                    return self.ctx.new_binary_op(None, binary, cty, l, r);
                } else if ty_is_any_float(&t1) && ty_is_any_int(&t2) {
                    let cty = self.ty_to_ctype(&t1);
                    let op: &str = op;
                    let binary = match op {
                        "+" => BinaryOp::Plus,
                        "-" => BinaryOp::Minus,
                        "*" => BinaryOp::Mult,
                        "/" => BinaryOp::Divide,
                        "%" => BinaryOp::Modulo,

                        _ => unreachable!(),
                    };
                    let l = self.gen_expr(e1);
                    let r = self.gen_expr(e2);
                    let r = self.ctx.new_cast(None, r, cty);
                    return self.ctx.new_binary_op(None, binary, cty, l, r);
                } else {
                    if t1.is_basic() && t2.is_basic() {
                        let t1 = t1.to_basic().unwrap();
                        let t2 = t2.to_basic().unwrap();
                        let s1: &str = &str(t1.name).to_string();
                        let s2: &str = &str(t2.name).to_string();
                        if s1 == "bool" && s2 == "bool" {
                            let op: &str = op;
                            let binary = match op {
                                "&&" => BinaryOp::LogicalAnd,
                                "||" => BinaryOp::LogicalOr,
                                _ => unreachable!(),
                            };
                            let l = self.gen_expr(e1);
                            let r = self.gen_expr(e2);
                            return self.ctx.new_binary_op(
                                None,
                                binary,
                                self.ctx.new_type::<bool>(),
                                l,
                                r,
                            );
                        } else {
                            unimplemented!()
                        }
                    }
                    unimplemented!()
                }
            }
            ExprKind::Char(c) => self
                .ctx
                .new_rvalue_from_int(self.ctx.new_type::<char>(), *c as i32),
            ExprKind::Null => self
                .ctx
                .new_rvalue_from_ptr(self.ctx.new_type::<*mut u8>(), 0 as *mut ()),
            v => panic!("{:?}", v),
        };

        return val;
    }

    pub fn gen_toplevel(&mut self, elems: &mut [Elem]) {
        for elem in elems.iter() {
            match elem {
                Elem::Struct(s) => {
                    let s: &crate::syntax::ast::Struct = s;
                    let mut fields = vec![];
                    let mut cfields = HashMap::new();
                    let mut types = vec![];
                    for field in s.fields.iter() {
                        let field: &StructField = field;
                        let cty = self.ty_to_ctype(&field.data_type).clone();
                        types.push(field.data_type.clone());
                        let name: &str = &str(field.name).to_string();
                        let cfield = self.ctx.new_field(None, cty, name);
                        cfields.insert(field.name, cfield);
                        fields.push(cfield);
                    }
                    let struct_ = self
                        .ctx
                        .new_struct_type(None, &str(s.name).to_string(), &fields);

                    let cstruct = GccStruct {
                        ty: struct_,
                        fields: cfields,
                        types: types,
                    };
                    self.structures.insert(s.name, cstruct);
                }
                Elem::Link(name) => {
                    self.ctx.add_driver_option(&format!("-l{}", str(*name)));
                }
                Elem::ConstExpr { name, expr, .. } => {
                    self.constants.insert(*name, *expr.clone());
                }
                Elem::Alias(name, ty) => {
                    self.aliases.insert(*name, ty.clone());
                }
                _ => (),
            }
        }

        /*for elem in elems.iter() {
            match elem {
                Elem::Struct(s) => {

                    let mut fields = vec![];
                    let mut cfields = HashMap::new();
                    for field in s.fields.iter() {
                        let field: &StructField = field;
                        let cty = self.ty_to_ctype(&field.data_type).clone();
                        let name: &str = &str(field.name).to_string();
                        let cfield = self.ctx.new_field(None, cty, name);
                        cfields.insert(field.name, cfield);
                        fields.push(cfield);
                    }
                    let cstruct: &mut GccStruct = self.structures.get_mut(&s.name).unwrap();
                    cstruct.fields = cfields;
                    cstruct.ty = self
                        .ctx
                        .new_struct_type(None, &str(s.name).to_string(), &fields);
                    //cstruct.ty.set_fields(None, &fields);
                }
                _ => {}
            }
        }*/
        for elem in elems.iter_mut() {
            match elem {
                Elem::Func(func) => {
                    let func: &mut Function = func;
                    let linkage = if func.external {
                        FunctionType::Extern
                    } else if func.static_ || !func.public {
                        FunctionType::Internal
                    } else if func.inline {
                        FunctionType::AlwaysInline
                    } else {
                        FunctionType::Exported
                    };

                    if func.external {
                        let mut params = vec![];

                        for (name, ty) in func.params.iter() {
                            let ty = self.ty_to_ctype(ty);
                            params.push(self.ctx.new_parameter(None, ty, &str(*name).to_string()));
                        }

                        let f = self.ctx.new_function(
                            None,
                            linkage,
                            self.ty_to_ctype(&func.ret),
                            &params,
                            &str(func.name).to_string(),
                            func.variadic,
                        );

                        let unit = FunctionUnit {
                            f: func.clone(),
                            c: f,
                            irname: str(func.name).to_string(),
                        };

                        self.external_functions.insert(func.name, unit);
                    } else {
                        let mut params = vec![];
                        if func.this.is_some() {
                            let (name, ty) = func.this.as_ref().unwrap();
                            let ty = self.ty_to_ctype(ty);
                            params.push(self.ctx.new_parameter(None, ty, &str(*name).to_string()));
                        }

                        for (name, ty) in func.params.iter() {
                            let ty = self.ty_to_ctype(ty);
                            params.push(self.ctx.new_parameter(None, ty, &str(*name).to_string()));
                        }
                        let name_str: &str = &str(func.name).to_string();
                        let id = self.fun_id;
                        func.ir_temp_id = id;
                        let name = if name_str == "main" {
                            "main".to_owned()
                        } else {
                            format!("a{}", self.fun_id)
                        };
                        self.fun_id += 1;
                        let f = self.ctx.new_function(
                            None,
                            linkage,
                            self.ty_to_ctype(&func.ret),
                            &params,
                            &name,
                            func.variadic,
                        );

                        if let Some(functions) = self.functions.get_mut(&func.name) {
                            for fun in functions.iter() {
                                if &fun.f == func {
                                    panic!("function exists");
                                }
                            }
                            let mut func = func.clone();
                            func.ir_temp_id = id;
                            functions.push(FunctionUnit {
                                f: func.clone(),
                                c: f,
                                irname: name,
                            });
                        } else {
                            self.functions.insert(
                                func.name,
                                vec![FunctionUnit {
                                    f: func.clone(),
                                    c: f,
                                    irname: name,
                                }],
                            );
                        }
                    }
                }
                _ => (),
            }
        }
        for elem in elems.iter() {
            match elem {
                Elem::Global(global) => {
                    let global: &crate::syntax::ast::Global = global;
                    let cty = self.ty_to_ctype(&global.typ);
                    let name: &str = &str(global.name).to_string();
                    let lval = if global.external {
                        self.ctx.new_global(None, GlobalKind::External, cty, name)
                    } else if global.public {
                        self.ctx.new_global(None, GlobalKind::Exported, cty, name)
                    } else {
                        self.ctx.new_global(None, GlobalKind::Internal, cty, name)
                    };

                    let varinfo = VarInfo {
                        lval,
                        cty,
                        ty: *global.typ.clone(),
                    };

                    self.globals
                        .insert(global.name, (varinfo, global.expr.clone()));
                }
                _ => (),
            }
        }
        for elem in elems.iter() {
            match elem {
                Elem::Func(func) => {
                    if func.external {
                        continue;
                    } else {
                        let func: &Function = func;

                        let functions = self.functions.get(&func.name).unwrap().clone();

                        for fun in functions.iter() {
                            if fun.f.ir_temp_id == func.ir_temp_id {
                                self.cur_func = Some(fun.c);
                                self.cur_block = Some(fun.c.new_block("entry"));
                                let block = self.cur_block.unwrap();

                                if &str(func.name).to_string() == "main" {
                                    for (_, (varinfo, expr)) in self.globals.clone().iter() {
                                        if expr.is_some() {
                                            let val = self.gen_expr(expr.as_ref().unwrap());
                                            block.add_assignment(None, varinfo.lval, val);
                                        }
                                    }
                                }

                                for (i, (name, param)) in func.params.iter().enumerate() {
                                    let cty = self.ty_to_ctype(param);
                                    let loc = fun.c.new_local(None, cty, &str(*name).to_string());
                                    let param_ = fun.c.get_param(i as _);
                                    block.add_assignment(None, loc, param_.to_rvalue());
                                    self.variables.insert(
                                        *name,
                                        VarInfo {
                                            lval: loc,
                                            cty,
                                            ty: *param.clone(),
                                        },
                                    );
                                }
                                self.cur_return = Some(*func.ret.clone());
                                self.gen_stmt(func.body.as_ref().unwrap(), true);
                                if !self.terminated.last().unwrap_or(&false) {
                                    let ret = self.cur_return.clone().unwrap().clone();
                                    if ret.is_void() {
                                        self.cur_block.unwrap().end_with_void_return(None);
                                    } else {
                                        if ret.is_struct() {
                                            panic!("Can't create zero value for struct");
                                        }
                                        if !self.terminated.last().unwrap_or(&false) {
                                            let val =
                                                self.ctx.new_rvalue_zero(self.ty_to_ctype(&ret));
                                            self.cur_block.unwrap().end_with_return(None, val);
                                        }
                                    }
                                }
                                //let cty = ty_to_ctype(&func.ret, &self.ctx);
                                //block.end_with_return(None,self.ctx.new_rvalue_zero(cty));
                            }
                        }
                    }
                }

                _ => (),
            }
        }
    }

    pub fn compile(&mut self) {
        if self.context.emit_asm {
            self.ctx.set_dump_code(true);
        }

        self.ctx
            .set_opt_level(unsafe { std::mem::transmute(self.context.opt as i32) });

        let mut elems = self.context.file.elems.clone();

        self.gen_toplevel(&mut elems);

        if self.context.jit {
            use std::env::args;

            let result = self.ctx.compile();
            let args = args();
            let argc = args.len() as i32;
            let argv: Vec<String> = args.collect::<Vec<String>>();
            let argv_c = argv
                .iter()
                .map(|s| std::ffi::CString::new(s.as_bytes()).unwrap().as_ptr())
                .collect::<Vec<_>>();

            let main_fn: fn(i32, *const *const i8) -> i32 =
                unsafe { std::mem::transmute(result.get_function("main")) };

            println!("Exit value: {}", main_fn(argc, argv_c.as_ptr()));
        } else {
            let out_path = if !self.context.output.is_empty() {
                self.context.output.clone()
            } else {
                "a.out".to_owned()
            };
            let kind = if self.context.emit_obj {
                OutputKind::ObjectFile
            } else if self.context.shared {
                OutputKind::DynamicLibrary
            } else {
                OutputKind::Executable
            };
            self.ctx.compile_to_file(kind, out_path);
        }
    }
}
