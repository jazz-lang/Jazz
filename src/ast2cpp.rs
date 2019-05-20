use crate::{str, Context};

pub struct Translator
{
    ctx: Context,
    pub code: String,
}

use crate::syntax::ast::*;

impl Translator
{
    pub fn new(ctx: Context) -> Translator
    {
        Translator {
            ctx,
            code: format!(
                "
#include <inttypes.h>
#include <stddef.h>



"
            ),
        }
    }

    pub fn type_to_c(&mut self, ty: &Type)
    {
        match ty
        {
            Type::Vector(_) => unimplemented!(),
            Type::Basic(basic) =>
            {
                let name: &str = &str(basic.name);
                let s = match name
                {

                    "u8" => "uint8_t",
                    "u16" => "uint16_t",
                    "u32" => "uint32_t",
                    "u64" => "uint64_t",
                    "i64" => "int64_t",
                    "i32" => "int32_t",
                    "i16" => "int16_t",
                    "i8" => "int8_t",
                    "char" => "char",
                    "usize" => "size_t",
                    "uchar" => "unsigned char",
                    "f32" => "float",
                    "f64" => "double",
                    s => s,
                };
                self.code.push_str(&s);
            }
            Type::Ptr(ptr) =>
            {
                self.type_to_c(&ptr.subtype);
                self.code.push('*');
            }
            Type::Array(array) =>
            {
                self.type_to_c(&array.subtype);
                self.code.push('*');
            }
            Type::Func(func) =>
            {
                self.type_to_c(&func.ret);
                self.code.push_str(&format!(" (*)("));
                for (i, ty) in func.params.iter().enumerate()
                {
                    self.type_to_c(ty);
                    if i != func.params.len() - 1
                    {
                        self.code.push(',');
                    }
                }
            }
            Type::Struct(s) => self.code.push_str(&str(s.name).to_string()),
            Type::Void(_) => self.code.push_str("void"),
        }
    }

    pub fn gen_stmt(&mut self, stmt: &Stmt)
    {
        match &stmt.kind
        {
            StmtKind::Expr(expr) =>
            {
                self.gen_expr(expr);
                self.code.push(';');
                self.code.push('\n');
            }
            StmtKind::Block(stmts) =>
            {
                self.code.push_str("{\n");
                for stmt in stmts.iter()
                {
                    self.code.push_str("\t");
                    self.gen_stmt(stmt);
                }
                self.code.push_str("\n}");
            }
            StmtKind::Return(expr) =>
            {
                self.code.push_str("return ");
                if expr.is_some()
                {
                    self.gen_expr(expr.as_ref().unwrap());
                }
                self.code.push_str(";\n");
            }
            StmtKind::Break => self.code.push_str("break;\n"),
            StmtKind::Continue => self.code.push_str("continue;\n"),
            StmtKind::If(cond, then, or) =>
            {
                self.code.push_str("if (");
                self.gen_expr(cond);
                self.code.push_str(")\n");
                self.gen_stmt(then);

                if or.is_some()
                {
                    let or = or.as_ref().unwrap().clone();
                    self.code.push_str(" else ");
                    if !or.is_if()
                    {
                        self.code.push_str("{\n");
                        self.gen_stmt(&or);
                        self.code.push_str("\n}");
                    }
                    else
                    {
                        self.gen_stmt(&or);
                    };
                }
            }
            StmtKind::While(cond, block) =>
            {
                self.code.push_str("while (");
                self.gen_expr(cond);
                self.code.push_str(")\n");
                self.gen_stmt(block);
                self.code.push_str("\n");
            }
            StmtKind::Var(name, reassignable, ty, expr) =>
            {
                if !*reassignable
                {
                    self.code.push_str("const");
                }
                if ty.is_none()
                {
                    self.code.push_str("auto ");
                }
                else
                {
                    self.type_to_c(ty.as_ref().unwrap());
                }
                self.code.push(' ');
                self.code.push_str(&str(*name).to_string());
                if expr.is_some()
                {
                    self.code.push_str(" = ");
                    self.gen_expr(expr.as_ref().unwrap());
                };
                self.code.push_str(";\n");
            }
            StmtKind::Loop(block) =>
            {
                self.code.push_str("while (true) \n");
                self.gen_stmt(block);
                self.code.push_str("\n");
            }
        }
    }

    pub fn gen_expr(&mut self, expr: &Expr)
    {
        match &expr.kind
        {
            ExprKind::Int(i, _, _) => self.code.push_str(&i.to_string()),
            ExprKind::Float(f, _) => self.code.push_str(&f.to_string()),
            ExprKind::Char(c) => self.code.push_str(&format!("'{}'", c)),
            ExprKind::Str(s) =>
            {
                self.code.push_str(&format!("{:?}", s));
            }
            ExprKind::Binary(op, lhs, rhs) =>
            {
                self.gen_expr(lhs);
                self.code.push_str(op);
                self.gen_expr(rhs);
            }
            ExprKind::Unary(op, val) =>
            {
                self.code.push_str(op);
                self.gen_expr(val);
            }
            ExprKind::Field(val, field) =>
            {
                let ty: Type = self.ctx.types.get(&val.id).unwrap().clone();

                self.gen_expr(val);
                if ty.is_ptr()
                {
                    let ptr = ty.to_ptr().unwrap();
                    if ptr.subtype.is_struct()
                    {
                        self.code.push_str(&format!("->{}", str(*field)));
                    }
                    else
                    {
                        panic!();
                    }
                }
                else
                {
                    self.code.push_str(&format!(".{}", str(*field)));
                }
            }
            ExprKind::Conv(val, ty) =>
            {
                self.code.push('(');
                self.type_to_c(ty);
                self.code.push(')');
                self.gen_expr(val);
            }
            ExprKind::Struct(_name, args) =>
            {
                self.code.push_str("{\n");
                for (i, arg) in args.iter().enumerate()
                {
                    let arg: &StructArg = arg;
                    self.code
                        .push_str(&format!(".{} = ", str(arg.name).to_string()));
                    self.gen_expr(&arg.expr);
                    if i != args.len() - 1
                    {
                        self.code.push_str(",\n");
                    }
                }
                self.code.push_str("\n}");
            }
            ExprKind::Deref(expr) =>
            {
                self.code.push('*');
                self.gen_expr(expr);
            }
            ExprKind::AddressOf(expr) =>
            {
                self.code.push('&');

                self.gen_expr(expr);
            }
            ExprKind::Assign(e1, e2) =>
            {
                self.gen_expr(e1);
                self.code.push_str(" = ");
                self.gen_expr(e2);
            }
            ExprKind::Ident(name) => self.code.push_str(&format!("{}", str(*name))),
            ExprKind::Call(path, obj, args) =>
            {
                let name = path.name();
                self.code.push_str(&str(name));
                self.code.push_str("(");
                if obj.is_some()
                {
                    let expr_ = obj.as_ref().unwrap();
                    let ty = self.ctx.types.get(&expr_.id).unwrap().clone();
                    if !ty.is_ptr()
                    {
                        self.code.push('&');
                    }
                    self.gen_expr(expr_);
                    if args.len() != 0
                    {
                        self.code.push(',');
                    }
                }
                for (i, val) in args.iter().enumerate()
                {
                    self.gen_expr(val);
                    if i != args.len() - 1
                    {
                        self.code.push(',');
                    }
                }
                self.code.push(')');
            }
            ExprKind::SizeOf(ty) =>
            {
                self.code.push_str("sizeof(");
                self.type_to_c(ty);
                self.code.push_str(")");
            }
            ExprKind::ArrayIdx(array, index) =>
            {
                self.gen_expr(array);
                self.code.push('[');
                self.gen_expr(index);
                self.code.push(']');
            }
            _ => panic!("{:?}", expr),
        }
    }

    pub fn gen_toplevel(&mut self, elems: &[Elem])
    {
        // predefining all structures
        for elem in elems.iter()
        {
            match elem
            {
                Elem::Struct(struct_) => self
                    .code
                    .push_str(&format!("struct {};\n", str(struct_.name).to_string())),
                Elem::ConstExpr { name, expr, .. } =>
                {
                    self.code
                        .push_str(&format!("#define {} ", str(*name).to_string()));
                    self.gen_expr(expr);
                    self.code.push('\n');
                }
                _ =>
                {}
            }
        }
        // predefining all functions
        for elem in elems.iter()
        {
            if let Elem::Func(func) = elem
            {
                let f: &Function = func;
                if f.external || f.internal
                {
                    self.code.push_str("extern \"C\" {\n");
                }
                self.type_to_c(&f.ret);
                self.code
                    .push_str(&format!(" {} (", str(f.name).to_string()));
                if f.this.is_some()
                {
                    let (name, ty) = f.this.as_ref().unwrap();

                    self.type_to_c(ty);
                    self.code.push_str(&format!(" {}", str(*name).to_owned()));
                    if f.params.len() != 0
                    {
                        self.code.push(',');
                    }
                }
                for (i, (_, ty)) in f.params.iter().enumerate()
                {
                    self.type_to_c(ty);
                    if i != f.params.len() - 1
                    {
                        self.code.push(',');
                    }
                    else
                    {
                        if f.variadic
                        {
                            self.code.push_str(",...");
                        }
                    }
                }
                self.code.push_str(");\n");
                if f.external || f.internal
                {
                    self.code.push_str(" } \n");
                }
            }
        }
        for elem in elems.iter()
        {
            match elem
            {
                Elem::Struct(s) =>
                {
                    self.code
                        .push_str(&format!("struct {} {{\n", str(s.name).to_string()));
                    let s: &Struct = s;
                    for field in s.fields.iter()
                    {
                        let f: &StructField = field;

                        self.type_to_c(&f.data_type);
                        self.code
                            .push_str(&format!(" {};\n", str(f.name).to_string()));
                    }
                    self.code.push_str("};\n");
                }
                _ =>
                {}
            }
        }
        for elem in elems.iter()
        {
            match elem
            {
                Elem::Const(c) =>
                {
                    let c: &Const = c;
                    self.code.push_str(&format!("const "));
                    self.type_to_c(&c.typ);
                    self.code
                        .push_str(&format!(" {} = ", str(c.name).to_string()));
                    self.gen_expr(&c.expr);
                    self.code.push_str(";\n");
                }
                Elem::Func(f) =>
                {
                    if f.body.is_some()
                    {
                        let f: &Function = f;
                        self.type_to_c(&f.ret);
                        self.code
                            .push_str(&format!(" {} (", str(f.name).to_string()));
                        if f.this.is_some()
                        {
                            let (name, ty) = f.this.as_ref().unwrap();

                            self.type_to_c(ty);
                            self.code.push_str(&format!(" {}", str(*name).to_owned()));
                            if f.params.len() != 0
                            {
                                self.code.push(',');
                            }
                        }
                        for (i, (name, ty)) in f.params.iter().enumerate()
                        {
                            self.type_to_c(ty);
                            self.code.push_str(&format!(" {} ", str(*name).to_string()));
                            if i != f.params.len() - 1
                            {
                                self.code.push(',');
                            }
                            else
                            {
                                if f.variadic
                                {
                                    self.code.push_str("...");
                                }
                            }
                        }

                        self.code.push(')');
                        let body = f.body.as_ref().unwrap();

                        self.gen_stmt(body);
                        self.code.push('\n');
                    }
                }

                Elem::Global(global) =>
                {
                    let global: &Global = global;
                    if global.external
                    {
                        self.code.push_str("extern \"C\" {\n");
                    }
                    self.code.push_str("static ");
                    self.type_to_c(&global.typ);
                    self.code
                        .push_str(&format!(" {}", str(global.name).to_owned()));
                    if global.expr.is_some()
                    {
                        self.code.push_str(" = ");
                        self.gen_expr(global.expr.as_ref().unwrap());
                    }
                    self.code.push(';');
                    if global.external
                    {
                        self.code.push_str("\n}\n");
                    }
                }
                Elem::Link(_) =>
                {}
                Elem::Enum =>
                {}
                Elem::Import(_) =>
                {}
                _ =>
                {}
            }
        }
    }

    pub fn run(&mut self)
    {
        let elems = self.ctx.file.elems.clone();
        self.gen_toplevel(&elems);

        let file = format!("output.cc");

        use std::io::Write;
        let mut f = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(file)
            .unwrap();
        f.write_all(self.code.as_bytes()).unwrap();

        std::process::Command::new("c++")
            .arg("-lc")
            .arg("output.cc")
            .spawn()
            .unwrap();
    }
}
