use crate::ast::{Const, Expr, ExprDecl};

pub fn translate(ast: &Vec<Box<Expr>>) -> String {
  let mut buf = String::new();
  buf.push_str("const rec = (le => ((f => f(f))(f => (le((...x) => f(f)(...x))))));\n");
  for e in ast.iter() {
    buf.push_str(&translate_expr(e));
    buf.push(';');
  }

  return buf;
}

pub fn translate_expr(expr: &Box<Expr>) -> String {
  match &expr.decl {
    ExprDecl::Return(e) => {
      let e = e.clone().unwrap();

      format!("return {}", translate_expr(&e))
    }
    ExprDecl::Const(c) => match c {
      Const::Int(i) => return i.to_string(),
      Const::Float(f) => return f.to_string(),
      Const::Ident(s) => return s.to_owned(),
      Const::Str(s) => format!("\"{}\"", s),
      Const::Bool(b) => return b.to_string(),
      _ => unimplemented!(),
    },
    ExprDecl::If(cond, then, or) => {
      let mut buf = String::new();
      buf.push_str("if ");
      buf.push('(');
      buf.push_str(&translate_expr(cond));
      buf.push(')');
      buf.push_str(" {\n");
      buf.push_str(&translate_expr(then));
      buf.push_str(" } ");
      if or.is_some() {
        let or = or.clone().unwrap().clone();

        match or.decl {
          ExprDecl::If(_, _, _) => {
            buf.push_str("else ");
            buf.push_str(&translate_expr(&or));
          }
          _ => {
            buf.push_str("else {\n");
            buf.push_str(&translate_expr(&or));
            buf.push_str("\n}");
          }
        }
      }
      return buf;
    }
    ExprDecl::Var(_, name, init) => {
      let mut buf = String::new();
      buf.push_str("var ");
      buf.push_str(name);
      buf.push_str(" = ");
      if init.is_some() {
        buf.push('(');
        buf.push_str(&translate_expr(&init.clone().unwrap()));
        buf.push(')');
      } else {
        buf.push_str("null");
      }
      buf.push('\n');
      return buf;
    }
    ExprDecl::Assign(to, from) => {
      let mut buf = String::new();
      buf.push_str(&translate_expr(to));
      buf.push_str(" = ");
      buf.push_str(&translate_expr(from));
      return buf;
    }
    ExprDecl::Field(obj, name) => {
      return format!("{}.{}", &translate_expr(obj), name);
    }
    ExprDecl::Call(callee, args) => {
      let mut buf = String::new();
      buf.push_str(&translate_expr(callee));
      buf.push('(');
      for (i, x) in args.iter().enumerate() {
        buf.push_str(&translate_expr(x));
        if i != args.len() - 1 {
          buf.push(',');
        }
      }
      buf.push(')');
      return buf;
    }
    ExprDecl::Function(rec, name, params, body) => {
      let mut buf = String::new();

      if name.is_some() {
        buf.push_str("const ");
        let name = name.clone().unwrap();
        buf.push_str(&name);
        if *rec {
          buf.push_str(&format!(" =  rec({} => (", name));
        } else {
          buf.push_str(" = ");
        }
        buf.push('(');
        for (i, p) in params.iter().enumerate() {
          buf.push_str(p);
          if i != params.len() - 1 {
            buf.push(',');
          }
        }
        buf.push(')');
        buf.push_str(" => ");
        buf.push_str("{\n");
        buf.push_str(&translate_expr(body));
        buf.push_str("\n}");
        if *rec {
          buf.push_str("))");
        }
      } else {
        buf.push('(');
        for (i, p) in params.iter().enumerate() {
          buf.push_str(p);
          if i != params.len() - 1 {
            buf.push(',');
          }
        }
        buf.push(')');
        buf.push_str(" => {\n");
        buf.push_str(&translate_expr(body));
        buf.push_str("\n}");
      }
      return buf;
    }
    ExprDecl::Binop(op, e1, e2) => {
      let mut buf = String::new();
      buf.push_str(&translate_expr(e1));
      buf.push_str(&format!(" {} ", op));
      buf.push_str(&translate_expr(e2));

      return buf;
    }
    ExprDecl::Block(exprs) => {
      let mut buf = String::new();
      for e in exprs.iter() {
        buf.push_str(&translate_expr(e));
        buf.push('\n');
      }
      return buf;
    }
    v => panic!("{:?}", v),
  }
}
