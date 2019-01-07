use crate::error::*;
use crate::ast::*;
use std::collections::HashMap;
use crate::lexer::token::{IntSuffix,FloatSuffix};

#[derive(Clone,Debug)]
pub struct Semchecker {
    pub globals: HashMap<Name,Elem>,
}

fn func_from_elem(elem: Elem) -> Function {
    match elem {
        Elem::Func(f) => f.clone(),
        _ => panic!(),
    }
}

fn compare_typs(lhs: Type,rhs: Type) -> bool {
    match (lhs,rhs) {
        (Type::Basic(lhs),Type::Basic(rhs)) => {
            let lhs: &str = &lhs;
            let rhs: &str = &rhs;

            match (lhs,rhs) {
                ("int","int") => true,
                ("int","int32") => true,
                ("int","int64") => true,
                ("long","long") => true,
                ("long","int") => true,
                ("long","int32") => true,
                ("long","int64") => true,
                ("float","float") => true,
                ("double","float") => true,
                ("float","double") => true,
                ("double","double") => true,
                ("int32","int") => true,
                ("int64","int") => true,
                ("int","long") => true,
                ("int32","int32") => true,
                ("int64","long") => true,
                ("NULL",_) => true,
                (_,"NULL") => true,
                _ => false,
            }
        }
        (lhs,rhs) => return lhs == rhs,
    }
}

impl Semchecker {
    pub fn new<'a>(ast: &'a AST) -> Semchecker {
        let mut globals = HashMap::new();
        for elem in ast.iter() {
            match elem {
                Elem::Func(func) => {
                    globals.insert(func.name.clone(),Elem::Func(func.clone()));
                }
                Elem::Struct(s) => {
                    globals.insert(s.name.clone(),Elem::Struct(s.clone()));
                }
                _ => unimplemented!()

            }
        }

        Semchecker {
            globals: globals,
        }
    }

    pub fn check(&mut self) -> Result<(),MsgWithPos> {
        for (_name,elem) in self.globals.clone().iter() {
            match elem {
                Elem::Func(func) => {
                    let mut fck = FctDefCheck {
                        global_vars: {
                            let mut typs = HashMap::new();
                            for (_,elem) in self.globals.iter() {
                                match elem {
                                    Elem::Func(f) => {
                                        typs.insert(f.name.clone(),f.return_ty.clone());
                                    }
                                    Elem::Struct(s) => {
                                        typs.insert(s.name.clone(),Type::Basic(s.name.clone()));
                                    }
                                    _ => unimplemented!()
                                }
                            }
                            typs
                        },
                        vars: HashMap::new(),
                        fct: func.clone(),
                        stack: Vec::new(),
                        semck: self,
                    };
                    fck.check()?;
                }
                _ => unimplemented!()
            }
        }
        Ok(())
    }
}

pub struct FctDefCheck<'a> {
    semck: &'a Semchecker,
    vars: HashMap<String,Type>,
    global_vars: HashMap<String,Type>,
    stack: Vec<Type>,
    fct: Function,
}

impl<'a> FctDefCheck<'a> {
    fn pop(&mut self) -> Type {
        self.stack.pop().expect("No value to pop").clone()
    }

    pub fn check(&mut self) -> Result<(),MsgWithPos> {
        if self.fct.block.is_some() {
            let block: Box<Stmt> = self.fct.clone().block.unwrap().clone();

            self.visit_stmt(block)?;
        }
        Ok(())
    }

    pub fn visit_stmt(&mut self,stmt: Box<Stmt>) -> Result<(),MsgWithPos> {
        let stmt: Stmt = *stmt;
        match stmt.kind {
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Block(stmts) => {
                for stmt in stmts.iter() {
                    self.visit_stmt(stmt.clone())?;
                }
                return Ok(());
            }
            StmtKind::Return(e) => if e.is_some() {
                self.visit_expr(e.unwrap())?;
                let ty = self.pop();
                if !compare_typs(ty.clone(), self.fct.return_ty.clone()) {
                    return Err(
                        MsgWithPos::new(
                            stmt.pos,Msg::Custom(format!("`{}` returns `{}` type but found `{}`",self.fct.name,self.fct.return_ty,ty))
                        )
                    );
                }
                return Ok(());
            } else {
                return Ok(());
            }
            StmtKind::If(cond,then,else_b) => {
                self.visit_expr(cond)?;
                self.visit_stmt(then)?;
                if else_b.is_some() {
                    self.visit_stmt(else_b.unwrap())?;
                }
                Ok(())
            } 
            StmtKind::While(cond,then) => {
                self.visit_expr(cond)?;
                self.visit_stmt(then)?;
                Ok(())
            }
            StmtKind::Var(name,ty,expr) => {
                if ty.is_some() {
                    let ty2 = ty.clone();
                    self.vars.insert(name.clone(),ty2.unwrap().clone());
                    if expr.is_some() {
                        self.visit_expr(expr.unwrap())?;
                        let expr_ty = self.pop();
                        if !compare_typs(ty.clone().unwrap(), expr_ty.clone()) {
                            return Err(
                                MsgWithPos::new(
                                    stmt.pos,Msg::Custom(format!("Expected `{}` type,found: `{}`",ty.unwrap(),expr_ty))
                                )
                            );
                        }
                    } 
                } else if expr.is_some() {
                        self.visit_expr(expr.unwrap())?;
                        let ty = self.pop();
                        self.vars.insert(name.clone(), ty);
                } else {
                    self.vars.insert(name.clone(),Type::Basic("NULL".into()));
                }
                Ok(())

            }
            StmtKind::Let(name,ty,expr) => {
                if ty.is_some() {
                    let ty2 = ty.clone();
                    self.vars.insert(name.clone(),ty2.unwrap().clone());
                    if expr.is_some() {
                        self.visit_expr(expr.unwrap())?;
                        let expr_ty = self.pop();
                        if !compare_typs(ty.clone().unwrap(), expr_ty.clone()) {
                            return Err(
                                MsgWithPos::new(
                                    stmt.pos,Msg::Custom(format!("Expected `{}` type,found: `{}`",ty.unwrap(),expr_ty))
                                )
                            );
                        }
                    } 
                } else if expr.is_some() {
                        self.visit_expr(expr.unwrap())?;
                        let ty = self.pop();
                        self.vars.insert(name.clone(), ty);
                } else {
                    self.vars.insert(name.clone(),Type::Basic("NULL".into()));
                }
                Ok(())     
            }
            _ => Ok(())
        }
    }

    pub fn visit_expr(&mut self,expr: Box<Expr>) -> Result<(), MsgWithPos> {
        let expr: Expr = *expr;
        match expr.kind {
            ExprKind::Ident(ref name) => {
                if self.global_vars.contains_key(name) {
                    let ty = self.global_vars.get(name).unwrap();
                    self.stack.push(ty.clone());
                    Ok(())
                } else if self.vars.contains_key(name) {
                    let ty = self.vars.get(name).unwrap();
                    self.stack.push(ty.clone());
                    Ok(())
                } else {
                    return Err(MsgWithPos::new(
                        *expr.pos(),Msg::VarNotDefined(name.to_owned())
                    ));
                }
            }
            ExprKind::Binary(rhs,_,lhs) => {
                self.visit_expr(rhs)?;
                self.visit_expr(lhs)?;

                let (rhs,lhs) = (self.pop(),self.pop());

                if compare_typs(lhs.clone(), rhs.clone()) {
                    self.stack.push(rhs.clone());
                    return Ok(());
                } else {
                    return Err(MsgWithPos::new(
                        expr.pos, Msg::Custom(format!("Cannot perform binary operation on diferent types: `{}` and `{}`",lhs,rhs))
                    ))
                }
                

            }
            ExprKind::IntLiteral(_,_,suffix) => {
                match suffix {
                    IntSuffix::Byte => self.stack.push(Type::Basic("int".into())),
                    IntSuffix::Int => self.stack.push(Type::Basic("int32".into())),
                    IntSuffix::Long => self.stack.push(Type::Basic("int64".into())),
                }
                Ok(())
            }
            ExprKind::FloatLiteral(_,suffix) => {
                match suffix {
                    FloatSuffix::Double => self.stack.push(Type::Basic("double".into())),
                    FloatSuffix::Float => self.stack.push(Type::Basic("float".into())),
                }
                Ok(())
            }
            ExprKind::NullLiteral => {
                self.stack.push(Type::Basic("NULL".into()));
                Ok(())
            }
            ExprKind::StrLiteral(_) => {
                self.stack.push(Type::Basic("string".into()));
                Ok(())
            }
            ExprKind::CharLiteral(_) => {
                self.stack.push(Type::Basic("char".into()));
                Ok(())
            }
            ExprKind::Call(path,_object,args) => {
                if path.len() > 1 {
                    unimplemented!()
                } 

                let name = path.name();
                for arg in args.iter().rev() {
                    self.visit_expr(arg.clone())?;
                }
                let fct: &Function = if let Some(Elem::Func(f)) = self.semck.globals.get(name) {
                    f
                } else {
                    return Err(MsgWithPos::new(
                        expr.pos,Msg::Custom(format!("Function `{}` not defined",name))
                    ));
                };
                for arg in fct.params.iter() {
                    let arg: &Param = arg;
                    let ty = self.pop();
                    if !compare_typs(arg.typ.clone(), ty.clone()) {
                        return Err(
                            MsgWithPos::new(
                                expr.pos,Msg::Custom(format!("Expected `{}` type, found {}.(Check function definition at {})",arg.typ,ty,fct.pos))
                            )
                        );
                    }
                }

                Ok(())
            }
            ExprKind::Assign(from,to) => {
                self.visit_expr(from)?;
                self.visit_expr(to)?;
                let (rhs,lhs) = (self.pop(),self.pop());
                if !compare_typs(rhs.clone(), lhs.clone()) {
                    return Err(
                        MsgWithPos::new(
                            expr.pos,Msg::Custom(format!("expected `{}` type,found `{}`",lhs,rhs))
                        )
                    );
                }
                Ok(())
            }
            ExprKind::Conv(to,_) => {
                self.stack.push(*to);
                Ok(())
            }
            _ => Ok(()) // raise error unimplemented?
        }
    }
}