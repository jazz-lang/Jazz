use crate::P;
#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Int(i64),
    Int32(i32),
    Float(f64),
    Bool(bool),
    Str(String),
    Object(P<Object>),
    Array(P<Vec<P<Value>>>),
    Func(P<Function>),

    Extern(
        *mut u8, /* extern pointer */
        String,  /* extern name */
    ),
}
#[derive(Clone, Debug)]
pub enum FuncVar {
    Offset(usize),
    Native(*const u8),
}
#[derive(Clone)]
pub struct Function {
    pub nargs: i32,
    pub var: FuncVar,
    pub env: P<Value>,
    pub module: P<crate::module::Module>,
}
#[derive(Clone, Debug)]
pub struct ObjField {
    pub val: P<Value>,
    pub hash: i64,
}

#[derive(Clone, Debug)]
pub struct Object {
    pub entries: Vec<P<ObjField>>,
}

impl Object {
    pub fn find(&self, field: i64) -> Option<P<Value>> {
        for entry in self.entries.iter() {
            let entry = entry.borrow();
            if entry.hash == field {
                return Some(entry.val.clone());
            }
        }
        None
    }
    pub fn insert(&mut self, field: i64, val: P<Value>) {
        for entry in self.entries.iter() {
            let entry = entry.borrow_mut();
            if entry.hash == field {
                entry.val = val;
                return;
            }
        }
        self.entries.push(P(ObjField {
            hash: field,
            val: val,
        }));
    }
}

use std::fmt;

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\nFunction:")?;
        writeln!(f, "\tnargs: {}", self.nargs)?;
        match self.var {
            FuncVar::Offset(off) => writeln!(f, "\toffset: {}", off)?,
            FuncVar::Native(_ptr) => writeln!(f, "\t<native>")?,
        }

        writeln!(f, "\tenv: {:?}", self.env)
    }
}

pub fn val_is_array(val: &P<Value>) -> bool {
    let val: &Value = val.borrow();
    match val {
        Value::Array(_) => true,
        _ => false,
    }
}
pub fn val_is_int(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Int(_) => true,
        _ => false,
    }
}

pub fn val_is_int32(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Int32(_) => true,
        _ => false,
    }
}
pub fn val_is_any_int(val: &P<Value>) -> bool {
    val_is_int(val) || val_is_int32(val)
}

pub fn val_is_bool(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Bool(_) => true,
        _ => false,
    }
}

pub fn val_is_float(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Float(_) => true,
        _ => false,
    }
}
pub fn val_is_str(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Str(_) => true,
        _ => false,
    }
}

pub fn val_is_obj(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Object(_) => true,
        _ => false,
    }
}

pub fn val_is_func(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Func(_) => true,
        _ => false,
    }
}
pub fn val_is_null(val: &P<Value>) -> bool {
    match val.borrow() {
        Value::Null => true,
        _ => false,
    }
}

pub fn val_int(val: &P<Value>) -> i64 {
    if val_is_int(val) {
        return match val.borrow() {
            Value::Int(i) => *i,
            _ => unreachable!(),
        };
    } else if val_is_int32(val) {
        return match val.borrow() {
            Value::Int32(i) => *i as i64,
            _ => unreachable!(),
        };
    } else {
        panic!("Integer expected")
    }
}

pub fn val_int32(val: &P<Value>) -> i32 {
    if val_is_int(val) {
        return match val.borrow() {
            Value::Int(i) => *i as i32,
            _ => unreachable!(),
        };
    } else if val_is_int32(val) {
        return match val.borrow() {
            Value::Int32(i) => *i,
            _ => unreachable!(),
        };
    } else {
        panic!("Integer expected")
    }
}

pub fn val_bool(val: &P<Value>) -> bool {
    if val_is_bool(val) {
        return match val.borrow() {
            Value::Bool(b) => *b,
            _ => unreachable!(),
        };
    } else {
        panic!("Boolean expected")
    }
}

pub fn val_float(val: &P<Value>) -> f64 {
    if val_is_float(val) {
        return match val.borrow() {
            Value::Float(f) => *f,
            _ => unreachable!(),
        };
    } else {
        panic!("Float expected")
    }
}
pub fn val_array(val: &P<Value>) -> P<Vec<P<Value>>> {
    if val_is_array(val) {
        return match val.borrow() {
            Value::Array(arr) => arr.clone(),
            _ => unreachable!(),
        };
    } else {
        panic!("Array expected")
    }
}

pub fn val_object(val: &P<Value>) -> P<Object> {
    if val_is_obj(val) {
        return match val.borrow() {
            Value::Object(obj) => obj.clone(),
            _ => unreachable!(),
        };
    } else {
        panic!("Object expected")
    }
}

pub fn val_str(val: &P<Value>) -> String {
    if val_is_str(val) {
        return match val.borrow() {
            Value::Str(s) => s.clone(),
            _ => unreachable!(),
        };
    } else {
        panic!("String expected")
    }
}

pub fn val_func(val: &P<Value>) -> P<Function> {
    if val_is_func(val) {
        return match val.borrow() {
            Value::Func(f) => f.clone(),
            _ => unreachable!(),
        };
    }
    unreachable!()
}
