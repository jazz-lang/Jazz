use waffle::vm::VirtualMachine;
use waffle::value::*;
use gc::{Gc,GcCell};

type Args = Vec<Gc<GcCell<Value>>>;
type Ret = Gc<GcCell<Value>>;



pub fn string(_vm: &mut VirtualMachine,args: Args) -> Ret {
    
    let value: &Value = &args[0].borrow();
    let value = match value {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_owned(),
        Value::String(s) => s.to_owned(),
        _ => unimplemented!()
    };
    
    gc!(Value::String(value))
}

pub fn print(vm: &mut VirtualMachine,args: Args) -> Ret {
    let mut i = 0;
    while i < args.len() {
        let s = string(vm,vec![args[i].clone()]);
        let value: &Value = &s.borrow();
        match value {
            Value::String(s) => print!("{}",s),
            _ => unreachable!()
        };
        i += 1;
    }
    println!("");
    gc!(Value::Null)
}