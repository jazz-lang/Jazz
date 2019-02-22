use waffle::vm::VirtualMachine;
use waffle::value::*;
use gc::{Gc,GcCell};

type Args = Vec<Gc<GcCell<Value>>>;
type Ret = Gc<GcCell<Value>>;



pub fn string(vm: &mut VirtualMachine,args: Args) -> Ret {
    
    let value: &Value = &args[0].borrow();
    let value = match value {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_owned(),
        Value::String(s) => s.to_owned(),
        Value::Array(arr) => {
            let array = vm.pool.get_array(*arr).borrow().clone();
            let mut buff = String::new();
            let mut i = 0;
            buff.push('[');
            while i < array.elements.len() {
                let s = string(vm,vec![array.elements[i].clone()]);
                let val: &Value = &s.borrow();
                if let Value::String(s) = val {
                    buff.push_str(s);    
                }
                if i != array.elements.len() - 1 {
                    buff.push_str(",");
                }
                i += 1;
            }
            buff.push(']');
            buff
        }
        _ => unimplemented!()
    };
    
    gc!(Value::String(value))
}

pub fn anew(vm: &mut VirtualMachine,args: Args) -> Ret {
    let mut arr = vec![];

    for arg in args.iter() {
        arr.push(arg.clone());
    }

    let array = Array {
        elements: arr,
    };

    gc!(Value::Array(vm.pool.new_array(array)))
}

pub fn print(vm: &mut VirtualMachine,args: Args) -> Ret {
    
    for val in args.iter() {
        let s = string(vm,vec![val.clone()]);
        let value: &Value = &s.borrow();
        match value {
            Value::String(s) => print!("{}",s),
            _ => unreachable!()
        };
    }
    println!("");
    gc!(Value::Null)
}