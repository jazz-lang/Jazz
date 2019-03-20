use gc::{Gc, GcCell};
use waffle::{gc, value::*, vm::VirtualMachine};

use crate::codegen::Compiler;

type Args = Vec<Gc<GcCell<Value>>>;
type Ret = Gc<GcCell<Value>>;

pub fn system_class<'a>(compiler: &mut Compiler<'a>) {
    let mut console = Object::new("Console", None, "stdlib".into());
    let f = Function {
        name: "WriteLine".into(),
        typ: FunctionType::Internal(writeline),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    console.insert(Value::String("WriteLine".into()), gc!(Value::Function(idx)));

    let f = Function {
        name: "Write".into(),
        typ: FunctionType::Internal(write),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    console.insert(Value::String("Write".into()), gc!(Value::Function(idx)));

    let f = Function {
        name: "Clear".into(),
        typ: FunctionType::Internal(clear),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    console.insert(Value::String("Clear".into()), gc!(Value::Function(idx)));

    let f = Function {
        name: "ReadLine".into(),
        typ: FunctionType::Internal(console_readline),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    console.insert(Value::String("ReadLine".into()), gc!(Value::Function(idx)));

    let idx = compiler.vm.pool.new_object(console);
    compiler.globals.insert("Console".into(), idx);
    compiler.vm.globals.insert(idx, gc!(Value::Object(idx)));

    let f = Function {
        typ: FunctionType::Internal(array_get),
        name: "aget".into(),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    compiler.globals.insert("aget".into(), idx);
    compiler.vm.globals.insert(idx, gc!(Value::Function(idx)));

    let f = Function {
        typ: FunctionType::Internal(array_set),
        name: "aset".into(),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    compiler.globals.insert("aset".into(), idx);
    compiler.vm.globals.insert(idx, gc!(Value::Function(idx)));

    let f = Function {
        typ: FunctionType::Internal(array_pop),
        name: "apop".into(),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    compiler.globals.insert("apop".into(), idx);
    compiler.vm.globals.insert(idx, gc!(Value::Function(idx)));

    let f = Function {
        typ: FunctionType::Internal(array_push),
        name: "apush".into(),
        module_name: "stdlib".into(),
        export: true,
    };

    let idx = compiler.vm.pool.new_func(f);
    compiler.globals.insert("apush".into(), idx);
    compiler.vm.globals.insert(idx, gc!(Value::Function(idx)));
}

pub fn array_get(vm: &mut VirtualMachine, args: Args) -> Ret {
    let array: usize = args[0].borrow().as_array();
    let idx: &Value = &args[1].borrow();
    let idx = if let Value::Int(i) = idx {
        *i
    } else {
        panic!("Integer expected");
    };
    let array = vm.pool.get_array(array).borrow();

    array.elements[idx as usize].clone()
}
pub fn array_set(vm: &mut VirtualMachine, args: Args) -> Ret {
    let array: usize = args[0].borrow().as_array();
    let idx: &Value = &args[1].borrow();

    let idx = if let Value::Int(i) = idx {
        *i
    } else {
        panic!("Integer expected");
    };

    let array = &mut vm.pool.get_array(array).borrow_mut();
    array.elements[idx as usize] = args[2].clone();
    gc!(Value::Null)
}

pub fn array_pop(vm: &mut VirtualMachine, args: Args) -> Ret {
    let array: usize = args[0].borrow().as_array();
    let array = &mut vm.pool.get_array(array).borrow_mut();

    array.elements.pop().unwrap_or_else(|| gc!(Value::Null))
}

pub fn array_push(vm: &mut VirtualMachine, args: Args) -> Ret {
    let array: usize = args[0].borrow().as_array();
    let array = &mut vm.pool.get_array(array).borrow_mut();

    array.elements.push(args[1].clone());
    gc!(Value::Null)
}

pub fn clear(_: &mut VirtualMachine, _: Args) -> Ret {
    use crossterm::Terminal;
    let term = Terminal::new();
    term.clear(crossterm::ClearType::All).unwrap();

    gc!(Value::Null)
}

pub fn console_readline(_: &mut VirtualMachine, _: Args) -> Ret {
    use std::io::stdin;

    let mut buff = String::new();

    stdin().read_line(&mut buff).unwrap();

    gc!(Value::String(buff))
}
pub fn string(vm: &mut VirtualMachine, args: Args) -> Ret {
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
                let s = string(vm, vec![array.elements[i].clone()]);
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
        _ => unimplemented!(),
    };

    gc!(Value::String(value))
}

pub fn anew(vm: &mut VirtualMachine, args: Args) -> Ret {
    let mut arr = vec![];

    for arg in args.iter() {
        arr.push(arg.clone());
    }

    let array = Array { elements: arr };

    gc!(Value::Array(vm.pool.new_array(array)))
}

pub fn writeline(vm: &mut VirtualMachine, args: Args) -> Ret {
    for val in args.iter() {
        let s = string(vm, vec![val.clone()]);
        let value: &Value = &s.borrow();
        match value {
            Value::String(s) => print!("{}", s),
            _ => unreachable!(),
        };
    }
    println!();
    gc!(Value::Null)
}

pub fn write(vm: &mut VirtualMachine, args: Args) -> Ret {
    for val in args.iter() {
        let s = string(vm, vec![val.clone()]);
        let value: &Value = &s.borrow();
        match value {
            Value::String(s) => print!("{}", s),
            _ => unreachable!(),
        };
    }
    gc!(Value::Null)
}

pub fn print(vm: &mut VirtualMachine, args: Args) -> Ret {
    for val in args.iter() {
        let s = string(vm, vec![val.clone()]);
        let value: &Value = &s.borrow();
        match value {
            Value::String(s) => print!("{}", s),
            _ => unreachable!(),
        };
    }
    println!();
    gc!(Value::Null)
}
