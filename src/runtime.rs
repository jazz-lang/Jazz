use waffle::builtins::init_builtins;
use waffle::VirtualMachine;
use waffle::value::{Value,Float,FuncKind,Function};
use waffle::builtins::read_line;
use rand::Rng;

pub fn error(_: &mut VirtualMachine,args: Vec<Value>) -> Value {
    println!("Error: {}",args[0].as_str());
    std::process::exit(-1);
}

pub fn rand_int(_: &mut VirtualMachine,_: Vec<Value>) -> Value {
    Value::Int(rand::random())
}

pub fn rand_float(_: &mut VirtualMachine,_: Vec<Value>) -> Value {
    Value::Float(Float::new(rand::random()))
}

pub fn rand_range(_: &mut VirtualMachine,args: Vec<Value>) -> Value {
    Value::Int(rand::thread_rng().gen_range(args[0].as_int(),args[1].as_int()))
}

pub fn string_trim(_: &mut VirtualMachine,args: Vec<Value>) -> Value {
    assert!(args[0].is_str());
    Value::Str(args[0].as_str().trim().to_owned())
}

pub fn len(vm: &mut VirtualMachine,args: Vec<Value>) -> Value {
    match &args[0] {
        Value::Str(s) => Value::Int(s.len() as i64),
        Value::ArrayRef(arr) => {
            let arr = vm.pool.get_array(*arr);
            Value::Int(arr.elements.len() as i64)
        }
        Value::ObjectRef(obj) => {
            let obj = vm.pool.get_object(*obj);
            Value::Int(obj.table.len() as i64)
        }
        _ => panic!("Can't get len on {:?}",args[0])
    }
}

use fxhash::FxHashMap;

pub fn init(vm: &mut VirtualMachine) -> FxHashMap<&'static str, usize> {
    macro register {
        ($map:expr => $name:ident $argc:expr) => {
            let func = Function {
                addr: FuncKind::Native($name as *const u8),
                is_native: true,
                nargs: $argc,
            };

            let id = vm.pool.add_func(func);
            let name: &str = &format!("builtin_{}",stringify!($name));
            $map.insert(unsafe {std::mem::transmute::<&str,&'static str>(name)},id);

        },
        ($map:expr => ($($name:ident $argc:expr),*)) => {
            $(
                register!($map => $name $argc);
            )*
        }
    }
    macro simply_register {
        ($map:expr => $name:ident) => {
            let name: &str = &format!("builtin_{}",stringify!($name));
            let fid = $map.get(name).unwrap();
            $map.insert(stringify!($name),*fid);
        }
    }

    let mut map = init_builtins(vm);

    register!(map => (
            len 1,
            rand_range 2,
            rand_int 0,
            rand_float 0,
            error 1,
            read_line 0,
            string_trim 1
        )
    );

    simply_register!(map => rand_range);
    simply_register!(map => rand_int);
    simply_register!(map => rand_float);
    simply_register!(map => error);
    simply_register!(map => read_line);
    simply_register!(map => string_trim);
    simply_register!(map => len);


    map
}