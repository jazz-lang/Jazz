#[macro_export]
macro_rules! def {
    ($module:expr,$compiler: expr,fn $name: ident ($($arg:ident),*) $b:block) => {
        fn $name(vm: &mut VirtualMachine,args: Vec<Gc<GcCell<Value>>>) -> Gc<GcCell<Value>> {
            let mut _i = 0;
            $(
                let $arg = &args[_i];
                _i += 1;
            )*
            $b
        }

        let f = Function {
            typ: FunctionType::Internal($name),
            name: stringify!($name).to_owned(),
            module_name: $module.into(),
            export: true,
        };
        let idx = $compiler.vm.pool.new_func(f);
        $compiler.globals.insert(stringify!($name).into(),idx);
        $compiler.vm.globals.insert(idx,gc!(waffle::value::Value::Function(idx)));
    };

    ($module:expr,$compiler:expr, class $name: ident {
        $(
        fn $fname: ident ($($arg:ident),*) $b:block
        )*
    }) => {
        let mut object: Object = Object::new(stringify!($name),None,stringify!($module).into());
        $(
            fn $fname(vm: &mut VirtualMachine,args: Vec<Gc<GcCell<Value>>>) -> Gc<GcCell<Value>> {
                let mut _i = 0;
                let vm = vm;
                let args = args;
                $(
                    let $arg = &$args[_i];
                    _i += 1;
                )*
                $b
            }

            let f = Function {
                typ: FunctionType::Internal($fname),
                name: stringify!($fname).to_owned(),
                module_name: $module.into(),
                export: true,
            };
            let idx = $compiler.vm.pool.new_func(f);

            object.insert(Value::String(stringify!($fname).into()),gc!(Value::Function(idx)));
        )*

        let idx = $compiler.vm.pool.new_object(object);
        $compiler.globals.insert($name.into(),idx);
        $compiler.vm.globals.insert(idx,gc!(waffle::value::Value::Object(idx)));
    };
}
