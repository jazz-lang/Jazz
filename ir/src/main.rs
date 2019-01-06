extern crate jazz_ir;

use jazz_ir::*;
use self::ty::*;
use self::module::*;

extern "C" {
    fn puts();
}

fn main() {
    
    let mut module = Module::new();
    module.declare_function("main".into(), Linkage::Local);
    module.declare_function("puts".into(), Linkage::Extern(puts as *const u8));
    
    let func = module.get_function(&"main".to_string());

    let string = func.iconst(Int(64),b"Hello,world!".as_ptr() as i64);
    let v1 = func.call_indirect("puts",&[string], Int(32));
    func.ret(v1);

    module.finish();

    let fdata = module.get_finalized_function(&"main".to_string());

    let func: fn() -> i32 = unsafe {::std::mem::transmute(fdata)};

    println!("{}",func());

}
