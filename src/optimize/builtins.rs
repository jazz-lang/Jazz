use super::const_eval::*;
use std::{cell::RefCell, rc::Rc};

pub fn __builtin_printf(args: &[Rc<RefCell<Const>>]) -> Rc<RefCell<Const>>
{
    let fmt: &Const = &args[0].borrow();

    let fmt: String = if let Const::Str(s) = fmt
    {
        s.clone()
    }
    else
    {
        panic!("String expected");
    };

    let mut chars = fmt.chars();
    let mut i = 0;
    let mut s = String::new();
    let mut argpc = 1;
    while i < fmt.len()
    {
        let c = chars.nth(i);
        let arg = args.get(argpc);
        match c
        {
            Some('%') => match chars.nth(i)
            {
                Some('i') =>
                {
                    if arg.is_some()
                    {
                        let arg: &Const = &arg.as_ref().unwrap().borrow();
                        if let Const::Imm(i, _, _) = arg
                        {
                            s.push_str(&i.to_string())
                        }
                        argpc += 1;
                        i += 1;
                        continue;
                    }
                    else
                    {
                        panic!("Not enough arguments");
                    }
                }
                Some('s') =>
                {
                    if arg.is_some()
                    {
                        let arg: &Const = &arg.as_ref().unwrap().borrow();
                        if let Const::Str(str) = arg
                        {
                            s.push_str(str)
                        }
                        argpc += 1;
                        i += 1;
                        continue;
                    }
                    else
                    {
                        panic!("Not enough arguments")
                    }
                }
                Some('%') => s.push('%'),

                None =>
                {}
                Some(c) => s.push(c),
            },

            Some(c) => s.push(c),
            None => break,
        }
    }
    print!("{}", s);
    rc(Const::Void)
}

pub fn __builtin_putc(args: &[Rc<RefCell<Const>>]) -> Rc<RefCell<Const>> { rc(Const::Void) }

pub fn __builtin_putchar(args: &[Rc<RefCell<Const>>]) -> Rc<RefCell<Const>> { rc(Const::Void) }

pub fn __builtin_puts(args: &[Rc<RefCell<Const>>]) -> Rc<RefCell<Const>> { rc(Const::Void) }

pub fn __builtins_write(args: &[Rc<RefCell<Const>>]) -> Rc<RefCell<Const>> { rc(Const::Void) }

macro_rules! builtin_def {
    ($map: expr,
        $(
            $name: expr => $fun: ident
        ),*
    ) => {
    $(
        $map.insert(crate::syntax::interner::intern($name),$fun as *const u8);
    )*
    };
}

use crate::syntax::interner::Name;
use std::collections::HashMap;
pub fn builtins() -> HashMap<Name, *const u8>
{
    let mut map = HashMap::new();
    builtin_def! {
    map,
        "printf" => __builtin_printf,
        "putc" => __builtin_putc,
        "putchar" => __builtin_putchar,
        "puts" => __builtin_puts,
        "write" => __builtins_write
    }

    map
}
