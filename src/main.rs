#![feature(box_syntax)]

extern crate jazz;
extern crate structopt;
/*
use jazz::ir::Function;
use jazz::ir::IrType;
use jazz::ir::FuncionId;
use jazz::ir::builder::FunctionBuilder;

fn main() {
    let mut f = Function::new(FuncionId(0), "main");

    let mut builder = FunctionBuilder::new(&mut f);

    {
        let entry = builder.new_block();
        builder.switch_to_block(entry);
        let int = IrType::Int(32);
        let v0 = builder.stack_alloc(8);
        builder.declare_variable(IrType::Ptr(box IrType::UInt(8)), 0);
        builder.def_var(0, v0);

        let val = builder.iconst(int.clone(), 42);

        let var = builder.use_var(0);
        builder.store(var, val);
    }

    println!("{}",f);
}*/

use jazz::{
    err::MsgWithPos,
    syntax::{ast::*, lexer::reader::Reader, parser::*},
};
use structopt::StructOpt;

use std::path::PathBuf;

#[derive(Debug, StructOpt)]
pub enum Backend
{
    #[structopt(help = "Default backend, allows JIT and AOT compilation")]
    GccJIT,
    #[structopt(help = "C++ backend,still W.I.P")]
    CPP,
    #[structopt(help = "Cranelift backend (UNIMPLEMENTED!)")]
    CraneLift,
}

impl Backend
{
    pub const fn gccjit() -> &'static str { "gccjit" }

    pub const fn cpp() -> &'static str { "cpp" }

    pub const fn cranelift() -> &'static str { "cranelift" }
}

use std::str::FromStr;

impl FromStr for Backend
{
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Backend, &'static str>
    {
        let s: &str = &s.to_lowercase();
        match s
        {
            "gccjit" => Ok(Backend::GccJIT),
            "cranelift" => Ok(Backend::CraneLift),
            "cpp" | "c++" => Ok(Backend::CPP),
            _ => Err("expected gccjit,cpp or cranelift backend"),
        }
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "jazz", about = "Jazz language compiler")]
pub struct Options
{
    #[structopt(parse(from_os_str))]
    pub file: PathBuf,
    #[structopt(
        short = "O",
        long = "opt-level",
        default_value = "2",
        help = "Set optimization level"
    )]
    pub opt_level: u8,
    #[structopt(long = "jit", help = "Use JIT compilation instead of AOT compilation")]
    pub jit: bool,
    #[structopt(long = "emit-obj", help = "Output object file")]
    pub emit_obj: bool,
    #[structopt(long = "emit-asm", help = "Print assembly to stdout")]
    pub emit_asm: bool,
    #[structopt(
        short = "o",
        long = "output",
        parse(from_os_str),
        help = "Set output filename"
    )]
    pub output: Option<PathBuf>,
    #[structopt(long = "shared", help = "Output shared library (.dll or .so)")]
    pub shared: bool,
    #[structopt(
        long = "emit-gimple",
        help = "Dump GIMPLE to stdout if gccjit backend used"
    )]
    pub emit_gimple: bool,
    #[structopt(
        long = "backend",
        raw(
            possible_values = "&[\"gccjit\",\"cranelift\",\"cpp\"]",
            case_insensitive = "true",
            default_value = "\"gccjit\""
        ),
        help = "Select backend"
    )]
    pub backend: Backend,
    #[structopt(short = "l", long = "link")]
    pub libraries_link: Vec<String>,
    #[structopt(short = "f")]
    pub gcc_opts: Vec<String>,
    #[structopt(
        long = "consteval",
        help = "Enables constant folding and const function evaluating"
    )]
    pub const_eval: bool,
    #[structopt(long = "print-ast", help = "Print program")]
    pub print_ast: bool,
    #[structopt(
        long = "aggressive-eval",
        help = "try to evaluate normal (not comptime) functions too"
    )]
    pub aggressive_eval: bool,
}

fn main() -> Result<(), MsgWithPos>
{
    let opts: Options = Options::from_args();
    let mut file = File {
        root: opts
            .file
            .parent()
            .unwrap_or(&std::path::Path::new(""))
            .to_str()
            .unwrap()
            .to_owned(),
        src: String::new(),
        path: opts.file.to_str().unwrap().to_owned(),
        elems: vec![],
    };

    let reader = Reader::from_file(opts.file.to_str().unwrap()).unwrap();

    let mut parser = Parser::new(reader, &mut file);

    let err = parser.parse();
    if err.is_err()
    {
        println!("{}", err.clone().err().unwrap());
        std::process::exit(-1);
    }
    for elem in file.elems.iter()
    {
        println!("{}", elem);
    }
    Ok(())
}
