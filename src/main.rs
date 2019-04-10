extern crate jazz;

use jazz::ast::{Token, TokenKind};
use jazz::lexer::Lexer;
use jazz::parser::Parser;
use jazz::reader::Reader;
use jazz::to_js::translate;
use structopt::StructOpt;

use std::path::PathBuf;

#[derive(StructOpt, Debug)]
#[structopt(name = "jazz", version = "0.0.1")]
pub struct Opts {
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
    #[structopt(short = "o", long = "output")]
    output: Option<String>,
}

fn main() {
    let opts = Opts::from_args();

    let file = opts.file.unwrap();
    let reader = Reader::from_file(file.to_str().unwrap()).unwrap();

    let mut ast = vec![];
    let mut parser = Parser::new(reader, &mut ast);
    parser.parse().unwrap();
    let s = translate(&ast);

    if opts.output.is_some() {
        let output = opts.output.unwrap();
        use std::fs::OpenOptions;
        use std::io::Write;

        let mut f = OpenOptions::new()
            .create(true)
            .write(true)
            .open(&output)
            .unwrap();
        f.write_all(s.as_bytes()).unwrap();
    } else {
        println!("{}", s);
    }
}
