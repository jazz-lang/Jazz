extern crate jazz;

use jazz::lexer::reader::Reader;
use jazz::parser::Parser;

fn main() {
    let reader = Reader::from_string(
        "
        func main() -> int {
            let a: *int = 25 as *int;
        }
    ",
    );
    let mut ast = vec![];
    let mut parser = Parser::new(reader, &mut ast);
    parser.parse().unwrap();
    println!("{:#?}", ast);
}
