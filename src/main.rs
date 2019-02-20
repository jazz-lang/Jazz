extern crate jazz;

use jazz::reader::Reader;
use jazz::parser::Parser;

fn main() {
    let reader = Reader::from_string("
        class Point: Object {
            function init(x,y) {
                this.x = x
                this.y = y
            }
        }
        
        ");
    let mut ast = vec![];
    let mut parser = Parser::new(reader,&mut ast);
    parser.parse().unwrap();
    println!("{:#?}",ast);
}