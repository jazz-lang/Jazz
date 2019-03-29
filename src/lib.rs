#![feature(const_string_new)]

pub mod ast;
#[macro_use]
pub mod macros;
pub mod compile;
pub mod lexer;
pub mod msg;
pub mod parser;
pub mod reader;
pub mod token;
use std::sync::Arc;

pub type P<T> = Arc<T>;

#[allow(non_snake_case)]
pub fn P<T>(value: T) -> Arc<T> {
    Arc::new(value)
}
