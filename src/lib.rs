#![warn(unused_must_use)]
#![warn(rust_2018_idioms)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::vec_box)]
#![feature(const_fn)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(unused_variables)]
#[macro_use]
pub mod macros;
pub mod err;
pub mod syntax;

use std::sync::atomic::{AtomicUsize, Ordering};
pub use syntax::{
    ast,
    interner::{intern, str, INTERNER},
    position::Position,
};
static IDGEN: AtomicUsize = AtomicUsize::new(0);
use ast::NodeId;
#[inline]
pub fn gen_id() -> NodeId { NodeId(IDGEN.fetch_add(1, Ordering::AcqRel)) }

use ast::Type;
use std::collections::HashMap;
use syntax::ast::File;

/// Context stores ifnromation about program
pub struct Context
{
    pub file: File,
    pub types: HashMap<NodeId, Type>,
    pub opt: u8,
}
