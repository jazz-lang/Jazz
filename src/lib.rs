#![feature(allocator_api)]
use std::sync::Arc;

pub type P<T> = Arc<Cell<T>>;

#[allow(non_snake_case)]
pub fn P<T>(value: T) -> P<T> {
    P::new(Cell::new(value))
}

pub mod fields;
pub mod hash;
pub mod module;
pub mod opcode;
pub mod value;
#[macro_use]
pub mod vm;

pub struct Cell<T> {
    val: *mut T,
}

unsafe impl<T: Sync> Sync for Cell<T> {}

impl<T> Cell<T> {
    pub fn new(val: T) -> Cell<T> {
        let boxed = Box::new(val);
        Cell {
            val: Box::into_raw(boxed) as *mut T,
        }
    }
    #[inline]
    pub fn borrow_mut(&self) -> &mut T {
        unsafe {
            let ptr = self.val as *const T as *mut T;
            &mut *ptr
        }
    }
    #[inline]
    pub fn borrow(&self) -> &T {
        unsafe {
            let ptr = self.val as *const T as *mut T;
            &*ptr
        }
    }

    #[inline]
    pub fn raw(&self) -> *mut T {
        self.val
    }
}

impl<T> Copy for Cell<T> {}
impl<T> Clone for Cell<T> {
    fn clone(&self) -> Self {
        Self { val: self.val }
    }
}
use std::fmt;

impl<T: fmt::Debug> fmt::Debug for Cell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.borrow())
    }
}
use std::hash::{Hash, Hasher};

impl<T: Hash> Hash for Cell<T> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.borrow().hash(h);
    }
}

use std::ops::{Deref, DerefMut};

impl<T> Deref for Cell<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.borrow()
    }
}
impl<T> DerefMut for Cell<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.borrow_mut()
    }
}
