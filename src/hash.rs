use crate::value::Value;
use crate::P;

pub fn hash_bytes(hash: &mut u64, b: &[u8]) {
    for byte in b.iter() {
        *hash = *hash ^ (*byte as u64);
        *hash = hash.wrapping_mul(0x100000001b3);
    }
}

use std::mem;

pub fn hash_val(hash: &mut u64, val: &P<Value>) {
    match val.borrow() {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let len = arr.len() as i64;
            let bytes: [u8; 8] = unsafe { mem::transmute(len) };
            hash_bytes(hash, &bytes);
            for val in arr.iter() {
                hash_val(hash, val);
            }
        }
        Value::Str(s) => {
            hash_bytes(hash, s.as_bytes());
        }
        Value::Int(i) => {
            let bytes: [u8; 8] = unsafe { mem::transmute(*i) };
            hash_bytes(hash, &bytes);
        }
        Value::Int32(i) => {
            let bytes: [u8; 4] = unsafe { mem::transmute(*i) };
            hash_bytes(hash, &bytes);
        }
        Value::Bool(b) => {
            let bytes: [u8; 1] = [*b as u8];
            hash_bytes(hash, &bytes);
        }
        Value::Null => {
            hash_bytes(hash, &[0]);
        }
        Value::Object(obj) => {
            let obj = obj.borrow();
            for entry in obj.entries.iter() {
                let entry = entry.borrow();
                let bytes: [u8; 8] = unsafe { mem::transmute(entry.hash) };
                hash_bytes(hash, &bytes);
                hash_val(hash, &entry.val);
            }
        }
        _ => (),
    }
}
