pub static mut FIELD_ADD: i64 = 0;
pub static mut FIELD_SUB: i64 = 0;
pub static mut FIELD_MUL: i64 = 0;
pub static mut FIELD_DIV: i64 = 0;
pub static mut FIELD_STRING: i64 = 0;
pub static mut FIELD_GET: i64 = 0;
pub static mut FIELD_SET: i64 = 0;
pub static mut FIELD_LOADER: i64 = 0;
pub static mut FIELD_EXPORTS: i64 = 0;
pub static mut FIELD_GT: i64 = 0;
pub static mut FIELD_LT: i64 = 0;
pub static mut FIELD_LTE: i64 = 0;
pub static mut FIELD_GTE: i64 = 0;
pub static mut FIELD_EQ: i64 = 0;
pub static mut FIELD_NEQ: i64 = 0;
fn hash_str(s: &str) -> i64 {
    let mut h = 0xcbf29ce484222325;
    crate::hash::hash_bytes(&mut h, s.as_bytes());
    h as i64
}
pub fn init_fields() {
    unsafe {
        FIELD_ADD = hash_str("__add");
        FIELD_SUB = hash_str("__sub");
        FIELD_DIV = hash_str("__div");
        FIELD_MUL = hash_str("__mul");
        FIELD_STRING = hash_str("__string");
        FIELD_GET = hash_str("__get");
        FIELD_SET = hash_str("__set");
        FIELD_LOADER = hash_str("loader");
        FIELD_EXPORTS = hash_str("exports");
    }
}
