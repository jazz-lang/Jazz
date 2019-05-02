use parking_lot::Mutex;
use parking_lot::RwLock;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use wrc::WRC as Arc;

lazy_static::lazy_static! {
    pub static ref INTERNER: Mutex<RwLock<Interner>> = Mutex::new(RwLock::new(Interner::new()));
}

#[inline]
pub fn intern(name: &str) -> Name {
    let lock = INTERNER.lock();
    let write = lock.write();

    write.intern(name)
}

#[inline]
pub fn str(name: Name) -> ArcStr {
    let lock = INTERNER.lock();
    let read = lock.read();

    read.str(name)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[repr(C)]
pub struct Name(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArcStr(pub Arc<String>);

impl fmt::Display for ArcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl fmt::Debug for ArcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl ArcStr {
    fn new(value: String) -> ArcStr {
        ArcStr(Arc::new(value))
    }
}

impl Borrow<str> for ArcStr {
    fn borrow(&self) -> &str {
        &self.0[..]
    }
}

impl Deref for ArcStr {
    type Target = String;

    fn deref<'a>(&'a self) -> &'a String {
        &self.0
    }
}

pub struct Interner {
    data: Mutex<Internal>,
}

struct Internal {
    map: HashMap<ArcStr, Name>,
    vec: Vec<ArcStr>,
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            data: Mutex::new(Internal {
                map: HashMap::new(),
                vec: Vec::new(),
            }),
        }
    }

    pub fn intern(&self, name: &str) -> Name {
        let mut data = self.data.lock();

        if let Some(&val) = data.map.get(name) {
            return val;
        }

        let key = ArcStr::new(String::from(name));
        let value = Name(data.vec.len());

        data.vec.push(key.clone());
        data.map.insert(key, value);

        value
    }

    pub fn str(&self, name: Name) -> ArcStr {
        let data = self.data.lock();
        data.vec[name.0].clone()
    }
}
