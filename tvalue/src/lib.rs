use bstr::BString;
use slab::Slab;
use std::{
    collections::HashMap,
    hash::Hash,
    sync::{OnceLock, RwLock, Arc},
};

static TABLES: OnceLock<RwLock<Slab<HashMap<TValue, TValue>>>> = OnceLock::new();

#[export_name = "::tvalue::add"]
pub fn add(left: &TValue, right: &TValue, out: &mut TValue) -> bool {
    match (left, right) {
        (TValue::Number { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number(lhs.0 + rhs.0);
            true
        }
        (&TValue::Integer { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number((lhs as f32) + rhs.0);
            true
        }
        (TValue::Number { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            *out = new_number(lhs.0 + (*rhs as f32));
            true
        }
        (TValue::Integer { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            *out = new_integer(*lhs + *rhs);
            true
        }
        _ => false,
    }
}

#[export_name = "::tvalue::subtract"]
pub fn subtract(left: &TValue, right: &TValue, out: &mut TValue) -> bool {
    match (left, right) {
        (TValue::Number { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number(lhs.0 - rhs.0);
            true
        }
        (&TValue::Integer { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number((lhs as f32) - rhs.0);
            true
        }
        (TValue::Number { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            *out = new_number(lhs.0 - (*rhs as f32));
            true
        }
        (TValue::Integer { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            *out = new_integer(*lhs - *rhs);
            true
        }
        _ => false,
    }
}

#[export_name = "::tvalue::subtract::multiply"]
pub fn multiply(left: &TValue, right: &TValue, out: &mut TValue) -> bool {
    match (left, right) {
        (TValue::Number { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number(lhs.0 * rhs.0);
            true
        }
        (&TValue::Integer { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number((lhs as f32) * rhs.0);
            true
        }
        (TValue::Number { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            *out = new_number(lhs.0 * (*rhs as f32));
            true
        }
        (TValue::Integer { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            *out = new_integer(*lhs * *rhs);
            true
        }
        _ => false,
    }
}

#[export_name = "::tvalue::power_of"]
pub fn power_of(left: &TValue, right: &TValue, out: &mut TValue) -> bool {
    match (left, right) {
        (TValue::Number { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number(lhs.0.powf(rhs.0));
            true
        }
        (&TValue::Integer { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number((lhs as f32).powf(rhs.0));
            true
        }
        (TValue::Number { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            *out = new_number(lhs.0.powf(*rhs as f32));
            true
        }
        (TValue::Integer { value: lhs, .. }, TValue::Integer { value: rhs, .. }) => {
            let v = (*lhs as f32).powf(*rhs as f32);
            *out = new_number(v);
            true
        }
        _ => false,
    }
}

#[export_name = "::tvalue::divide"]
pub fn divide(left: &TValue, right: &TValue, out: &mut TValue) -> bool {
    match (left, right) {
        (TValue::Number { value: lhs, .. }, TValue::Number { value: rhs, .. }) => {
            *out = new_number(lhs.0 / rhs.0);
            true
        }
        (&TValue::Integer { value: lhs, ..}, TValue::Number { value: rhs, ..}) => {
            *out = new_number((lhs as f32) / rhs.0);
            true
        }
        (TValue::Number { value: lhs, ..}, TValue::Integer { value: rhs, ..}) => {
            *out = new_number(lhs.0 / (*rhs as f32));
            true
        }
        (TValue::Integer { value: lhs, ..}, TValue::Integer { value: rhs, ..}) => {
            *out = if *rhs == 0 {
                new_number((*lhs as f32) / (*rhs as f32))
            } else {
                new_integer(*lhs * *rhs)
            };
            true
        }
        _ => false,
    }
}

#[export_name = "::tvalue::logical_and"]
pub fn logical_and(lhs: &TValue, rhs: &TValue) -> TValue {
    new_bool(lhs.is_truthy() && rhs.is_truthy())
}

#[export_name = "::tvalue::logical_or"]
pub fn logical_or(lhs: &TValue, rhs: &TValue) -> TValue {
    new_bool(lhs.is_truthy() || rhs.is_truthy())
}

#[export_name = "::tvalue::new_nil"]
pub fn new_nil() -> TValue {
    TValue::Nil
}

#[export_name = "::tvalue::new_bool"]
pub fn new_bool(b: bool) -> TValue {
    TValue::Bool {
        value: b,
        t: None,
    }
}

#[export_name = "::tvalue::new_number"]
pub fn new_number(n: f32) -> TValue {
    TValue::Number {
        value: Number(n),
        t: None,
    }
}

#[export_name = "::tvalue::new_integer"]
pub fn new_integer(n: i32) -> TValue {
    TValue::Integer {
        value: n,
        t: None,
    }
}

#[export_name = "::tvalue::new_string"]
pub fn new_string(s: BString) -> TValue {
    TValue::String {
        value: s,
        t: None
    }
}

#[export_name = "::tvalue::new_table"]
pub fn new_table() -> TValue {
    let ts = TABLES.get_or_init(|| RwLock::new(Slab::with_capacity(512)));
    let mut guard = ts.write().unwrap();
    let entry = guard.vacant_entry();
    let ret = TValue::Table {
        idx: Arc::new(entry.key()),
        t: None,
    };
    entry.insert(HashMap::new());
    ret
}

impl TValue {
    #[export_name = "::tvalue::is_truthy"]
    pub fn is_truthy(&self) -> bool {
        !matches!(self, TValue::Bool { value: false, .. } | TValue::Nil)
    }

    // fn get_parent(&self) -> Option<Arc<usize>> {
    //     match self {
    //         TValue::Nil => None,
    //         TValue::Bool {  t, .. } |
    //         TValue::Integer { t, .. } |
    //         TValue::Number { t, .. } |
    //         TValue::String { t, .. } |
    //         TValue::Table { t, .. } => t.clone(),
    //     }
    // }

    fn set_parent(&mut self, idx: Option<Arc<usize>>) {
        match self {
            TValue::Nil => {},
            TValue::Bool {  t, .. } |
            TValue::Integer { t, .. } |
            TValue::Number { t, .. } |
            TValue::String { t, .. } |
            TValue::Table { t, .. } => {
                *t = idx;
            },
        }
    }
}

#[export_name = "::tvalue::table_insert"]
pub fn table_insert(target: &TValue, key: TValue, mut value: TValue) -> bool {
    if let TValue::Number {value: n, ..} = &key {
        if n.0.is_nan() {
            panic!("table index is NaN");
        }
    }
    let TValue::Table {idx, ..} = target else {
        return false;
    };
    let map = TABLES.get_or_init(|| RwLock::new(Slab::with_capacity(512)));
    let mut guard = map.write().unwrap();
    let Some(t) = guard.get_mut(**idx) else {
        return false;
    };
    value.set_parent(Some(idx.clone()));
    t.insert(key, value);
    true
}

#[export_name = "::tvalue::table_get"]
pub fn table_get(target: &TValue, key: &TValue, dest: &mut TValue) -> bool {
    let TValue::Table { idx, .. } = target else {
        return false;
    };
    let tables = TABLES.get_or_init(|| RwLock::new(Slab::with_capacity(512)));
    let tables = tables.read().expect("non-poisoned");
    let Some(t) = tables.get(**idx) else {
        return false;
    };
    let value = t.get(key).cloned().unwrap_or(TValue::Nil);
    *dest = value;
    true
}

#[repr(C)]
#[derive(PartialEq, Hash, Eq, Clone)]
pub enum TValue {
    Nil,
    Bool {
        value: bool,
        t: Option<Arc<usize>>,
    },
    Integer {
        t: Option<Arc<usize>>,
        value: i32,
    },
    Number {
        value: Number,
        t: Option<Arc<usize>>,
    },
    String {
        value: bstr::BString,
        t: Option<Arc<usize>>,
    },
    Table {
        idx: Arc<usize>,
        t: Option<Arc<usize>>,
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Number(pub f32);

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_be_bytes().hash(state);
    }
}
