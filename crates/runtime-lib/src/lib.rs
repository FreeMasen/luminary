#[cfg(feature = "runtime")]
use core::fmt::{Debug, Display};

pub mod math;

#[cfg(feature = "runtime")]
mod tags {
    pub const NIL: u8 = 0;
    pub const BOOLEAN: u8 = 1;
    pub const INTEGER: u8 = 2;
    pub const FLOAT: u8 = 3;
    pub const STRING_CONST: u8 = 4;
}

#[cfg(feature = "runtime")]
#[repr(C)]
#[derive(Clone)]
pub struct TValue {
    pub tag: u8,
    pub value: TValueInner,
}

#[cfg(feature = "runtime")]
impl TValue {
    pub const fn size() -> u32 {
        core::mem::size_of::<Self>() as _
    }
}

#[cfg(feature = "runtime")]
impl Debug for TValue {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.tag {
            tags::NIL => write!(f, "TValue::Nil"),
            tags::BOOLEAN => f
                .debug_tuple("TValue::Bool")
                .field(unsafe { &self.value.b })
                .finish(),
            tags::INTEGER => f
                .debug_tuple("TValue::Int")
                .field(unsafe { &self.value.i })
                .finish(),
            tags::FLOAT => f
                .debug_tuple("TValue::Float")
                .field(unsafe { &self.value.f })
                .finish(),
            tags::STRING_CONST => f
                .debug_tuple("TValue::StringConst")
                .field(unsafe { &self.value.s })
                .finish(),
            _ => f.debug_tuple("TValue::Unknown").field(&self.tag).finish(),
        }
    }
}

#[cfg(feature = "runtime")]
impl Display for TValue {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.tag {
            tags::NIL => write!(f, ""),
            tags::BOOLEAN => write!(f, "{}", unsafe { self.value.b }),
            tags::INTEGER => write!(f, "{}", unsafe { self.value.i }),
            tags::FLOAT => write!(f, "{}", unsafe { self.value.f }),
            tags::STRING_CONST => {
                let slice = unsafe {
                    core::slice::from_raw_parts(self.value.s.data, self.value.s.len as _)
                };
                if let Ok(s) = core::str::from_utf8(slice) {
                    write!(f, "{s}")
                } else {
                    for b in slice {
                        write!(f, "\\{b}",)?;
                    }
                    Ok(())
                }
            }
            _ => write!(f, "<unknown_value>"),
        }
    }
}

#[cfg(feature = "runtime")]
impl PartialEq for TValue {
    fn eq(&self, other: &Self) -> bool {
        match (self.tag, other.tag) {
            (tags::NIL, tags::NIL) => true,
            (tags::BOOLEAN, tags::BOOLEAN) => unsafe { self.value.b == other.value.b },
            (tags::INTEGER, tags::INTEGER) => unsafe { self.value.i == other.value.i },
            (tags::FLOAT, tags::INTEGER) => {
                let f = unsafe { self.value.f };
                if f.is_finite() && f.floor() == f {
                    return f as i64 == unsafe { other.value.i };
                }
                false
            }
            (tags::INTEGER, tags::FLOAT) => {
                let f = unsafe { other.value.f };
                if f.is_finite() && f.floor() == f {
                    return f as i64 == unsafe { self.value.i };
                }
                false
            }
            (tags::FLOAT, tags::FLOAT) => unsafe { self.value.f == other.value.f },
            (tags::STRING_CONST, tags::STRING_CONST) => unsafe { self.value.s == other.value.s },
            _ => false,
        }
    }
}

#[cfg(feature = "runtime")]
impl TValue {
    pub fn new_nil() -> Self {
        Self {
            tag: tags::NIL,
            value: TValueInner { b: false },
        }
    }

    pub fn new_bool(b: bool) -> Self {
        Self {
            tag: tags::BOOLEAN,
            value: TValueInner { b },
        }
    }
    pub fn new_int(i: i64) -> Self {
        Self {
            tag: tags::INTEGER,
            value: TValueInner { i },
        }
    }
    pub fn new_float(f: f64) -> Self {
        if f.is_finite() && f.floor() == f && f <= i64::MAX as _ && f >= i64::MIN as _ {
            return TValue::new_int(f as i64);
        }
        Self {
            tag: tags::FLOAT,
            value: TValueInner { f },
        }
    }

    pub fn new_str(s: &'static mut str) -> Self {
        let data = s.as_mut_ptr();
        let len = s.len() as u32;
        Self {
            tag: tags::STRING_CONST,
            value: TValueInner {
                s: StringConst { len, data },
            },
        }
    }

    pub fn is_nan(&self) -> bool {
        if self.tag != tags::FLOAT {
            return false;
        }
        unsafe { self.value.f }.is_nan()
    }

    pub fn is_zero(&self) -> bool {
        match self.tag {
            tags::FLOAT => unsafe { self.value.f == 0.0 },
            tags::INTEGER => unsafe { self.value.i == 0 },
            _ => false,
        }
    }
}

#[cfg(feature = "runtime")]
#[repr(C)]
#[derive(Clone, Copy)]
pub union TValueInner {
    pub b: bool,
    pub i: i64,
    pub f: f64,
    pub s: StringConst,
}

#[cfg(feature = "runtime")]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct StringConst {
    pub len: u32,
    pub data: *mut u8,
}

#[cfg(feature = "runtime")]
impl Debug for StringConst {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.len == 0 {
            return write!(f, r#""""#);
        }
        let data = unsafe {
            let Some(l_data) = core::ptr::slice_from_raw_parts(self.data, self.len as _).as_ref()
            else {
                return write!(f, "<invalid string>");
            };
            l_data
        };
        for &byte in data {
            if byte.is_ascii() {
                write!(f, "{}", byte as char)?;
            } else {
                write!(f, "\\{byte}")?;
            }
        }
        Ok(())
    }
}

#[cfg(feature = "runtime")]
impl PartialEq for StringConst {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }
        unsafe {
            let Some(l_data) = core::ptr::slice_from_raw_parts(self.data, self.len as _).as_ref()
            else {
                return false;
            };
            let Some(r_data) = core::ptr::slice_from_raw_parts(other.data, other.len as _).as_ref()
            else {
                return false;
            };
            l_data == r_data
        }
    }
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn size() -> u32 {
    core::mem::size_of::<TValue>() as _
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn println(ptr: *const TValue) {
    let Some(val) = ptr.as_ref() else {
        println!();
        return;
    };
    println!("{val}");
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn init(ptr: *mut TValue) {
    ptr.as_mut().map(|v| *v = TValue::new_nil());
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn init_bool(ptr: *mut TValue, b: bool) {
    ptr.as_mut().map(|v| *v = TValue::new_bool(b));
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn init_int(ptr: *mut TValue, i: i64) {
    ptr.as_mut().map(|v| *v = TValue::new_int(i));
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn init_float(ptr: *mut TValue, f: f64) {
    ptr.as_mut().map(|v| {
        *v = TValue::new_float(f);
    });
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn init_str(ptr: *mut TValue, len: u32, data: *mut u8) {
    ptr.as_mut().map(|v| {
        v.tag = tags::STRING_CONST;
        v.value = TValueInner {
            s: StringConst { len, data },
        }
    });
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn is_truthy(ptr: *mut TValue) -> bool {
    let Some(v) = ptr.as_ref() else {
        return false;
    };
    if v.tag == 0 {
        return false;
    }
    if v.tag == tags::BOOLEAN {
        v.value.b
    } else {
        true
    }
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn get_tag(ptr: *mut TValue) -> u8 {
    let Some(v) = ptr.as_ref() else {
        return 0;
    };
    v.tag
}

#[macro_export]
macro_rules! get_or_return {
    ($ptr:ident, $out:ident) => {
        if let Some(v) = $ptr.as_ref() {
            v
        } else {
            *$out = $crate::TValue::new_bool(false);
            return;
        }
    };
}

#[macro_export]
macro_rules! get_mut_or_return {
    ($ptr:ident) => {
        if let Some(v) = $ptr.as_mut() {
            v
        } else {
            return;
        }
    };
}

#[cfg(all(test, feature = "runtime"))]
mod tests {
    extern crate std;

    use super::*;

    #[test]
    fn tvalue_debug() {
        static mut HW: &mut [u8] = &mut [
            b'h', b'e', b'l', b'l', b'o', b' ', b'w', b'o', b'r', b'l', b'd', b'!',
        ];
        let s: &'static mut str = core::str::from_utf8_mut(unsafe { HW }).unwrap();
        insta::assert_debug_snapshot!(&[
            TValue::new_nil(),
            TValue::new_bool(false),
            TValue::new_bool(true),
            TValue::new_int(42),
            TValue::new_float(42.1),
            TValue::new_str(s),
        ])
    }

}
