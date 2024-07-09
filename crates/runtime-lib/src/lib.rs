#[cfg(feature = "runtime")]
use core::fmt::{Debug, Display};

pub mod math;

pub const MIN_SAFE_INT: i64 = -(1i64 << f64::MANTISSA_DIGITS);
pub const MAX_SAFE_INT: i64 = 1i64 << f64::MANTISSA_DIGITS;
pub const MIN_SAFE_FLOAT: f64 = MIN_SAFE_INT as f64;
pub const MAX_SAFE_FLOAT: f64 = MAX_SAFE_INT as f64;

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

    pub fn clone_number(&self) -> Option<Self> {
        match self.tag {
            tags::INTEGER => Some(TValue::new_int(unsafe { self.value.i })),
            tags::FLOAT => Some(TValue::new_float(unsafe { self.value.f })),
            _ => None,
        }
    }

    pub fn get_integer(&self) -> Option<i64> {
        if self.tag == tags::INTEGER {
            return Some(unsafe { self.value.i });
        }
        None
    }

    pub fn get_float_or_cast(&self) -> Option<f64> {
        match self.tag {
            tags::INTEGER => Some(unsafe { self.value.i as f64 }),
            tags::FLOAT => Some(unsafe { self.value.f }),
            _ => None,
        }
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

pub fn float_fits_int(v: f64) -> bool {
    v.is_finite() && v.floor() == v && MAX_SAFE_FLOAT <= v && v >= MIN_SAFE_FLOAT
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
pub unsafe extern "C" fn to_string(ptr: *mut TValue, out: *mut TValue) {
    static mut BUF: &mut [u8] = &mut [0; 255];
    let arg = get_mut_or_return!(ptr);
    match arg.tag {
        0 => {
            static mut NIL: &mut [u8] = &mut [b'n', b'i', b'l'];
            init_str(out, 3, NIL.as_mut_ptr())
        }
        tags::BOOLEAN => {
            static mut TRUE: &mut [u8] = &mut [b't', b'r', b'u', b'e'];
            static mut FALSE: &mut [u8] = &mut [b'f', b'a', b'l', b's', b'e'];
            if arg.value.b {
                init_str(out, TRUE.len() as _, TRUE.as_mut_ptr())
            } else {
                init_str(out, FALSE.len() as _, FALSE.as_mut_ptr())
            }
        }
        tags::INTEGER => {
            let len = write_int_to(arg.value.i, BUF);
            let slice = &mut BUF[..len];
            init_str(out, slice.len() as _, slice.as_mut_ptr());
        }
        tags::FLOAT => {
            let v = arg.value.f;
            let len = ryu::raw::format64(v, BUF.as_mut_ptr());
            init_str(out, len as _, (&mut BUF[..len]).as_mut_ptr());
        }
        tags::STRING_CONST => core::ptr::copy(ptr, out, 1),
        _ => {
            for (i, ch) in "table: 0x".chars().enumerate() {
                BUF[i] = ch as u8;
            };
            let len = write_int_to(ptr as _, BUF);
            init_str(out, len as _, (&mut BUF[..len]).as_mut_ptr());
        },
    }
}

fn write_int_to(mut v: i64, buf: &mut [u8]) -> usize {
    let mut len = if v.is_negative() {
        buf[0] = b'-';
        1
    } else {
        0
    };
    let mut rev = [0u8; 255];
    let mut i = 0;
    while v > 0 {
        match v % 10 {
            1 => rev[i] = b'1',
            2 => rev[i] = b'2',
            3 => rev[i] = b'3',
            4 => rev[i] = b'4',
            5 => rev[i] = b'5',
            6 => rev[i] = b'6',
            7 => rev[i] = b'7',
            8 => rev[i] = b'8',
            9 => rev[i] = b'9',
            _ => rev[i] = b'0',
        }
        v /= 10;
        i += 1;
    }
    for &ch in rev[..i].into_iter().rev() {
        buf[len] = ch;
        len += 1;
    }
    len
}

#[runtime_macros::std_tvalue_export]
pub unsafe extern "C" fn print_error_message(ptr: *mut TValue) {
    const ERROR_MSG_NIL: &str = "(error object is a nil value)";
    
    let Some(v) = ptr.as_ref() else {
        println!("{ERROR_MSG_NIL}");
        return;
    };
    if v.tag == tags::NIL {
        println!("{ERROR_MSG_NIL}");
        return;
    }
    println(ptr);
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

    // #[test]
    // fn assert_positive() {
    //     static mut HW: &mut [u8] = &mut [
    //         b'h', b'e', b'l', b'l', b'o', b' ', b'w', b'o', b'r', b'l', b'd', b'!',
    //     ];
    //     let s: &'static mut str = core::str::from_utf8_mut(unsafe { HW }).unwrap();
    //     unsafe {
    //         assert(&mut TValue::new_bool(true), &mut TValue::new_nil());
    //         assert(&mut TValue::new_str(s), &mut TValue::new_nil());
    //         assert(&mut TValue::new_float(0.1), std::ptr::null_mut());
    //         assert(&mut TValue::new_int(1), &mut TValue::new_nil());
    //     }
    // }

    // #[test]
    // #[should_panic = "assertion failed!"]
    // fn assert_neg_bool_no_msg() {
    //     unsafe {
    //         assert(&mut TValue::new_bool(false), std::ptr::null_mut());
    //     }
    // }

    // #[test]
    // #[should_panic = "assertion failed!"]
    // fn assert_neg_bool_no_msg2() {
    //     unsafe {
    //         assert(&mut TValue::new_bool(false), &mut TValue::new_nil());
    //     }
    // }

    // #[test]
    // #[should_panic = "assertion failed!"]
    // fn assert_neg_nil_no_msg() {
    //     unsafe {
    //         assert(&mut TValue::new_bool(false), &mut TValue::new_nil());
    //     }
    // }

    // #[test]
    // #[should_panic = "error found!"]
    // fn assert_neg_nil_msg() {
    //     static mut HW: &mut [u8] = &mut [
    //         b'e', b'r', b'r', b'o', b'r', b' ', b'f', b'o', b'u', b'n', b'd', b'!',
    //     ];
    //     let s: &'static mut str = core::str::from_utf8_mut(unsafe { HW }).unwrap();
    //     unsafe {
    //         assert(&mut TValue::new_bool(false), &mut TValue::new_str(s));
    //     }
    // }
}
