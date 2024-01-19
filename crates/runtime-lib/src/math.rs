#[cfg(feature = "runtime")]
use crate::{tags, TValue, TValueInner};
#[cfg(feature = "runtime")]
use core::ops::{Add, Mul, Sub};

#[cfg(feature = "runtime")]
macro_rules! bin_math_prefix {
    ($l:ident, $r:ident, $o:ident) => {
        (
            $crate::get_or_return!($l, $o),
            $crate::get_or_return!($r, $o),
            $crate::get_mut_or_return!($o),
        )
    };
}
#[cfg(feature = "runtime")]
macro_rules! bin_math_op {
    ($l:ident, $r:ident, $o:ident, $iop:path, $fop:path) => {{
        let (l, r, o) = bin_math_prefix!($l, $r, $o);

        *o = match (l.tag, r.tag) {
            (tags::INTEGER, tags::INTEGER) => TValue::new_int($iop(l.value.i, r.value.i)),
            (tags::FLOAT, tags::FLOAT) => {
                TValue::new_float($fop(l.value.f as f64, r.value.f as f64))
            }
            (tags::INTEGER, tags::FLOAT) => TValue::new_float($fop(l.value.i as f64, r.value.f)),
            (tags::FLOAT, tags::INTEGER) => TValue::new_float($fop(l.value.f, r.value.i as f64)),
            _ => TValue::new_bool(false),
        }
    }};
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn add(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    bin_math_op!(lhs, rhs, out, i64::wrapping_add, f64::add)
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn sub(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    bin_math_op!(lhs, rhs, out, i64::wrapping_sub, f64::sub)
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn mul(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    bin_math_op!(lhs, rhs, out, i64::wrapping_mul, f64::mul)
}

#[cfg(feature = "runtime")]
fn int_rem(lhs: i64, rhs: i64) -> TValue {
    if rhs == 0 {
        return crate::TValue::new_bool(false);
    }
    let Some(mut ret_val) = lhs.checked_rem_euclid(rhs) else {
        return float_rem(lhs as _, rhs as _);
    };
    // see lua5.3 lvm.c#568
    if ret_val != 0 && ret_val ^ rhs < 0 {
        ret_val += rhs;
    }
    crate::TValue::new_int(ret_val)
}

#[cfg(feature = "runtime")]
fn float_rem(lhs: f64, rhs: f64) -> TValue {
    let mut ret_val = lhs % rhs;
    // see lua5.3 llimits.h:280
    if ret_val * rhs < 0.0 {
        ret_val += rhs;
    }
    crate::TValue::new_float(ret_val)
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn rem(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let (l, r) = unsafe {
        match (l.tag, r.tag) {
            (tags::INTEGER, tags::INTEGER) => {
                *o = int_rem(l.value.i, r.value.i);
                return;
            }
            (tags::INTEGER, tags::FLOAT) => (l.value.i as f64, r.value.f),
            (tags::FLOAT, tags::INTEGER) => (l.value.f, r.value.i as f64),
            (tags::FLOAT, tags::FLOAT) => (l.value.f, r.value.f),
            _ => {
                *o = TValue::new_bool(false);
                return;
            }
        }
    };
    *o = float_rem(l, r)
}

#[cfg(feature = "runtime")]
fn tvalue_div_(l: &TValue, r: &TValue, o: &mut TValue) {
    let (l, r) = unsafe {
        match (l.tag, r.tag) {
            (tags::INTEGER, tags::INTEGER) => (l.value.i as f64, r.value.i as f64),
            (tags::INTEGER, tags::FLOAT) => (l.value.i as f64, r.value.f),
            (tags::FLOAT, tags::INTEGER) => (l.value.f, r.value.i as f64),
            (tags::FLOAT, tags::FLOAT) => (l.value.f, r.value.f),
            _ => {
                *o = TValue::new_bool(false);
                return;
            }
        }
    };
    let f = if r == 0.0 || r == -0.0 {
        if l == 0.0 || l == -0.0 {
            -f64::NAN
        } else if l.is_sign_negative() {
            -f64::INFINITY
        } else {
            f64::INFINITY
        }
    } else {
        l / r
    };
    *o = TValue::new_float(f);
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn div(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    tvalue_div_(l, r, o);
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn floor_div(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    if r.is_zero() && l.tag == tags::INTEGER {
        *o = TValue::new_bool(false);
        return;
    }
    tvalue_div_(l, r, o);
    let f = match o.tag {
        tags::INTEGER => o.value.i as f64,
        tags::FLOAT => o.value.f,
        _ => return,
    };
    *o = TValue::new_float(f.floor());
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn pow(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    *o = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => {
            let v = (l.value.i as f64).powf(r.value.i as _);
            TValue::new_float(v)
        }
        (tags::FLOAT, tags::FLOAT) => {
            let v = l.value.f.powf(r.value.f);
            TValue::new_float(v)
        }
        (tags::FLOAT, tags::INTEGER) => {
            let v = l.value.f.powf(r.value.i as f64);
            TValue::new_float(v)
        }
        (tags::INTEGER, tags::FLOAT) => {
            let v = (l.value.i as f64).powf(r.value.f);
            TValue::new_float(v)
        }
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn neg(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_int(l.value.i.wrapping_neg()),
        tags::FLOAT => TValue::new_float(-l.value.f),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn bin_and(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i & r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn bin_or(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i | r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn bin_xor(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i ^ r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn bin_rhs(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i >> r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn bin_lhs(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i << r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn bin_not(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    let i = match l.tag {
        tags::INTEGER => !l.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}