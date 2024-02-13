#[cfg(feature = "runtime")]
use crate::{get_mut_or_return, get_or_return, tags, TValue, TValueInner};
#[cfg(feature = "runtime")]
use core::ops::{Add, Mul, Sub};
#[cfg(feature = "runtime")]
mod rng;

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
        };
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
    let (l, r) = match (l.tag, r.tag) {
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
    };
    *o = float_rem(l, r)
}

#[cfg(feature = "runtime")]
fn tvalue_div_(l: &TValue, r: &TValue, o: &mut TValue) {
    let (l, r) = match (l.get_float_or_cast(), r.get_float_or_cast()) {
        (Some(l), Some(r)) => (l, r),
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    let f = if r == 0.0 || r == -0.0 {
        if l == 0.0 || l == -0.0 {
            f64::NAN
        } else {
            let mut ret = f64::INFINITY;

            if l.is_sign_negative() {
                ret = -ret;
            }
            if r.is_sign_negative() {
                ret = -ret;
            }
            ret
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
pub unsafe extern "C" fn log(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    *o = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => {
            let v = (l.value.i as f64).log(r.value.i as _);
            TValue::new_float(v)
        }
        (tags::FLOAT, tags::FLOAT) => {
            let v = if r.value.f == 10.0 {
                l.value.f.log10()
            } else {
                l.value.f.ln() / r.value.f.ln()
            };
            // let v = l.value.f.log(r.value.f);
            TValue::new_float(v)
        }
        (tags::FLOAT, tags::INTEGER) => {
            let v = l.value.f.log(r.value.i as f64);
            TValue::new_float(v)
        }
        (tags::INTEGER, tags::FLOAT) => {
            let v = (l.value.i as f64).log(r.value.f);
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
pub unsafe extern "C" fn abs(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_int(l.value.i.abs()),
        tags::FLOAT => TValue::new_float(l.value.f.abs()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn acos(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => {
            let v = (l.value.i as f64).acos();
            TValue::new_float(v)
        }
        tags::FLOAT => {
            let v = l.value.f.acos();
            TValue::new_float(v)
        }
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn asin(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => {
            let v = (l.value.i as f64).asin();
            TValue::new_float(v)
        }
        tags::FLOAT => {
            let v = l.value.f.asin();
            TValue::new_float(v)
        }
        _ => TValue::new_bool(false),
    };
}

#[cfg(feature = "runtime")]
fn atan_(lhs: f64, rhs: Option<f64>) -> f64 {
    let ret = if let Some(r) = rhs {
        lhs.atan2(r)
    } else {
        lhs.atan()
    };
    ret
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn atan(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    let l = match l.tag {
        tags::INTEGER => l.value.i as f64,
        tags::FLOAT => l.value.f,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    let r = rhs.as_ref().and_then(|tv| {
        Some(if tv.tag == tags::FLOAT {
            unsafe { tv.value.f }
        } else if tv.tag == tags::INTEGER {
            unsafe { tv.value.i as f64 }
        } else {
            return None;
        })
    });
    *o = TValue::new_float(atan_(l, r));
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn ceil(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_int(l.value.i),
        tags::FLOAT => TValue::new_float(l.value.f.ceil()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn cos(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_float((l.value.i as f64).cos()),
        tags::FLOAT => TValue::new_float(l.value.f.cos()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn deg(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_float((l.value.i as f64).to_degrees()),
        tags::FLOAT => TValue::new_float(l.value.f.to_degrees()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn rad(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_float((l.value.i as f64).to_radians()),
        tags::FLOAT => TValue::new_float(l.value.f.to_radians()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn sin(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_float((l.value.i as f64).sin()),
        tags::FLOAT => TValue::new_float(l.value.f.sin()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn sqrt(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_float((l.value.i as f64).sqrt()),
        tags::FLOAT => TValue::new_float(l.value.f.sqrt()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn tan(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_float((l.value.i as f64).tan()),
        tags::FLOAT => TValue::new_float(l.value.f.tan()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn exp(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_float((l.value.i as f64).exp()),
        tags::FLOAT => TValue::new_float(l.value.f.exp()),
        _ => TValue::new_bool(false),
    };
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn floor(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (
        crate::get_or_return!(lhs, out),
        crate::get_mut_or_return!(out),
    );
    *o = match l.tag {
        tags::INTEGER => TValue::new_int(l.value.i),
        tags::FLOAT => TValue::new_float(l.value.f.floor()),
        _ => TValue::new_bool(false),
    };
}

#[cfg(feature = "runtime")]
fn int_fits_float(i: i64) -> bool {
    -(1 << f64::MANTISSA_DIGITS) <= i && i <= (1 << f64::MANTISSA_DIGITS)
}

#[cfg(feature = "runtime")]
fn int_less_than_float(i: i64, f: f64) -> bool {
    if int_fits_float(i) {
        return (i as f64) < f;
    }
    const LIMIT: f64 = -(i64::MIN as f64);
    if f >= LIMIT {
        true
    } else if f > (i64::MIN as f64) {
        i < (f as i64)
    } else {
        false
    }
}

#[cfg(feature = "runtime")]
fn min_step(lhs: &TValue, rhs: &TValue, out: &mut TValue) -> bool {
    *out = match (lhs.tag, rhs.tag) {
        (tags::INTEGER, tags::INTEGER) => TValue::new_int(unsafe { lhs.value.i.min(rhs.value.i) }),
        (tags::FLOAT, tags::INTEGER) => {
            let l = unsafe { lhs.value.f };
            let r = unsafe { rhs.value.i };
            if int_less_than_float(r, l) {
                TValue::new_int(r)
            } else {
                TValue::new_float(l)
            }
        }
        (tags::INTEGER, tags::FLOAT) => {
            let l = unsafe { lhs.value.i };
            let r = unsafe { rhs.value.f };
            if int_less_than_float(l, r) {
                TValue::new_int(l)
            } else {
                TValue::new_float(r)
            }
        }
        (tags::FLOAT, tags::FLOAT) => TValue::new_float(unsafe { lhs.value.f.min(rhs.value.f) }),
        _ => {
            *out = TValue::new_bool(false);
            return false;
        }
    };
    true
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn min(values: *mut TValue, len: u64, out: *mut TValue) {
    let sl = core::ptr::slice_from_raw_parts(values, len as _)
        .as_ref()
        .unwrap_or_default();

    let out = crate::get_mut_or_return!(out);
    let mut current_min;
    if sl.len() <= 1 {
        *out = sl
            .first()
            .and_then(TValue::clone_number)
            .unwrap_or_else(|| TValue::new_bool(false));
        return;
    } else {
        current_min = sl
            .first()
            .and_then(TValue::clone_number)
            .unwrap_or_else(|| TValue::new_bool(false))
    }
    for test in sl.iter().skip(1) {
        let mut pass = TValue::new_nil();
        if !min_step(&current_min, test, &mut pass) {
            *out = pass;
            return;
        }
        current_min = pass;
    }
    *out = current_min;
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn fmod(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    if l.tag == tags::INTEGER && r.tag == tags::INTEGER {
        let d = r.value.i;
        if (d as u64).wrapping_add(1) <= 1 {
            if d == 0 {
                *o = TValue::new_bool(false);
                return;
            }
            *o = TValue::new_int(0);
            return;
        }
    }
    let (Some(lhs), Some(rhs)) = (l.get_float_or_cast(), r.get_float_or_cast()) else {
        *o = TValue::new_bool(false);
        return;
    };
    *o = TValue::new_float(lhs % rhs);
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn modf(lhs: *mut TValue, out_i: *mut TValue, out_rem: *mut TValue) {
    let (l, o_i, o_r) = (
        crate::get_or_return!(lhs, out_i),
        crate::get_mut_or_return!(out_i),
        crate::get_mut_or_return!(out_rem),
    );
    match l.tag {
        tags::INTEGER => {
            *o_i = TValue::new_int(l.value.i);
            *o_r = TValue::new_float(0.0);
        }
        tags::FLOAT => {
            let f = l.value.f;
            let i = if f < 0.0 { f.ceil() } else { f.floor() };
            *o_i = TValue::new_float(i);
            let r = if f == i { 0.0 } else { f - i };
            *o_r = TValue::new_float(r);
        }
        _ => {
            *o_i = TValue::new_bool(false);
            *o_r = TValue::new_nil();
        }
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
pub unsafe extern "C" fn bin_shr(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => {
            // Lua treats - shifts in a left shift as a right shift
            bin_shl_(l.value.i, -r.value.i)
        }
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

#[cfg(feature = "runtime")]
const NBITS: i64 = 64;
#[cfg(feature = "runtime")]
fn bin_shl_(lhs: i64, rhs: i64) -> i64 {
    if rhs.abs() >= NBITS {
        return 0;
    }
    if rhs < 0 {
        return lhs >> rhs;
    }
    lhs << rhs
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn bin_shl(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => bin_shl_(l.value.i, r.value.i),
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

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn to_number(v: *mut TValue) -> f64 {
    let Some(v) = v.as_ref() else {
        return f64::NAN;
    };
    match v.tag {
        tags::BOOLEAN => {
            let v = unsafe { v.value.b };
            if v {
                1.0
            } else {
                0.0
            }
        }
        tags::FLOAT => unsafe { v.value.f },
        tags::INTEGER => {
            let i = { v.value.i };
            i as f64
        }
        tags::STRING_CONST => {
            let s = { v.value.s };
            let slice = unsafe { core::slice::from_raw_parts(s.data, s.len as _) };
            if let Ok(s) = core::str::from_utf8(slice) {
                if let Ok(f) = s.parse::<f64>() {
                    f
                } else {
                    f64::NAN
                }
            } else {
                f64::NAN
            }
        }
        tags::NIL => 0.0,
        _ => unreachable!(),
    }
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn seedrandom(m: *mut TValue, out: *mut TValue) {
    let o = get_mut_or_return!(out);
    let m = get_or_return!(m, o);
    let Some(m) = m.get_integer() else {
        *out = TValue::new_bool(false);
        return;
    };
    rng::seed_random(m);
    *out = TValue::new_bool(true);
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn random(m: *mut TValue, n: *mut TValue, out: *mut TValue) {
    const RAND_MAX_PLUS_ONE: f64 = (0x7fffffffu32) as f64 + 1.0;
    let o = get_mut_or_return!(out);
    let r1 = rng::get_random() as f64;
    let r2 = 1.0 / RAND_MAX_PLUS_ONE;
    let mut r = r1 / r2;
    let Some(m) = m.as_ref() else {
        // random between 0 and 1 floating point
        // double r = (double)l_rand() * (1.0 / ((double)L_RANDMAX + 1.0));
        *o = TValue::new_float(r);
        return;
    };
    let Some(mut m) = m.get_integer() else {
        *o = TValue::new_bool(false);
        return;
    };
    let n = if let Some(n) = n.as_ref() {
        let Some(n) = n.get_integer() else {
            *o = TValue::new_bool(false);
            return;
        };
        n
    } else {
        let n = m;
        m = 1;
        n
    };
    if n.saturating_sub(m) < 0 {
        *o = TValue::new_bool(false);
        return;
    }
    // r *= (double)(up - low) + 1.0;
    // lua_pushinteger(L, (lua_Integer)r + low);
    r *= (n - m) as f64 + 1.0;
    *out = TValue::new_int(r as i64 + n);
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn ult(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (lhs, rhs, out) = bin_math_prefix!(lhs, rhs, out);
    let (Some(lhs), Some(rhs)) = (lhs.get_integer(), rhs.get_integer()) else {
        *out = TValue::new_nil();
        return;
    };
    let ret = (lhs as u64) < (rhs as u64);
    *out = TValue::new_bool(ret);
}

#[cfg(test)]
mod tests {
    use crate::{tags, TValue};

    #[test]
    fn int_add() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);

            let expected = lua.load(&format!("({})+({})", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::add(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_add() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            let script = format!("({:e})+({:e})", l, r);
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::add(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(&expected, &result, "{} != {} found {:?}", script, expected, result);
        });
    }
    #[test]
    fn fffloat_add() {
        let lua = mlua::Lua::new();
        let l = -1.3583984383179134e32;
        let r = 1.655207760591641e21;
        let mut lhs = TValue::new_float(l);
        let mut rhs = TValue::new_float(r);
        let mut result = TValue::new_bool(false);
        let script = format!("({:e})+({:e})", l, r);
        let expected = lua
            .load(&script)
            .eval()
            .map(convert_expectation)
            .unwrap_or_else(|_| TValue::new_bool(false));
        println!("{script} -> {expected}");
        unsafe {
            crate::math::add(&mut lhs, &mut rhs, &mut result);
        }
        if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
            panic!()
        }
        assert_eq!(
            &expected, &result,
            "{} != {} found {:?}",
            script, expected, result
        );
    }

    #[test]
    fn int_neg() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("-({})", l)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::neg(&mut lhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_neg() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("-({})", l)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::neg(&mut lhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_sub() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("({})-({})", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::sub(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_sub() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("({:e})-({:e})", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::sub(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_mul() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}*{}", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::mul(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_mul() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}*{}", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::mul(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_div() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}/{}", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::div(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_div() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            unsafe {
                crate::math::div(&mut lhs, &mut rhs, &mut result);
            }
            let expected = lua.load(format!("{:e}/{:e}", l, r, )).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{:?} / {:?} != {:?} found {:?}",
                    lhs, rhs, expected, result,
                );
            }
        });
    }

    #[test]
    fn int_floor_div() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}//{}", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::floor_div(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_floor_div() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::floor_div(&mut lhs, &mut rhs, &mut result);
            }

            let expected = lua.load(format!("{:e}//{:e}", l, r, )).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{:?} // {:?} != {:?} found {:?}",
                    lhs, rhs, expected, result,
                );
            }
        });
    }

    #[test]
    fn int_rem() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}%{}", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::rem(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(&expected, &result, "{} % {} != {} ({})", lhs, rhs, expected, result);
        });
    }

    #[test]
    fn float_rem() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::rem(&mut lhs, &mut rhs, &mut result);
            }

            let expected = lua.load(format!("{l:e}%{r:e}")).eval().map(convert_expectation).unwrap_or_else(|e| {
                println!("invalid float rem operation: {l} % {r}: {e}");
                TValue::new_bool(false)
            });
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{:?} % {:?} != {:?} found {:?}",
                    lhs, rhs, expected, result,
                );
            }
        });
    }

    #[test]
    fn int_exp() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("({})^({})", l, r)).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                crate::math::pow(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(&expected, &result, "{:?} ^ {:?} != {:?} found {:?}", lhs, rhs, expected, result);
        });
    }

    #[test]
    fn float_floor_exp() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::pow(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("({l:e})^({r:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn int_bin_and() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{l}&{r}")).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                crate::math::bin_and(&mut lhs, &mut rhs, &mut result);
            }
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
            proptest::prop_assert_eq!(&expected, &result, "{:?} & {:?} != {:?} found {:?}",
                lhs, rhs, expected, result,
            );
        });
    }

    #[test]
    fn int_bin_or() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{l}|{r}")).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                crate::math::bin_or(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_bin_xor() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{l}~{r}")).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                crate::math::bin_xor(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_bin_lhs() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let script = format!("({l}) << ({r})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| {
                TValue::new_bool(false)
            });
            unsafe {
                crate::math::bin_shl(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}", script, expected, result);
        });
    }

    #[test]
    fn int_bin_shr() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let script = format!("({l}) >> ({r})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| {
                TValue::new_bool(false)
            });
            unsafe {
                crate::math::bin_shr(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}", script, expected, result);
        });
    }

    #[test]
    fn int_bin_not() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("~{l}")).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                crate::math::bin_not(&mut lhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn test_min_ints_pairs() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {

            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("math.min({l}, {r})")).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_int(l),
                TValue::new_int(r)
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 2, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn test_min_int_trips() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64, v: i64)| {

            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("math.min({l}, {r}, {v})")).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_int(l),
                TValue::new_int(r),
                TValue::new_int(v),
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 3, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn test_min_flts_pairs() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {

            let mut result = TValue::new_bool(false);
            let script = format!("math.min({l:e}, {r:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_float(l),
                TValue::new_float(r)
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 2, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "Expected {:?} from {} found {:?}", expected, script, result);
        });
    }

    #[test]
    fn test_min_flts_trips() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64, v: f64)| {
            let mut result = TValue::new_bool(false);
            let script = &format!("math.min({l:e}, {r:e}, {v:e})");
            println!("{script}");
            let expected = lua.load(script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_float(l),
                TValue::new_float(r),
                TValue::new_float(v),
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 3, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "Expected {} from ({}, {}, {}): found {}", expected, l, r, v, result);
        });
    }

    #[test]
    fn test_min_mixed_trips() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: f64, v: f64)| {
            let mut result = TValue::new_bool(false);
            let script = &format!("math.min({l}, {r:e}, {v:e})");
            let expected = lua.load(script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_int(l),
                TValue::new_float(r),
                TValue::new_float(v),
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 3, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "Expected {} from {}: found {}", expected, script, result);
        });
    }

    #[test]
    fn test_min_mixed_trips2() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64, v: f64)| {
            let mut result = TValue::new_bool(false);
            let script = &format!("math.min({l}, {r}, {v:e})");
            let expected = lua.load(script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_int(l),
                TValue::new_int(r),
                TValue::new_float(v),
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 3, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "Expected {} from ({}, {}, {}): found {}", expected, l, r, v, result);
        });
    }

    #[test]
    fn test_min_mixed_trips3() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: f64, v: i64)| {
            let mut result = TValue::new_bool(false);
            let script = &format!("math.min({l}, {r:e}, {v})");
            let expected = lua.load(script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_int(l),
                TValue::new_float(r),
                TValue::new_int(v),
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 3, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "Expected {} from ({}, {}, {}): found {}", expected, l, r, v, result);
        });
    }

    #[test]
    fn test_min_mixed_trips4() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: i64, v: i64)| {
            let mut result = TValue::new_bool(false);
            let script = &format!("math.min({l:e}, {r}, {v})");
            let expected = lua.load(script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_float(l),
                TValue::new_int(r),
                TValue::new_int(v),
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 3, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "Expected {} from ({}, {}, {}): found {}", expected, l, r, v, result);
        });
    }

    #[test]
    fn test_min_mixed_trips5() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64, v: i64)| {
            let mut result = TValue::new_bool(false);
            let script = &format!("math.min({l:e}, {r:e}, {v})");
            let expected = lua.load(script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            let args = &mut [
                TValue::new_float(l),
                TValue::new_float(r),
                TValue::new_int(v),
            ];
            unsafe {
                crate::math::min(args.as_mut_ptr(), 3, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "Expected {} from ({}, {}, {}): found {}", expected, l, r, v, result);
        });
    }

    #[test]
    fn test_log_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::log(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("math.log({l:e}, {r:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_log_ints() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::log(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("math.log({l}, {r})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_log_mixed() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: i64, r: f64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::log(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("math.log({l}, {r:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_log_mixed2() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: i64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::log(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("math.log({l:e}, {r})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_atan_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut rhs = TValue::new_float(format!("{r:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::atan(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("math.atan({l:e}, {r:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_ceil_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::ceil(&mut lhs, &mut result);
            }
            let script = format!("math.ceil({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_cos_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::cos(&mut lhs, &mut result);
            }
            let script = format!("math.cos({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_deg_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::deg(&mut lhs, &mut result);
            }
            let script = format!("math.deg({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_rad_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::rad(&mut lhs, &mut result);
            }
            let script = format!("math.rad({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_sin_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::sin(&mut lhs, &mut result);
            }
            let script = format!("math.sin({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_sqrt_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::sqrt(&mut lhs, &mut result);
            }
            let script = format!("math.sqrt({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_tan_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::tan(&mut lhs, &mut result);
            }
            let script = format!("math.tan({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_exp_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::exp(&mut lhs, &mut result);
            }
            let script = format!("math.exp({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_floor_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::floor(&mut lhs, &mut result);
            }
            let script = format!("math.floor({l:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_fmod_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut rhs = TValue::new_float(format!("{r:e}").parse().unwrap());
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::fmod(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("math.fmod({l:e}, {r:e})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    #[test]
    fn test_modf_floats() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(format!("{l:e}").parse().unwrap());
            let mut result_i = TValue::new_bool(false);
            let mut result_r = TValue::new_bool(false);

            unsafe {
                crate::math::modf(&mut lhs, &mut result_i, &mut result_r);
            }
            let script = format!("math.modf({l:e})");
            let (expected_i, expected_r) = lua.load(&script).eval().map(convert_expectation2).unwrap_or_else(|_| (TValue::new_bool(false), TValue::new_bool(false)));
            if !(expected_i.is_nan() && result_i.is_nan()) {
                proptest::prop_assert_eq!(&expected_i, &result_i, "{} != {:?} found {:?}",
                    script, expected_i, result_i,
                );
            }
            if !(expected_r.is_nan() && result_r.is_nan()) {
                proptest::prop_assert_eq!(&expected_i, &result_i, "{} != {:?} found {:?}",
                    script, expected_i, result_i,
                );
            }
        });
    }

    #[test]
    fn test_modf_ints() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut result_i = TValue::new_bool(false);
            let mut result_r = TValue::new_bool(false);

            unsafe {
                crate::math::modf(&mut lhs, &mut result_i, &mut result_r);
            }
            let script = format!("math.modf({l})");
            let (expected_i, expected_r) = lua.load(&script).eval().map(convert_expectation2).unwrap_or_else(|_| (TValue::new_bool(false), TValue::new_bool(false)));
            if !(expected_i.is_nan() && result_i.is_nan()) {
                proptest::prop_assert_eq!(&expected_i, &result_i, "{} != {:?} found {:?}",
                    script, expected_i, result_i,
                );
            }
            if !(expected_r.is_nan() && result_r.is_nan()) {
                proptest::prop_assert_eq!(&expected_i, &result_i, "{} != {:?} found {:?}",
                    script, expected_i, result_i,
                );
            }
        });
    }

    #[test]
    fn test_ult_ints() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                crate::math::ult(&mut lhs, &mut rhs, &mut result);
            }
            let script = format!("math.ult({l}, {r})");
            let expected = lua.load(&script).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{} != {:?} found {:?}",
                    script, expected, result,
                );
            }
        });
    }

    fn convert_expectation(v: mlua::MultiValue) -> TValue {
        match v.get(0) {
            Some(v) => convert_single_value(v),
            first => {
                let mut msg = format!("eval returned non-number: {first:?}");
                if let Some(err) = v.get(1) {
                    msg.push_str(&format!(" {err:?}"));
                }
                println!("{msg}");
                TValue::new_bool(false)
            }
        }
    }
    fn convert_expectation2(v: mlua::MultiValue) -> (TValue, TValue) {
        match (v.get(0), v.get(1)) {
            (None, None) => (TValue::new_bool(false), TValue::new_nil()),
            (None, Some(_)) => (TValue::new_bool(false), TValue::new_nil()),
            (Some(one), None) => (convert_single_value(one), TValue::new_nil()),
            (Some(one), Some(two)) => (convert_single_value(one), convert_single_value(two)),
        }
    }

    fn convert_single_value(v: &mlua::Value) -> TValue {
        match v {
            mlua::Value::Integer(i) => TValue::new_int(*i),
            mlua::Value::Number(f) => TValue::new_float(*f),
            mlua::Value::Boolean(b) => TValue::new_bool(*b),
            _ => TValue::new_bool(false),
        }
    }
}
