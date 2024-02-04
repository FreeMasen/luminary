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

fn atan_(lhs: f64, rhs: Option<f64>) -> f64 {
    if let Some(r) = rhs {
        lhs.atan2(r)
    } else {
        lhs.atan()
    }
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
    *o = TValue::new_float(atan_(
        l,
        rhs.as_ref().and_then(|tv| {
            Some(if tv.tag == tags::FLOAT {
                unsafe { tv.value.f }
            } else if tv.tag == tags::INTEGER {
                unsafe { tv.value.i as f64 }
            } else {
                return None;
            })
        }),
    ));
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

fn int_fits_float(i: i64) -> bool {
    -(1 << f64::MANTISSA_DIGITS) <= i && i <= (1 << f64::MANTISSA_DIGITS)
}

fn int_less_than_float(i: i64, f: f64) -> bool {
    if int_fits_float(i) {
        return (i as f64) < f
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
        current_min = dbg!(pass);
    }
    *out = current_min;
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn fmod(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    rem(lhs, rhs, out)
}

#[runtime_macros::std_tvalue_export(module = "math")]
pub unsafe extern "C" fn modf(lhs: *mut TValue, out: *mut TValue) {
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
            let expected = lua.load(format!("{}/{}", l, r, )).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
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

            let expected = lua.load(format!("{}//{}", l, r, )).eval().map(convert_expectation).unwrap_or_else(|_| TValue::new_bool(false));
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

            let expected = lua.load(format!("{l}%{r}")).eval().map(convert_expectation).unwrap_or_else(|_| {
                println!("invalid float rem operation: {l} % {r}");
                TValue::new_bool(false)
            });
            if expected.tag == tags::INTEGER && !super::int_fits_float(unsafe { expected.value.i }) {
                return Ok(())
            }
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
    fn test_log() {
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

    fn convert_expectation(v: mlua::Value) -> TValue {
        match v {
            mlua::Value::Integer(i) => TValue::new_int(i),
            mlua::Value::Number(f) => TValue::new_float(f),
            _ => TValue::new_bool(false)
        }
    }
}
