use std::process::Command;

mod common;

#[test]
fn add_static() {
    let lua = r#"return 1 + 1"#;
    let test = common::setup("add_static");
    let bin_path = test.build_static(lua);
    println!("Executing add binary");
    let exec = Command::new(bin_path)
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();
    assert_eq!(exec.status.code(), Some(2));
}

#[test]
fn add_dynamic() {
    let lua = mlua::Lua::new();
    proptest::proptest!(|(l: i64, r: i64)| {
        // Because mlua doesn't allow for capturing the output
        // of a function, we need to evaluate these operations
        // slightly differently, for the mlua, we are just using
        // the expression wrapped in parens but for the binary
        // test we are using the print function to output our
        // value to stdout and then we parse that.
        let expr = format!("({l}+{r})");
        let script = format!("print{expr}");

        let expected = lua
            .load(&expr).eval().map(|v: i64| {
            v
        }).unwrap_or(0);
        let test = common::setup("add_dynamic");
        let bin_path = test.build_dynamic(&script);
        let exec = test.run_dynamic(&bin_path);
        let stdout = String::from_utf8_lossy(&exec.stdout);
        if !exec.status.success() {
            panic!("Failed to exec bin:\n{}\n{stdout}", String::from_utf8_lossy(&exec.stderr));
        }
        let v_str = stdout.lines().next().expect(">= 1 line");
        let v: i64 = v_str.parse().unwrap();
        if v.to_string() != v_str {
            eprintln!("Error paring:\ns: {v_str}\ni: {v}")
        } else {
            proptest::prop_assert_eq!(expected, v);
        }
    });
}

// #[cfg(test)]
// mod tests {
//     use crate::TValue;

//     #[test]
//     fn int_add() {

//     }

//     #[test]
//     fn float_add() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: f64, r: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut rhs = TValue::new_float(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{}+{}", l, r)).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::add(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn int_neg() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("-({})", l)).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::neg(&mut lhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn float_neg() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("-({})", l)).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::neg(&mut lhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn int_sub() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("({})-({})", l, r)).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::sub(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn float_sub() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: f64, r: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut rhs = TValue::new_float(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("({})-({})", l, r)).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::sub(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn int_mul() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{}*{}", l, r)).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::mul(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn float_mul() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: f64, r: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut rhs = TValue::new_float(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{}*{}", l, r)).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::mul(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn int_div() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{}/{}", l, r)).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::div(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn float_div() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: f64, r: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut rhs = TValue::new_float(r);
//             let mut result = TValue::new_bool(false);
//             unsafe {
//                 crate::math::div(&mut lhs, &mut rhs, &mut result);
//             }
//             let expected = lua.load(format!("{}/{}", l, r, )).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             if !(expected.is_nan() && result.is_nan()) {
//                 proptest::prop_assert_eq!(&expected, &result, "{:?} / {:?} != {:?} found {:?}",
//                     lhs, rhs, expected, result,
//                 );
//             }
//         });
//     }

//     #[test]
//     fn int_floor_div() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{}//{}", l, r)).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::floor_div(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn float_floor_div() {
//         let lua = mlua::Lua::new();

//         proptest::proptest!(|(l: f64, r: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut rhs = TValue::new_float(r);
//             let mut result = TValue::new_bool(false);

//             unsafe {
//                 crate::math::floor_div(&mut lhs, &mut rhs, &mut result);
//             }

//             let expected = lua.load(format!("{}//{}", l, r, )).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             if !(expected.is_nan() && result.is_nan()) {
//                 proptest::prop_assert_eq!(&expected, &result, "{:?} // {:?} != {:?} found {:?}",
//                     lhs, rhs, expected, result,
//                 );
//             }
//         });
//     }

//     #[test]
//     fn int_rem() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{}%{}", l, r)).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::rem(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(&expected, &result, "{} % {} != {} ({})", lhs, rhs, expected, result);
//         });
//     }

//     #[test]
//     fn float_rem() {
//         let lua = mlua::Lua::new();

//         proptest::proptest!(|(l: f64, r: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut rhs = TValue::new_float(r);
//             let mut result = TValue::new_bool(false);

//             unsafe {
//                 crate::math::rem(&mut lhs, &mut rhs, &mut result);
//             }

//             let expected = lua.load(format!("{}%{}", l, r, )).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             if !(expected.is_nan() && result.is_nan()) {
//                 proptest::prop_assert_eq!(&expected, &result, "{:?} % {:?} != {:?} found {:?}",
//                     lhs, rhs, expected, result,
//                 );
//             }
//         });
//     }

//     #[test]
//     fn int_exp() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("({})^({})", l, r)).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));

//             unsafe {
//                 crate::math::pow(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(&expected, &result, "{:?} ^ {:?} != {:?} found {:?}", lhs, rhs, expected, result);
//         });
//     }

//     #[test]
//     fn float_floor_exp() {
//         let lua = mlua::Lua::new();

//         proptest::proptest!(|(l: f64, r: f64)| {
//             let mut lhs = TValue::new_float(l);
//             let mut rhs = TValue::new_float(r);
//             let mut result = TValue::new_bool(false);

//             unsafe {
//                 crate::math::pow(&mut lhs, &mut rhs, &mut result);
//             }

//             let expected = lua.load(format!("({})^({})", l, r, )).eval().map(|v: f64| {
//                 TValue::new_float(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             if !(expected.is_nan() && result.is_nan()) {
//                 proptest::prop_assert_eq!(&expected, &result, "{:?} ^ {:?} != {:?} found {:?}",
//                     lhs, rhs, expected, result,
//                 );
//             }
//         });
//     }

//     #[test]
//     fn int_bin_and() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{l}&{r}")).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             unsafe {
//                 crate::math::bin_and(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(&expected, &result, "{:?} & {:?} != {:?} found {:?}",
//                 lhs, rhs, expected, result,
//             );
//         });
//     }

//     #[test]
//     fn int_bin_or() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{l}|{r}")).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             unsafe {
//                 crate::math::bin_or(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn int_bin_xor() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64, r: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut rhs = TValue::new_int(r);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("{l}~{r}")).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             unsafe {
//                 crate::math::bin_xor(&mut lhs, &mut rhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }

//     #[test]
//     fn int_bin_not() {
//         let lua = mlua::Lua::new();
//         proptest::proptest!(|(l: i64)| {
//             let mut lhs = TValue::new_int(l);
//             let mut result = TValue::new_bool(false);
//             let expected = lua.load(&format!("~{l}")).eval().map(|v: i64| {
//                 TValue::new_int(v)
//             }).unwrap_or_else(|_| TValue::new_bool(false));
//             unsafe {
//                 crate::math::bin_not(&mut lhs, &mut result);
//             }
//             proptest::prop_assert_eq!(expected, result);
//         });
//     }
// }
