use std::{fmt, str::FromStr};

use common::check_test;
use proptest::test_runner::TestCaseError;
mod common;

#[test]
fn add() {
    let lua = mlua::Lua::new();
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64, r: i64)| {
        math_prop_test_integer::<i64>(l, r, "+", &lua, &thread_name).unwrap();
    });
}

#[test]
fn sub() {
    let lua = mlua::Lua::new();
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64, r: i64)| {
        math_prop_test_integer::<i64>(l, r, "-", &lua, &thread_name).unwrap();
    });
}

#[test]
fn mul() {
    let lua = mlua::Lua::new();
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64, r: i64)| {
        math_prop_test_integer::<i64>(l, r, "*", &lua, &thread_name).unwrap();
    });
}

#[test]
fn div() {
    let lua = mlua::Lua::new();
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64, r: i64)| {
        if r == 0 {
            return Ok(())
        }
        math_prop_test_integer::<f64>(l, r, "/", &lua, &thread_name).unwrap();
    });
}

#[test]
fn floor_div() {
    let lua = mlua::Lua::new();
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64, r: i64)| {
        if r == 0 {
            return Ok(())
        }
        math_prop_test_integer::<f64>(l, r, "//", &lua, &thread_name).unwrap()
    });
}

#[test]
fn pow() {
    let lua = mlua::Lua::new();
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64, r: i64)| {
        math_prop_test_integer::<f64>(l, r, "^", &lua, &thread_name).unwrap()
    });
}

#[test]
fn rem() {
    let lua = mlua::Lua::new();
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64, r: i64)| {
        if r == 0 {
            return Ok(())
        }
        math_prop_test_integer::<f64>(l, r, "%", &lua, &thread_name).unwrap()
    });
}

fn math_prop_test_integer<'lua, R>(
    lhs: i64,
    rhs: i64,
    op: &str,
    l: &'lua mlua::Lua,
    name: &str,
) -> Result<(), TestCaseError> 
where R: Copy + FromStr + ToString + PartialEq + fmt::Display + fmt::Debug + Default + mlua::FromLua<'lua>,
    <R as FromStr>::Err: fmt::Display + fmt::Debug
{
    // Because mlua doesn't allow for capturing the output
    // of a function (print statements), we need to evaluate these operations
    // slightly differently, for the mlua, we are just using
    // the expression wrapped in parens but for the binary
    // test we are using the print function to output our
    // value to stdout and then we parse that.
    let expr = format!("({lhs}{op}({rhs}))");
    let script = format!("print{expr}");
    let name = format!("{name}_{lhs}_{rhs}");
    let test = common::setup(name.as_str());
    let expected: R = l
            .load(&expr).eval().map(|v: R| {
            v
        }).unwrap_or_default();
    let (d, s) = test.run_lua(&script);
    check_test(&d);
    check_test(&s);
    let (v1, v_str1) = convert_stdout(&d.stdout).unwrap();
    let (v2, v_str2) = convert_stdout(&s.stdout).unwrap();
    check_converted(v1, expected, &v_str1)?;
    check_converted(v2, expected, &v_str2)?;
    Ok(())
}


fn convert_stdout<T>(stdout: &[u8]) -> Result<(T, String), TestCaseError> 
where T: FromStr, <T as FromStr>::Err: fmt::Display + fmt::Debug
{
    let stdout = String::from_utf8_lossy(stdout);
    let v_str = stdout.lines().next().expect(">= 1 line");
    let v = match v_str.parse::<T>() {
        Ok(v) => v,
        Err(e) => {
            proptest::prop_assert!(false, "Failed to parse `{}`: {}", v_str, e);
            panic!();
        }
    };
    Ok((v, v_str.to_string()))
}

fn check_converted<T>(lhs: T, expected: T, rhs: &str) -> Result<(), TestCaseError> 
where T: ToString + PartialEq + fmt::Display + fmt::Debug
{
    if lhs.to_string() != rhs {
        eprintln!("Error paring:\ns: `{rhs}`\ni: {lhs}")
    } else {
        proptest::prop_assert_eq!(expected, lhs);
    };
    Ok(())
}
