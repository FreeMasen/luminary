use common::check_return_code;
mod common;

#[test]
fn bool_eq() {
    let cmps = [
        ("true == true", 1),
        ("true == false", 0),
        ("false == false", 1),
        ("false == true", 0)
    ];
    for (i, (cmp, exit)) in cmps.into_iter().enumerate() {
        let lua = format!("return {cmp}");
        let test = common::setup(&format!("{}{i}", std::thread::current().name().unwrap()));
        let (d, s) = test.run_lua(&lua);
        check_return_code(&d, exit);
        check_return_code(&s, exit);
    }
}

#[test]
fn int_eq() {
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(l: i64)| {
        let cmps = [
            (format!("({l}) == ({l})"), 1),
            (format!("({l}) == ({})", l.wrapping_add(1)), 0),
        ];
        for (i, (cmp, exit)) in cmps.into_iter().enumerate() {
            let lua = format!("return ({cmp})");
            let test = common::setup(&format!("{thread_name}{l}{i}"));
            let (d, s) = test.run_lua(&lua);
            check_return_code(&d, exit);
            check_return_code(&s, exit);
        }
    })
}

#[test]
fn str_eq() {
    let thread_name = std::thread::current().name().unwrap().to_string();
    proptest::proptest!(|(s in "[a-zA-Z0-9 -_]*")| {
        let cmps = [
            (format!("('{s}') == (\"{s}\")"), 1),
            (format!("('{s}') == false"), 0),
        ];
        for (i, (cmp, exit)) in cmps.into_iter().enumerate() {
            let lua = format!("return ({cmp})");
            let test = common::setup(&format!("{thread_name}{s}{i}"));
            let (d, s) = test.run_lua(&lua);
            check_return_code(&d, exit);
            check_return_code(&s, exit);
        }
    })
}
