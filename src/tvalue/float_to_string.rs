use inkwell::{
    intrinsics::Intrinsic,
    module::Module,
    types::BasicType,
    values::{AnyValue, FunctionValue},
    FloatPredicate, IntPredicate,
};

pub mod names {
    pub const FLOAT_TO_STRING: &str = "std::float_to_string::float_to_string";
    pub const INT_TO_STRING: &str = "std::float_to_string::int_to_string";
    /// Name of a function that will append the argument to the provided buffer at the provided
    /// length. This function will shift the provided integer by `'0'` to be in the ascii/utf8
    /// integer space. The length pointer provided will be incremented by 1
    /// 
    /// ```llvmir
    /// call void @append(ptr %buf_ptr, ptr %len_ptr, i8 1)
    /// ```
    /// 
    /// the above example will append the character `'1'` to the buffer
    pub(super) const APPEND: &str = "std::float_to_string::append";
    /// Name of a function into the module that will append the provided i8 into the buffer
    /// at the postion contained in the length argument.
    pub(super) const APPEND_CHAR: &str = "std::float_to_string::append_char";
    pub(super) const CALCULATE_MAGNITUDE: &str = "std::float_to_string::calculate_magnitude";
    pub(super) const CHECK_FOR_NEG: &str = "std::float_to_string::check_for_neg";
    pub(super) const LOOP_SHOULD_CONTINUE: &str = "std::float_to_string::loop_should_continue";
    pub(super) const PRECISION_LOOP_STEP: &str = "std::float_to_string::precision_loop_step";
}

pub fn add_float_to_string<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let _append_char = emit_append_char(module);
    let _append = emit_append(module);
    let precision = module.add_global(c.f32_type(), Default::default(), "PRECISION");
    precision.set_initializer(&c.f32_type().const_float(0.0001));
    precision.set_constant(true);
    let f = module.add_function(
        names::FLOAT_TO_STRING,
        c.i32_type().fn_type(
            &[
                c.f32_type().into(),
                c.i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
            ],
            false,
        ),
        None,
    );
    let value = f.get_first_param().unwrap().into_float_value();
    value.set_name("value");
    let dest = f.get_last_param().unwrap().into_pointer_value();
    dest.set_name("dest");
    b.position_at_end(c.append_basic_block(f, "entry"));
    let isnan = c.append_basic_block(f, "isnan");
    let isnnan = c.append_basic_block(f, "isnnan");
    let n = b.build_float_compare(
        inkwell::FloatPredicate::UNO,
        value,
        c.f32_type().const_zero(),
        "n",
    );
    b.build_conditional_branch(n, isnan, isnnan);
    b.position_at_end(isnan);
    b.build_store(dest, c.const_string(b"nan", false));
    b.build_return(Some(&c.i32_type().const_int(3, false)));

    b.position_at_end(isnnan);
    let inf = b.build_float_compare(
        inkwell::FloatPredicate::OEQ,
        value,
        c.f32_type().const_float(f64::INFINITY),
        "inf",
    );
    let isinf = c.append_basic_block(f, "isinf");
    let isninf = c.append_basic_block(f, "isninf");
    b.build_conditional_branch(inf, isinf, isninf);
    b.position_at_end(isinf);
    b.build_store(dest, c.const_string(b"inf", false));
    b.build_return(Some(&c.i32_type().const_int(3, false)));

    b.position_at_end(isninf);
    let z = b.build_float_compare(
        inkwell::FloatPredicate::OEQ,
        value,
        c.f32_type().const_float(0.0),
        "z",
    );
    let isz = c.append_basic_block(f, "isz");
    let isnz = c.append_basic_block(f, "isnz");
    b.build_conditional_branch(z, isz, isnz);
    b.position_at_end(isz);
    b.build_store(dest, c.const_string(b"0", false));
    b.build_return(Some(&c.i32_type().const_int(1, false)));

    b.position_at_end(isnz);
    let m_ptr = b.build_alloca(c.i32_type(), "m_ptr");

    let n_ptr = b.build_alloca(c.f32_type(), "n_ptr");
    b.build_store(n_ptr, value);
    let ret_ptr = b.build_alloca(c.f32_type(), "ret_ptr");
    b.build_store(ret_ptr, c.i32_type().const_zero());
    let check_for_neg = emit_check_for_neg(module);

    let _neg = b
        .build_call(
            check_for_neg,
            &[dest.into(), n_ptr.into(), ret_ptr.into()],
            "neg",
        )
        .as_any_value_enum()
        .into_int_value();

    let n = b.build_load(c.f32_type(), n_ptr, "n").into_float_value();
    let calculate_magnitude = emit_calculate_magnitude(module);
    let m = b
        .build_call(calculate_magnitude, &[n.into()], "m")
        .as_any_value_enum()
        .into_int_value();
    b.build_store(m_ptr, m);
    let loop_top = c.append_basic_block(f, "loop_top");
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_top);
    let loop_should_continue = emit_loop_should_continue(module);
    let loop_step = emit_loop_step(module);
    let should_continue = b
        .build_call(
            loop_should_continue,
            &[n_ptr.into(), m_ptr.into()],
            "should_continue",
        )
        .as_any_value_enum()
        .into_int_value();
    let loop_exit = c.append_basic_block(f, "loop_exit");
    let loop_body = c.append_basic_block(f, "loop_body");
    b.build_conditional_branch(should_continue, loop_body, loop_exit);

    b.position_at_end(loop_body);
    b.build_call(
        loop_step,
        &[dest.into(), n_ptr.into(), m_ptr.into(), ret_ptr.into()],
        "_",
    );
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_exit);
    let ret = b.build_load(c.i32_type(), ret_ptr, "ret");
    b.build_return(Some(&ret));

    f
}

fn emit_append<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let append_char = module.get_function(names::APPEND_CHAR).unwrap();
    let f = module.add_function(
        names::APPEND,
        c.void_type().fn_type(
            &[
                c.i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
                c.i32_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
                c.i8_type().into(),
            ],
            false,
        ),
        None,
    );
    let buf_ptr = f.get_first_param().unwrap().into_pointer_value();
    buf_ptr.set_name("buf_ptr");
    let len_ptr = f.get_nth_param(1).unwrap().into_pointer_value();
    len_ptr.set_name("len_ptr");
    let value = f.get_last_param().unwrap().into_int_value();
    value.set_name("value");
    b.position_at_end(c.append_basic_block(f, "entry"));

    let ch = b.build_int_add(c.i8_type().const_int(b'0' as _, false), value, "ch");
    b.build_call(
        append_char,
        &[buf_ptr.into(), len_ptr.into(), ch.into()],
        "_",
    );
    b.build_return(None);
    f
}


/// Emit a function into the module that will append the provided i8 into the buffer
/// at the postion contained in the length argument. 
fn emit_append_char<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        names::APPEND_CHAR,
        c.void_type().fn_type(
            &[
                c.i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
                c.i32_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
                c.i8_type().into(),
            ],
            false,
        ),
        None,
    );
    let buf_ptr = f.get_first_param().unwrap().into_pointer_value();
    buf_ptr.set_name("buf_ptr");
    let len_ptr = f.get_nth_param(1).unwrap().into_pointer_value();
    len_ptr.set_name("len_ptr");
    let value = f.get_last_param().unwrap().into_int_value();
    value.set_name("value");
    b.position_at_end(c.append_basic_block(f, "entry"));
    let len = b
        .build_load(c.i32_type(), len_ptr, "len")
        .as_any_value_enum()
        .into_int_value();
    let next_len = b.build_int_add(len, c.i32_type().const_int(1, false), "next_len");
    let ch_ptr = unsafe {
        b.build_gep(
            c.i8_type().array_type(0),
            buf_ptr,
            &[c.i32_type().const_zero().into(), len.into()],
            "ch_ptr",
        )
    };
    b.build_store(ch_ptr, value);
    b.build_store(len_ptr, next_len);
    b.build_return(None);
    f
}

fn emit_check_for_neg<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        names::CHECK_FOR_NEG,
        c.bool_type().fn_type(
            &[
                c.i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
                c.f32_type().ptr_type(Default::default()).into(),
                c.i32_type().ptr_type(Default::default()).into(),
            ],
            false,
        ),
        None,
    );
    let entry = c.append_basic_block(f, "entry");
    b.position_at_end(entry);
    let buf = f
        .get_first_param()
        .unwrap()
        .as_any_value_enum()
        .into_pointer_value();
    buf.set_name("buf");
    let n_ptr = f
        .get_nth_param(1)
        .unwrap()
        .as_any_value_enum()
        .into_pointer_value();
    n_ptr.set_name("n_ptr");
    let len_ptr = f
        .get_last_param()
        .unwrap()
        .as_any_value_enum()
        .into_pointer_value();
    let n = b.build_load(c.f32_type(), n_ptr, "n").into_float_value();
    let neg = b.build_float_compare(FloatPredicate::OLT, n, c.f32_type().const_zero(), "neg");
    let is_neg = c.append_basic_block(f, "is_neg");
    let not_neg = c.append_basic_block(f, "not_neg");
    b.build_conditional_branch(neg, is_neg, not_neg);
    b.position_at_end(not_neg);
    b.build_return(Some(&c.bool_type().const_zero()));

    b.position_at_end(is_neg);
    let inverted = b.build_float_neg(n, "inverted");
    b.build_store(n_ptr, inverted);

    let append_char = module.get_function(names::APPEND_CHAR).unwrap();
    b.build_call(
        append_char,
        &[
            buf.into(),
            len_ptr.into(),
            c.i8_type().const_int(b'-' as _, false).into(),
        ],
        "_",
    );

    b.build_return(Some(&neg));

    f
}

fn emit_calculate_magnitude<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        names::CALCULATE_MAGNITUDE,
        c.i32_type().fn_type(&[c.f32_type().into()], false),
        None,
    );
    let entry = c.append_basic_block(f, "entry");
    b.position_at_end(entry);
    let n = f.get_first_param().unwrap().into_float_value();
    n.set_name("n");

    let log10_intrinsic = Intrinsic::find("llvm.log10.f32").expect("llvm.pow.f32");
    let log10_f = log10_intrinsic
        .get_declaration(module, &[c.f32_type().as_basic_type_enum()])
        .unwrap();
    let mf = b
        .build_call(log10_f, &[n.into()], "mf")
        .as_any_value_enum()
        .into_float_value();
    let lt1 = c.append_basic_block(f, "lt1");
    let ge1 = c.append_basic_block(f, "ge1");
    let mf_lt1 = b.build_float_compare(
        FloatPredicate::OLT,
        mf,
        c.f32_type().const_float(1.0),
        "mf_lt1",
    );
    b.build_conditional_branch(mf_lt1, lt1, ge1);

    b.position_at_end(lt1);
    b.build_return(Some(&c.i32_type().const_zero()));

    b.position_at_end(ge1);
    let m = b.build_float_to_signed_int(mf, c.i32_type(), "m");
    b.build_return(Some(&m));

    f
}

fn emit_loop_should_continue<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        names::LOOP_SHOULD_CONTINUE,
        c.bool_type().fn_type(
            &[
                c.f32_type().ptr_type(Default::default()).into(),
                c.i32_type().ptr_type(Default::default()).into(),
            ],
            false,
        ),
        None,
    );
    let n_ptr = f
        .get_first_param()
        .unwrap()
        .as_any_value_enum()
        .into_pointer_value();
    n_ptr.set_name("n_ptr");
    let m_ptr = f
        .get_last_param()
        .unwrap()
        .as_any_value_enum()
        .into_pointer_value();
    m_ptr.set_name("m_ptr");
    b.position_at_end(c.append_basic_block(f, "entry"));
    let check_m = c.append_basic_block(f, "check_m");
    let ret_true = c.append_basic_block(f, "ret_true");
    let ret_false = c.append_basic_block(f, "ret_false");
    let n = b.build_load(c.f32_type(), n_ptr, "n").into_float_value();
    let m = b.build_load(c.i32_type(), m_ptr, "m").into_int_value();
    let prec = b
        .build_load(
            c.f32_type(),
            module.get_global("PRECISION").unwrap().as_pointer_value(),
            "prec",
        )
        .into_float_value();
    let n_gt_prec = b.build_float_compare(FloatPredicate::OGT, n, prec, "n_gt_prec");
    b.build_conditional_branch(n_gt_prec, ret_true, check_m);

    b.position_at_end(check_m);
    let m_ge_z = b.build_int_compare(IntPredicate::SGE, m, c.i32_type().const_zero(), "m_ge_z");
    b.build_conditional_branch(m_ge_z, ret_true, ret_false);

    b.position_at_end(ret_true);
    b.build_return(Some(&c.bool_type().const_int(1, false)));

    b.position_at_end(ret_false);
    b.build_return(Some(&c.bool_type().const_zero()));
    f
}

fn emit_loop_step<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        names::PRECISION_LOOP_STEP,
        c.void_type().fn_type(
            &[
                c.i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
                c.f32_type().ptr_type(Default::default()).into(),
                c.i32_type().ptr_type(Default::default()).into(),
                c.i32_type().ptr_type(Default::default()).into(),
            ],
            false,
        ),
        None,
    );
    b.position_at_end(c.append_basic_block(f, "entry"));
    let buf = f.get_first_param().unwrap().into_pointer_value();
    buf.set_name("buf");
    let n_ptr = f.get_nth_param(1).unwrap().into_pointer_value();
    n_ptr.set_name("n_ptr");
    let m_ptr = f.get_nth_param(2).unwrap().into_pointer_value();
    m_ptr.set_name("m_ptr");
    let len_ptr = f.get_last_param().unwrap().into_pointer_value();
    len_ptr.set_name("len");
    let n = b.build_load(c.f32_type(), n_ptr, "n").into_float_value();
    let m = b.build_load(c.i32_type(), m_ptr, "m").into_int_value();
    let mf = b.build_signed_int_to_float(m, c.f32_type(), "m");
    let ten = c.f32_type().const_float(10.0);
    let pow_intrinsic = Intrinsic::find("llvm.pow.f32").expect("llvm.pow.f32");
    let pow_intrinsic_fn = pow_intrinsic
        .get_declaration(&module, &[c.f32_type().as_basic_type_enum()])
        .unwrap();
    let weight = b
        .build_call(pow_intrinsic_fn, &[ten.into(), mf.into()], "weight")
        .as_any_value_enum()
        .into_float_value();
    let weight_gt_zero = b.build_float_compare(
        FloatPredicate::OGT,
        weight,
        c.f32_type().const_zero(),
        "weight_gt_zero",
    );
    let wgtz = c.append_basic_block(f, "wgtz");
    let digit_bb = c.append_basic_block(f, "digit_bb");
    let after_digit = c.append_basic_block(f, "after_digit");
    b.build_conditional_branch(weight_gt_zero, wgtz, after_digit);

    b.position_at_end(wgtz);
    let weight_is_inf = b.build_float_compare(
        FloatPredicate::OEQ,
        weight,
        c.f32_type().const_float(f32::INFINITY as _),
        "weight_is_inf",
    );
    b.build_conditional_branch(weight_is_inf, after_digit, digit_bb);

    b.position_at_end(digit_bb);
    let digit_pre = b.build_float_div(n, weight, "digit_pre");
    let floor = Intrinsic::find("llvm.floor").unwrap();
    let floor_fn = floor
        .get_declaration(&module, &[c.f32_type().into()])
        .unwrap();
    let digit_pre2 = b
        .build_call(floor_fn, &[digit_pre.into()], "digit_pre2")
        .as_any_value_enum()
        .into_float_value();
    let n_update = b.build_float_mul(digit_pre2, weight, "n_update");
    let digit = b.build_float_to_unsigned_int(digit_pre2, c.i8_type(), "digit");
    let append_char = module.get_function(names::APPEND_CHAR).unwrap(); // emit_append_char(module);
    let append = module.get_function(names::APPEND).unwrap(); //emit_append(module);
    b.build_call(append, &[buf.into(), len_ptr.into(), digit.into()], "_");
    let new_n = b.build_float_sub(n, n_update, "new_n");
    b.build_store(n_ptr, new_n);
    b.build_unconditional_branch(after_digit);

    b.position_at_end(after_digit);
    let m_eq_z = b.build_int_compare(IntPredicate::EQ, m, c.i32_type().const_zero(), "m_eq_z");
    let m_eq_z_bb = c.append_basic_block(f, "m_eq_z_bb");
    let handle_dot = c.append_basic_block(f, "handle_dot");
    let after_dot = c.append_basic_block(f, "after_dot");
    b.build_conditional_branch(m_eq_z, m_eq_z_bb, after_dot);

    b.position_at_end(m_eq_z_bb);
    let n = b.build_load(c.f32_type(), n_ptr, "n").into_float_value();
    let n_gt_z = b.build_float_compare(FloatPredicate::OGT, n, c.f32_type().const_zero(), "n_gt_z");
    b.build_conditional_branch(n_gt_z, handle_dot, after_dot);

    b.position_at_end(handle_dot);
    b.build_call(
        append_char,
        &[
            buf.into(),
            len_ptr.into(),
            c.i8_type().const_int(b'.' as u64, false).into(),
        ],
        "_",
    );

    b.build_unconditional_branch(after_dot);

    b.position_at_end(after_dot);
    let new_m = b.build_int_sub(m, c.i32_type().const_int(1, false), "new_m");
    b.build_store(m_ptr, new_m);
    b.build_return(None);

    f
}


fn emit_int_to_buffer<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(names::INT_TO_STRING, c.i32_type().fn_type(&[
        c.i32_type().into(),
        c.i8_type().array_type(0).ptr_type(Default::default()).into(),
    ], false), None);
    let value = f.get_first_param().unwrap().into_int_value();
    value.set_name("value");
    let dest = f.get_last_param().unwrap().into_pointer_value();
    dest.set_name("dest");
    
    b.position_at_end(c.append_basic_block(f, "entry"));
    let zero = c.append_basic_block(f, "zero");
    let loop_prep = c.append_basic_block(f, "loop_prep");
    let handle_neg = c.append_basic_block(f, "handle_neg");
    let loop_top = c.append_basic_block(f, "loop_top");
    let loop_body = c.append_basic_block(f, "loop_body");
    let loop_exit = c.append_basic_block(f, "loop_exit");
    let append_neg = c.append_basic_block(f, "append_neg");
    let exit = c.append_basic_block(f, "exit");
    let n_ptr = b.build_alloca(value.get_type(), "n_ptr");
    b.build_store(n_ptr, value);
    let len_ptr = b.build_alloca(c.i32_type(), "len_ptr");
    b.build_store(len_ptr, c.i32_type().const_zero());
    
    let viz = b.build_int_compare(IntPredicate::EQ, value, c.i32_type().const_zero(), "viz");
    b.build_conditional_branch(viz, zero, loop_prep);
    
    b.position_at_end(zero);
    b.build_store(dest, c.const_string(b"0", false));
    b.build_return(Some(&c.i32_type().const_int(1, false)));
    
    b.position_at_end(loop_prep);
    let neg = b.build_int_compare(IntPredicate::SLT, value, c.i32_type().const_zero(), "neg");
    b.build_conditional_branch(neg, handle_neg, loop_top);

    b.position_at_end(handle_neg);
    let nv = b.build_load(c.i32_type(), n_ptr, "nv").into_int_value();
    let pv = b.build_int_neg(nv, "pv");
    b.build_store(n_ptr, pv);
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_top);
    let n = b.build_load(c.i32_type(), n_ptr, "n").into_int_value();
    let done = b.build_int_compare(IntPredicate::SLE, n, c.i32_type().const_zero(), "done");
    b.build_conditional_branch(done, loop_exit, loop_body);
    
    b.position_at_end(loop_body);
    let ch = b.build_int_unsigned_rem(n, c.i32_type().const_int(10, false), "ch");
    let ch8 = b.build_int_truncate_or_bit_cast(ch, c.i8_type(), "ch8");
    let next_n = b.build_int_unsigned_div(n, c.i32_type().const_int(10, false), "next_n");
    let append = module.get_function(names::APPEND).unwrap();
    let append_char = module.get_function(names::APPEND_CHAR).unwrap();
    b.build_call(append, &[dest.into(), len_ptr.into(), ch8.into()], "_");
    b.build_store(n_ptr, next_n);
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_exit);
    b.build_conditional_branch(neg, append_neg, exit);

    b.position_at_end(append_neg);
    b.build_call(append_char, &[
        dest.into(),
        len_ptr.into(),
        c.i8_type().const_int(b'-' as _, false).into(),
    ], "_");
    b.build_unconditional_branch(exit);

    b.position_at_end(exit);
    let rev = emit_reverse_buffer(&module);
    let ret = b.build_load(c.i32_type(), len_ptr, "ret").into_int_value();
    b.build_call(rev, &[
        dest.into(),
        ret.into(),
    ], "_");
    b.build_return(Some(&ret));

    f
}

fn emit_reverse_buffer<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function("reverse_buffer", c.void_type().fn_type(&[
        c.i8_type().array_type(0).ptr_type(Default::default()).into(),
        c.i32_type().into(),
    ], false), None);
    let buf = f.get_first_param().unwrap().into_pointer_value();
    buf.set_name("buf");
    let len = f.get_last_param().unwrap().into_int_value();
    len.set_name("len");
    b.position_at_end(c.append_basic_block(f, "entry"));
    let loop_top = c.append_basic_block(f, "loop_top");
    let loop_body = c.append_basic_block(f, "loop_body");
    let loop_exit = c.append_basic_block(f, "loop_exit");
    
    let i_ptr = b.build_alloca(c.i32_type(), "i_ptr");
    let j_ptr = b.build_alloca(c.i32_type(), "j_ptr");
    b.build_store(i_ptr, c.i32_type().const_zero());
    let j_init = b.build_int_sub(len, c.i32_type().const_int(1, false), "j_init");
    b.build_store(j_ptr, j_init);
    let lt1 = b.build_int_compare(IntPredicate::SLE, len, c.i32_type().const_int(1, false), "lt1");
    b.build_conditional_branch(lt1, loop_exit, loop_top);

    b.position_at_end(loop_top);
    let i = b.build_load(c.i32_type(), i_ptr, "i").into_int_value();
    let j = b.build_load(c.i32_type(), j_ptr, "j").into_int_value();
    let done = b.build_int_compare(IntPredicate::SGE, i, j, "done");
    b.build_conditional_branch(done, loop_exit, loop_body);
    
    b.position_at_end(loop_body);
    let ci_ptr = unsafe {
        b.build_gep(c.i8_type().array_type(0), buf, &[
            c.i32_type().const_int(0, false),
            i
        ], "ci_ptr")
    };
    let cj_ptr = unsafe {
        b.build_gep(c.i8_type().array_type(0), buf, &[
            c.i32_type().const_int(0, false),
            j
        ], "cj_ptr")
    };
    let ci = b.build_load(c.i8_type(), ci_ptr, "ci1").into_int_value();
    let cj = b.build_load(c.i8_type(), cj_ptr, "cj1").into_int_value();
    b.build_store(ci_ptr, cj);
    b.build_store(cj_ptr, ci);

    let next_i = b.build_int_add(i, c.i32_type().const_int(1, false), "next_i");
    let next_j = b.build_int_sub(j, c.i32_type().const_int(1, false), "next_j");
    b.build_store(i_ptr, next_i);
    b.build_store(j_ptr, next_j);
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_exit);
    b.build_return(None);

    f
}



#[cfg(test)]
mod tests {
    use inkwell::{context::Context, values::AnyValue};

    use crate::tvalue::float_to_string::{emit_reverse_buffer, emit_int_to_buffer, emit_append, emit_append_char};

    #[test]
    fn snapshot() {
        let c = Context::create();
        let m = c.create_module("snapshot::float_to_string");
        super::add_float_to_string(&m);
        m.verify().unwrap_or_else(|e| {
            for line in e.to_string().lines() {
                eprintln!("{line}");
            }
            panic!("module not verified");
        });
        insta::assert_snapshot!(m.to_string());
    }

    #[test]
    fn convert_reasonable_numbers() {
        let c = Context::create();
        let m = c.create_module("harder::float_to_string");
        super::add_float_to_string(&m);
        std::fs::write("test-ir/float_to_string.ll", m.to_string()).unwrap();
        m.verify().unwrap_or_else(|e| {
            for line in e.to_string().lines() {
                eprintln!("{line}");
            }
            panic!("module not verified");
        });
        let jit = m
            .create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive)
            .unwrap();
        type F = unsafe extern "C" fn(f32, *mut u8) -> u32;
        let f = unsafe {
            jit.get_function::<F>(super::names::FLOAT_TO_STRING)
                .unwrap()
        };
        // TODO: unreasonable floats should also work...
        proptest::proptest!(
        |(v in (-999999999.0f32..9999999999999.0f32))| {
            let mut buf = [0u8; 4096];
            let ptr = buf.as_mut_ptr();
            let len = unsafe { f.call(v, ptr) };
            let ret = String::from_utf8_lossy(&buf[0..len as usize]);
            let round_trip: f32 = ret.parse().unwrap();
            let diff = (v - round_trip).abs();
            if diff >= 0.001 && m.get_function("main").is_none() {
                let b = c.create_builder();
                let main = m.add_function("main", c.i32_type().fn_type(&[], false), None);
                let write = m.add_function("write", c.i32_type().fn_type(&[
                    c.i32_type().into(),
                    c.i8_type().ptr_type(Default::default()).into(),
                    c.i32_type().into(),
                ], false), None);
                b.position_at_end(c.append_basic_block(main, "entry"));
                let buf = b.build_alloca(c.i8_type().array_type(255), "buf");
                let test_value = c.f32_type().const_float(v as _);
                let f = m.get_function(super::names::FLOAT_TO_STRING).unwrap();
                let len = b.build_call(f, &[test_value.into(), buf.into()], "len");
                b.build_call(write, &[
                    c.i32_type().const_int(1, false).into(),
                    buf.into(),
                    len.as_any_value_enum().into_int_value().into(),
                ], "_");
                b.build_return(Some(&c.i32_type().const_zero()));
                std::fs::write("test-ir/float_to_string.ll", m.to_string()).unwrap();
            }
            assert!(
                diff < 0.001,
                "
   v:{v}
  rt:{round_trip}
 len:{len}
 ret:{ret:?}
diff:{diff}
"
            );
        });
    }

    #[test]
    fn convert_ints() {
        let c = Context::create();
        let m = c.create_module("int_to_string");
        emit_append_char(&m);
        emit_append(&m);
        emit_int_to_buffer(&m);
        
        m.verify().unwrap_or_else(|e| {
            for line in e.to_string().lines() {
                eprintln!("{line}");
            }
            panic!("module not verified");
        });
        let printf = m.add_function("printf", c.i32_type().fn_type(&[
            c.i8_type().ptr_type(Default::default()).into(),
        ], true), None);
        let b = c.create_builder();
        for f in m.get_functions() {
            let name = f.get_name().to_str().unwrap();
            if name == "printf" {
                continue;
            }
            for bb in f.get_basic_blocks() {
                let mut args = Vec::new();
                b.position_before(&bb.get_first_instruction().unwrap());
                let bb_name = bb.get_name().to_str().unwrap();
                let mut output = format!("{name}->{bb_name}");
                let mut wrote_arg = false;
                if bb_name == "entry" {
                    output.push_str("(");
                    for arg in f.get_param_iter() {
                        if arg.is_int_value() {
                            if wrote_arg {
                                output.push_str(", ");
                            }
                            wrote_arg = true;
                            let name = arg.into_int_value().get_name().to_str().unwrap().to_string();
                            let app = format!("{name}: %i");
                            output.push_str(&app);
                            args.push(inkwell::values::BasicMetadataValueEnum::from(arg));
                        }
                    }
                    output.push_str(")");
                }
                output.push_str("\n\0");
                
                let buf = b.build_alloca(c.i8_type().array_type(output.len() as _),"buf");
                b.build_store(buf, c.const_string(output.as_bytes(), false));
                args.insert(0, buf.into());
                b.build_call(printf, &args, "_");
            }
        }
        std::fs::write("test-ir/int_to_string.ll", m.to_string()).unwrap();
        let jit = m
            .create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive)
            .unwrap();
        type F = unsafe extern "C" fn(i32, *mut u8) -> u32;
        let f = unsafe {
            jit.get_function::<F>(super::names::INT_TO_STRING)
                .unwrap()
        };
        // TODO: unreasonable floats should also work...
        proptest::proptest!(
        |(v in (i32::MIN..=i32::MAX))| {
            println!("attempt: {v}");
            let mut buf = [0u8; 4096];
            let ptr = buf.as_mut_ptr();
            let len = unsafe { f.call(v, ptr) };
            let ret = String::from_utf8_lossy(&buf[0..len as usize]);
            let round_trip: i32 = ret.parse().unwrap();
            let diff = (v - round_trip).abs();
            assert_eq!(round_trip, v,
                "
   v:{v}
  rt:{round_trip}
 len:{len}
 ret:{ret:?}
diff:{diff}
"
            );
        });
    }
}
