use inkwell::{
    builder::Builder,
    context::ContextRef,
    intrinsics::Intrinsic,
    module::Module,
    types::BasicType,
    values::{AnyValue, FunctionValue, IntValue, PointerValue},
    FloatPredicate, IntPredicate,
};

pub fn add_float_to_string<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let precision = module.add_global(c.f32_type(), Default::default(), "PRECISION");
    precision.set_initializer(&c.f32_type().const_float(0.0001));
    precision.set_constant(true);
    let precision_ptr = precision.as_any_value_enum().into_pointer_value();
    let f = module.add_function(
        "float_to_string",
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
    let digit_ptr = b.build_alloca(c.i32_type(), "digit_ptr");
    let m_ptr = b.build_alloca(c.i32_type(), "m_ptr");
    let m1_ptr = b.build_alloca(c.i32_type(), "m1_ptr");

    let n_ptr = b.build_alloca(c.f32_type(), "n_ptr");
    b.build_store(n_ptr, value);
    let ret_ptr = b.build_alloca(c.f32_type(), "ret_ptr");
    b.build_store(ret_ptr, c.i32_type().const_zero());
    let check_for_neg = emit_check_for_neg(module);

    let neg = b
        .build_call(
            check_for_neg,
            &[dest.into(), n_ptr.into(), ret_ptr.into()],
            "neg",
        )
        .as_any_value_enum()
        .into_int_value();

    let log10_intrinsic = Intrinsic::find("llvm.log10.f32").expect("llvm.pow.f32");
    let log10_f = log10_intrinsic
        .get_declaration(module, &[c.f32_type().as_basic_type_enum()])
        .unwrap();
    let n = b.build_load(c.f32_type(), n_ptr, "n").into_float_value();
    let mf = b
        .build_call(log10_f, &[n.into()], "mf")
        .as_any_value_enum()
        .into_float_value();
    let m = b.build_float_to_signed_int(mf, c.i32_type(), "m");
    b.build_store(m_ptr, m);
    let use_exp = c.append_basic_block(f, "use_exp");
    let dont_use_exp = c.append_basic_block(f, "dont_use_exp");
    let misge14 = b.build_int_compare(
        IntPredicate::SGE,
        m,
        c.i32_type().const_int(14, false),
        "misge14",
    );
    let misge9 = b.build_int_compare(
        IntPredicate::SGE,
        m,
        c.i32_type().const_int(9, false),
        "misge9",
    );
    let misge9_ge0 = b.build_and(neg, misge9, "misge9_ge0");
    let neg9 = b.build_int_neg(c.i32_type().const_int(9, false), "neg9");
    let misle_neg9 = b.build_int_compare(IntPredicate::SLE, m, neg9, "misle_neg9");
    let should_use_exp_pre = b.build_or(misge14, misge9_ge0, "should_use_exp_pre");
    let should_use_exp = b.build_or(should_use_exp_pre, misle_neg9, "should_use_exp");
    b.build_conditional_branch(should_use_exp, use_exp, dont_use_exp);
    // if (useExp)
    b.position_at_end(use_exp);
    let m = b.build_load(c.i32_type(), m_ptr, "m").into_int_value();
    let m_lt_z = b.build_int_compare(
        IntPredicate::SLT,
        m,
        c.i32_type().const_int(0, false),
        "mlez",
    );
    let use_exp_cont = c.append_basic_block(f, "use_exp_cont");
    let m_lt_z_bb = c.append_basic_block(f, "m_lt_z_bb");
    b.build_conditional_branch(m_lt_z, m_lt_z_bb, use_exp_cont);
    // if m < 0
    b.position_at_end(m_lt_z_bb);
    let new_m = b.build_int_sub(m, c.i32_type().const_int(1, false), "new_m");
    b.build_store(m_ptr, new_m);
    b.build_unconditional_branch(use_exp_cont);

    b.position_at_end(use_exp_cont);
    let pow_intrinsic = Intrinsic::find("llvm.pow.f32").expect("llvm.pow.f32");
    let pow_intrinsic_fn = pow_intrinsic
        .get_declaration(&module, &[c.f32_type().as_basic_type_enum()])
        .unwrap();
    let m = b.build_load(c.i32_type(), m_ptr, "m").into_int_value();
    let mf = b.build_signed_int_to_float(m, c.f32_type(), "mf");
    let new_n_pre = b
        .build_call(
            pow_intrinsic_fn,
            &[c.f32_type().const_float(10.0).into(), mf.into()],
            "new_n_pre",
        )
        .as_any_value_enum()
        .into_float_value();
    let n = b.build_load(c.f32_type(), n_ptr, "n").into_float_value();
    let new_n = b.build_float_div(n, new_n_pre, "new_n");
    b.build_store(n_ptr, new_n);
    b.build_store(m1_ptr, m);
    b.build_store(m_ptr, c.i32_type().const_zero());
    b.build_unconditional_branch(dont_use_exp);
    let loop_top = c.append_basic_block(f, "loop_top");
    let m_le1_bb = c.append_basic_block(f, "m_le1_bb");

    b.position_at_end(dont_use_exp);
    let m = b.build_load(c.i32_type(), m_ptr, "m").into_int_value();
    let m_le1 = b.build_int_compare(
        IntPredicate::SLE,
        m,
        c.i32_type().const_int(1, false),
        "m_le1",
    );
    b.build_conditional_branch(m_le1, m_le1_bb, loop_top);

    b.position_at_end(m_le1_bb);
    b.build_store(m_ptr, c.i32_type().const_zero());
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

    let exit = c.append_basic_block(f, "exit");
    let add_exp = c.append_basic_block(f, "add_exp");
    b.build_conditional_branch(should_use_exp, add_exp, exit);

    b.position_at_end(add_exp);
    emit_append_char('e', &c, &b, dest, ret_ptr);
    let m1 = b.build_load(c.i32_type(), m1_ptr, "m1").into_int_value();
    let m1_gtz = b.build_int_compare(IntPredicate::SGT, m1, c.i32_type().const_zero(), "m1gtz");
    let m1gtzbb = c.append_basic_block(f, "m1gtzbb");
    let m1ltzbb = c.append_basic_block(f, "m1ltzbb");
    let exp_cont = c.append_basic_block(f, "exp_cont");
    b.build_conditional_branch(m1_gtz, m1gtzbb, m1ltzbb);

    b.position_at_end(m1gtzbb);
    emit_append_char('+', &c, &b, dest, ret_ptr);
    b.build_unconditional_branch(exp_cont);

    b.position_at_end(m1ltzbb);
    emit_append_char('-', &c, &b, dest, ret_ptr);
    let m1 = b.build_load(c.i32_type(), m_ptr, "m1").into_int_value();
    let new_m1 = b.build_int_neg(m1, "new_m1");
    b.build_store(m1_ptr, new_m1);
    b.build_unconditional_branch(exp_cont);

    b.position_at_end(exp_cont);
    b.build_store(m_ptr, c.i32_type().const_zero());
    let loop_top = c.append_basic_block(f, "loop_top");
    let loop_body = c.append_basic_block(f, "loop_body");
    let loop_exit = c.append_basic_block(f, "loop_exit");
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_top);
    let m1 = b.build_load(c.i32_type(), m1_ptr, "m1").into_int_value();
    let cont = b.build_int_compare(IntPredicate::SGE, m1, c.i32_type().const_zero(), "cont");
    b.build_conditional_branch(cont, loop_body, loop_exit);

    b.position_at_end(loop_body);
    let ch_pre = b.build_int_signed_rem(m1, c.i32_type().const_int(10, false), "ch_pre");
    emit_append_int(ch_pre, &c, &b, dest, ret_ptr);
    let new_m1 = b.build_int_signed_div(m1, c.i32_type().const_int(10, false), "new_m1");
    b.build_store(m1_ptr, new_m1);
    let m = b.build_load(c.i32_type(), m_ptr, "m").into_int_value();
    let new_m = b.build_int_add(m, c.i32_type().const_int(1, false), "new_m");
    b.build_store(m_ptr, new_m);
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_exit);
    let current_len = b
        .build_load(c.i32_type(), ret_ptr, "current_len")
        .into_int_value();
    let current_m = b
        .build_load(c.i32_type(), m_ptr, "current_m")
        .into_int_value();
    let new_len = b.build_int_sub(current_len, current_m, "new_len");
    b.build_store(ret_ptr, new_len);
    let j_ptr = b.build_alloca(c.i32_type(), "j_ptr");
    let i_ptr = b.build_alloca(c.i32_type(), "i_ptr");
    b.build_store(i_ptr, c.i32_type().const_zero());
    let j_init = b.build_int_sub(current_m, c.i32_type().const_int(1, false), "j_init_pre");
    b.build_store(j_ptr, j_init);
    let loop_top = c.append_basic_block(f, "loop_top");
    let loop_body = c.append_basic_block(f, "loop_body");
    let loop_exit = c.append_basic_block(f, "loop_exit");
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_top);
    let i = b.build_load(c.i32_type(), i_ptr, "i").into_int_value();
    let j = b.build_load(c.i32_type(), j_ptr, "j").into_int_value();
    let cont = b.build_int_compare(IntPredicate::SLT, i, j, "cont");
    b.build_conditional_branch(cont, loop_body, loop_exit);

    b.position_at_end(loop_body);
    let current_len = b
        .build_load(c.i32_type(), ret_ptr, "current_len")
        .into_int_value();
    let i_offset = b.build_int_add(i, current_len, "i_offset");
    let j_offset = b.build_int_add(current_len, j, "j_offset");
    let dest_ty = c.i8_type().array_type(255);
    let i_ele_ptr = unsafe {
        b.build_gep(
            dest_ty,
            dest,
            &[c.i32_type().const_zero().into(), i_offset.into()],
            "swap_lhs1_ptr",
        )
    };
    let j_ele_ptr = unsafe {
        b.build_gep(
            dest_ty,
            dest,
            &[c.i32_type().const_zero().into(), j_offset.into()],
            "swap_rhs1_ptr",
        )
    };
    emit_swap_step(&c, &b, i_ele_ptr, j_ele_ptr);
    emit_swap_step(&c, &b, j_ele_ptr, i_ele_ptr);
    emit_swap_step(&c, &b, i_ele_ptr, j_ele_ptr);
    let next_len = b.build_int_add(current_len, c.i32_type().const_int(1, false), "next_len");
    b.build_store(ret_ptr, next_len);
    b.build_unconditional_branch(loop_top);

    b.position_at_end(loop_exit);
    b.build_unconditional_branch(exit);

    b.position_at_end(exit);
    let ret = b.build_load(c.i32_type(), ret_ptr, "ret");
    b.build_return(Some(&ret));

    f
}

fn emit_swap_step<'ctx>(
    c: &ContextRef<'ctx>,
    b: &Builder<'ctx>,
    lhs_ptr: PointerValue<'ctx>,
    rhs_ptr: PointerValue<'ctx>,
) {
    let lhs = b.build_load(c.i32_type(), lhs_ptr, "lhs").into_int_value();
    let rhs = b.build_load(c.i32_type(), rhs_ptr, "rhs").into_int_value();
    let res = b.build_xor(lhs, rhs, "res");
    b.build_store(lhs_ptr, res);
}

fn emit_append_int<'ctx>(
    i: IntValue,
    c: &ContextRef<'ctx>,
    b: &Builder<'ctx>,
    buf: PointerValue<'ctx>,
    len_ptr: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let current = b
        .build_load(c.i32_type(), len_ptr, "len")
        .as_any_value_enum()
        .into_int_value();
    let ch_ptr = unsafe {
        b.build_in_bounds_gep(
            c.i8_type().array_type(0),
            buf,
            &[c.i32_type().const_int(0, false).into(), current.into()],
            "char_ptr",
        )
    };
    let ch = b.build_int_add(i, c.i32_type().const_int('0' as _, false), "ch");
    b.build_store(ch_ptr, ch);
    let next_char = b.build_int_add(current, c.i32_type().const_int(1, false), "next_char");
    b.build_store(len_ptr, next_char);
    current
}

fn emit_append_char<'ctx>(
    ch: char,
    c: &ContextRef<'ctx>,
    b: &Builder<'ctx>,
    buf: PointerValue<'ctx>,
    len_ptr: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let current = b
        .build_load(c.i32_type(), len_ptr, "len")
        .as_any_value_enum()
        .into_int_value();
    let ch_ptr = unsafe {
        b.build_in_bounds_gep(
            c.i8_type().array_type(0),
            buf,
            &[c.i32_type().const_int(0, false).into(), current.into()],
            "char_ptr",
        )
    };
    b.build_store(ch_ptr, c.i8_type().const_int(ch as _, false));
    let next_char = b.build_int_add(current, c.i32_type().const_int(1, false), "next_char");
    b.build_store(len_ptr, next_char);
    current
}

fn emit_check_for_neg<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        "check_for_neg",
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
    let idx = b
        .build_load(c.i32_type(), len_ptr, "idx")
        .as_any_value_enum()
        .into_int_value();
    let ch_ptr = unsafe {
        b.build_gep(
            c.i8_type().array_type(0),
            buf,
            &[c.i32_type().const_zero(), idx],
            "ch_ptr",
        )
    };
    b.build_store(ch_ptr, c.i8_type().const_int(b'-' as _, false));
    let new_idx = b.build_int_add(idx, c.i32_type().const_int(1, false), "new_idx");
    b.build_store(len_ptr, new_idx);
    b.build_return(Some(&neg));

    f
}

fn emit_swap_m_to_len<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        "swap_m_to_len",
        c.void_type().fn_type(
            &[
                c.i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
                c.i32_type().into(),
                c.i32_type().into(),
            ],
            false,
        ),
        None,
    );
    let entry = c.append_basic_block(f, "entry");
    let buf = f
        .get_first_param()
        .unwrap()
        .as_any_value_enum()
        .into_pointer_value();
    buf.set_name("buf");
    let len = f
        .get_nth_param(1)
        .unwrap()
        .as_any_value_enum()
        .into_int_value();
    len.set_name("len");
    let m = f
        .get_last_param()
        .unwrap()
        .as_any_value_enum()
        .into_int_value();
    m.set_name("m");
    b.position_at_end(entry);
    let offset_ptr = b.build_alloca(c.i32_type(), "offset_ptr");
    let offset_init = b.build_int_sub(len, m, "offset_init");

    f
}

fn emit_loop_should_continue<'ctx>(module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let c = module.get_context();
    let b = c.create_builder();
    let f = module.add_function(
        "loop_should_continue",
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
        "precision_loop_step",
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
    let digit = b.build_float_to_unsigned_int(digit_pre2, c.i32_type(), "digit");
    emit_append_int(digit, &c, &b, buf, len_ptr);
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
    let n_gt_z = b.build_float_compare(
        FloatPredicate::OGT,
        n,
        c.f32_type().const_zero(),
        "n_gt_z",
    );
    b.build_conditional_branch(n_gt_z, handle_dot, after_dot);

    b.position_at_end(handle_dot);
    emit_append_char('.', &c, &b, buf, len_ptr);
    b.build_unconditional_branch(after_dot);

    b.position_at_end(after_dot);
    let new_m = b.build_int_sub(m, c.i32_type().const_int(1, false), "new_m");
    b.build_store(m_ptr, new_m);
    b.build_return(None);

    f
}

#[cfg(test)]
mod tests {

    use inkwell::context::Context;

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
    fn easy() {
        let c = Context::create();
        let m = c.create_module("snapshot::float_to_string");
        super::add_float_to_string(&m);
        let printf = m.add_function(
            "printf",
            c.i32_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], true),
            None,
        );
        let b = c.create_builder();
        for f in m.get_functions() {
            let base = f.get_name().to_str().unwrap();
            for bb in f.get_basic_blocks() {
                b.position_before(&bb.get_first_instruction().unwrap());
                let name = bb.get_name().to_str().unwrap();
                let mut printed = format!("{base}->{name}");
                let size = printed.len() + 2;
                printed.push('\n');
                printed.push('\0');
                let fmt = b.build_alloca(c.i8_type().array_type(size as _), "fmt");
                b.build_store(fmt, c.const_string(printed.as_bytes(), false));
                b.build_call(printf, &[fmt.into()], "_");
            }
        }
        std::fs::write("test-ir/float_to_string.ll", m.to_string()).unwrap();
        m.verify().unwrap_or_else(|e| {
            for line in e.to_string().lines() {
                eprintln!("{line}");
            }
            panic!("module not verified");
        });
        let jit = m.create_jit_execution_engine(Default::default()).unwrap();
        type F = unsafe extern "C" fn(f32, *mut u8) -> u32;
        let f = unsafe { jit.get_function::<F>("float_to_string").unwrap() };
        let mut v = -15.0f32;
        for _i in 0..100 {
            let mut buf = [0u8; 4096];
            let ptr = buf.as_mut_ptr();
            let len = unsafe { f.call(v, ptr) };
            println!("{len}: {:?}", &buf[0..(len as usize)+2]);
            assert_eq!(
                String::from_utf8_lossy(&buf[0..len as usize]),
                format!("{v}"),
            );
            v += 0.5;
        }
    }
}
