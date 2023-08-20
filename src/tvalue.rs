use inkwell::{
    builder::Builder, context::Context, module::Module, types::StructType, values::AnyValue,
};

pub enum TValue {
    Nil,
    Bool(bool),
    Number(f32),
    String(Vec<u8>),
    Table(Vec<TableField>),
    Function { id: u32 },
    Thread { id: u32 },
    UserData { id: u32 },
}

impl TValue {
    pub(crate) const BASE_TYPE_NAME: &str = "enum.TValue";
    pub(crate) const GET_TAG_FN_NAME: &str = "getTValueTag";

    fn tvalue_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().into(),
                context.i8_type().array_type(7).into(),
            ],
            false,
        )
    }
    fn tvalue_nil_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(&[context.i8_type().into()], false)
    }

    fn tvalue_bool_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().array_type(1).into(),
                context.i8_type().into(),
            ],
            false,
        )
    }

    fn tvalue_number_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i32_type().array_type(1).into(),
                context.f32_type().into(),
            ],
            false,
        )
    }

    fn tvalue_table_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i32_type().array_type(1).into(),
                context.i32_type().into(),
            ],
            false,
        )
    }

    fn tvalue_func_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().into(),
                // args length
            ],
            false,
        )
    }

    pub fn gen_lib<'ctx>(context: &'ctx Context) -> Module<'ctx> {
        let module = context.create_module("std::tvalue");
        let builder = context.create_builder();
        Self::add_get_tag_function(context, &module, &builder);
        Self::add_get_value_bool(context, &module, &builder);
        Self::add_new_nil(context, &module, &builder);
        Self::add_new_bool(context, &module, &builder);
        Self::add_new_number(context, &module, &builder);
        module.verify().unwrap_or_else(|e| {
            eprintln!("{}", module.to_string());
            let es = e.to_string();
            let mut ct = 0;
            for line in es.lines() {
                eprintln!("{line}");
                ct += 1;
            }
            panic!("Found {ct} errors");
        });
        module
    }

    fn add_new_nil<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
        let enum_base_ty = Self::tvalue_type(context);
        let f = enum_base_ty.ptr_type(0u16.into()).fn_type(&[], false);
        let f = module.add_function("enum_TValue_nil_new", f, None);
        let entry = context.append_basic_block(f.clone(), "entry");
        builder.position_at_end(entry);
        let ret = builder.build_alloca(enum_base_ty, "ret");
        let tag_ptr = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_type(context),
                ret,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        builder.build_store(tag_ptr, context.i8_type().const_int(0, false));
        let val_ptr = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_type(context),
                ret,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "tag_ptr",
            )
        };
        builder.build_store(val_ptr, context.i8_type().array_type(7).const_zero());
        builder.build_return(Some(&ret));
    }

    fn add_new_bool<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
        let enum_base_ty = Self::tvalue_type(context);
        let f = enum_base_ty
            .ptr_type(0u16.into())
            .fn_type(&[context.bool_type().into()], false);
        let f = module.add_function("enum_TValue_bool_new", f, None);
        let entry = context.append_basic_block(f.clone(), "entry");
        builder.position_at_end(entry);
        let ret = builder.build_alloca(enum_base_ty, "ret");
        let tag_ptr = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_type(context),
                ret,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        builder.build_store(tag_ptr, context.i8_type().const_int(1, false));
        let casted = builder.build_bitcast(
            ret,
            Self::tvalue_bool_type(context).ptr_type(0u16.into()),
            "casted",
        );
        let gep = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_bool_type(context),
                casted.into_pointer_value(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "bool_ptr",
            )
        };
        let arg = f.get_param_iter().next().expect("new bool has 1 argument");
        arg.set_name("is_true");
        let wide_arg =
            builder.build_int_z_extend(arg.into_int_value(), context.i8_type(), "wide_arg");
        builder.build_store(gep, wide_arg);
        builder.build_return(Some(&ret));
    }

    fn add_new_number<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        let enum_base_ty = Self::tvalue_type(context);
        let f = enum_base_ty
            .ptr_type(0u16.into())
            .fn_type(&[context.f32_type().into()], false);
        let f = module.add_function("enum_TValue_number_new", f, None);
        let entry = context.append_basic_block(f.clone(), "entry");
        builder.position_at_end(entry);
        let ret = builder.build_alloca(enum_base_ty, "ret");
        let tag_ptr = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_type(context),
                ret,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        builder.build_store(tag_ptr, context.i8_type().const_int(2, false));
        let casted = builder.build_bitcast(
            ret.clone(),
            Self::tvalue_number_type(context).ptr_type(Default::default()),
            "casted",
        );

        let n_ptr = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_number_type(context),
                casted.into_pointer_value(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "n_ptr",
            )
        };
        let arg = f
            .get_param_iter()
            .next()
            .expect("new number has 1 argument");
        arg.set_name("n");
        // builder.build_store(n_ptr, arg.into_float_value());
        builder.build_return(Some(&ret));
    }

    fn add_get_tag_function<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        let get_tag_ty = context.i8_type().fn_type(
            &[Self::tvalue_type(context).ptr_type(0u16.into()).into()],
            false,
        );
        let func = module.add_function(Self::GET_TAG_FN_NAME, get_tag_ty, None);
        let arg_ptr = func.get_param_iter().next().unwrap().into_pointer_value();
        arg_ptr.set_name("tagged_value");
        let entry_block = context.append_basic_block(func, "entry");
        builder.position_at_end(entry_block);
        let tag_alloc = builder.build_alloca(context.i8_type(), "tag");
        let tag_gep = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_type(context),
                arg_ptr,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        let tag_load = builder.build_load(context.i8_type(), tag_gep, "tag_val");
        builder.build_store(tag_alloc.clone(), tag_load);
        let val = builder.build_load(context.i8_type(), tag_alloc, "ret_val");
        builder.build_return(Some(&val));
    }

    fn add_get_value_bool<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        let get_tag_ty = context.bool_type().fn_type(
            &[Self::tvalue_type(context).ptr_type(0u16.into()).into()],
            false,
        );
        let func = module.add_function("getTValueValue_bool", get_tag_ty, None);
        let arg_ptr = func.get_param_iter().next().unwrap().into_pointer_value();
        arg_ptr.set_name("tagged_value");
        let entry_block = context.append_basic_block(func.clone(), "entry");
        builder.position_at_end(entry_block.clone());
        let get_tag_func = module
            .get_function(Self::GET_TAG_FN_NAME)
            .expect("getTagFn");
        let call = builder.build_call(get_tag_func, &[arg_ptr.into()], "tag");
        let cmp_tag = call.as_any_value_enum();
        let is_nil = builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            cmp_tag.into_int_value(),
            context.i8_type().const_int(0, false),
            "is_nil",
        );
        let is_nil_block = context.append_basic_block(func.clone(), "is_nil");
        let is_not_nil_block = context.append_basic_block(func.clone(), "is_not_nil");
        builder.build_conditional_branch(is_nil.clone(), is_nil_block.clone(), is_not_nil_block);
        builder.position_at_end(is_nil_block.clone());
        builder.build_return(Some(&context.bool_type().const_int(0, false)));
        builder.position_at_end(is_not_nil_block.clone());
        let is_bool = builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            cmp_tag.into_int_value(),
            context.i8_type().const_int(1, false),
            "is_bool",
        );
        let is_bool_block = context.append_basic_block(func.clone(), "is_bool");
        let is_not_bool_block = context.append_basic_block(func, "is_not_bool");
        builder.build_conditional_branch(is_bool, is_bool_block.clone(), is_not_bool_block.clone());
        builder.position_at_end(is_not_bool_block.clone());
        builder.build_return(Some(&context.bool_type().const_int(1, false)));
        builder.position_at_end(is_bool_block);
        let casted_t = builder.build_bitcast(
            arg_ptr,
            Self::tvalue_bool_type(context).ptr_type(0u16.into()),
            "t_bool",
        );
        let gep = unsafe {
            builder.build_in_bounds_gep(
                Self::tvalue_bool_type(context),
                casted_t.into_pointer_value(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "raw_variant_pointer",
            )
        };
        let load_bool = builder.build_load(context.i8_type(), gep, "bool_t_int");
        let ret = builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            load_bool.into_int_value(),
            context.i8_type().const_int(1, false),
            "ret",
        );
        builder.build_return(Some(&ret));
    }
}

pub struct TableField {
    pub name: String,
    pub value: TValue,
}

#[cfg(test)]
mod test {
    use super::*;
    use inkwell::{
        execution_engine::{ExecutionEngine, JitFunction},
        values::{BasicMetadataValueEnum, FunctionValue},
    };

    #[test]
    fn generate_t_value_module() {
        let context = Context::create();
        let module = TValue::gen_lib(&context);
        insta::assert_snapshot!(module.to_string())
    }

    #[test]
    fn check_get_tag() {
        let context = Context::create();
        let module = TValue::gen_lib(&context);
        let tag_fun = module.get_function(TValue::GET_TAG_FN_NAME).unwrap();
        let new_nil_fun = module.get_function("enum_TValue_nil_new").unwrap();
        let new_bool_fun = module.get_function("enum_TValue_bool_new").unwrap();
        let new_num_fun = module.get_function("enum_TValue_number_new").unwrap();
        let builder = context.create_builder();

        build_test_tag_func(&context, &module, &builder, "test_nil", new_nil_fun, &[]);
        build_test_tag_func(
            &context,
            &module,
            &builder,
            "test_bool",
            new_bool_fun,
            &[context.bool_type().const_int(1, false).into()],
        );
        build_test_tag_func(
            &context,
            &module,
            &builder,
            "test_num",
            new_num_fun,
            &[context.f32_type().const_float(u32::MAX as _).into()],
        );
        eprintln!("{}", module.to_string());
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();

        call_tag_test_fn("test_nil", &jit, 0);
        call_tag_test_fn("test_bool", &jit, 1);
        call_tag_test_fn("test_num", &jit, 2);
    }

    fn build_test_tag_func<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        test_name: &'static str,
        ctor: FunctionValue<'ctx>,
        ctor_args: &[BasicMetadataValueEnum<'ctx>],
    ) {
        let test_func = module.add_function(test_name, context.i8_type().fn_type(&[], false), None);
        let tag_fun = module.get_function(TValue::GET_TAG_FN_NAME).unwrap();
        let entry = context.append_basic_block(test_func, "entry");
        builder.position_at_end(entry);
        let arg = builder.build_call(ctor.into(), ctor_args, "arg");
        let ret = builder.build_call(
            tag_fun,
            &[arg.as_any_value_enum().into_pointer_value().into()],
            "ret",
        );
        builder.build_return(Some(&ret.as_any_value_enum().into_int_value()));
    }

    fn call_tag_test_fn<'ctx>(
        name: &'static str,
        jit: &'ctx ExecutionEngine<'ctx>,
        expected_tag: u8,
    ) {
        type TestTagFunc = unsafe extern "C" fn() -> u8;
        println!("looking up {name}");
        let func: JitFunction<TestTagFunc> = unsafe { jit.get_function(name).unwrap() };
        println!("calling {name}");
        let val = unsafe { func.call() };
        println!("called {name} -> {val}");
        assert_eq!(val, expected_tag, "{name}");
    }
}
