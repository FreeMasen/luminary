use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, StructType},
    values::{AnyValue, BasicValue, BasicValueEnum, PointerValue},
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
    pub(crate) const MODULE_NAME: &str = "std::tvalue";
    pub(crate) const BASE_TYPE_NAME: &str = "enum.TValue";
    pub(crate) const GET_TAG_FN_NAME: &str = "getTValueTag";

    fn tvalue_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().into(),
                context.i8_type().array_type(8).into(),
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

    fn tvalue_string_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().array_type(1).into(),
                Self::byte_array_type(context)
                    .ptr_type(Default::default())
                    .into(),
            ],
            false,
        )
    }

    fn tvalue_table_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().array_type(1).into(),
                Self::table_field_type(context)
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
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

    fn table_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().into(),
                Self::table_field_type(context)
                    .ptr_type(Default::default())
                    .into(),
            ],
            false,
        )
    }

    fn byte_array_type<'ctx>(context: &'ctx Context) -> StructType {
        context.struct_type(
            &[
                context.i64_type().into(),
                context
                    .i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
            ],
            false,
        )
    }

    fn table_field_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                Self::tvalue_type(context)
                    .ptr_type(Default::default())
                    .into(),
                Self::tvalue_type(context)
                    .ptr_type(Default::default())
                    .into(),
            ],
            false,
        )
    }

    pub fn gen_lib<'ctx>(context: &'ctx Context) -> Module<'ctx> {
        let module = context.create_module(Self::MODULE_NAME);
        let builder = context.create_builder();
        let tvalue = context.opaque_struct_type(Self::BASE_TYPE_NAME);
        tvalue.set_body(
            &[
                context.i8_type().into(),
                context.i8_type().array_type(8).into(),
            ],
            false,
        );
        let tvalue_bool = context.opaque_struct_type("tvalue_bool");
        tvalue_bool.set_body(
            &[
                context.i64_type().array_type(1).into(),
                context.i8_type().into(),
            ],
            false,
        );
        let tvalue_nil = context.opaque_struct_type("tvalue_nil");
        tvalue_nil.set_body(
            &[
                context.i8_type().into(),
                context.i8_type().array_type(8).into(),
            ],
            false,
        );
        let tvalue_number = context.opaque_struct_type("tvalue_number");
        tvalue_number.set_body(
            &[
                context.i32_type().array_type(1).into(),
                context.f32_type().into(),
            ],
            false,
        );
        let tvalue_string = context.opaque_struct_type("tvalue_string");
        tvalue_string.set_body(
            &[
                context.i8_type().array_type(1).into(),
                Self::byte_array_type(context)
                    .ptr_type(Default::default())
                    .into(),
            ],
            false,
        );
        let byte_array = context.opaque_struct_type("byte_array");
        byte_array.set_body(
            &[
                context.i64_type().into(),
                context
                    .i8_type()
                    .array_type(0)
                    .ptr_type(Default::default())
                    .into(),
            ],
            false,
        );
        Self::add_get_tag_function(context, &module, &builder);
        Self::add_get_value_bool(context, &module, &builder);
        Self::add_new_nil(context, &module, &builder);
        Self::add_new_bool(context, &module, &builder);
        Self::add_new_number(context, &module, &builder);
        Self::add_new_string(context, &module, &builder);
        Self::add_new_byte_array(context, &module, &builder);
        // Self::add_new_table(context, &module, &builder);
        module.verify().unwrap_or_else(|e| {
            eprintln!("{}", module.to_string());
            let es = e.to_string();
            let mut ct = 0;
            for line in es.lines() {
                eprintln!("{line}");
                ct += 1;
            }
            panic!("Found {} errors", ct / 2);
        });
        module
    }

    fn add_new_nil<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
        Self::add_new_ctor(
            context,
            module,
            builder,
            "tvalue_nil",
            &[],
            "tvalue_new_nil",
            0,
        );
    }

    fn add_new_bool<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
        Self::add_new_ctor(
            context,
            module,
            builder,
            "tvalue_bool",
            &[context.i8_type().into()],
            "tvalue_new_bool",
            1,
        );
    }

    fn add_new_number<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        Self::add_new_ctor(
            context,
            module,
            builder,
            "tvalue_number",
            &[],
            "tvalue_new_number",
            2,
        );
    }

    fn add_new_byte_array<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        let f = module.add_function(
            "new_byte_array",
            Self::byte_array_type(context)
                .ptr_type(Default::default())
                .fn_type(
                    &[
                        context.i64_type().into(),
                        context
                            .i8_type()
                            .array_type(0)
                            .ptr_type(Default::default())
                            .into(),
                    ],
                    false,
                ),
            None,
        );
        let len_param = f.get_first_param().expect("len param");
        len_param.set_name("length");
        let bytes_param = f.get_last_param().expect("bytes param");
        bytes_param.set_name("bytes");
        let entry = context.append_basic_block(f, "entry");
        builder.position_at_end(entry);
        let ret = builder.build_alloca(Self::byte_array_type(context), "ret");
        let len_ptr = unsafe {
            builder.build_in_bounds_gep(
                Self::byte_array_type(context),
                ret.clone(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "len_ptr",
            )
        };
        builder.build_store(len_ptr, len_param.as_basic_value_enum());
        let bytes_ptr = unsafe {
            builder.build_in_bounds_gep(
                Self::byte_array_type(context),
                ret.clone(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "bytes_ptr",
            )
        };
        let loaded_ptr = builder.build_load(
            context.i8_type().array_type(0).ptr_type(Default::default()),
            bytes_ptr,
            "loaded_ptr",
        );
        builder.build_store(loaded_ptr.into_pointer_value(), bytes_param);
        builder.build_return(Some(&ret));
    }

    fn add_new_ctor<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        struct_name: &str,
        ctor_args: &[BasicMetadataTypeEnum<'ctx>],
        ctor_name: &'static str,
        variant_id: u64,
    ) {
        let ret_ty = module
            .get_struct_type(struct_name)
            .unwrap_or_else(|| panic!("{struct_name} is not defined"));
        let enum_base_ty = module
            .get_struct_type(Self::BASE_TYPE_NAME)
            .expect("base struct name to be defined");
        let f = enum_base_ty.ptr_type(0u16.into()).fn_type(ctor_args, false);
        let f = module.add_function(ctor_name, f, None);
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
        builder.build_store(tag_ptr, context.i8_type().const_int(variant_id, false));
        if ctor_args.is_empty() {
            builder.build_return(Some(&ret));
            return;
        }
        let data_ptr = unsafe {
            builder.build_in_bounds_gep(
                ret_ty,
                ret.clone(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "data_ptr",
            )
        };
        let arg = f.get_first_param().expect("ctor has 1 argument");
        builder.build_store(data_ptr, arg);
        builder.build_return(Some(&ret));
    }

    fn add_new_string<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        let byte_array = module.get_struct_type("byte_array").expect("byte array");
        Self::add_new_ctor(
            context,
            module,
            builder,
            "tvalue_string",
            &[byte_array.ptr_type(Default::default()).into()],
            "tvalue_new_string",
            3,
        );
    }

    fn add_new_table<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
        // Self::add_new_ctor(
        //     context,
        //     module,
        //     builder,
        //     "tvalue_table",
        //     &[],
        //     "tvalue_new_table",
        //     4,
        //     |context, module, builder, data_ptr, arg| {

        //     }
        //     // Self::tvalue_table_type(context),
        //     // Self::table_field_type(context).array_type(0).ptr_type(Default::default())
        // );
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
        values::{BasicMetadataValueEnum, FunctionValue, ArrayValue},
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
        let new_nil_fun = module.get_function("tvalue_new_nil").unwrap();
        let new_bool_fun = module.get_function("tvalue_new_bool").unwrap();
        let new_num_fun = module.get_function("tvalue_new_number").unwrap();
        let new_string_fun = module.get_function("tvalue_new_string").unwrap();
        // let new_table_fun = module.get_function("enum_TValue_new_table").unwrap();
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
        let s_ctor = build_string_ctor_func("hello world", &context, &module, &builder);
        build_test_tag_func(&context, &module, &builder, "test_string", s_ctor, &[]);
        eprintln!("{}", module.to_string());
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        call_tag_test_fn("test_nil", &jit, 0);
        call_tag_test_fn("test_bool", &jit, 1);
        call_tag_test_fn("test_num", &jit, 2);
        call_tag_test_fn("test_string", &jit, 3);
        // call_tag_test_fn("test_table", &jit, 4);
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
        eprintln!("setting call for {test_name}");
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

    fn build_string_ctor_func<'ctx>(
        s: &str,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> FunctionValue<'ctx> {
        let bytes = s.as_bytes();

        let f_ty = TValue::tvalue_string_type(context)
            .ptr_type(Default::default())
            .fn_type(&[], false);
        let f = module.add_function("new_seeded_string", f_ty, None);
        let entry = context.append_basic_block(f.clone(), "entry");
        let ba_f = module
            .get_function("new_byte_array")
            .expect("new_byte_array");
        builder.position_at_end(entry);
        let s_ptr = builder.build_alloca(context.i8_type().array_type(0), "s_ptr");
        let mut b = Vec::with_capacity(bytes.len());
        
        for &ch in bytes.iter() {
            b.push(context.i8_type().const_int(ch as _, false))
        }
        
        let b = context.i8_type()
            .const_array(b.as_slice());
        builder.build_store(s_ptr.clone(), b);
        let new_s_f = module
            .get_function("tvalue_new_string")
            .expect("string ctor");
        let ba = builder.build_call(
            ba_f,
            &[
                context.i8_type().const_int(bytes.len() as _, false).into(),
                s_ptr.into(),
            ],
            "ba",
        );
        let ret = builder.build_call(
            new_s_f,
            &[ba.as_any_value_enum().into_pointer_value().into()],
            "ret",
        );
        builder.build_return(Some(&ret.as_any_value_enum().into_pointer_value()));
        f
    }
}
