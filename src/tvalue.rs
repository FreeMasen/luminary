use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::{
        AsDIScope, DIBasicType, DICompileUnit, DIFile, DIScope, DIType, DebugInfoBuilder,
    },
    module::Module,
    types::{AnyType, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::{AnyValue, BasicValue, FunctionValue, IntValue},
    AddressSpace,
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

pub struct TValueModuleBuilder<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    debug_builder: DebugInfoBuilder<'ctx>,
    debug_compile_unit: DICompileUnit<'ctx>,
    debug_file: DIFile<'ctx>,
    global_scope: DIScope<'ctx>,
    di_types: DebugTypes<'ctx>,
}

struct DebugTypes<'ctx> {
    bool_type: DIBasicType<'ctx>,
    i8_type: DIBasicType<'ctx>,
    i32_type: DIBasicType<'ctx>,
    i64_type: DIBasicType<'ctx>,
    f32_type: DIBasicType<'ctx>,
    others: HashMap<String, DIType<'ctx>>,
}

impl<'ctx> DebugTypes<'ctx> {
    fn new(builder: &DebugInfoBuilder<'ctx>) -> Self {
        let i8_type = builder.create_basic_type("i8", 8, 0, 0).unwrap();
        Self {
            i8_type,
            bool_type: builder.create_basic_type("i1", 1, 0, 0).unwrap(),
            i32_type: builder.create_basic_type("i32", 32, 0, 0).unwrap(),
            i64_type: builder.create_basic_type("i64", 64, 0, 0).unwrap(),
            f32_type: builder.create_basic_type("f32", 32, 0, 0).unwrap(),
            others: HashMap::new(),
        }
    }
}

impl<'ctx> TValueModuleBuilder<'ctx> {
    pub(crate) const MODULE_NAME: &str = "std::tvalue";
    pub(crate) const BASE_TYPE_NAME: &str = "std::tvalue::TValue";
    pub(crate) const NIL_TYPE_NAME: &str = "std::tvalue::TValue::Nil";
    pub(crate) const BOOL_TYPE_NAME: &str = "std::tvalue::TValue::Bool";
    pub(crate) const NUM_TYPE_NAME: &str = "std::tvalue::TValue::Number";
    pub(crate) const STR_TYPE_NAME: &str = "std::tvalue::TValue::String";
    pub(crate) const TBL_TYPE_NAME: &str = "std::tvalue::TValue::Table";
    pub(crate) const BYTE_ARRAY_TYPE: &str = "std::tvalue::ByteArray";
    pub(crate) const BASE_CTOR_NAME: &str = "std::tvalue_new";
    pub(crate) const NIL_CTOR_NAME: &str = "std::tvalue_new_nil";
    pub(crate) const BOOL_CTOR_NAME: &str = "std::tvalue_new_bool";
    pub(crate) const NUM_CTOR_NAME: &str = "std::tvalue_new_num";
    pub(crate) const STR_CTOR_NAME: &str = "std::tvalue_new_str";
    pub(crate) const BARR_CTOR_NAME: &str = "std::byte_array_new";
    pub(crate) const GET_TAG_FN_NAME: &str = "getTValueTag";
    const DATA_SIZE: u32 = 15;

    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("std::tvalue");
        let (debug_builder, debug_compile_unit) = module.create_debug_info_builder(
            false,
            inkwell::debug_info::DWARFSourceLanguage::C,
            "std_tvalue.ll",
            ".",
            "luminary",
            false,
            "",
            1,
            "",
            inkwell::debug_info::DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );
        let di_file = debug_builder.create_file(&format!("{}.ll", Self::MODULE_NAME), "");
        let di_scope = di_file.as_debug_info_scope();
        let di_types = DebugTypes::new(&debug_builder);
        Self {
            builder: context.create_builder(),
            module,
            debug_builder,
            debug_compile_unit,
            context,
            debug_file: di_file,
            global_scope: di_scope,
            di_types,
        }
    }
    #[cfg(test)]
    fn gen_test_stuff(&self) {
        let printf = self.module.add_function(
            "printf",
            self.context.i32_type().fn_type(
                &[self.context.i8_type().ptr_type(Default::default()).into()],
                true,
            ),
            None,
        );
        let print_tvalue = self.module.add_function(
            "print_tvalue",
            self.context.void_type().fn_type(
                &[self
                    .module
                    .get_struct_type(Self::BASE_TYPE_NAME)
                    .unwrap()
                    .ptr_type(Default::default())
                    .into()],
                false,
            ),
            None,
        );
        let entry = self.context.append_basic_block(print_tvalue, "entry");
        let loop_top = self.context.append_basic_block(print_tvalue, "looptop");
        let exit = self.context.append_basic_block(print_tvalue, "exit");
        self.builder.position_at_end(entry);

        let start_brace = self
            .builder
            .build_alloca(self.context.i8_type().array_type(2), "ob");
        self.builder
            .build_store(start_brace, self.context.const_string(b"[", true));
        self.builder.build_call(printf, &[start_brace.into()], "");
        self.builder.build_unconditional_branch(loop_top);
        self.builder.position_at_end(loop_top);
        let phi = self.builder.build_phi(self.context.i32_type(), "idx");
        let next_idx = self.builder.build_int_add(
            phi.as_any_value_enum().into_int_value(),
            self.context.i32_type().const_int(1, false),
            "nextidx",
        );
        phi.add_incoming(&[
            (&next_idx, loop_top),
            (&self.context.i32_type().const_int(0, false), entry),
        ]);
        let ch = unsafe {
            self.builder.build_gep(
                self.context.i8_type().array_type(16),
                print_tvalue
                    .get_first_param()
                    .unwrap()
                    .as_any_value_enum()
                    .into_pointer_value(),
                &[
                    self.context.i32_type().const_int(0, false),
                    phi.as_basic_value().into_int_value(),
                ],
                "ch",
            )
        };
        let ch = self.builder.build_load(self.context.i8_type(), ch, "ch");
        let ch_fmt = self
            .builder
            .build_alloca(self.context.i8_type().array_type(5), "ch_fmt");
        self.builder
            .build_store(ch_fmt, self.context.const_string(b"%*i,", true));
        self.builder.build_call(
            printf,
            &[
                ch_fmt.into(),
                self.context.i8_type().const_int(3, false).into(),
                ch.into(),
            ],
            "",
        );
        let br = self.builder.build_int_compare(
            inkwell::IntPredicate::UGT,
            next_idx,
            self.context.i32_type().const_int(15, false),
            "done",
        );
        self.builder.build_conditional_branch(br, exit, loop_top);
        self.builder.position_at_end(exit);
        let end_brace = self
            .builder
            .build_alloca(self.context.i8_type().array_type(3), "cb");
        self.builder
            .build_store(end_brace, self.context.const_string(b"]\n", true));
        self.builder.build_call(printf, &[end_brace.into()], "");
        self.builder.build_return(None);
    }

    pub fn gen_lib(&mut self) -> Module<'ctx> {
        self.gen_types();
        #[cfg(test)]
        {
            self.gen_test_stuff();
        }
        self.add_init_nil();
        self.add_init_bool();
        self.add_init_number();
        self.add_get_tag_function();
        self.add_get_value_bool();
        self.add_tvalue_is_number();
        self.add_tvalue_get_number();
        self.add_tvalue_add();
        self.debug_builder.finalize();
        self.module.verify().unwrap_or_else(|e| {
            eprintln!("{}", self.module.to_string());
            let es = e.to_string();
            let mut ct = 0;
            for line in es.lines() {
                eprintln!("{line}");
                ct += 1;
            }
            panic!("Found {} errors", ct / 2);
        });
        self.module.clone()
    }

    fn add_init_nil(&self) {
        self.add_new_init(Self::NIL_TYPE_NAME, None, Self::NIL_CTOR_NAME, 0);
    }

    fn add_init_bool(&self) {
        self.add_new_init(
            Self::BOOL_TYPE_NAME,
            Some(self.context.bool_type().into()),
            Self::BOOL_CTOR_NAME,
            1,
        );
    }

    fn add_init_number(&self) {
        self.add_new_init(
            Self::NUM_TYPE_NAME,
            Some(self.context.f32_type().into()),
            Self::NUM_CTOR_NAME,
            2,
        );
    }

    fn gen_types(&mut self) {
        self.gen_type(
            Self::BASE_TYPE_NAME,
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
            ],
        );
        let base_type = self.debug_builder.create_struct_type(
            self.global_scope,
            Self::BASE_TYPE_NAME,
            self.debug_file,
            3,
            8 * 16,
            8,
            0,
            None,
            &[
                self.di_types.i8_type.as_type(),
                self.debug_builder
                    .create_array_type(
                        self.di_types.i8_type.as_type(),
                        (8 * Self::DATA_SIZE) as u64,
                        8,
                        &[],
                    )
                    .as_type(),
            ],
            0,
            None,
            Self::BASE_TYPE_NAME,
        );
        self.di_types
            .others
            .insert(Self::BASE_TYPE_NAME.into(), base_type.as_type());
        self.gen_type(
            Self::NIL_TYPE_NAME,
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
            ],
        );

        let nil_variant = self.debug_builder.create_struct_type(
            self.global_scope,
            Self::NIL_TYPE_NAME,
            self.debug_file,
            3,
            8 * 16,
            8,
            0,
            None,
            &[
                self.di_types.i8_type.as_type(),
                self.debug_builder
                    .create_array_type(
                        self.di_types.i8_type.as_type(),
                        (8 * Self::DATA_SIZE) as u64,
                        8,
                        &[],
                    )
                    .as_type(),
            ],
            0,
            None,
            Self::NIL_TYPE_NAME,
        );
        self.di_types
            .others
            .insert(Self::NIL_TYPE_NAME.into(), nil_variant.as_type());
        self.gen_type(
            Self::BOOL_TYPE_NAME,
            &[
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
                self.context.i8_type().into(),
            ],
        );
        let bool_variant = self.debug_builder.create_struct_type(
            self.global_scope,
            Self::BOOL_TYPE_NAME,
            self.debug_file,
            3,
            8 * 16,
            8,
            0,
            None,
            &[
                self.di_types.i8_type.as_type(),
                self.debug_builder
                    .create_array_type(
                        self.di_types.i8_type.as_type(),
                        (8 * Self::DATA_SIZE) as u64,
                        8,
                        &[],
                    )
                    .as_type(),
            ],
            0,
            None,
            Self::BOOL_TYPE_NAME,
        );
        self.di_types
            .others
            .insert(Self::BOOL_TYPE_NAME.into(), bool_variant.as_type());
        self.gen_type(
            Self::NUM_TYPE_NAME,
            &[
                self.context
                    .i8_type()
                    .array_type((Self::DATA_SIZE + 1) / 2)
                    .into(),
                self.context.f32_type().into(),
            ],
        );
        let num_variant = self.debug_builder.create_struct_type(
            self.global_scope,
            Self::NUM_TYPE_NAME,
            self.debug_file,
            3,
            8 * 16,
            8,
            0,
            None,
            &[
                self.di_types.i8_type.as_type(),
                self.di_types.f32_type.as_type(),
            ],
            0,
            None,
            Self::NUM_TYPE_NAME,
        );
        self.di_types
            .others
            .insert(Self::NUM_TYPE_NAME.into(), num_variant.as_type());
        self.gen_type(
            Self::BYTE_ARRAY_TYPE,
            &[
                self.context.i32_type().into(),
                self.context.i8_type().ptr_type(Default::default()).into(),
            ],
        );
        let byte_array_type = self.debug_builder.create_struct_type(
            self.global_scope,
            Self::BYTE_ARRAY_TYPE,
            self.debug_file,
            3,
            8 * 16,
            8,
            0,
            None,
            &[
                self.di_types.i32_type.as_type(),
                self.di_types.i64_type.as_type(),
            ],
            0,
            None,
            Self::BYTE_ARRAY_TYPE,
        );
        self.di_types
            .others
            .insert(Self::BYTE_ARRAY_TYPE.into(), byte_array_type.as_type());
        self.gen_type(
            Self::STR_TYPE_NAME,
            &[
                self.context.i8_type().into(),
                // ptr is fully opaque so using the i8_type() here doesn't matter
                self.context.i8_type().ptr_type(Default::default()).into(),
            ],
        );
        let string_variant = self.debug_builder.create_struct_type(
            self.global_scope,
            Self::STR_TYPE_NAME,
            self.debug_file,
            3,
            8 * 16,
            8,
            0,
            None,
            &[
                self.di_types.i8_type.as_type(),
                self.debug_builder
                    .create_pointer_type(
                        "byte_array_ptr",
                        byte_array_type.as_type(),
                        64,
                        8,
                        Default::default(),
                    )
                    .as_type(),
            ],
            0,
            None,
            Self::STR_TYPE_NAME,
        );
        self.di_types
            .others
            .insert(Self::STR_TYPE_NAME.into(), string_variant.as_type());
        self.di_types.others.insert(
            Self::STR_TYPE_NAME.into(),
            self.debug_builder
                .create_pointer_type(
                    &format!("{}_ptr", Self::BASE_TYPE_NAME),
                    base_type.as_type(),
                    8 * 16,
                    8,
                    Default::default(),
                )
                .as_type(),
        );
    }

    fn gen_type(&self, name: &str, fields: &[BasicTypeEnum<'ctx>]) {
        let base = self.context.opaque_struct_type(name);
        base.set_body(fields, false);
    }

    fn add_new_init(
        &self,
        struct_name: &str,
        ctor_arg: Option<BasicMetadataTypeEnum<'ctx>>,
        init_name: &'static str,
        variant_id: u64,
    ) -> FunctionValue<'ctx> {
        let ret_ty = self
            .module
            .get_struct_type(struct_name)
            .unwrap_or_else(|| panic!("{struct_name} is not defined"));

        let enum_base_ty = self
            .module
            .get_struct_type(Self::BASE_TYPE_NAME)
            .expect("base struct name to be defined");
        // let base_ctor = self.module.get_function(Self::BASE_CTOR_NAME).unwrap();
        let mut ctor_args: Vec<BasicMetadataTypeEnum> =
            vec![enum_base_ty.ptr_type(Default::default()).into()];

        if let Some(ctor_arg) = ctor_arg {
            ctor_args.push(ctor_arg)
        }
        let f = self.context.void_type().fn_type(&ctor_args, false);
        let f = self.module.add_function(init_name, f, None);
        let entry = self.context.append_basic_block(f.clone(), "entry");
        self.builder.position_at_end(entry);
        let arg1 = f.get_first_param().expect("1 arg");

        let tag_ptr = self
            .builder
            .build_struct_gep(
                self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap(),
                arg1.as_any_value_enum().into_pointer_value(),
                0,
                "tag_ptr",
            )
            .expect("tag_ptr");

        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(variant_id, false));
        if let Some(print_t) = self.module.get_function("print_tvalue") {
            self.builder.build_call(
                print_t,
                &[arg1.as_any_value_enum().into_pointer_value().into()],
                "",
            );
        }
        if ctor_args.len() < 2 {
            let data_ptr = self
                .builder
                .build_struct_gep(
                    self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap(),
                    arg1.into_pointer_value(),
                    1,
                    "data_ptr",
                )
                .unwrap();
            self.builder
                .build_store(data_ptr, self.context.i8_type().array_type(15).const_zero());
            self.builder.build_return(None);
            return f;
        }
        let data_ptr = self
            .builder
            .build_struct_gep(
                ret_ty,
                arg1.as_any_value_enum().into_pointer_value(),
                1,
                "data_ptr",
            )
            .expect("data_ptr");
        let arg2 = f.get_last_param().expect("init has 2 argument");
        self.builder.build_store(data_ptr, arg2);
        if let Some(print_t) = self.module.get_function("print_tvalue") {
            self.builder.build_call(
                print_t,
                &[arg1.as_any_value_enum().into_pointer_value().into()],
                "",
            );
        }
        self.builder.build_return(None);
        f
    }

    fn add_get_tag_function(&self) {
        let base_type = self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap();
        let get_tag_ty = self
            .context
            .i8_type()
            .fn_type(&[base_type.ptr_type(0u16.into()).into()], false);
        let func = self
            .module
            .add_function(Self::GET_TAG_FN_NAME, get_tag_ty, None);
        let arg_ptr = func.get_param_iter().next().unwrap().into_pointer_value();
        arg_ptr.set_name("tagged_value");
        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);
        let tag_gep = unsafe {
            self.builder.build_in_bounds_gep(
                base_type,
                arg_ptr,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        let tag_load = self
            .builder
            .build_load(self.context.i8_type(), tag_gep, "tag_val");
        self.builder.build_return(Some(&tag_load));
    }

    fn add_get_value_bool(&self) {
        let get_tag_ty = self.context.bool_type().fn_type(
            &[self
                .module
                .get_struct_type(Self::BASE_TYPE_NAME)
                .unwrap()
                .ptr_type(0u16.into())
                .into()],
            false,
        );
        let func = self
            .module
            .add_function("getTValueValue_bool", get_tag_ty, None);
        let arg_ptr = func.get_param_iter().next().unwrap().into_pointer_value();
        arg_ptr.set_name("tagged_value");
        let entry_block = self.context.append_basic_block(func.clone(), "entry");
        self.builder.position_at_end(entry_block.clone());

        let get_tag_func = self
            .module
            .get_function(Self::GET_TAG_FN_NAME)
            .expect("getTagFn");
        let call = self
            .builder
            .build_call(get_tag_func, &[arg_ptr.into()], "tag");
        let cmp_tag = call.as_any_value_enum();
        let is_nil = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            cmp_tag.into_int_value(),
            self.context.i8_type().const_int(0, false),
            "is_nil",
        );
        let is_nil_block = self.context.append_basic_block(func.clone(), "is_nil");
        let is_not_nil_block = self.context.append_basic_block(func.clone(), "is_not_nil");
        self.builder.build_conditional_branch(
            is_nil.clone(),
            is_nil_block.clone(),
            is_not_nil_block,
        );
        self.builder.position_at_end(is_nil_block.clone());
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
        self.builder.position_at_end(is_not_nil_block.clone());
        let is_bool = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            cmp_tag.into_int_value(),
            self.context.i8_type().const_int(1, false),
            "is_bool",
        );
        let is_bool_block = self.context.append_basic_block(func.clone(), "is_bool");
        let is_not_bool_block = self.context.append_basic_block(func, "is_not_bool");
        self.builder.build_conditional_branch(
            is_bool,
            is_bool_block.clone(),
            is_not_bool_block.clone(),
        );
        self.builder.position_at_end(is_not_bool_block.clone());
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(is_bool_block);
        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                self.module.get_struct_type(Self::BOOL_TYPE_NAME).unwrap(),
                arg_ptr,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(1, false),
                ],
                "raw_variant_pointer",
            )
        };
        let load_bool = self
            .builder
            .build_load(self.context.i8_type(), gep, "bool_t_int");
        let ret = self.builder.build_int_compare(
            inkwell::IntPredicate::UGT,
            load_bool.into_int_value(),
            self.context.i8_type().const_int(0, false),
            "ret",
        );
        self.builder.build_return(Some(&ret));
    }

    /// Add a function to self.module and append a basic block to that function. Set self.builder's
    /// position to the entry block
    fn add_function(
        &self,
        name: &str,
        ty: FunctionType<'ctx>,
    ) -> (FunctionValue<'ctx>, BasicBlock<'ctx>) {
        let f = self.module.add_function(name, ty, None);
        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);
        (f, entry)
    }

    fn add_tvalue_is_number(&self) {
        let tvalue = self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap();
        let get_tag = self.module.get_function(Self::GET_TAG_FN_NAME).unwrap();
        let is_num_ty = self
            .context
            .bool_type()
            .fn_type(&[tvalue.ptr_type(Default::default()).into()], false);
        let (is_num_fn, _entry) = self.add_function("tvalue_is_number", is_num_ty);
        let tag = self.builder.build_call(
            get_tag,
            &[is_num_fn
                .get_first_param()
                .unwrap()
                .as_basic_value_enum()
                .into()],
            "tag",
        );
        let cmp = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            tag.as_any_value_enum().into_int_value(),
            self.context.i8_type().const_int(2, false),
            "cmp",
        );
        self.builder.build_return(Some(&cmp));
    }

    fn add_tvalue_get_number(&self) {
        let tvalue = self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap();
        let tvalue_number = self.module.get_struct_type(Self::NUM_TYPE_NAME).unwrap();
        let is_num_fn = self
            .module
            .get_function("tvalue_is_number")
            .expect("is_number");
        let ret_ty = self.context.f32_type();
        let (get_num_fn, _entry) = self.add_function(
            "tvalue_get_number",
            ret_ty.fn_type(&[tvalue.ptr_type(Default::default()).into()], false),
        );
        let arg = get_num_fn.get_first_param().expect("1 param");
        let is_num = self.builder.build_call(is_num_fn, &[arg.into()], "is_num");
        let ian = self.context.append_basic_block(get_num_fn, "ian");
        let nan = self.context.append_basic_block(get_num_fn, "nan");
        self.builder.build_conditional_branch(
            is_num.as_any_value_enum().into_int_value(),
            ian,
            nan,
        );
        self.builder.position_at_end(ian);
        let value_ptr = self
            .builder
            .build_struct_gep(tvalue_number, arg.into_pointer_value(), 1, "value_ptr")
            .expect("2 props");
        let value = self
            .builder
            .build_load(self.context.f32_type(), value_ptr, "value");
        self.builder.build_return(Some(&value));
        self.builder.position_at_end(nan);
        let ret_val = ret_ty.const_float(f64::NAN);
        self.builder
            .build_return(Some(&ret_val.as_basic_value_enum()));
    }

    fn add_tvalue_add(&self) {
        let tvalue = self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap();
        let tvalue_number = self.module.get_struct_type(Self::NUM_TYPE_NAME).unwrap();
        let is_num_fn = self
            .module
            .get_function("tvalue_is_number")
            .expect("tvalue_is_number");
        let get_num_fn = self
            .module
            .get_function("tvalue_get_number")
            .expect("tvalue_get_number");
        let init_num_fn = self
            .module
            .get_function(Self::NUM_CTOR_NAME)
            .expect(Self::NUM_CTOR_NAME);
        let (f, _entry) = self.add_function(
            "tvalue_add",
            self.context.bool_type().fn_type(
                &[
                    tvalue.ptr_type(Default::default()).into(),
                    tvalue.ptr_type(Default::default()).into(),
                    tvalue.ptr_type(Default::default()).into(),
                ],
                false,
            ),
        );

        let params = f.get_params();
        let lhs = params.get(0).expect("1 param");
        lhs.set_name("lhs");
        let rhs = params.get(1).expect("2 params");
        rhs.set_name("rhs");
        let out = params.get(2).expect("3 params");
        out.set_name("out");
        let nan = self.context.append_basic_block(f, "nan");
        let lhs_ian = self.context.append_basic_block(f, "lhs_ian");
        let rhs_ian = self.context.append_basic_block(f, "rhs_ian");
        let lhs_is_num =
            self.builder
                .build_call(is_num_fn, &[lhs.as_basic_value_enum().into()], "lhs_is_num");

        self.builder.build_conditional_branch(
            lhs_is_num.as_any_value_enum().into_int_value(),
            lhs_ian,
            nan,
        );
        self.builder.position_at_end(lhs_ian);
        let rhs_is_num =
            self.builder
                .build_call(is_num_fn, &[rhs.as_basic_value_enum().into()], "rhs_is_num");

        self.builder.build_conditional_branch(
            rhs_is_num.as_any_value_enum().into_int_value(),
            rhs_ian,
            nan,
        );
        self.builder.position_at_end(rhs_ian);
        let lhs_value =
            self.builder
                .build_call(get_num_fn, &[lhs.as_basic_value_enum().into()], "lhs_value");
        let rhs_value =
            self.builder
                .build_call(get_num_fn, &[rhs.as_basic_value_enum().into()], "rhs_value");
        let out_value = self.builder.build_float_add(
            rhs_value.as_any_value_enum().into_float_value(),
            lhs_value.as_any_value_enum().into_float_value(),
            "out_value",
        );
        self.builder
            .build_call(init_num_fn, &[out.as_basic_value_enum().into()], "");
        let out_value_ptr = self
            .builder
            .build_struct_gep(tvalue_number, out.into_pointer_value(), 1, "out_value_ptr")
            .expect("gep");
        self.builder.build_store(out_value_ptr, out_value);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(nan);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
    }
}

pub struct TableField {
    pub name: String,
    pub value: TValue,
}

pub struct TvalueRust;

impl TvalueRust {
    pub fn get_module<'ctx>(context: &'ctx Context) -> Module<'ctx> {
        let bitcode_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("tvalue.bc");
        Module::parse_bitcode_from_path(bitcode_path, context).unwrap()
    }
}

#[cfg(test)]
mod test {

    use std::path::PathBuf;

    use super::*;
    use inkwell::{
        execution_engine::{ExecutionEngine, JitFunction},
        values::{BasicMetadataValueEnum, FunctionValue, IntValue},
    };

    #[test]
    fn generate_t_value_module() {
        let context = Context::create();
        let mut builder = TValueModuleBuilder::new(&context);
        let module = builder.gen_lib();
        insta::assert_snapshot!(module.to_string())
    }

    #[test]
    fn test_tvalue_bc() {
        let context = Context::create();
        let module = context.create_module("tvalue_test");
        let rust_mod = TvalueRust::get_module(&context);
        module.link_in_module(rust_mod).unwrap();
        let main = module.add_function("main", context.i8_type().fn_type(&[], false), None);
        let entry = context.append_basic_block(main, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry);
        let tvalue_type = module.get_struct_type("TValue::Bool").unwrap();
        let tvalue_ptr = builder.build_alloca(tvalue_type, "tvalue_ptr");
        let init_bool = module.get_function("::tvalue::new_bool").unwrap();
        builder.build_call(
            init_bool,
            &[
                tvalue_ptr.into(),
                context.bool_type().const_int(1, false).into(),
            ],
            "",
        );
        let bool_ptr = builder
            .build_struct_gep(tvalue_type, tvalue_ptr, 1, "bool_ptr")
            .unwrap();
        let b = builder.build_load(context.bool_type(), bool_ptr, "b");
        let succ = context.append_basic_block(main, "succ");
        let fail = context.append_basic_block(main, "fail");
        builder.build_conditional_branch(b.into_int_value(), succ, fail);
        builder.position_at_end(fail);
        builder.build_return(Some(&context.i8_type().const_int(1, false)));
        builder.position_at_end(succ);
        let init_number = module.get_function("::tvalue::new_number").unwrap();
        let number_type = module.get_struct_type("TValue::Number").unwrap();
        let n = builder.build_alloca(number_type, "n");
        builder.build_call(
            init_number,
            &[
                n.as_basic_value_enum().into(),
                context.f32_type().const_float(42.0).into(),
            ],
            "",
        );
        let num_ptr = builder
            .build_struct_gep(number_type, n, 1, "num_ptr")
            .unwrap();
        let num = builder.build_load(context.f32_type(), num_ptr, "num");
        let is_eq = builder.build_float_compare(
            inkwell::FloatPredicate::OEQ,
            num.into_float_value(),
            context.f32_type().const_float(42.0),
            "is_eq",
        );
        let succ = context.append_basic_block(main, "succ");
        builder.build_conditional_branch(is_eq, succ, fail);
        builder.position_at_end(succ);
        let table_type = module.get_struct_type("TValue::Table").unwrap();
        let table_init = module.get_function("::tvalue::new_table").unwrap();
        let table_ptr = builder.build_alloca(table_type, "table_ptr");
        builder.build_call(table_init, &[
            table_ptr.into()
        ], "");
        let table_insert = module.get_function("::tvalue::table_insert").unwrap();
        // builder.build_call(
        //     table_insert,
        //     &[
        //         table_ptr.into(),
        //         n.into(),
        //         tvalue_ptr.into(),
        //     ],
        //     ""
        // );
        // let table_get = module.get_function("::tvalue::table_get").unwrap();
        // let dest = builder.build_alloca(tvalue_type, "dest");
        // builder.build_call(table_get, &[
        //     table_ptr.into(), n.into(), dest.into()
        // ], "");
        // let b2_ptr = builder.build_struct_gep(tvalue_type, dest, 1, "b2_ptr").unwrap();
        // let b2 = builder.build_load(context.bool_type(), b2_ptr, "b2");
        let succ = context.append_basic_block(main, "succ");
        builder.build_conditional_branch(context.bool_type().const_int(1, false), succ, fail);
        // builder.build_conditional_branch(b2.into_int_value(), succ, fail);
        builder.position_at_end(succ);
        builder.build_return(Some(&context.i8_type().const_int(0, false)));
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
        std::fs::write("test.ll", module.to_string()).unwrap();
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        jit.run_static_constructors();
        type Func = unsafe extern "C" fn() -> u8;
        println!("looking up main");
        let func: JitFunction<Func> = unsafe {
            jit.get_function("main").unwrap_or_else(|e| {
                panic!("main: {e}");
            })
        };
        println!("calling main");
        let val = unsafe { func.call() };
        println!("called main -> {val}");
    }

    #[test]
    fn check_get_tag() {
        let context = Context::create();
        let mut builder = TValueModuleBuilder::new(&context);
        let module = builder.gen_lib();
        let new_nil_fun = module
            .get_function(TValueModuleBuilder::NIL_CTOR_NAME)
            .unwrap();
        let new_bool_fun = module
            .get_function(TValueModuleBuilder::BOOL_CTOR_NAME)
            .unwrap();
        let new_num_fun = module
            .get_function(TValueModuleBuilder::NUM_CTOR_NAME)
            .unwrap();
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
        // let s_ctor = build_string_ctor_func("hello world", &context, &module, &builder);
        // build_test_tag_func(&context, &module, &builder, "test_string", s_ctor, &[]);
        if std::env::var("LUMINARY_WRITE_TEST_IR")
            .map(|v| v == "1")
            .unwrap_or(false)
        {
            write_test_module("tag_value", &module);
        }
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        call_tag_test_fn("test_nil", &jit, 0);
        call_tag_test_fn("test_bool", &jit, 1);
        call_tag_test_fn("test_num", &jit, 2);
        // call_tag_test_fn("test_string", &jit, 3);
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
        let tag_fun = module
            .get_function(TValueModuleBuilder::GET_TAG_FN_NAME)
            .unwrap();
        let entry = context.append_basic_block(test_func, "entry");
        builder.position_at_end(entry);
        eprintln!("setting call for {test_name}");
        let arg = builder.build_alloca(
            module
                .get_struct_type(TValueModuleBuilder::BASE_TYPE_NAME)
                .unwrap(),
            "base",
        );
        let mut args: Vec<BasicMetadataValueEnum> = vec![arg.into()];
        for arg in ctor_args {
            args.push(arg.clone());
        }
        builder.build_call(ctor.into(), args.as_slice(), "arg");
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
        let func: JitFunction<TestTagFunc> = unsafe {
            jit.get_function(name).unwrap_or_else(|e| {
                panic!("{name}: {e}");
            })
        };
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
        let f_ty = module
            .get_struct_type(TValueModuleBuilder::STR_TYPE_NAME)
            .unwrap();
        let f_ty = f_ty.ptr_type(Default::default()).fn_type(&[], false);
        let f = module.add_function("new_seeded_string", f_ty, None);
        let entry = context.append_basic_block(f.clone(), "entry");
        let ba_f = module
            .get_function(TValueModuleBuilder::BARR_CTOR_NAME)
            .expect("new_byte_array");
        builder.position_at_end(entry);
        let s_ptr = builder.build_alloca(context.i8_type().array_type(s.len() as _), "s_ptr");
        let mut b = Vec::with_capacity(bytes.len());

        for &ch in bytes.iter() {
            b.push(context.i8_type().const_int(ch as _, false))
        }

        let b = context.i8_type().const_array(b.as_slice());
        builder.build_store(s_ptr.clone(), b);
        let new_s_f = module
            .get_function(TValueModuleBuilder::STR_CTOR_NAME)
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

    #[test]
    fn check_truthiness() {
        let context = Context::create();
        inkwell::support::enable_llvm_pretty_stack_trace();

        let mut builder = TValueModuleBuilder::new(&context);
        let module = builder.gen_lib();
        let builder = context.create_builder();
        let is_bool = module
            .get_function("getTValueValue_bool")
            .expect("getTValueValue_bool");
        for bb in is_bool.get_basic_blocks() {
            let mut name = bb.get_name().to_str().unwrap();
            if name == "entry" {
                name = "getTValueValue_bool";
            }
            builder.position_at(bb, &bb.get_first_instruction().unwrap());
            build_print(&context, &module, &builder, name);
        }
        let f = module.add_function(
            "print_tvalue",
            context.void_type().fn_type(
                &[module
                    .get_struct_type(TValueModuleBuilder::BASE_TYPE_NAME)
                    .unwrap()
                    .ptr_type(Default::default())
                    .into()],
                false,
            ),
            None,
        );
        let entry = context.append_basic_block(f, "entry");
        builder.position_at_end(entry);
        let buf_ptr = builder.build_alloca(context.i8_type().array_type(17), "buf");
        let struct_bytes = builder.build_load(
            context.i8_type().array_type(17),
            f.get_first_param().unwrap().into_pointer_value(),
            "struct_bytes",
        );
        builder.build_store(buf_ptr, context.i8_type().array_type(17).const_zero());
        builder.build_store(buf_ptr, struct_bytes);
        builder.build_return(None);
        build_truthy_test(
            &context,
            &module,
            &builder,
            module
                .get_function(TValueModuleBuilder::NIL_CTOR_NAME)
                .unwrap(),
            context.bool_type().const_int(0, false).into(),
            "test_nil",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module
                .get_function(TValueModuleBuilder::BOOL_CTOR_NAME)
                .unwrap(),
            context.bool_type().const_int(1, false).into(),
            "test_bool_true",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module
                .get_function(TValueModuleBuilder::BOOL_CTOR_NAME)
                .unwrap(),
            context.bool_type().const_int(0, false).into(),
            "test_bool_false",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module
                .get_function(TValueModuleBuilder::NUM_CTOR_NAME)
                .unwrap(),
            context.f32_type().const_float(0.11).into(),
            "test_number",
        );

        if std::env::var("LUMINARY_WRITE_TEST_IR")
            .map(|v| v == "1")
            .unwrap_or(false)
        {
            write_test_module("truthy", &module);
        }

        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        execute_truthy_test("test_nil", &jit, false);
        execute_truthy_test("test_bool_true", &jit, true);
        execute_truthy_test("test_bool_false", &jit, false);
        execute_truthy_test("test_number", &jit, true);
    }

    fn build_print<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        s: &str,
    ) {
        // let printf = module.get_function("printf").expect("printf");
        // let s_ptr = builder.build_alloca(context.i8_type().array_type((s.len() + 2) as _), "s");
        // builder.build_store(s_ptr, context.i8_type().const_array(&s_to_arr_nl_z(context, s)));
        // builder.build_call(printf, &[
        //     s_ptr.into(),
        // ], "");
    }

    fn s_to_arr_nl_z<'ctx>(context: &'ctx Context, s: &str) -> Vec<IntValue<'ctx>> {
        s.as_bytes()
            .into_iter()
            .map(|&b| context.i8_type().const_int(b as _, false))
            .chain([
                context.i8_type().const_int(10, false), // \n
                context.i8_type().const_int(0, false),  // \0
            ])
            .collect()
    }

    fn build_truthy_test<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        ctor_fn: FunctionValue<'ctx>,
        ctor_arg: BasicMetadataValueEnum<'ctx>,
        name: &str,
    ) {
        let f = module.add_function(name, context.i32_type().fn_type(&[], false), None);
        let entry = context.append_basic_block(f, "entry");
        let is_bool = module
            .get_function("getTValueValue_bool")
            .expect("getTValueValue_bool");
        builder.position_at_end(entry);
        build_print(context, module, builder, name);
        let alloc = builder.build_alloca(
            module
                .get_struct_type(TValueModuleBuilder::BASE_TYPE_NAME)
                .expect("base"),
            "t",
        );
        builder.build_call(ctor_fn, &[alloc.into(), ctor_arg], "");
        let ret = builder.build_call(is_bool, &[alloc.into()], "ret");
        let output_fmt = "is_true: %i";
        let fmt_ptr = builder.build_alloca(
            context.i8_type().array_type((output_fmt.len() + 2) as _),
            "fmt_ptr",
        );
        builder.build_store(
            fmt_ptr,
            context
                .i8_type()
                .const_array(&s_to_arr_nl_z(context, output_fmt)),
        );
        // let _output = builder.build_call(print, &[
        //     fmt_ptr.into(),
        //     ret.as_any_value_enum().into_int_value().into(),
        // ], "");
        let ret = builder.build_int_z_extend(
            ret.as_any_value_enum().into_int_value(),
            context.i32_type(),
            "ret",
        );
        build_print(context, module, builder, "----------");
        builder.build_return(Some(&ret));
    }

    fn execute_truthy_test<'ctx>(
        name: &'static str,
        jit: &'ctx ExecutionEngine<'ctx>,
        expected: bool,
    ) {
        type TestFunc = unsafe extern "C" fn() -> i8;
        println!("looking up {name}");
        let func: JitFunction<TestFunc> = unsafe {
            jit.get_function(name).unwrap_or_else(|e| {
                panic!("{name}: {e}");
            })
        };
        let val = unsafe { func.call() };
        assert_eq!(val, expected as _, "{name}");
    }

    fn write_test_module<'ctx>(name: &str, module: &Module<'ctx>) {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let path = path.join("test-ir");
        if !path.exists() {
            std::fs::create_dir_all(&path).ok();
        }
        let mut path = path.join(name);
        path.set_extension("ll");
        std::fs::write(path, module.to_string()).unwrap();
    }
}
