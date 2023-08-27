use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    debug_info::{
        AsDIScope, DIBasicType, DICompileUnit, DIFile, DIScope, DIType, DebugInfoBuilder,
    },
    module::Module,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{AnyValue, BasicValue, FunctionValue, IntValue},
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
        // self.add_new_base_ctor();
        self.add_get_tag_function();
        self.add_get_value_bool();
        // self.add_new_nil();
        // self.add_new_bool();
        // self.add_new_number();
        // self.add_new_string();
        // self.add_new_byte_array();
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

    fn add_new_base_ctor(&self) {
        let ret_ty = self
            .module
            .get_struct_type(Self::BASE_TYPE_NAME)
            .unwrap_or_else(|| panic!("{} is not defined", Self::BASE_TYPE_NAME));
        let sub_ty = self.debug_builder.create_subroutine_type(
            self.debug_file,
            self.di_types
                .others
                .get(&format!("{}_ptr", Self::BASE_TYPE_NAME))
                .cloned(),
            &[],
            0,
        );
        let _sub_program = self.debug_builder.create_function(
            self.global_scope,
            Self::BASE_CTOR_NAME,
            None,
            self.debug_file,
            6,
            sub_ty,
            true,
            true,
            0,
            0,
            false,
        );
        let f = ret_ty.ptr_type(0u16.into()).fn_type(&[], false);
        let f = self.module.add_function(Self::BASE_CTOR_NAME, f, None);
        // f.set_subprogram(sub_program);
        let entry = self.context.append_basic_block(f.clone(), "entry");
        self.builder.position_at_end(entry);
        let ret = self.builder.build_alloca(ret_ty, "ret");
        let tag_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap(),
                ret,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(0, false));
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                ret_ty,
                ret.clone(),
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(1, false),
                ],
                "data_ptr",
            )
        };
        self.builder
            .build_store(data_ptr, self.context.i8_type().array_type(15).const_zero());
        if let Some(print_t) = self.module.get_function("print_tvalue") {
            self.builder.build_call(print_t, &[ret.into()], "");
        }
        self.builder.build_return(Some(&ret));
    }

    fn add_new_nil(&self) {
        self.add_new_ctor(Self::NIL_TYPE_NAME, &[], Self::NIL_CTOR_NAME, 0);
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

    fn i8_const(&self, v: u64) -> IntValue {
        self.context.i8_type().const_int(v, false)
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
                self.context.i8_type().into(),
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

    fn add_new_bool(&self) {
        let bool_ctor = self.add_new_ctor(
            Self::BOOL_TYPE_NAME,
            &[self.context.i8_type().into()],
            Self::BOOL_CTOR_NAME,
            1,
        );
        let sub_rout_type = self.debug_builder.create_subroutine_type(
            self.debug_file,
            self.di_types
                .others
                .get(&format!("{}_ptr", Self::BASE_TYPE_NAME))
                .cloned(),
            &[self.di_types.bool_type.as_type()],
            0,
        );
        let arg = self.debug_builder.create_parameter_variable(
            self.global_scope,
            "b",
            0,
            self.debug_file,
            6,
            self.di_types.bool_type.as_type(),
            true,
            1,
        );
        let f = self.debug_builder.create_function(
            self.global_scope,
            Self::BOOL_CTOR_NAME,
            None,
            self.debug_file,
            6,
            sub_rout_type,
            true,
            true,
            6,
            0,
            false,
        );
        bool_ctor.set_subprogram(f);
    }

    fn add_new_number(&self) {
        self.add_new_ctor(Self::NUM_TYPE_NAME, &[], Self::NUM_CTOR_NAME, 2);
    }

    fn add_new_byte_array(&self) {
        let f = self.module.add_function(
            Self::BARR_CTOR_NAME,
            self.module
                .get_struct_type(Self::BYTE_ARRAY_TYPE)
                .unwrap()
                .ptr_type(Default::default())
                .fn_type(
                    &[
                        self.context.i64_type().into(),
                        self.context
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
        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);
        let ret = self.builder.build_alloca(
            self.module.get_struct_type(Self::BYTE_ARRAY_TYPE).unwrap(),
            "ret",
        );
        let len_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                self.module.get_struct_type(Self::BYTE_ARRAY_TYPE).unwrap(),
                ret.clone(),
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "len_ptr",
            )
        };
        self.builder
            .build_store(len_ptr, len_param.as_basic_value_enum());
        let bytes_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                self.module.get_struct_type(Self::BYTE_ARRAY_TYPE).unwrap(),
                ret.clone(),
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(1, false),
                ],
                "bytes_ptr",
            )
        };
        self.builder.build_store(bytes_ptr, bytes_param);
        self.builder.build_return(Some(&ret));
    }

    fn add_new_ctor(
        &self,
        struct_name: &str,
        ctor_args: &[BasicMetadataTypeEnum<'ctx>],
        ctor_name: &'static str,
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
        let base_ctor = self.module.get_function(Self::BASE_CTOR_NAME).unwrap();
        let f = enum_base_ty.ptr_type(0u16.into()).fn_type(ctor_args, false);
        let f = self.module.add_function(ctor_name, f, None);
        let entry = self.context.append_basic_block(f.clone(), "entry");
        self.builder.position_at_end(entry);
        let ret = self.builder.build_call(base_ctor, &[], "ret");

        let tag_ptr = self
            .builder
            .build_struct_gep(
                self.module.get_struct_type(Self::BASE_TYPE_NAME).unwrap(),
                ret.as_any_value_enum().into_pointer_value(),
                0,
                "tag_ptr",
            )
            .expect("tag_ptr");

        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(variant_id, false));
        if let Some(print_t) = self.module.get_function("print_tvalue") {
            self.builder.build_call(
                print_t,
                &[ret.as_any_value_enum().into_pointer_value().into()],
                "",
            );
        }
        if ctor_args.is_empty() {
            self.builder
                .build_return(Some(&ret.as_any_value_enum().into_pointer_value()));
            return f;
        }
        let data_ptr = self
            .builder
            .build_struct_gep(
                ret_ty,
                ret.as_any_value_enum().into_pointer_value(),
                1,
                "data_ptr",
            )
            .expect("data_ptr");
        let arg = f.get_first_param().expect("ctor has 1 argument");
        self.builder.build_store(data_ptr, arg);
        if let Some(print_t) = self.module.get_function("print_tvalue") {
            self.builder.build_call(
                print_t,
                &[ret.as_any_value_enum().into_pointer_value().into()],
                "",
            );
        }
        self.builder
            .build_return(Some(&ret.as_any_value_enum().into_pointer_value()));
        f
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

    fn add_new_string(&self) {
        let byte_array = self
            .module
            .get_struct_type(Self::BYTE_ARRAY_TYPE)
            .expect("byte array");
        self.add_new_ctor(
            Self::STR_TYPE_NAME,
            &[byte_array.ptr_type(Default::default()).into()],
            Self::STR_CTOR_NAME,
            3,
        );
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
}

pub struct TableField {
    pub name: String,
    pub value: TValue,
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
        let s_ctor = build_string_ctor_func("hello world", &context, &module, &builder);
        build_test_tag_func(&context, &module, &builder, "test_string", s_ctor, &[]);
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
        let tag_fun = module
            .get_function(TValueModuleBuilder::GET_TAG_FN_NAME)
            .unwrap();
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
        // let print = module.get_function("printf").expect("printf");
        builder.position_at_end(entry);
        build_print(context, module, builder, name);
        let alloc = builder.build_alloca(
            module
                .get_struct_type(TValueModuleBuilder::BASE_TYPE_NAME)
                .expect("base"),
            "t",
        );
        builder.build_call(ctor_fn, &[alloc.into(), ctor_arg], "");

        // let print_struct = module.get_function("print_tvalue").expect("print_tvalue");

        // builder.build_call(print_struct, &[
        //     arg.as_any_value_enum().into_pointer_value().into(),
        // ], "");
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
        // println!("calling {name}");
        let val = unsafe { func.call() };
        // println!("{name} -> {val}");
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
