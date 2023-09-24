//! This module is for generating a "standard library" that outlines how all lua values
//! will be represented in the LLVM IR, operator functions, etc.

use std::collections::HashMap;

use inkwell::{
    attributes::Attribute,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::{
        AsDIScope, DIBasicType, DICompileUnit, DIFile, DIScope, DIType, DebugInfoBuilder,
    },
    intrinsics::Intrinsic,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        AnyValue, BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue,
        PointerValue,
    },
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
    #[allow(unused)]
    debug_compile_unit: DICompileUnit<'ctx>,
    debug_file: DIFile<'ctx>,
    global_scope: DIScope<'ctx>,
    di_types: DebugTypes<'ctx>,
}
#[allow(dead_code)]
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

pub(crate) mod tvalue_names {
    pub const MODULE_NAME: &str = "std::tvalue";
    pub mod types {
        pub const BASE: &str = "std::tvalue::TValue";
        pub const NIL: &str = "std::tvalue::TValue::Nil";
        pub const BOOL: &str = "std::tvalue::TValue::Bool";
        pub const NUMBER: &str = "std::tvalue::TValue::Number";
        pub const STRING: &str = "std::tvalue::TValue::String";
        // pub const TABLE: &str = "std::tvalue::TValue::Table";
        pub const BYTE_ARRAY: &str = "std::tvalue::ByteArray";
    }

    pub mod ctors {
        pub const NIL: &str = "std::tvalue_new_nil";
        pub const BOOL: &str = "std::tvalue_new_bool";
        pub const NUMBER: &str = "std::tvalue::new_num";
        // pub const STRING: &str = "std::tvalue::new_str";
        // pub const BYTE_ARRAY: &str = "std::tvalue::byte_array_new";
    }

    pub mod helper_funcs {
        pub const GET_TAG: &str = "std::tvalue::get_tag";
        pub const TRUTHY: &str = "std::tvalue::is_truthy";
        pub const GET_VALUE_NUMBER: &str = "std::tvalue::get_value_number";
        pub const IS_NUMBER: &str = "std::tvalue::is_number";
        pub const IS_TWO_NUMBERS: &str = "std::tvalue::is_two_numbers";
    }

    pub mod math {
        pub const ADD: &str = "std::tvalue::add";
        pub const SUB: &str = "std::tvalue::sub";
        pub const MUL: &str = "std::tvalue::mul";
        pub const DIV: &str = "std::tvalue::div";
        pub const POW: &str = "std::tvalue::pow";
        pub const MOD: &str = "std::tvalue::mod";
        pub const NEG: &str = "std::tvalue::NEG";
    }
}

impl<'ctx> TValueModuleBuilder<'ctx> {
    const DATA_SIZE: u32 = 15;

    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module(tvalue_names::MODULE_NAME);
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
        let di_file = debug_builder.create_file(&format!("{}.ll", tvalue_names::MODULE_NAME), "");
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

    fn init_foreign_fns(&self) {
        let malloc = self.module.add_function(
            "malloc",
            self.context
                .i8_type()
                .ptr_type(Default::default())
                .fn_type(&[self.context.i32_type().into()], false),
            None,
        );
        let kind_id = Attribute::get_named_enum_kind_id("nounwind");
        malloc.add_attribute(
            inkwell::attributes::AttributeLoc::Function,
            self.context.create_enum_attribute(kind_id, 0),
        );
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
                    .get_struct_type(tvalue_names::types::BASE)
                    .unwrap()
                    .ptr_type(Default::default())
                    .into()],
                false,
            ),
            None,
        );

        let entry = self.context.append_basic_block(print_tvalue, "entry");
        
        self.builder.position_at_end(entry);
        if std::env::var("LUMINARY_PRINT_TVALUE").map(|v| v == "1").unwrap_or(false) {
            let loop_top = self.context.append_basic_block(print_tvalue, "looptop");
            let exit = self.context.append_basic_block(print_tvalue, "exit");

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
        }
        self.builder.build_return(None);
    }

    pub fn gen_lib(&mut self) -> Module<'ctx> {
        self.gen_types();
        #[cfg(test)]
        {
            self.gen_test_stuff();
        }
        self.init_foreign_fns();
        self.add_init_nil();
        self.add_init_bool();
        self.add_init_number();
        self.add_get_tag_function();
        self.add_get_value_bool();
        self.add_tvalue_is_number();
        self.add_tvalue_get_number();
        self.add_tvalue_check_two_numbers();
        self.add_tvalue_add();
        self.add_tvalue_sub();
        self.add_tvalue_mul();
        self.add_tvalue_div();
        self.add_tvalue_pow();
        self.add_tvalue_mod();
        self.add_tvalue_neg();
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
        self.add_new_init(tvalue_names::types::NIL, None, tvalue_names::ctors::NIL, 0);
    }

    fn add_init_bool(&self) {
        self.add_new_init(
            tvalue_names::types::BOOL,
            Some(self.context.bool_type().into()),
            tvalue_names::ctors::BOOL,
            1,
        );
    }

    fn add_init_number(&self) {
        self.add_new_init(
            tvalue_names::types::NUMBER,
            Some(self.context.f32_type().into()),
            tvalue_names::ctors::NUMBER,
            2,
        );
    }

    fn gen_types(&mut self) {
        self.gen_type(
            tvalue_names::types::BASE,
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
            ],
        );
        let base_type = self.debug_builder.create_struct_type(
            self.global_scope,
            tvalue_names::types::BASE,
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
            tvalue_names::types::BASE,
        );
        self.di_types
            .others
            .insert(tvalue_names::types::BASE.into(), base_type.as_type());
        self.gen_type(
            tvalue_names::types::NIL,
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
            ],
        );

        let nil_variant = self.debug_builder.create_struct_type(
            self.global_scope,
            tvalue_names::types::NIL,
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
            tvalue_names::types::NIL,
        );
        self.di_types
            .others
            .insert(tvalue_names::types::NIL.into(), nil_variant.as_type());
        self.gen_type(
            tvalue_names::types::BOOL,
            &[
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
                self.context.i8_type().into(),
            ],
        );
        let bool_variant = self.debug_builder.create_struct_type(
            self.global_scope,
            tvalue_names::types::BOOL,
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
            tvalue_names::types::BOOL,
        );
        self.di_types
            .others
            .insert(tvalue_names::types::BOOL.into(), bool_variant.as_type());
        self.gen_type(
            tvalue_names::types::NUMBER,
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
            tvalue_names::types::NUMBER,
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
            tvalue_names::types::NUMBER,
        );
        self.di_types
            .others
            .insert(tvalue_names::types::NUMBER.into(), num_variant.as_type());
        self.gen_type(
            tvalue_names::types::BYTE_ARRAY,
            &[
                self.context.i32_type().into(),
                self.context.i8_type().ptr_type(Default::default()).into(),
            ],
        );
        let byte_array_type = self.debug_builder.create_struct_type(
            self.global_scope,
            tvalue_names::types::BYTE_ARRAY,
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
            tvalue_names::types::BYTE_ARRAY,
        );
        self.di_types.others.insert(
            tvalue_names::types::BYTE_ARRAY.into(),
            byte_array_type.as_type(),
        );
        self.gen_type(
            tvalue_names::types::STRING,
            &[
                self.context.i8_type().into(),
                // ptr is fully opaque so using the i8_type() here doesn't matter
                self.context.i8_type().ptr_type(Default::default()).into(),
            ],
        );
        let string_variant = self.debug_builder.create_struct_type(
            self.global_scope,
            tvalue_names::types::STRING,
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
            tvalue_names::types::STRING,
        );
        self.di_types
            .others
            .insert(tvalue_names::types::STRING.into(), string_variant.as_type());
        self.di_types.others.insert(
            tvalue_names::types::STRING.into(),
            self.debug_builder
                .create_pointer_type(
                    &format!("{}_ptr", tvalue_names::types::BASE),
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
            .get_struct_type(tvalue_names::types::BASE)
            .expect("base struct name to be defined");
        let mut ctor_args: Vec<BasicMetadataTypeEnum> =
            vec![enum_base_ty.ptr_type(Default::default()).into()];

        if let Some(ctor_arg) = ctor_arg {
            ctor_args.push(ctor_arg)
        }
        let f = self.context.void_type().fn_type(&ctor_args, false);
        let f = self.module.add_function(init_name, f, None);
        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);
        let arg1 = f.get_first_param().expect("1 arg");

        let tag_ptr = self
            .builder
            .build_struct_gep(
                self.module
                    .get_struct_type(tvalue_names::types::BASE)
                    .unwrap(),
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
                    self.module
                        .get_struct_type(tvalue_names::types::BASE)
                        .unwrap(),
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
        let base_type = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let get_tag_ty = self
            .context
            .i8_type()
            .fn_type(&[base_type.ptr_type(0u16.into()).into()], false);
        let func = self
            .module
            .add_function(tvalue_names::helper_funcs::GET_TAG, get_tag_ty, None);
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
        let fn_ty = self.context.bool_type().fn_type(
            &[self
                .module
                .get_struct_type(tvalue_names::types::BASE)
                .unwrap()
                .ptr_type(Default::default())
                .into()],
            false,
        );
        let func = self
            .module
            .add_function(tvalue_names::helper_funcs::TRUTHY, fn_ty, None);
        let arg_ptr = func.get_param_iter().next().unwrap().into_pointer_value();
        arg_ptr.set_name("tagged_value");
        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);

        let get_tag_func = self
            .module
            .get_function(tvalue_names::helper_funcs::GET_TAG)
            .expect(tvalue_names::helper_funcs::GET_TAG);
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
        let is_nil_block = self.context.append_basic_block(func, "is_nil");
        let is_not_nil_block = self.context.append_basic_block(func, "is_not_nil");
        self.builder
            .build_conditional_branch(is_nil, is_nil_block, is_not_nil_block);
        self.builder.position_at_end(is_nil_block);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
        self.builder.position_at_end(is_not_nil_block);
        let is_bool = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            cmp_tag.into_int_value(),
            self.context.i8_type().const_int(1, false),
            "is_bool",
        );
        let is_bool_block = self.context.append_basic_block(func, "is_bool");
        let is_not_bool_block = self.context.append_basic_block(func, "is_not_bool");
        self.builder
            .build_conditional_branch(is_bool, is_bool_block, is_not_bool_block);
        self.builder.position_at_end(is_not_bool_block);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(is_bool_block);
        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                self.module
                    .get_struct_type(tvalue_names::types::BOOL)
                    .unwrap(),
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
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let get_tag = self
            .module
            .get_function(tvalue_names::helper_funcs::GET_TAG)
            .unwrap();
        let is_num_ty = self
            .context
            .bool_type()
            .fn_type(&[tvalue.ptr_type(Default::default()).into()], false);
        let (is_num_fn, _entry) =
            self.add_function(tvalue_names::helper_funcs::IS_NUMBER, is_num_ty);
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
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let tvalue_number = self
            .module
            .get_struct_type(tvalue_names::types::NUMBER)
            .unwrap();
        let is_num_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_NUMBER)
            .expect(tvalue_names::helper_funcs::IS_NUMBER);
        let ret_ty = self.context.f32_type();
        let (get_num_fn, _entry) = self.add_function(
            tvalue_names::helper_funcs::GET_VALUE_NUMBER,
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
        self.setup_binary_num_fn(tvalue_names::math::ADD, |lhs, rhs| {
            self.builder.build_float_add(
               lhs, rhs,
                "out_value",
            )
        });
    }

    /// Insert a new function into the module that takes 3 tvalue pointers (lhs, rhs, out)
    /// it asserts both lhs and rhs are number variants and if not, return `false`, otherwise
    /// extract the 2 float values from the lhs & rhs arguments and then perform the work of
    /// the `op` argument with those values. After performing the op, the value provided will
    /// be initialized into the `out` argument and then return `true`
    fn setup_binary_num_fn(
        &self,
        name: &str,
        op: impl FnOnce(FloatValue<'ctx>, FloatValue<'ctx>) -> FloatValue<'ctx>,
    ) {
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let two_nums_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_TWO_NUMBERS)
            .expect(tvalue_names::helper_funcs::IS_TWO_NUMBERS);
        let get_num_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::GET_VALUE_NUMBER)
            .expect("tvalue_get_number");
        let init_num_fn = self
            .module
            .get_function(tvalue_names::ctors::NUMBER)
            .expect(tvalue_names::ctors::NUMBER);
        let (f, _entry) = self.add_function(
            name,
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
        let lhs = *params.get(0).expect("1 param");
        lhs.set_name("lhs");
        let rhs = *params.get(1).expect("2 params");
        rhs.set_name("rhs");
        let out = *params.get(2).expect("3 params");
        out.set_name("out");
        let ian = self.context.append_basic_block(f, "ian");
        let nan = self.context.append_basic_block(f, "nan");
        let are_nums = self
            .builder
            .build_call(two_nums_fn, &[lhs.into(), rhs.into()], "are_nums");
        self.builder.build_conditional_branch(
            are_nums.as_any_value_enum().into_int_value(),
            ian,
            nan,
        );

        self.builder.position_at_end(nan);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));

        self.builder.position_at_end(ian);
        let lhs_value =
            self.builder
                .build_call(get_num_fn, &[lhs.as_basic_value_enum().into()], "lhs_value");
        let rhs_value =
            self.builder
                .build_call(get_num_fn, &[rhs.as_basic_value_enum().into()], "rhs_value");
        let out_value = op(
            lhs_value
                .as_any_value_enum()
                .into_float_value(),
            rhs_value.as_any_value_enum()
                .into_float_value(),
        );
        self.builder.build_call(
            init_num_fn,
            &[
                out.as_basic_value_enum().into(),
                out_value.as_any_value_enum().into_float_value().into(),
            ],
            "",
        );
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
    }

    fn add_tvalue_sub(&self) {
        self.setup_binary_num_fn(tvalue_names::math::SUB, |lhs, rhs| {
            self.builder.build_float_sub::<FloatValue>(
                lhs, rhs,
                "out_value",
            )
        });
    }

    fn add_tvalue_mul(&self) {
        self.setup_binary_num_fn(tvalue_names::math::MUL, |lhs, rhs| {
            self.builder
                .build_float_mul(
                    lhs, rhs,
                    "out_value",
                )
                .into()
        });
    }

    fn add_tvalue_div(&self) {
        self.setup_binary_num_fn(tvalue_names::math::DIV, |lhs, rhs| {
            self.builder
                .build_float_div(
                    lhs, rhs,
                    "out_value",
                )
        });
    }

    fn add_tvalue_mod(&self) {
        self.setup_binary_num_fn(tvalue_names::math::MOD, |lhs, rhs| {
            self.builder
                .build_float_rem(lhs, rhs,"out_value")
        });
    }

    fn add_tvalue_neg(&self) {
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let is_num_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_NUMBER)
            .expect(tvalue_names::helper_funcs::IS_NUMBER);
        let get_num_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::GET_VALUE_NUMBER)
            .expect(tvalue_names::helper_funcs::GET_VALUE_NUMBER);
        let init_num_fn = self
            .module
            .get_function(tvalue_names::ctors::NUMBER)
            .expect(tvalue_names::ctors::NUMBER);
        let f = self.module.add_function(
            tvalue_names::math::NEG,
            self.context.bool_type().fn_type(
                &[
                    tvalue.ptr_type(Default::default()).into(),
                    tvalue.ptr_type(Default::default()).into(),
                ],
                false,
            ),
            None,
        );
        let arg = f.get_first_param().expect("1 arg");
        let out = f.get_last_param().expect("2 args");
        let entry = self.context.append_basic_block(f, "entry");
        let nan = self.context.append_basic_block(f, "nan");
        let ian = self.context.append_basic_block(f, "ian");
        self.builder.position_at_end(entry);
        let arg_ian = self.builder.build_call(is_num_fn, &[arg.into()], "arg_ian");
        self.builder.build_conditional_branch(
            arg_ian.as_any_value_enum().into_int_value(),
            ian,
            nan,
        );
        self.builder.position_at_end(ian);
        let value =
            self.builder
                .build_call(get_num_fn, &[arg.as_basic_value_enum().into()], "lhs_value");

        let out_value = self
            .builder
            .build_float_neg(value.as_any_value_enum().into_float_value(), "out_value");
        self.builder.build_call(
            init_num_fn,
            &[out.as_basic_value_enum().into(), out_value.into()],
            "",
        );
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(nan);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
    }

    fn add_tvalue_pow(&self) {
        let pow_intrinsic = Intrinsic::find("llvm.pow.f32").expect("llvm.pow.f32");
        let pow_intrinsic_fn = pow_intrinsic
            .get_declaration(
                &self.module,
                &[self.context.f32_type().as_basic_type_enum()],
            )
            .unwrap();
        self.setup_binary_num_fn(tvalue_names::math::POW, |lhs, rhs| {
            self.builder
                .build_call(
                    pow_intrinsic_fn,
                    &[lhs.into(), rhs.into()],
                    "out_value",
                )
                .as_any_value_enum()
                .into_float_value()
        });
    }

    fn add_tvalue_check_two_numbers(&self) {
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .expect(tvalue_names::types::BASE);
        let is_num_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_NUMBER)
            .expect(tvalue_names::helper_funcs::IS_NUMBER);

        let (f, _entry) = self.add_function(
            tvalue_names::helper_funcs::IS_TWO_NUMBERS,
            self.context.bool_type().fn_type(
                &[
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

        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(nan);

        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
    }

    pub fn emit_new_number(&self, name: &str, init: BasicMetadataValueEnum) -> PointerValue<'ctx> {
        Self::emit_new_stmts(
            name,
            tvalue_names::types::NUMBER,
            tvalue_names::ctors::NUMBER,
            &self.module,
            &self.builder,
            Some(init),
        )
    }

    pub fn emit_new_bool(&self, name: &str, init: BasicMetadataValueEnum) -> PointerValue<'ctx> {
        Self::emit_new_stmts(
            name,
            tvalue_names::types::BOOL,
            tvalue_names::ctors::BOOL,
            &self.module,
            &self.builder,
            Some(init),
        )
    }

    pub fn emit_new_stmts<'a>(
        var_name: &str,
        ty_name: &str,
        ctor_name: &str,
        module: &Module<'a>,
        builder: &Builder<'a>,
        init: Option<BasicMetadataValueEnum>,
    ) -> PointerValue<'a> {
        let ty = module.get_struct_type(ty_name).unwrap();
        let ctor = module.get_function(ctor_name).unwrap();
        let v = builder.build_alloca(ty, var_name);
        let mut args: Vec<BasicMetadataValueEnum> = vec![v.as_basic_value_enum().into()];
        if let Some(init) = init {
            args.push(init);
        }
        builder.build_call(ctor, &args, "");
        v
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
        let new_nil_fun = module.get_function(tvalue_names::ctors::NIL).unwrap();
        let new_bool_fun = module.get_function(tvalue_names::ctors::BOOL).unwrap();
        let new_num_fun = module.get_function(tvalue_names::ctors::NUMBER).unwrap();
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

        maybe_write_test_module("tag_value", &module);
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
        let tag_fun = module
            .get_function(tvalue_names::helper_funcs::GET_TAG)
            .unwrap();
        let entry = context.append_basic_block(test_func, "entry");
        builder.position_at_end(entry);
        eprintln!("setting call for {test_name}");
        let arg = builder.build_alloca(
            module.get_struct_type(tvalue_names::types::BASE).unwrap(),
            "base",
        );
        let mut args: Vec<BasicMetadataValueEnum> = vec![arg.into()];
        for arg in ctor_args {
            args.push(*arg);
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

    #[test]
    fn check_truthiness() {
        let context = Context::create();
        inkwell::support::enable_llvm_pretty_stack_trace();

        let mut builder = TValueModuleBuilder::new(&context);
        let module = builder.gen_lib();
        let builder = context.create_builder();
        // let is_bool = module
        //     .get_function(tvalue_names::helper_funcs::TRUTHY)
        //     .expect(tvalue_names::helper_funcs::TRUTHY);
        // for bb in is_bool.get_basic_blocks() {
        //     let mut name = bb.get_name().to_str().unwrap();
        //     if name == "entry" {
        //         name = tvalue_names::helper_funcs::TRUTHY;
        //     }
        //     builder.position_at(bb, &bb.get_first_instruction().unwrap());
        //     build_print(&context, &module, &builder, name);
        // }
        // let f = module.add_function(
        //     "print_tvalue",
        //     context.void_type().fn_type(
        //         &[module
        //             .get_struct_type(tvalue_names::types::BASE)
        //             .unwrap()
        //             .ptr_type(Default::default())
        //             .into()],
        //         false,
        //     ),
        //     None,
        // );
        // let entry = context.append_basic_block(f, "entry");
        // builder.position_at_end(entry);
        // let buf_ptr = builder.build_alloca(context.i8_type().array_type(17), "buf");
        // let struct_bytes = builder.build_load(
        //     context.i8_type().array_type(17),
        //     f.get_first_param().unwrap().into_pointer_value(),
        //     "struct_bytes",
        // );
        // builder.build_store(buf_ptr, context.i8_type().array_type(17).const_zero());
        // builder.build_store(buf_ptr, struct_bytes);
        // builder.build_return(None);
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::NIL).unwrap(),
            context.bool_type().const_int(0, false).into(),
            "test_nil",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::BOOL).unwrap(),
            context.bool_type().const_int(1, false).into(),
            "test_bool_true",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::BOOL).unwrap(),
            context.bool_type().const_int(0, false).into(),
            "test_bool_false",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::NUMBER).unwrap(),
            context.f32_type().const_float(0.11).into(),
            "test_number",
        );

        maybe_write_test_module("truthy", &module);

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
        let printf = module.get_function("printf").expect("printf");
        let s_ptr = builder.build_alloca(context.i8_type().array_type((s.len() + 2) as _), "s");
        builder.build_store(
            s_ptr,
            context.i8_type().const_array(&s_to_arr_nl_z(context, s)),
        );
        builder.build_call(printf, &[s_ptr.into()], "");
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
            .get_function(tvalue_names::helper_funcs::TRUTHY)
            .expect("getTValueValue_bool");
        builder.position_at_end(entry);
        // build_print(context, module, builder, name);
        let alloc = builder.build_alloca(
            module
                .get_struct_type(tvalue_names::types::BASE)
                .expect("base"),
            "t",
        );
        builder.build_call(ctor_fn, &[alloc.into(), ctor_arg], "");
        let ret = builder.build_call(is_bool, &[alloc.into()], "ret");
        // let output_fmt = "is_true: %i";
        // let fmt_ptr = builder.build_alloca(
        //     context.i8_type().array_type((output_fmt.len() + 2) as _),
        //     "fmt_ptr",
        // );
        // builder.build_store(
        //     fmt_ptr,
        //     context
        //         .i8_type()
        //         .const_array(&s_to_arr_nl_z(context, output_fmt)),
        // );
        let ret = builder.build_int_z_extend(
            ret.as_any_value_enum().into_int_value(),
            context.i32_type(),
            "ret",
        );
        // build_print(context, module, builder, "----------");
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

    #[test]
    fn test_num_add() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_num_add_happy";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::ADD, name1);
        let add_fn = module.get_function(tvalue_names::math::ADD).unwrap();
        let get_num = module
            .get_function(tvalue_names::helper_funcs::GET_VALUE_NUMBER)
            .unwrap();

        let name2 = "add_num_bool";
        let test_bool_fn = module.add_function(name2, context.f32_type().fn_type(&[], false), None);
        let entry = context.append_basic_block(test_bool_fn, "entry");
        module_builder.builder.position_at_end(entry);
        let lhs = module_builder.emit_new_number("lhs", context.f32_type().const_float(1.0).into());
        let rhs = module_builder.emit_new_bool("rhs", context.f32_type().const_float(1.0).into());
        let ret =
            module_builder.emit_new_number("ret", context.f32_type().const_float(f64::NAN).into());
        let _succ = module_builder.builder.build_call(
            add_fn,
            &[
                lhs.as_basic_value_enum().into(),
                rhs.as_basic_value_enum().into(),
                ret.as_basic_value_enum().into(),
            ],
            "ret",
        );
        let ret2 =
            module_builder
                .builder
                .build_call(get_num, &[ret.as_basic_value_enum().into()], "ret");
        module_builder
            .builder
            .build_return(Some(&ret2.as_any_value_enum().into_float_value()));

        let name3 = "add_num_nil";
        let test_bool_fn = module.add_function(name3, context.f32_type().fn_type(&[], false), None);
        let entry = context.append_basic_block(test_bool_fn, "entry");
        module_builder.builder.position_at_end(entry);
        let lhs = TValueModuleBuilder::emit_new_stmts(
            "lhs",
            tvalue_names::types::BASE,
            tvalue_names::ctors::NIL,
            &module,
            &module_builder.builder,
            None,
        );
        let rhs = module_builder.emit_new_bool("rhs", context.f32_type().const_float(1.0).into());
        let ret =
            module_builder.emit_new_number("ret", context.f32_type().const_float(f64::NAN).into());
        let _succ = module_builder.builder.build_call(
            add_fn,
            &[
                lhs.as_basic_value_enum().into(),
                rhs.as_basic_value_enum().into(),
                ret.as_basic_value_enum().into(),
            ],
            "ret",
        );
        let ret2 =
            module_builder
                .builder
                .build_call(get_num, &[ret.as_basic_value_enum().into()], "ret");
        module_builder
            .builder
            .build_return(Some(&ret2.as_any_value_enum().into_float_value()));

        maybe_write_test_module("add", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        let op = |l, r| l + r;
        execute_happy_math_test(name1, &jit, 1.0, 1.0, op);
        execute_happy_math_test(name1, &jit, 5.0, 5.0, op);
        execute_happy_math_test(name1, &jit, 1.5, 0.5, op);
        execute_happy_math_test(name1, &jit, 200.0, 600.0, op);
        execute_sad_num_add_test(name2, &jit);
        execute_sad_num_add_test(name3, &jit);
    }

    #[test]
    fn test_num_sub() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_sub";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::SUB, name1);
        maybe_write_test_module("sub", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        execute_happy_math_test(name1, &jit, 1.0, 1.0, |l, r| l - r)
    }

    #[test]
    fn test_num_mul() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_mul";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::MUL, name1);
        maybe_write_test_module("mul", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        execute_happy_math_test(name1, &jit, 1.0, 1.0, |l, r| l * r)
    }

    #[test]
    fn test_num_div() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_div";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::MUL, name1);
        maybe_write_test_module("div", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        execute_happy_math_test(name1, &jit, 1.0, 1.0, |l, r| l / r)
    }

    #[test]
    fn test_num_pow() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_pow";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::POW, name1);
        maybe_write_test_module("pow", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        execute_happy_math_test(name1, &jit, 1.1, 2.2, |l, r| l.powf(r));
        execute_happy_math_test(name1, &jit, 0.1, -2.2, |l, r| l.powf(r));
    }

    #[test]
    fn test_num_mod() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_mod";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::MOD, name1);
        maybe_write_test_module("mod", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        execute_happy_math_test(name1, &jit, 1.2, 1.0, |l, r| l % r);
    }

    fn execute_happy_math_test<'ctx>(
        name: &'static str,
        jit: &'ctx ExecutionEngine<'ctx>,
        lhs: f32,
        rhs: f32,
        op: impl Fn(f32, f32) -> f32,
    ) {
        type TestFunc = unsafe extern "C" fn(f32, f32) -> f32;
        println!("looking up {name}");
        let func: JitFunction<TestFunc> = unsafe {
            jit.get_function(name).unwrap_or_else(|e| {
                panic!("{name}: {e}");
            })
        };
        let val = unsafe { func.call(lhs, rhs) };
        assert_eq!(val, op(lhs, rhs), "{name}");
    }

    fn execute_sad_num_add_test<'ctx>(name: &'static str, jit: &'ctx ExecutionEngine<'ctx>) {
        type TestFunc = unsafe extern "C" fn() -> f32;
        println!("looking up {name}");
        let func: JitFunction<TestFunc> = unsafe {
            jit.get_function(name).unwrap_or_else(|e| {
                panic!("{name}: {e}");
            })
        };
        let val = unsafe { func.call() };
        assert!(val.is_nan(), "{name} was not nan");
    }

    fn build_binary_math_happy_test<'ctx>(
        module_builder: &TValueModuleBuilder<'ctx>,
        module: &Module<'ctx>,
        math_fn_name: &str,
        name: &str,
    ) {
        let get_num = module
            .get_function(tvalue_names::helper_funcs::GET_VALUE_NUMBER)
            .unwrap_or_else(|| {
                panic!("get_num missing for {math_fn_name}");
            });
        let math_fn = module.get_function(math_fn_name).unwrap_or_else(|| {
            panic!("math_fn: {math_fn_name} missing");
        });
        let test_fn = module.add_function(
            name,
            module_builder.context.f32_type().fn_type(
                &[
                    module_builder.context.f32_type().into(),
                    module_builder.context.f32_type().into(),
                ],
                false,
            ),
            None,
        );
        let arg1 = test_fn.get_first_param().unwrap();
        let arg2 = test_fn.get_last_param().unwrap();
        let entry1 = module_builder.context.append_basic_block(test_fn, "entry");
        module_builder.builder.position_at_end(entry1);
        let lhs = module_builder.emit_new_number("lhs", arg1.as_basic_value_enum().into());
        let rhs = module_builder.emit_new_number("lhs", arg2.as_basic_value_enum().into());
        let ret = module_builder.emit_new_number(
            "ret",
            module_builder
                .context
                .f32_type()
                .const_float(f64::NAN)
                .into(),
        );
        let _succ = module_builder.builder.build_call(
            math_fn,
            &[
                lhs.as_basic_value_enum().into(),
                rhs.as_basic_value_enum().into(),
                ret.as_basic_value_enum().into(),
            ],
            "ret",
        );
        let ret2 =
            module_builder
                .builder
                .build_call(get_num, &[ret.as_basic_value_enum().into()], "ret");
        module_builder
            .builder
            .build_return(Some(&ret2.as_any_value_enum().into_float_value()));
    }

    #[test]
    fn test_neg() {
        let context = Context::create();
        let mut builder = TValueModuleBuilder::new(&context);
        let _module = builder.gen_lib();
        let (f, _) = builder.add_function("test_neg", builder.context.f32_type().fn_type(&[
            builder.context.f32_type().into(),
        ], false));
        let arg = f.get_first_param().unwrap();
        let n = builder.emit_new_number("value", arg.into());
        let o = builder.emit_new_number("out", context.f32_type().const_float(f64::NAN).into());
        let neg_fn = builder.module.get_function(tvalue_names::math::NEG).unwrap();
        let get_num_fn = builder.module.get_function(tvalue_names::helper_funcs::GET_VALUE_NUMBER).unwrap();
        let _ret = builder.builder.build_call(neg_fn, &[
            n.as_basic_value_enum().into(),
            o.as_basic_value_enum().into(),
        ], "ret");
        let ret = builder.builder.build_call(get_num_fn, &[
            o.as_basic_value_enum().into()
        ], "ret");
        builder.builder.build_return(Some(&ret.as_any_value_enum().into_float_value()));
        let jit = builder.module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        type TestFunc = unsafe extern "C" fn(f32) -> f32;
        let test_neg = unsafe {
            jit.get_function::<TestFunc>("test_neg").unwrap()
        };
        let negged = unsafe  {
            test_neg.call(1.0)
        };
        assert_eq!(negged, -1.0);
        let negged = unsafe  {
            test_neg.call(-1.0)
        };
        assert_eq!(negged, 1.0)
    }

    fn maybe_write_test_module<'ctx>(name: &str, module: &Module<'ctx>) {
        if std::env::var("LUMINARY_WRITE_TEST_IR")
            .map(|v| v == "1")
            .unwrap_or(false)
        {
            write_test_module(name, &module);
        }
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
