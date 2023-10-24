//! This module is for generating a "standard library" that outlines how all lua values
//! will be represented in the LLVM IR, operator functions, etc.

use std::collections::HashMap;

use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::{
        AsDIScope, DIBasicType, DICompileUnit, DIFile, DIScope, DIType, DebugInfoBuilder,
    },
    intrinsics::Intrinsic,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        AnyValue, BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, IntValue,
        PointerValue,
    },
};

mod float_to_string;
pub use float_to_string::add_float_to_string;

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
    // debug_builder: DebugInfoBuilder<'ctx>,
    // #[allow(unused)]
    // debug_compile_unit: DICompileUnit<'ctx>,
    // debug_file: DIFile<'ctx>,
    // global_scope: DIScope<'ctx>,
    // di_types: DebugTypes<'ctx>,
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
    }

    pub mod ctors {
        pub const NIL: &str = "std::tvalue::new_nil";
        pub const BOOL: &str = "std::tvalue::new_bool";
        pub const NUMBER: &str = "std::tvalue::new_num";
        pub const STRING: &str = "std::tvalue::new_str";
    }

    pub mod helper_funcs {
        pub const GET_TAG: &str = "std::tvalue::get_tag";
        pub const TRUTHY: &str = "std::tvalue::is_truthy";
        pub const GET_VALUE_NUMBER: &str = "std::tvalue::get_value_number";
        pub const IS_NUMBER: &str = "std::tvalue::is_number";
        pub const IS_TWO_NUMBERS: &str = "std::tvalue::is_two_numbers";
        pub const IS_INT: &str = "std::tvalue::is_int";
        pub const IS_TWO_INTS: &str = "std::tvalue::is_two_ints";
        pub const IS_STRING: &str = "std::tvalue::is_str";
        pub const IS_TWO_STRINGS: &str = "std::tvalue::is_two_strs";
    }

    pub mod print_names {
        pub const PRINT_TVALUE: &str = "std::tvalue::fmt::print_tvalue";
        pub const PRINT_TVALUE_STRING: &str = "std::tvalue::fmt::print_tvalue_string";
        pub const PRINT_TVALUE_RAW: &str = "std::tvalue::fmt::print_tvalue_raw";
    }

    pub mod math {
        /// lhs + rhs
        pub const ADD: &str = "std::tvalue::add";
        /// lhs - rhs
        pub const SUB: &str = "std::tvalue::sub";
        /// lhs * rhs
        pub const MUL: &str = "std::tvalue::mul";
        /// lhs / rhs
        pub const DIV: &str = "std::tvalue::div";
        /// lhs // rhs
        pub const FLOOR_DIV: &str = "std::tvalue::floor_div";
        /// lhs ^ rhs
        pub const POW: &str = "std::tvalue::pow";
        /// lhs % rhs
        pub const MOD: &str = "std::tvalue::mod";
        /// -value
        pub const NEG: &str = "std::tvalue::NEG";

        /// ~value
        pub const NOT: &str = "std::tvalue::bin_not";
        /// lsh & rhs
        pub const AND: &str = "std::tvalue::bin_and";
        /// lhs | rhs
        pub const OR: &str = "std::tvalue::bin_or";
        /// lhs >> rhs
        pub const RSH: &str = "std::tvalue::rsh";
        /// lhs << rhs
        pub const LSH: &str = "std::tvalue::lsh";
    }

    pub mod string {
        /// lhs .. rhs
        pub const CONCAT: &str = "std::tvalue::concat";
    }
}

pub mod tvalue_generate {}

impl<'ctx> TValueModuleBuilder<'ctx> {
    const DATA_SIZE: u32 = 23;

    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module(tvalue_names::MODULE_NAME);
        // let (debug_builder, debug_compile_unit) = module.create_debug_info_builder(
        //     false,
        //     inkwell::debug_info::DWARFSourceLanguage::C,
        //     "std_tvalue.ll",
        //     ".",
        //     "luminary",
        //     false,
        //     "",
        //     1,
        //     "",
        //     inkwell::debug_info::DWARFEmissionKind::Full,
        //     0,
        //     false,
        //     false,
        //     "",
        //     "",
        // );
        // let di_file = debug_builder.create_file(&format!("{}.ll", tvalue_names::MODULE_NAME), "");
        // let di_scope = di_file.as_debug_info_scope();
        // let di_types = DebugTypes::new(&debug_builder);
        Self {
            builder: context.create_builder(),
            module,
            // debug_builder,
            // debug_compile_unit,
            context,
            // debug_file: di_file,
            // global_scope: di_scope,
            // di_types,
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

    fn gen_print_stuff(&self) {
        let get_tag_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::GET_TAG)
            .unwrap();
        let printf = self.module.add_function(
            "printf",
            self.context.i32_type().fn_type(
                &[self.context.i8_type().ptr_type(Default::default()).into()],
                true,
            ),
            None,
        );
        let write = self.module.add_function(
            "write",
            self.context.i32_type().fn_type(
                &[
                    self.context.i32_type().into(),
                    self.context.i8_type().ptr_type(Default::default()).into(),
                    self.context.i32_type().into(),
                ],
                true,
            ),
            None,
        );

        let print_string = self.add_tvalue_print_string();
        let print_unknown = self.add_tvalue_print_unknown();
        let print_tvalue = self.module.add_function(
            tvalue_names::print_names::PRINT_TVALUE,
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
        let arg = print_tvalue
            .get_first_param()
            .unwrap()
            .as_any_value_enum()
            .into_pointer_value();

        let unknown = self.context.append_basic_block(print_tvalue, "unknown");
        self.builder.position_at_end(unknown);
        self.builder.build_call(print_unknown, &[arg.into()], "_");
        self.builder.build_return(None);

        self.builder.position_at_end(entry);
        let nil = self.context.append_basic_block(print_tvalue, "nil");
        let boolean = self.context.append_basic_block(print_tvalue, "boolean");
        let number = self.context.append_basic_block(print_tvalue, "number");
        let string = self.context.append_basic_block(print_tvalue, "string");
        let tag = self
            .builder
            .build_call(get_tag_fn, &[arg.as_basic_value_enum().into()], "tag")
            .as_any_value_enum()
            .into_int_value();
        let is_nil = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            tag,
            self.context.i8_type().const_int(0, false),
            "is_nil",
        );
        let not_nil = self.context.append_basic_block(print_tvalue, "notnil");
        self.builder.build_conditional_branch(is_nil, nil, not_nil);
        self.builder.position_at_end(not_nil);
        let is_bool = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            tag,
            self.context.i8_type().const_int(1, false),
            "is_bool",
        );
        let not_bool = self.context.append_basic_block(print_tvalue, "notbool");
        self.builder
            .build_conditional_branch(is_bool, boolean, not_bool);
        self.builder.position_at_end(not_bool);
        let is_num = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            tag,
            self.context.i8_type().const_int(2, false),
            "is_num",
        );

        let not_num = self.context.append_basic_block(print_tvalue, "notnum");
        self.builder
            .build_conditional_branch(is_num, number, not_num);
        self.builder.position_at_end(not_num);
        let is_str = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            tag,
            self.context.i8_type().const_int(3, false),
            "is_str",
        );

        self.builder
            .build_conditional_branch(is_str, string, unknown);

        self.builder.position_at_end(nil);
        let nil_fmt = self
            .builder
            .build_alloca(self.context.i8_type().array_type(4), "nil_fmt");
        let s = b"nil\n"
            .into_iter()
            .map(|c| self.context.i8_type().const_int(*c as _, false))
            .collect::<Vec<_>>();
        self.builder
            .build_store(nil_fmt, self.context.i8_type().const_array(&s));
        self.builder.build_call(
            write,
            &[
                self.context.i32_type().const_int(1, true).into(),
                nil_fmt.into(),
                self.context.i32_type().const_int(s.len() as _, true).into(),
            ],
            "_",
        );
        self.builder.build_return(None);

        self.builder.position_at_end(boolean);
        let truthy_ptr = self
            .builder
            .build_struct_gep(
                self.module
                    .get_struct_type(tvalue_names::types::BOOL)
                    .unwrap(),
                arg,
                1,
                "truthy_ptr",
            )
            .unwrap();
        let truthy = self
            .builder
            .build_load(self.context.bool_type(), truthy_ptr, "truthy")
            .as_any_value_enum()
            .into_int_value();
        let bool_true = self.context.append_basic_block(print_tvalue, "bool_true");
        let bool_false = self.context.append_basic_block(print_tvalue, "bool_false");
        self.builder
            .build_conditional_branch(truthy, bool_true, bool_false);
        self.builder.position_at_end(bool_true);
        let true_fmt = self
            .builder
            .build_alloca(self.context.i8_type().array_type(4), "nil_fmt");
        let s: Vec<IntValue<'_>> = b"true\n"
            .into_iter()
            .map(|c| self.context.i8_type().const_int(*c as _, false))
            .collect::<Vec<_>>();
        self.builder
            .build_store(true_fmt, self.context.i8_type().const_array(&s));
        self.builder.build_call(
            write,
            &[
                self.context.i32_type().const_int(1, true).into(),
                true_fmt.into(),
                self.context.i32_type().const_int(s.len() as _, true).into(),
            ],
            "_",
        );
        self.builder.build_return(None);

        self.builder.position_at_end(bool_false);
        let false_fmt = self
            .builder
            .build_alloca(self.context.i8_type().array_type(4), "nil_fmt");
        let s: Vec<IntValue<'_>> = b"false\n"
            .into_iter()
            .map(|c| self.context.i8_type().const_int(*c as _, false))
            .collect::<Vec<_>>();
        self.builder
            .build_store(false_fmt, self.context.i8_type().const_array(&s));
        self.builder.build_call(
            write,
            &[
                self.context.i32_type().const_int(1, true).into(),
                false_fmt.into(),
                self.context.i32_type().const_int(s.len() as _, true).into(),
            ],
            "_",
        );
        self.builder.build_return(None);

        self.builder.position_at_end(number);
        let num_ptr = self
            .builder
            .build_struct_gep(
                self.module
                    .get_struct_type(tvalue_names::types::NUMBER)
                    .unwrap(),
                arg,
                1,
                "num_ptr",
            )
            .unwrap();
        let num = self
            .builder
            .build_load(self.context.f32_type(), num_ptr, "num");
        let dbl =
            self.builder
                .build_float_ext(num.into_float_value(), self.context.f64_type(), "dbl");
        let s = b"%f\n\0";
        let num_fmt = self
            .builder
            .build_alloca(self.context.i8_type().array_type(s.len() as _), "num_fmt");
        let bytes = s
            .into_iter()
            .map(|b| self.context.i8_type().const_int(*b as _, false))
            .collect::<Vec<_>>();
        let bytes = self.context.i8_type().const_array(&bytes);
        self.builder.build_store(num_fmt, bytes);
        self.builder
            .build_call(printf, &[num_fmt.into(), dbl.into()], "_");
        self.builder.build_return(None);
        self.builder.position_at_end(string);
        self.builder.build_call(print_string, &[arg.into()], "_");
        self.builder.build_return(None);
    }

    fn add_tvalue_print_string(&self) -> FunctionValue<'ctx> {
        let ty = self
            .module
            .get_struct_type(tvalue_names::types::STRING)
            .unwrap();
        let write = self.module.get_function("write").unwrap();
        let (f, _entry) = self.add_function(
            tvalue_names::print_names::PRINT_TVALUE_STRING,
            self.context
                .void_type()
                .fn_type(&[ty.ptr_type(Default::default()).into()], false),
        );
        let arg = f.get_first_param().unwrap().into_pointer_value();
        let len_ptr = self
            .builder
            .build_struct_gep(ty, arg, 1, "len_ptr")
            .unwrap();
        let len = self
            .builder
            .build_load(self.context.i32_type(), len_ptr, "len")
            .into_int_value();
        let bytes_ptr_ptr = self
            .builder
            .build_struct_gep(ty, arg, 4, "bytes_ptr_ptr")
            .unwrap();
        let bytes_ptr = self.builder.build_load(
            self.context.i8_type().ptr_type(Default::default()),
            bytes_ptr_ptr,
            "bytes_ptr",
        );
        self.builder.build_call(
            write,
            &[
                self.context.i32_type().const_int(1, false).into(),
                bytes_ptr.into(),
                len.into(),
            ],
            "_",
        );
        let nl = self
            .builder
            .build_alloca(self.context.i8_type().array_type(1), "nl");
        self.builder.build_store(
            nl,
            self.context
                .i8_type()
                .const_array(&[self.context.i8_type().const_int(b'\n' as _, false)]),
        );
        self.builder.build_call(
            write,
            &[
                self.context.i32_type().const_int(1, false).into(),
                nl.into(),
                self.context.i32_type().const_int(1, false).into(),
            ],
            "_",
        );
        self.builder.build_return(None);
        f
    }

    fn add_tvalue_print_unknown(&self) -> FunctionValue<'ctx> {
        let base_ty = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let printf = self.module.get_function("printf").unwrap();
        let (f, entry) = self.add_function(
            tvalue_names::print_names::PRINT_TVALUE_RAW,
            self.context
                .void_type()
                .fn_type(&[base_ty.ptr_type(Default::default()).into()], false),
        );
        let arg = f.get_first_param().unwrap().into_pointer_value();
        let base_size_ptr = unsafe {
            self.builder.build_gep(
                base_ty,
                self.context
                    .i8_type()
                    .ptr_type(Default::default())
                    .const_null(),
                &[self.context.i32_type().const_int(1, false)],
                "size_ptr",
            )
        };
        let size = self
            .builder
            .build_ptr_to_int(base_size_ptr, self.context.i32_type(), "size");
        let loop_top = self.context.append_basic_block(f, "looptop");
        let exit = self.context.append_basic_block(f, "exit");
        let start_brace = self
            .builder
            .build_alloca(self.context.i8_type().array_type(2), "ob");
        self.builder
            .build_store(start_brace, self.context.const_string(b"[", true));
        self.builder.build_call(printf, &[start_brace.into()], "_");
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
                arg,
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
            "_",
        );
        let br = self
            .builder
            .build_int_compare(inkwell::IntPredicate::UGE, next_idx, size, "done");
        self.builder.build_conditional_branch(br, exit, loop_top);
        self.builder.position_at_end(exit);
        let end_brace = self
            .builder
            .build_alloca(self.context.i8_type().array_type(3), "cb");
        self.builder
            .build_store(end_brace, self.context.const_string(b"]\n", true));
        self.builder.build_call(printf, &[end_brace.into()], "_");
        self.builder.build_return(None);
        f
    }

    fn verify(&self) {
        if let Err(e) = self.module.verify() {
            eprintln!("{}", self.module.to_string());
            let es = e.to_string();
            let mut ct = 0;
            for line in es.lines() {
                eprintln!("{line}");
                ct += 1;
            }
            panic!("Found {} errors", ct / 2);
        }
    }

    pub fn gen_lib(&mut self) -> Module<'ctx> {
        self.gen_types();
        self.init_foreign_fns();
        self.add_init_nil();
        self.add_init_bool();
        self.add_init_number();
        self.add_init_string();
        self.add_get_tag_function();
        self.add_get_value_bool();
        self.add_tvalue_is_number();
        self.add_tvalue_get_number();
        self.gen_print_stuff();
        self.verify();
        self.add_tvalue_check_two_numbers();
        self.add_tvalue_add();
        self.add_tvalue_sub();
        self.add_tvalue_mul();
        self.add_tvalue_div();
        self.add_tvalue_floor_div();
        self.add_tvalue_pow();
        self.add_tvalue_mod();
        self.add_tvalue_neg();
        self.add_is_int();
        self.add_is_two_ints();
        self.add_is_str();
        self.add_is_two_strs();
        self.add_tvalue_bin_and();
        self.add_tvalue_bin_or();
        self.add_tvalue_bin_lsh();
        self.add_tvalue_bin_rsh();
        self.add_tvalue_bin_not();
        // self.debug_builder.finalize();
        self.verify();
        self.module.clone()
    }

    fn add_init_nil(&self) {
        self.add_new_simple_init(tvalue_names::types::NIL, None, tvalue_names::ctors::NIL, 0);
    }

    fn add_init_bool(&self) {
        self.add_new_simple_init(
            tvalue_names::types::BOOL,
            Some(self.context.bool_type().into()),
            tvalue_names::ctors::BOOL,
            1,
        );
    }

    fn add_init_number(&self) {
        self.add_new_simple_init(
            tvalue_names::types::NUMBER,
            Some(self.context.f32_type().into()),
            tvalue_names::ctors::NUMBER,
            2,
        );
    }

    fn add_init_string(&self) {
        let ty = self
            .module
            .get_struct_type(tvalue_names::types::STRING)
            .unwrap();
        let f = self.setup_ctor(
            tvalue_names::ctors::STRING,
            3,
            vec![self.context.i32_type().into()],
        );
        let str_arg = f.get_first_param().expect("1 arg").into_pointer_value();
        let len_arg = f.get_last_param().expect("2 args").into_int_value();
        let len_ptr = self
            .builder
            .build_struct_gep(ty, str_arg, 1, "len_ptr")
            .unwrap();
        self.builder
            .build_store(len_ptr, self.context.i32_type().const_int(0, false));
        let cap_ptr = self
            .builder
            .build_struct_gep(ty, str_arg, 2, "cap_ptr")
            .unwrap();
        self.builder.build_store(cap_ptr, len_arg);
        let ref_ct_ptr = self
            .builder
            .build_struct_gep(ty, str_arg, 3, "ref_ct")
            .unwrap();
        self.builder
            .build_store(ref_ct_ptr, self.context.i32_type().const_int(0, false));
        let malloc = self.module.get_function("malloc").expect("malloc");
        let bytes_ptr = self
            .builder
            .build_call(malloc, &[len_arg.into()], "bytes_ptr");
        let storage_ptr = self
            .builder
            .build_struct_gep(ty, str_arg, 4, "storage")
            .unwrap();
        self.builder.build_store(
            storage_ptr,
            bytes_ptr.as_any_value_enum().into_pointer_value(),
        );
        self.builder.build_return(None);
    }

    fn gen_types(&mut self) {
        self.gen_type(
            tvalue_names::types::BASE,
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
            ],
        );
        // let base_type = self.debug_builder.create_struct_type(
        //     self.global_scope,
        //     tvalue_names::types::BASE,
        //     self.debug_file,
        //     3,
        //     8 * 16,
        //     8,
        //     0,
        //     None,
        //     &[
        //         self.di_types.i8_type.as_type(),
        //         self.debug_builder
        //             .create_array_type(
        //                 self.di_types.i8_type.as_type(),
        //                 (8 * Self::DATA_SIZE) as u64,
        //                 8,
        //                 &[],
        //             )
        //             .as_type(),
        //     ],
        //     0,
        //     None,
        //     tvalue_names::types::BASE,
        // );
        // self.di_types
        //     .others
        //     .insert(tvalue_names::types::BASE.into(), base_type.as_type());
        self.gen_type(
            tvalue_names::types::NIL,
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
            ],
        );

        // let nil_variant = self.debug_builder.create_struct_type(
        //     self.global_scope,
        //     tvalue_names::types::NIL,
        //     self.debug_file,
        //     3,
        //     8 * 16,
        //     8,
        //     0,
        //     None,
        //     &[
        //         self.di_types.i8_type.as_type(),
        //         self.debug_builder
        //             .create_array_type(
        //                 self.di_types.i8_type.as_type(),
        //                 (8 * Self::DATA_SIZE) as u64,
        //                 8,
        //                 &[],
        //             )
        //             .as_type(),
        //     ],
        //     0,
        //     None,
        //     tvalue_names::types::NIL,
        // );
        // self.di_types
        //     .others
        //     .insert(tvalue_names::types::NIL.into(), nil_variant.as_type());
        self.gen_type(
            tvalue_names::types::BOOL,
            &[
                self.context.i8_type().array_type(Self::DATA_SIZE).into(),
                self.context.i8_type().into(),
            ],
        );
        // let bool_variant = self.debug_builder.create_struct_type(
        //     self.global_scope,
        //     tvalue_names::types::BOOL,
        //     self.debug_file,
        //     3,
        //     8 * 16,
        //     8,
        //     0,
        //     None,
        //     &[
        //         self.di_types.i8_type.as_type(),
        //         self.debug_builder
        //             .create_array_type(
        //                 self.di_types.i8_type.as_type(),
        //                 (8 * Self::DATA_SIZE) as u64,
        //                 8,
        //                 &[],
        //             )
        //             .as_type(),
        //     ],
        //     0,
        //     None,
        //     tvalue_names::types::BOOL,
        // );
        // self.di_types
        //     .others
        //     .insert(tvalue_names::types::BOOL.into(), bool_variant.as_type());
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
        // let num_variant = self.debug_builder.create_struct_type(
        //     self.global_scope,
        //     tvalue_names::types::NUMBER,
        //     self.debug_file,
        //     3,
        //     8 * 16,
        //     8,
        //     0,
        //     None,
        //     &[
        //         self.di_types.i8_type.as_type(),
        //         self.di_types.f32_type.as_type(),
        //     ],
        //     0,
        //     None,
        //     tvalue_names::types::NUMBER,
        // );
        // self.di_types
        //     .others
        //     .insert(tvalue_names::types::NUMBER.into(), num_variant.as_type());

        self.gen_type(
            tvalue_names::types::STRING,
            &[
                // variant
                self.context.i8_type().into(),
                // length
                self.context.i32_type().into(),
                // capacity
                self.context.i32_type().into(),
                // reference count
                self.context.i32_type().into(),
                // ptr is fully opaque so using the i8_type() here doesn't matter
                self.context.i8_type().ptr_type(Default::default()).into(),
            ],
        );
        // let string_variant = self.debug_builder.create_struct_type(
        //     self.global_scope,
        //     tvalue_names::types::STRING,
        //     self.debug_file,
        //     3,
        //     8 * 16,
        //     8,
        //     0,
        //     None,
        //     &[
        //         self.di_types.i8_type.as_type(),
        //         self.di_types.i32_type.as_type(),
        //         self.di_types.i32_type.as_type(),
        //         self.debug_builder
        //             .create_pointer_type(
        //                 "byte_array_ptr",
        //                 self.di_types.i8_type.as_type(),
        //                 32,
        //                 8,
        //                 Default::default(),
        //             )
        //             .as_type(),
        //     ],
        //     0,
        //     None,
        //     tvalue_names::types::STRING,
        // );
        // self.di_types
        //     .others
        //     .insert(tvalue_names::types::STRING.into(), string_variant.as_type());
    }

    fn gen_type(&self, name: &str, fields: &[BasicTypeEnum<'ctx>]) {
        let base = self.context.opaque_struct_type(name);
        base.set_body(fields, false);
    }

    fn setup_ctor(
        &self,
        init_name: &'static str,
        variant_id: u64,
        mut ctor_args: Vec<BasicMetadataTypeEnum<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let base_ty = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap_or_else(|| panic!("{} is not defined", tvalue_names::types::BASE));
        ctor_args.insert(0, base_ty.ptr_type(Default::default()).into());
        let (f, _bb) = self.add_function(
            init_name,
            self.context.void_type().fn_type(&ctor_args, false),
        );
        let arg1 = f.get_first_param().unwrap().into_pointer_value();
        self.emit_set_tag(arg1, variant_id);
        f
    }

    pub fn emit_set_tag(&self, arg: PointerValue<'ctx>, variant_id: u64) {
        let tag_ptr = self
            .builder
            .build_struct_gep(
                self.module
                    .get_struct_type(tvalue_names::types::BASE)
                    .unwrap(),
                arg.as_any_value_enum().into_pointer_value(),
                0,
                "tag_ptr",
            )
            .expect("tag_ptr");

        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(variant_id, false));
    }
    fn apply_sret_to_param(
        &self,
        f: &FunctionValue<'ctx>,
        param_idx: u32,
        ty: impl Into<Option<AnyTypeEnum<'ctx>>>,
    ) {
        let ty = ty.into().unwrap_or_else(|| {
            let base = self
                .module
                .get_struct_type(tvalue_names::types::BASE)
                .unwrap();
            base.into()
        });
        let kind_id = Attribute::get_named_enum_kind_id("sret");
        let attr = self.context.create_type_attribute(kind_id, ty);
        f.add_attribute(AttributeLoc::Param(param_idx), attr);
    }
    fn add_new_simple_init(
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
        let f = self.setup_ctor(init_name, variant_id, ctor_arg.into_iter().collect());
        let arg1 = f.get_first_param().expect("1 arg");
        if let Some(print_t) = self
            .module
            .get_function(tvalue_names::print_names::PRINT_TVALUE)
        {
            self.builder.build_call(
                print_t,
                &[arg1.as_any_value_enum().into_pointer_value().into()],
                "_",
            );
        }
        self.apply_sret_to_param(&f, 0, None);
        if f.count_params() < 2 {
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
        if let Some(print_t) = self
            .module
            .get_function(tvalue_names::print_names::PRINT_TVALUE)
        {
            self.builder.build_call(
                print_t,
                &[arg1.as_any_value_enum().into_pointer_value().into()],
                "_",
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
        let is_null = self.builder.build_is_null(arg_ptr, "is_null");
        let null_block = self.context.append_basic_block(func, "null_block");
        let not_null_block = self.context.append_basic_block(func, "not_null");
        self.builder
            .build_conditional_branch(is_null, null_block, not_null_block);
        self.builder.position_at_end(null_block);
        self.builder
            .build_return(Some(&self.context.i8_type().const_int(0, false)));
        self.builder.position_at_end(not_null_block);
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
            self.builder.build_float_add(lhs, rhs, "out_value")
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
        // self.apply_sret_to_param(&f, 0, None);
        let params = f.get_params();
        let out = *params.get(2).expect("1 param");
        out.set_name("out");
        let lhs = *params.get(0).expect("2 params");
        lhs.set_name("lhs");
        let rhs = *params.get(1).expect("3 params");
        rhs.set_name("rhs");
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
            lhs_value.as_any_value_enum().into_float_value(),
            rhs_value.as_any_value_enum().into_float_value(),
        );
        self.builder.build_call(
            init_num_fn,
            &[
                out.as_basic_value_enum().into(),
                out_value.as_any_value_enum().into_float_value().into(),
            ],
            "_",
        );
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
    }

    /// Insert a new function into the module that takes 3 tvalue pointers (lhs, rhs, out)
    /// it asserts both lhs and rhs are number variants and also integers and if not, return
    /// `false`, otherwise extract the 2 float values from the lhs & rhs arguments and
    /// then perform the work of the `op` argument with those values. After performing the op,
    /// the value provided will be initialized into the `out` argument and then return `true`
    fn setup_binary_int_fn(
        &self,
        name: &str,
        op: impl FnOnce(IntValue<'ctx>, IntValue<'ctx>) -> IntValue<'ctx>,
    ) {
        let is_two_ints = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_TWO_INTS)
            .unwrap();
        self.setup_binary_num_fn(name, |lhs, rhs| {
            let are_ints =
                self.builder
                    .build_call(is_two_ints, &[lhs.into(), rhs.into()], "are_ints");
            let current_block = self.builder.get_insert_block().unwrap();
            let current_fn = current_block.get_parent().unwrap();
            let two_ints = self.context.append_basic_block(current_fn, "two_ints");
            let not_ints = self.context.append_basic_block(current_fn, "not_ints");
            self.builder.build_conditional_branch(
                are_ints.as_any_value_enum().into_int_value(),
                two_ints,
                not_ints,
            );
            self.builder.position_at_end(not_ints);
            self.builder
                .build_return(Some(&self.context.bool_type().const_int(0, false)));
            self.builder.position_at_end(two_ints);
            let lhs = self
                .builder
                .build_float_to_signed_int(lhs, self.context.i32_type(), "lhs");
            let rhs = self
                .builder
                .build_float_to_signed_int(rhs, self.context.i32_type(), "rhs");
            let out = op(lhs, rhs);
            self.builder
                .build_signed_int_to_float(out, self.context.f32_type(), "out")
        });
    }

    fn add_is_int(&self) {
        let (f, _e) = self.add_function(
            tvalue_names::helper_funcs::IS_INT,
            self.context
                .bool_type()
                .fn_type(&[self.context.f32_type().into()], false),
        );
        let floor = Intrinsic::find("llvm.floor").unwrap();
        let floor_fn = floor
            .get_declaration(&self.module, &[self.context.f32_type().into()])
            .unwrap();
        let input = f.get_first_param().unwrap();
        let floored = self
            .builder
            .build_call(floor_fn, &[input.into()], "floored");
        let is_int = self.builder.build_float_compare(
            inkwell::FloatPredicate::OEQ,
            input.as_any_value_enum().into_float_value(),
            floored.as_any_value_enum().into_float_value(),
            "is_int",
        );
        let exit = self.context.append_basic_block(f, "exit");
        let nai = self.context.append_basic_block(f, "nai");
        self.builder.build_conditional_branch(is_int, exit, nai);
        self.builder.position_at_end(exit);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(nai);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
    }

    fn add_is_two_ints(&self) {
        let is_int_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_INT)
            .unwrap();
        let (f, _e) = self.add_function(
            tvalue_names::helper_funcs::IS_TWO_INTS,
            self.context.bool_type().fn_type(
                &[
                    self.context.f32_type().into(),
                    self.context.f32_type().into(),
                ],
                false,
            ),
        );
        let lhs = f.get_first_param().unwrap();
        lhs.set_name("lhs");
        let rhs = f.get_last_param().unwrap();
        rhs.set_name("rhs");
        let lhs_is = self
            .builder
            .build_call(is_int_fn, &[lhs.into_float_value().into()], "lhs_is");
        let lhs_iai = self.context.append_basic_block(f, "lhs_iai");
        let rhs_iai = self.context.append_basic_block(f, "rhs_iai");
        let nai = self.context.append_basic_block(f, "nai");
        self.builder.build_conditional_branch(
            lhs_is.as_any_value_enum().into_int_value(),
            lhs_iai,
            nai,
        );
        self.builder.position_at_end(lhs_iai);
        let rhs_is = self.builder.build_call(
            is_int_fn,
            &[rhs.as_any_value_enum().into_float_value().into()],
            "rhs_is",
        );
        self.builder.build_conditional_branch(
            rhs_is.as_any_value_enum().into_int_value(),
            rhs_iai,
            nai,
        );
        self.builder.position_at_end(rhs_iai);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(nai);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
    }

    fn add_is_str(&self) {
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let get_tag_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::GET_TAG)
            .unwrap();
        let (f, _e) = self.add_function(
            tvalue_names::helper_funcs::IS_STRING,
            self.context
                .bool_type()
                .fn_type(&[tvalue.ptr_type(Default::default()).into()], false),
        );

        let input = f
            .get_first_param()
            .unwrap()
            .as_any_value_enum()
            .into_pointer_value();
        let kind = self.builder.build_call(get_tag_fn, &[input.into()], "kind");
        let is_int = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            kind.as_any_value_enum().into_int_value(),
            self.context.i8_type().const_int(3, false),
            "is_str",
        );
        self.builder.build_return(Some(&is_int));
    }

    fn add_is_two_strs(&self) {
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let is_str_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_STRING)
            .unwrap();
        let (f, _e) = self.add_function(
            tvalue_names::helper_funcs::IS_TWO_STRINGS,
            self.context.bool_type().fn_type(
                &[
                    tvalue.ptr_type(Default::default()).into(),
                    tvalue.ptr_type(Default::default()).into(),
                ],
                false,
            ),
        );
        let lhs = f.get_first_param().unwrap().into_pointer_value();
        lhs.set_name("lhs");
        let rhs = f.get_last_param().unwrap().into_pointer_value();
        rhs.set_name("rhs");
        let lhs_is = self.builder.build_call(is_str_fn, &[lhs.into()], "lhs_is");
        let lhs_ias = self.context.append_basic_block(f, "lhs_ias");
        let rhs_ias = self.context.append_basic_block(f, "rhs_ias");
        let nas = self.context.append_basic_block(f, "nai");
        self.builder.build_conditional_branch(
            lhs_is.as_any_value_enum().into_int_value(),
            lhs_ias,
            nas,
        );
        self.builder.position_at_end(lhs_ias);
        let rhs_is = self.builder.build_call(is_str_fn, &[rhs.into()], "rhs_is");
        self.builder.build_conditional_branch(
            rhs_is.as_any_value_enum().into_int_value(),
            rhs_ias,
            nas,
        );
        self.builder.position_at_end(rhs_ias);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(nas);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
    }

    fn add_tvalue_sub(&self) {
        self.setup_binary_num_fn(tvalue_names::math::SUB, |lhs, rhs| {
            self.builder
                .build_float_sub::<FloatValue>(lhs, rhs, "out_value")
        });
    }

    fn add_tvalue_mul(&self) {
        self.setup_binary_num_fn(tvalue_names::math::MUL, |lhs, rhs| {
            self.builder.build_float_mul(lhs, rhs, "out_value").into()
        });
    }

    fn add_tvalue_div(&self) {
        self.setup_binary_num_fn(tvalue_names::math::DIV, |lhs, rhs| {
            self.builder.build_float_div(lhs, rhs, "out_value")
        });
    }

    fn add_tvalue_floor_div(&self) {
        let llvm_floor = Intrinsic::find("llvm.floor").expect("llvm.floor");
        let floor_fn = llvm_floor
            .get_declaration(&self.module, &[self.context.f32_type().into()])
            .expect("llvm.floor-decl");
        self.setup_binary_num_fn(tvalue_names::math::FLOOR_DIV, |lhs, rhs| {
            let basic = self.builder.build_float_div(lhs, rhs, "out_value");
            let ret = self.builder.build_call(floor_fn, &[basic.into()], "ret");
            ret.as_any_value_enum().into_float_value()
        });
    }

    fn add_tvalue_mod(&self) {
        self.setup_binary_num_fn(tvalue_names::math::MOD, |lhs, rhs| {
            self.builder.build_float_rem(lhs, rhs, "out_value")
        });
    }

    fn add_tvalue_bin_and(&self) {
        self.setup_binary_int_fn(tvalue_names::math::AND, |lhs, rhs| {
            self.builder
                .build_and(lhs, rhs, "anded")
                .as_any_value_enum()
                .into_int_value()
        });
    }

    fn add_tvalue_bin_or(&self) {
        self.setup_binary_int_fn(tvalue_names::math::OR, |lhs, rhs| {
            self.builder
                .build_or(lhs, rhs, "ored")
                .as_any_value_enum()
                .into_int_value()
        });
    }

    fn add_tvalue_bin_lsh(&self) {
        self.setup_binary_int_fn(tvalue_names::math::LSH, |lhs, rhs| {
            self.builder
                .build_left_shift(lhs, rhs, "lshed")
                .as_any_value_enum()
                .into_int_value()
        });
    }

    fn add_tvalue_bin_rsh(&self) {
        self.setup_binary_int_fn(tvalue_names::math::RSH, |lhs, rhs| {
            self.builder
                .build_right_shift(lhs, rhs, true, "rshed")
                .as_any_value_enum()
                .into_int_value()
        });
    }

    fn add_tvalue_bin_not(&self) {
        let tvalue = self
            .module
            .get_struct_type(tvalue_names::types::BASE)
            .unwrap();
        let is_num_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_NUMBER)
            .unwrap();
        let get_num_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::GET_VALUE_NUMBER)
            .unwrap();
        let is_int_fn = self
            .module
            .get_function(tvalue_names::helper_funcs::IS_INT)
            .unwrap();
        let init_num_fn = self
            .module
            .get_function(tvalue_names::ctors::NUMBER)
            .unwrap();
        let (f, _entry) = self.add_function(
            tvalue_names::math::NOT,
            self.context.bool_type().fn_type(
                &[
                    tvalue.ptr_type(Default::default()).into(),
                    tvalue.ptr_type(Default::default()).into(),
                ],
                false,
            ),
        );
        let val_arg = f.get_first_param().unwrap();
        let out_arg = f.get_last_param().unwrap();
        val_arg.set_name("value");
        out_arg.set_name("out");
        let ian = self.context.append_basic_block(f, "ian");
        let no = self.context.append_basic_block(f, "no");
        let iai = self.context.append_basic_block(f, "iai");
        let is_num = self
            .builder
            .build_call(is_num_fn, &[val_arg.into()], "is_num");
        self.builder
            .build_conditional_branch(is_num.as_any_value_enum().into_int_value(), ian, no);
        self.builder.position_at_end(ian);
        let value_num = self
            .builder
            .build_call(get_num_fn, &[val_arg.into()], "value_num");
        let is_int = self.builder.build_call(
            is_int_fn,
            &[value_num.as_any_value_enum().into_float_value().into()],
            "is_int",
        );
        self.builder
            .build_conditional_branch(is_int.as_any_value_enum().into_int_value(), iai, no);
        self.builder.position_at_end(iai);
        let value_int = self.builder.build_float_to_signed_int(
            value_num.as_any_value_enum().into_float_value(),
            self.context.i32_type(),
            "value_int",
        );
        let notted = self.builder.build_not(value_int, "value_int");
        let ret_float =
            self.builder
                .build_signed_int_to_float(notted, self.context.f32_type(), "ret_float");
        self.builder
            .build_call(init_num_fn, &[out_arg.into(), ret_float.into()], "_");
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(1, false)));
        self.builder.position_at_end(no);
        self.builder
            .build_return(Some(&self.context.bool_type().const_int(0, false)));
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
            "_",
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
                .build_call(pow_intrinsic_fn, &[lhs.into(), rhs.into()], "out_value")
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
        builder.build_call(ctor, &args, "_");
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
        values::{BasicMetadataValueEnum, FunctionValue},
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
        append_hello_world_ctor(&context, &module, &builder.builder);
        let new_str_fun = module.get_function("init_hello_world_str").unwrap();
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
        build_test_tag_func(&context, &module, &builder, "test_str", new_str_fun, &[]);
        maybe_write_test_module("tag_value", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();

        call_tag_test_fn("test_nil", &jit, 0);
        call_tag_test_fn("test_bool", &jit, 1);
        call_tag_test_fn("test_num", &jit, 2);
        call_tag_test_fn("test_str", &jit, 3);
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

    fn append_hello_world_ctor<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> FunctionValue<'ctx> {
        let ty = module.get_struct_type(tvalue_names::types::STRING).unwrap();
        let f = module.add_function(
            "init_hello_world_str",
            context.void_type().fn_type(
                &[context.i8_type().ptr_type(Default::default()).into()],
                false,
            ),
            None,
        );
        let arg = f.get_first_param().unwrap().into_pointer_value();
        let entry = context.append_basic_block(f, "entry");
        builder.position_at_end(entry);
        let arr = "hello world\n"
            .as_bytes()
            .into_iter()
            .map(|b| context.i8_type().const_int(*b as _, false))
            .collect::<Vec<_>>();
        let bytes_array = context.i8_type().const_array(&arr);
        let init_string = module.get_function(tvalue_names::ctors::STRING).unwrap();
        builder.build_call(
            init_string,
            &[
                arg.into(),
                context.i32_type().const_int(arr.len() as _, false).into(),
            ],
            "_",
        );
        let bytes_ptr = builder.build_struct_gep(ty, arg, 4, "bytes_ptr").unwrap();
        let bytes = builder
            .build_load(
                context.i8_type().ptr_type(Default::default()),
                bytes_ptr,
                "bytes",
            )
            .into_pointer_value();
        builder.build_store(bytes, bytes_array);
        let len_ptr = builder.build_struct_gep(ty, arg, 1, "len_ptr").unwrap();
        builder.build_store(len_ptr, context.i32_type().const_int(arr.len() as _, false));
        builder.build_return(None);
        f
    }

    #[test]
    fn check_truthiness() {
        let context = Context::create();
        inkwell::support::enable_llvm_pretty_stack_trace();

        let mut builder = TValueModuleBuilder::new(&context);
        let module = builder.gen_lib();
        let builder = context.create_builder();
        append_hello_world_ctor(&context, &module, &builder);
        let is_bool = module
            .get_function(tvalue_names::helper_funcs::TRUTHY)
            .expect(tvalue_names::helper_funcs::TRUTHY);
        let bb = is_bool.get_first_basic_block().unwrap();
        builder.position_at(bb, &bb.get_first_instruction().unwrap());
        builder.build_call(
            module
                .get_function(tvalue_names::print_names::PRINT_TVALUE)
                .unwrap(),
            &[is_bool
                .get_first_param()
                .unwrap()
                .as_basic_value_enum()
                .into()],
            "_",
        );

        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::NIL).unwrap(),
            None,
            "test_nil",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::BOOL).unwrap(),
            BasicMetadataValueEnum::from(context.bool_type().const_int(1, false)),
            "test_bool_true",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::BOOL).unwrap(),
            BasicMetadataValueEnum::from(context.bool_type().const_int(0, false)),
            "test_bool_false",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function(tvalue_names::ctors::NUMBER).unwrap(),
            BasicMetadataValueEnum::from(context.f32_type().const_float(1.11)),
            "test_number",
        );
        build_truthy_test(
            &context,
            &module,
            &builder,
            module.get_function("init_hello_world_str").unwrap(),
            None,
            "test_string",
        );

        maybe_write_test_module("truthy", &module);

        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        execute_truthy_test("test_nil", &jit, false);
        execute_truthy_test("test_bool_true", &jit, true);
        execute_truthy_test("test_bool_false", &jit, false);
        execute_truthy_test("test_number", &jit, true);
        execute_truthy_test("test_string", &jit, true);
    }

    fn build_truthy_test<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        ctor_fn: FunctionValue<'ctx>,
        ctor_arg: impl Into<Option<BasicMetadataValueEnum<'ctx>>>,
        name: &str,
    ) {
        let f = module.add_function(name, context.i32_type().fn_type(&[], false), None);
        let entry = context.append_basic_block(f, "entry");
        let is_bool = module
            .get_function(tvalue_names::helper_funcs::TRUTHY)
            .expect("getTValueValue_bool");
        builder.position_at_end(entry);
        let alloc = builder.build_alloca(
            module
                .get_struct_type(tvalue_names::types::BASE)
                .expect("base"),
            "t",
        );
        let mut args = vec![BasicMetadataValueEnum::from(alloc)];
        if let Some(arg) = ctor_arg.into() {
            args.push(arg)
        }
        builder.build_call(ctor_fn, &args, "_");
        let ret = builder.build_call(is_bool, &[alloc.into()], "ret");

        let ret = builder.build_int_z_extend(
            ret.as_any_value_enum().into_int_value(),
            context.i32_type(),
            "ret",
        );
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
                ret.as_basic_value_enum().into(),
                lhs.as_basic_value_enum().into(),
                rhs.as_basic_value_enum().into(),
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
        proptest::proptest!(|(l: f32, r: f32)| {
            execute_happy_math_test(name1, &jit, l, r, op);
        });
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
        proptest::proptest!(|(l: f32, r: f32)| {
            execute_happy_math_test(name1, &jit, l, r, |l, r| l - r)
        });
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
        proptest::proptest!(|(l: f32, r: f32)| {
        execute_happy_math_test(name1, &jit, l, r, |l, r| l * r)
        });
    }

    #[test]
    fn test_num_div() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_div";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::DIV, name1);
        maybe_write_test_module("div", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(l: f32, r: f32)| {
            execute_happy_math_test(name1, &jit, l, r, |l, r| l / r)
        });
    }

    #[test]
    fn test_num_floor_div() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_floor_div";
        build_binary_math_happy_test(
            &module_builder,
            &module,
            tvalue_names::math::FLOOR_DIV,
            name1,
        );
        maybe_write_test_module("floor_div", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(l: f32, r: f32)| {
            execute_happy_math_test(name1, &jit, l, r, |l, r| (l / r).floor())
        });
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
        proptest::proptest!(|(l: f32, r: f32)| {
            execute_happy_math_test(name1, &jit, l, r, |l, r| l.powf(r));
        });
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
        proptest::proptest!(|(l: f32, r: f32)| {
            execute_happy_math_test(name1, &jit, l, r, |l, r| l % r);
        });
    }

    #[test]
    fn test_num_bin_and() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_bin_and";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::AND, name1);
        maybe_write_test_module("bin_and", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(l: i32, r: i32)| {
            execute_happy_math_test(name1, &jit, l as f32, r as f32, |l, r| {
                (l as i32 & r as i32) as f32
            });
        });
    }

    #[test]
    fn test_num_bin_or() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_bin_or";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::OR, name1);
        maybe_write_test_module("bin_or", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(l: i32, r: i32)| {
            execute_happy_math_test(name1, &jit, l as f32, r as f32, |l, r| {
                (l as i32 | r as i32) as f32
            });
        });
    }

    #[test]
    fn test_num_bin_lsh() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_bin_lsh";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::LSH, name1);
        maybe_write_test_module("bin_lsh", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(l: i32, r: u32)| {
            execute_happy_math_test(name1, &jit, l as f32, r as f32, |l, r| {
                (l as i32).wrapping_shl(r as u32) as f32
            });
        });
    }

    #[test]
    fn test_num_bin_rsh() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_bin_rsh";
        build_binary_math_happy_test(&module_builder, &module, tvalue_names::math::RSH, name1);
        maybe_write_test_module("bin_rsh", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(l: i32, r: u32)| {
            execute_happy_math_test(name1, &jit, l as f32, r as f32, |l, r| {
                ((l as i32).wrapping_shr(r as u32)) as f32
            });
        });
    }

    #[test]
    fn test_num_bin_not() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_bin_not";
        build_unary_math_happy_test(&module_builder, &module, tvalue_names::math::NOT, name1);
        maybe_write_test_module("bin_not", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(v: i32)| {
            let v = v as f32;
            execute_happy_unary_math_test(name1, &jit, v, |l| {
                (!(l as i32)) as f32
            });
        });
    }

    #[test]
    fn test_num_bin_neg() {
        let context = Context::create();
        let mut module_builder = TValueModuleBuilder::new(&context);
        let module = module_builder.gen_lib();
        let name1 = "test_happy_bin_neg";
        build_unary_math_happy_test(&module_builder, &module, tvalue_names::math::NEG, name1);
        maybe_write_test_module("bin_neg", &module);
        let jit = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        proptest::proptest!(|(v: f32)| {
            execute_happy_unary_math_test(name1, &jit, v, |l| {
                -l
            });
        });
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
        let expected = op(lhs, rhs);
        if val.is_nan() && expected.is_nan() {
            assert_eq!(
                val.is_sign_positive(),
                expected.is_sign_positive(),
                "{} != {}",
                val,
                expected
            );
            return;
        }
        assert_eq!(
            val, expected,
            "{name}: ({lhs}, {rhs}) -> {val} expected {expected}"
        );
    }

    fn execute_happy_unary_math_test<'ctx>(
        name: &'static str,
        jit: &'ctx ExecutionEngine<'ctx>,
        lhs: f32,
        op: impl Fn(f32) -> f32,
    ) {
        type TestFunc = unsafe extern "C" fn(f32) -> f32;
        println!("looking up {name}");
        let func: JitFunction<TestFunc> = unsafe {
            jit.get_function(name).unwrap_or_else(|e| {
                panic!("{name}: {e}");
            })
        };

        let val = unsafe { func.call(lhs) };
        let expected = op(lhs);
        assert_eq!(val, expected, "{name} ({lhs} -> {val} != {expected})");
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
        let rhs = module_builder.emit_new_number("rhs", arg2.as_basic_value_enum().into());
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
        // module_builder
        //     .builder
        //     .build_return(Some(&module_builder.context.f32_type().const_float(2.0)));
    }

    fn build_unary_math_happy_test<'ctx>(
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
            module_builder
                .context
                .f32_type()
                .fn_type(&[module_builder.context.f32_type().into()], false),
            None,
        );
        let arg1 = test_fn.get_first_param().unwrap();
        let entry1 = module_builder.context.append_basic_block(test_fn, "entry");
        module_builder.builder.position_at_end(entry1);
        let lhs = module_builder.emit_new_number("lhs", arg1.as_basic_value_enum().into());
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
        let (f, _) = builder.add_function(
            "test_neg",
            builder
                .context
                .f32_type()
                .fn_type(&[builder.context.f32_type().into()], false),
        );
        let arg = f.get_first_param().unwrap();
        let n = builder.emit_new_number("value", arg.into());
        let o = builder.emit_new_number("out", context.f32_type().const_float(f64::NAN).into());
        let neg_fn = builder
            .module
            .get_function(tvalue_names::math::NEG)
            .unwrap();
        let get_num_fn = builder
            .module
            .get_function(tvalue_names::helper_funcs::GET_VALUE_NUMBER)
            .unwrap();
        let _ret = builder.builder.build_call(
            neg_fn,
            &[
                n.as_basic_value_enum().into(),
                o.as_basic_value_enum().into(),
            ],
            "ret",
        );
        let ret = builder
            .builder
            .build_call(get_num_fn, &[o.as_basic_value_enum().into()], "ret");
        builder
            .builder
            .build_return(Some(&ret.as_any_value_enum().into_float_value()));
        let jit = builder
            .module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        type TestFunc = unsafe extern "C" fn(f32) -> f32;
        let test_neg = unsafe { jit.get_function::<TestFunc>("test_neg").unwrap() };
        proptest::proptest!(|(v: f32)| {
            let negged = unsafe { test_neg.call(v) };
            assert_eq!(negged, -v);
        });
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
