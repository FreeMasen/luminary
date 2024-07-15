use analisar::ast::{BinaryOperator, UnaryOperator};
use inkwell::{
    attributes::{Attribute, AttributeLoc}, basic_block::BasicBlock, builder::Builder, context::ContextRef, intrinsics::Intrinsic, module::Module, types::{FloatType, IntType, PointerType, VoidType}, values::{AnyValue, ArrayValue, BasicValue, FloatValue, FunctionValue, IntValue, PointerValue}
};

pub struct CodeGenerator<'ctx> {
    context: ContextRef<'ctx>,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    ctors: ExpectedCtors<'ctx>,
    helpers: ExpectedHelpers<'ctx>,
}

struct ExpectedCtors<'ctx> {
    nil: FunctionValue<'ctx>,
    bool: FunctionValue<'ctx>,
    int: FunctionValue<'ctx>,
    float: FunctionValue<'ctx>,
    string_const: FunctionValue<'ctx>,
}

struct ExpectedHelpers<'ctx> {
    print: FunctionValue<'ctx>,
    print_err_msg: FunctionValue<'ctx>,
    tvalue_size: FunctionValue<'ctx>,
    get_tag: FunctionValue<'ctx>,
    to_number: FunctionValue<'ctx>,
    is_truthy: FunctionValue<'ctx>,
    to_string: FunctionValue<'ctx>,
    assert: FunctionValue<'ctx>,
    error: FunctionValue<'ctx>,
}

impl<'ctx> ExpectedCtors<'ctx> {
    pub fn new(module: &Module<'ctx>) -> Self {
        let c = module.get_context();
        let nil = module.add_function(
            runtime::INIT,
            c.void_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        apply_attrs_to_function(&c, &nil);
        let bool = module.add_function(
            runtime::INIT_BOOL,
            c.void_type().fn_type(
                &[
                    c.i8_type().ptr_type(Default::default()).into(),
                    c.bool_type().into(),
                ],
                false,
            ),
            None,
        );
        apply_attrs_to_function(&c, &bool);
        let int = module.add_function(
            runtime::INIT_INT,
            c.void_type().fn_type(
                &[
                    c.i8_type().ptr_type(Default::default()).into(),
                    c.i64_type().into(),
                ],
                false,
            ),
            None,
        );
        apply_attrs_to_function(&c, &int);
        let float = module.add_function(
            runtime::INIT_FLOAT,
            c.void_type().fn_type(
                &[
                    c.i8_type().ptr_type(Default::default()).into(),
                    c.f64_type().into(),
                ],
                false,
            ),
            None,
        );
        apply_attrs_to_function(&c, &float);
        let string_const = module.add_function(
            runtime::INIT_STR,
            c.void_type().fn_type(
                &[
                    c.i8_type().ptr_type(Default::default()).into(),
                    c.i32_type().into(),
                    c.i8_type().ptr_type(Default::default()).into(),
                ],
                false,
            ),
            None,
        );
        apply_attrs_to_function(&c, &string_const);
        Self {
            nil,
            bool,
            int,
            float,
            string_const,
        }
    }
}

impl<'ctx> ExpectedHelpers<'ctx> {
    pub fn new(module: &Module<'ctx>) -> Self {
        let c = module.get_context();
        let print = module.add_function(
            runtime::PRINTLN,
            c.void_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        let print_err_msg = module.add_function(
            runtime::PRINT_ERROR_MESSAGE,
            c.void_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        let tvalue_size =
            module.add_function(runtime::SIZE, c.i32_type().fn_type(&[], false), None);

        module.add_function(
            "printf",
            c.i32_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], true),
            None,
        );
        let to_number = module.add_function(
            runtime::math::TO_NUMBER,
            c.f64_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        let is_truthy = module.add_function(
            runtime::IS_TRUTHY,
            c.bool_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        let to_string = module.add_function(
            runtime::TO_STRING,
            c.void_type().fn_type(
                &[
                    c.i8_type().ptr_type(Default::default()).into(),
                    c.i8_type().ptr_type(Default::default()).into(),
                ],
                false,
            ),
            None,
        );
        let get_tag = module.add_function(
            runtime::GET_TAG,
            c.i8_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        apply_attrs_to_function(&c, &print);
        apply_attrs_to_function(&c, &tvalue_size);
        apply_attrs_to_function(&c, &to_number);
        apply_attrs_to_function(&c, &is_truthy);
        apply_attrs_to_function(&c, &to_string);
        Self {
            print,
            print_err_msg,
            tvalue_size,
            get_tag,
            to_number,
            is_truthy,
            to_string,
            // to be back-filled in CodeGenerator::new
            error: print,
            assert: print,
        }
    }
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(module: Module<'ctx>) -> Self {
        let context = module.get_context();
        let builder = context.create_builder();
        let ctors = ExpectedCtors::new(&module);
        let helpers = ExpectedHelpers::new(&module);
        for name in &[
            runtime::math::ADD,
            runtime::math::SUB,
            runtime::math::MUL,
            runtime::math::DIV,
            runtime::math::FLOOR_DIV,
            runtime::math::POW,
            runtime::math::REM,
            runtime::math::BIN_AND,
            runtime::math::BIN_OR,
            runtime::math::BIN_SHR,
            runtime::math::BIN_SHL,
            runtime::EQUALS,
        ] {
            let f = module.add_function(
                name,
                context.void_type().fn_type(
                    &[
                        context.i8_type().ptr_type(Default::default()).into(),
                        context.i8_type().ptr_type(Default::default()).into(),
                        context.i8_type().ptr_type(Default::default()).into(),
                    ],
                    false,
                ),
                None,
            );
            apply_attrs_to_function(&context, &f);
        }
        for name in &[runtime::math::NEG, runtime::math::BIN_NOT] {
            let f = module.add_function(
                name,
                context.void_type().fn_type(
                    &[
                        context.i8_type().ptr_type(Default::default()).into(),
                        context.i8_type().ptr_type(Default::default()).into(),
                    ],
                    false,
                ),
                None,
            );
            apply_attrs_to_function(&context, &f);
        }
        let mut ret = Self {
            context,
            module,
            builder,
            ctors,
            helpers,
        };
        ret.back_fill_helpers();
        ret
    }

    fn back_fill_helpers(&mut self) {
        self.back_fill_error();
        self.back_fill_assert();
    }

    fn back_fill_error(&mut self) {
        let f = self.module.add_function(
            "luminary::std::error",
            self.void_type().fn_type(
                &[
                    // msg
                    self.ptr_type().into(),
                    // level
                    self.ptr_type().into(),
                ],
                false,
            ),
            None,
        );
        let trap = Intrinsic::find("llvm.trap").expect("find trap");
        let trap = trap.get_declaration(&self.module, &[]).expect("trap decl");
        self.builder
            .position_at_end(self.context.append_basic_block(f, "entry"));
        let first_param = f
            .get_first_param()
            .unwrap()
            .as_any_value_enum()
            .into_pointer_value();
        first_param.set_name("message");
        self.builder
            .build_call(self.helpers.print_err_msg, &[first_param.into()], "_").unwrap();
        self.builder.build_call(trap, &[], "_").unwrap();
        self.builder.build_return(None).unwrap();
        self.helpers.error = f;
    }

    fn back_fill_assert(&mut self) {
        const DEFAULT_MSG: &[u8] = b"assertion failed!";
        let f = self.module.add_function(
            "luminary::assert",
            self.void_type().fn_type(
                &[
                    // test
                    self.ptr_type().into(),
                    // msg (optional)
                    self.ptr_type().into(),
                ],
                false,
            ),
            None,
        );
        self.builder
            .position_at_end(self.context.append_basic_block(f, "entry"));
        let first_param = f
            .get_first_param()
            .unwrap()
            .as_any_value_enum()
            .into_pointer_value();
        first_param.set_name("value");
        let last_param = f
            .get_nth_param(1)
            .unwrap()
            .as_any_value_enum()
            .into_pointer_value();
        last_param.set_name("message");
        let default_msg = self.init_tvalue_string(DEFAULT_MSG, "default_msg");
        let is_true = self
            .builder
            .build_call(self.helpers.is_truthy, &[first_param.into()], "is_true")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();

        let should_trap = self.context.append_basic_block(f, "should_trap");
        let exit = self.context.append_basic_block(f, "exit");
        self.builder
            .build_conditional_branch(is_true, exit, should_trap).unwrap();

        self.builder.position_at_end(should_trap);
        let is_null = self.builder.build_is_null(last_param, "is_null").unwrap();

        let arg_null = self.context.append_basic_block(f, "arg_null");
        let arg_nn = self.context.append_basic_block(f, "arg_nn");
        let arg_nil = self.context.append_basic_block(f, "arg_nil");
        let trap = self.context.append_basic_block(f, "trap");
        self.builder
            .build_conditional_branch(is_null, arg_null, arg_nn).unwrap();

        self.builder.position_at_end(arg_null);
        self.builder.build_unconditional_branch(trap).unwrap();

        self.builder.position_at_end(arg_nn);
        let tag = self
            .builder
            .build_call(self.helpers.get_tag, &[last_param.into()], "tag")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let arg_is_nil = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            tag,
            self.const_u8(0),
            "arg_is_nil",
        ).unwrap();

        self.builder
            .build_conditional_branch(arg_is_nil, arg_nil, trap).unwrap();

        self.builder.position_at_end(arg_nil);
        self.builder.build_unconditional_branch(trap).unwrap();

        self.builder.position_at_end(trap);
        let msg = self.builder.build_phi(self.ptr_type(), "msg").unwrap();

        msg.add_incoming(&[
            (&last_param, arg_nn),
            (&default_msg, arg_null),
            (&default_msg, arg_nil),
        ]);
        let msg = msg.as_any_value_enum().into_pointer_value();
        self.builder.build_call(
            self.helpers.error,
            &[
                msg.into(),
                // TODO: set this to TValue(0)...
                self.ptr_type().const_null().into(),
            ],
            "error",
        ).unwrap();
        // will never execute but llvm can't figure that out
        self.builder.build_unconditional_branch(exit).unwrap();

        self.builder.position_at_end(exit);
        self.builder.build_return(None).unwrap();
        self.helpers.assert = f;
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn emit_main_and_move_to_entry(&self) {
        let f = self
            .module
            .add_function("main", self.i32_type().fn_type(&[], false), None);

        let bb = self.context.append_basic_block(f, "entry");
        self.position_at_end(bb);
        self.apply_attrs(&f);
    }

    fn apply_attrs<'a>(&self, f: &FunctionValue<'a>) {
        apply_attrs_to_function(&self.context, f);
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn into_module(self) -> Module<'ctx> {
        self.module
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn emit_main_return(&self, value: i32) {
        self.emit_return(Some(&self.const_i32(value)));
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn emit_return(&self, v: Option<&dyn BasicValue<'ctx>>) {
        self.builder.build_return(v).unwrap();
    }

    pub fn void_type(&self) -> VoidType<'ctx> {
        self.context.void_type()
    }

    pub fn ptr_type(&self) -> PointerType<'ctx> {
        self.context.i8_type().ptr_type(Default::default())
    }

    pub fn i8_type(&self) -> IntType<'ctx> {
        self.context.i8_type()
    }

    pub fn i32_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    pub fn i64_type(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }

    pub fn f32_type(&self) -> FloatType<'ctx> {
        self.context.f32_type()
    }

    pub fn f64_type(&self) -> FloatType<'ctx> {
        self.context.f64_type()
    }

    pub fn bool_type(&self) -> IntType<'ctx> {
        self.context.bool_type()
    }

    pub fn const_u32(&self, value: u32) -> IntValue<'ctx> {
        self.i32_type().const_int(value as _, false)
    }

    pub fn const_u8(&self, value: u8) -> IntValue<'ctx> {
        self.i8_type().const_int(value as _, false)
    }

    pub fn const_i32(&self, value: i32) -> IntValue<'ctx> {
        let be_bytes = value.to_be_bytes();
        let casted = u32::from_be_bytes(be_bytes);
        self.i32_type().const_int(casted as _, true)
    }

    pub fn const_i64(&self, value: i64) -> IntValue<'ctx> {
        let be_bytes = value.to_be_bytes();
        let casted = u64::from_be_bytes(be_bytes);
        self.i64_type().const_int(casted as _, true)
    }

    pub fn const_f32(&self, value: f32) -> FloatValue<'ctx> {
        self.f32_type().const_float(value as _)
    }

    pub fn const_f64(&self, value: f64) -> FloatValue<'ctx> {
        self.f64_type().const_float(value as _)
    }

    pub fn const_bool(&self, value: bool) -> IntValue<'ctx> {
        self.bool_type().const_int(value as _, false)
    }

    pub fn const_string(&self, value: &[u8]) -> ArrayValue<'ctx> {
        let init = value
            .into_iter()
            .map(|b| self.const_u8(*b))
            .collect::<Vec<_>>();
        self.context.i8_type().const_array(&init)
    }

    pub fn position_at_end(&self, bb: BasicBlock) {
        self.builder.position_at_end(bb)
    }

    /// Generate code that will emit a single alloca for the tvalue base type setting all values
    /// to their defaults (all 0)
    #[tracing::instrument(level = "debug", skip(self))]
    pub fn alloca_tvalue(&self, name: &str) -> PointerValue<'ctx> {
        let size = self
            .builder
            .build_call(self.helpers.tvalue_size, &[], "size")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let ptr = self.builder.build_array_alloca(self.i8_type(), size, name).unwrap();
        self.builder.build_call(self.ctors.nil, &[ptr.into()], "_").unwrap();
        ptr
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn alloca_str(&self, value: &[u8], name: &str) -> PointerValue<'ctx> {
        let ptr = self.builder.build_array_alloca(
            self.i8_type(),
            self.const_i32(value.len() as i32),
            name,
        ).unwrap();
        let value = self.const_string(value);
        self.builder.build_store(ptr, value).unwrap();
        ptr
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn init_tvalue_bool(&self, value: bool, name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        let init = self.const_bool(value);
        self.builder
            .build_call(self.ctors.bool, &[alloca.into(), init.into()], "_").unwrap();
        alloca
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn init_tvalue_num(&self, value: f64, name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        if value == value.trunc() {
            let init = self.const_i64(value as i64);
            self.builder
                .build_call(self.ctors.int, &[alloca.into(), init.into()], "_").unwrap();
        } else {
            let init = self.const_f64(value);
            self.builder
                .build_call(self.ctors.float, &[alloca.into(), init.into()], "_").unwrap();
        }
        alloca
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn init_tvalue_int(&self, value: i64, name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        let init = self.const_i64(value);
        self.builder
            .build_call(self.ctors.int, &[alloca.into(), init.into()], "_").unwrap();
        alloca
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn init_tvalue_string(&self, value: &[u8], name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        let capacity = self.const_u32(value.len() as _);
        let ptr = self.alloca_str(value, &format!("{name}init"));
        self.builder.build_call(
            self.ctors.string_const,
            &[alloca.into(), capacity.into(), ptr.into()],
            "_",
        ).unwrap();
        alloca
    }

    /// generate code that will perform a uniary math operation placing the result in the `dest` pointer
    ///
    /// Returns the `IntValue` indicating if the operation was successful
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn perform_unary_op(
        &self,
        success_name: &str,
        op_name: &str,
        value: PointerValue<'ctx>,
        dest: PointerValue<'ctx>,
    ) {
        let op_fn = self
            .module
            .get_function(op_name)
            .unwrap_or_else(|| panic!("{op_name} is not a function in this module"));
        self.builder
            .build_call(op_fn, &[value.into(), dest.into()], success_name).unwrap();
    }

    /// generate code that will perform a binary math operation placing the result in the `dest` pointer
    ///
    /// Returns the `IntValue` indicating if the operation was successful
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn perform_binary_op(
        &self,
        success_name: &str,
        op_name: &str,
        lhs: PointerValue<'ctx>,
        rhs: PointerValue<'ctx>,
        dest: PointerValue<'ctx>,
    ) {
        let op_fn = self
            .module
            .get_function(op_name)
            .unwrap_or_else(|| panic!("{op_name} is not a function in this module"));
        self.builder
            .build_call(op_fn, &[lhs.into(), rhs.into(), dest.into()], success_name).unwrap();
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn perform_print(&self, value: PointerValue<'ctx>) {
        self.builder
            .build_call(self.helpers.print, &[value.into()], "_").unwrap();
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn perform_assert(
        &self,
        value: PointerValue<'ctx>,
        msg: PointerValue<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_call(self.helpers.assert, &[value.into(), msg.into()], "_").unwrap();        
        value
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn perform_error(
        &self,
        value: PointerValue<'ctx>,
        level: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_call(self.helpers.error, &[value.into(), level.into()], "_").unwrap();
        value
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn perform_to_number(&self, value: PointerValue<'ctx>) -> FloatValue<'ctx> {
        self.builder
            .build_call(self.helpers.to_number, &[value.into()], "_")
            .unwrap()
            .as_any_value_enum()
            .into_float_value()
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn perform_to_string(
        &self,
        value: PointerValue<'ctx>,
        dest: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_call(self.helpers.to_string, &[value.into(), dest.into()], "_").unwrap();
        dest
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn convert_float_to_i32(&self, value: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.builder
            .build_float_to_signed_int(value, self.i32_type(), "_").unwrap()
    }

    #[tracing::instrument(level = "trace")]
    pub fn binary_op_name(op: BinaryOperator) -> &'static str {
        use BinaryOperator::*;
        match op {
            Add => runtime::math::ADD,
            Subtract => runtime::math::SUB,
            Multiply => runtime::math::MUL,
            Divide => runtime::math::DIV,
            FloorDivide => runtime::math::FLOOR_DIV,
            Power => runtime::math::POW,
            Modulo => runtime::math::REM,
            BitwiseAnd => runtime::math::BIN_AND,
            BitwiseXor => runtime::math::BIN_XOR,
            BitwiseOr => runtime::math::BIN_OR,
            RightShift => runtime::math::BIN_SHR,
            LeftShift => runtime::math::BIN_SHL,
            Concatenate => todo!(),
            GreaterThan => todo!(),
            GreaterThanEqual => todo!(),
            LessThan => todo!(),
            LessThanEqual => todo!(),
            Equal => runtime::EQUALS,
            NotEqual => todo!(),
            And => todo!(),
            Or => todo!(),
        }
    }

    #[tracing::instrument(level = "trace")]
    pub fn unary_op_name(op: UnaryOperator) -> &'static str {
        use UnaryOperator::*;
        match op {
            Negate => runtime::math::NEG,
            Not => todo!(),
            Length => todo!(),
            BitwiseNot => runtime::math::BIN_NOT,
        }
    }
}

fn apply_attrs_to_function<'a>(context: &ContextRef<'a>, f: &FunctionValue<'a>) {
    for name in [
        "noinline", "nounwind", "optnone",
        // "uwtable"
    ]
    .into_iter()
    {
        let attr = Attribute::get_named_enum_kind_id(name);
        let attr = context.create_enum_attribute(attr, 0);
        f.add_attribute(AttributeLoc::Function, attr)
    }
}

pub enum Error {
    
}
