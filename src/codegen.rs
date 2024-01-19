use analisar::ast::BinaryOperator;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::ContextRef,
    module::Module,
    types::{FloatType, IntType, VoidType},
    values::{AnyValue, ArrayValue, FloatValue, FunctionValue, IntValue, PointerValue}, attributes::{Attribute, AttributeLoc},
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
    tvalue_size: FunctionValue<'ctx>,
}

impl<'ctx> ExpectedCtors<'ctx> {
    pub fn new(module: &Module<'ctx>) -> Self {
        let attr = Attribute::get_named_enum_kind_id("nounwind");
        let c = module.get_context();
        let attr = c.create_enum_attribute(attr, 0);
        let nil = module.add_function(
            runtime::INIT,
            c.void_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        nil.add_attribute(AttributeLoc::Function, attr);
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
        bool.add_attribute(AttributeLoc::Function, attr);
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
        int.add_attribute(AttributeLoc::Function, attr);
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
        float.add_attribute(AttributeLoc::Function, attr);
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
        string_const.add_attribute(AttributeLoc::Function, attr);
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
        let attr = c.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0);
        let print = module.add_function(
            runtime::PRINTLN,
            c.void_type()
                .fn_type(&[c.i8_type().ptr_type(Default::default()).into()], false),
            None,
        );
        let tvalue_size =
            module.add_function(runtime::SIZE, c.i32_type().fn_type(&[], false), None);
        print.add_attribute(AttributeLoc::Function, attr);
        tvalue_size.add_attribute(AttributeLoc::Function, attr);
        module.add_function("printf", c.i32_type().fn_type(&[c.i8_type().ptr_type(Default::default()).into()], true), None);
        Self { print, tvalue_size }
    }
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(module: Module<'ctx>) -> Self {
        let context = module.get_context();
        let builder = context.create_builder();
        let ctors = ExpectedCtors::new(&module);
        let helpers = ExpectedHelpers::new(&module);
        let attr = context.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0);
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
            runtime::math::BIN_RHS,
            runtime::math::BIN_LHS,
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
            f.add_attribute(AttributeLoc::Function, attr);
        }
        Self {
            context,
            module,
            builder,
            ctors,
            helpers,
        }
    }

    pub fn emit_main_and_move_to_entry(&self) {
        let f = self
            .module
            .add_function("main", self.void_type().fn_type(&[], false), None);
        
        let bb = self.context.append_basic_block(f, "entry");
        self.position_at_end(bb);
        self.apply_attrs_to_main(&f);
    }

    fn apply_attrs_to_main<'a>(&self, f: &FunctionValue<'a>) {
        for name in ["noinline", "nounwind", "optnone", "uwtable"].into_iter() {
            let attr = Attribute::get_named_enum_kind_id(name);
            let attr = self.context.create_enum_attribute(attr, 0);
            f.add_attribute(AttributeLoc::Function, attr)
        }
        
    }

    pub fn into_module(self) -> Module<'ctx> {
        self.module
    }

    pub fn emit_main_return(&self) {
        self.builder.build_return(None);
    }

    pub fn void_type(&self) -> VoidType<'ctx> {
        self.context.void_type()
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
    pub fn alloca_tvalue(&self, name: &str) -> PointerValue<'ctx> {
        let size = self
            .builder
            .build_call(self.helpers.tvalue_size, &[], "size")
            .as_any_value_enum()
            .into_int_value();
        let ptr = self.builder.build_array_alloca(self.i8_type(), size, name);
        self.builder.build_call(self.ctors.nil, &[ptr.into()], "_");
        ptr
    }

    pub fn alloca_str(&self, value: &[u8], name: &str) -> PointerValue<'ctx> {
        let ptr = self.builder.build_array_alloca(
            self.i8_type(),
            self.const_i32(value.len() as i32),
            name,
        );
        let value = self.const_string(value);
        self.builder.build_store(ptr, value);
        ptr
    }

    pub fn init_tvalue_bool(&self, value: bool, name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        let init = self.const_bool(value);
        self.builder
            .build_call(self.ctors.bool, &[alloca.into(), init.into()], "_");
        alloca
    }

    /// Generate code
    pub fn init_tvalue_num(&self, value: f64, name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        if value == value.abs() {
            let init = self.const_i64(value as i64);
            self.builder
                .build_call(self.ctors.int, &[alloca.into(), init.into()], "_");
        } else {
            let init = self.const_f64(value);
            self.builder
                .build_call(self.ctors.float, &[alloca.into(), init.into()], "_");
        }
        alloca
    }

    pub fn init_tvalue_string(&self, value: &[u8], name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        let capacity = self.const_u32(value.len() as _);
        let ptr = self.alloca_str(value, &format!("{name}init"));
        self.builder.build_call(
            self.ctors.string_const,
            &[alloca.into(), capacity.into(), ptr.into()],
            "_",
        );
        alloca
    }

    /// generate code that will perform a uniary math operation placing the result in the `dest` pointer
    ///
    /// Returns the `IntValue` indicating if the operation was successful
    pub fn perform_unary_op(
        &self,
        success_name: &str,
        op_name: &str,
        value: PointerValue<'ctx>,
        dest: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        let op_fn = self
            .module
            .get_function(op_name)
            .unwrap_or_else(|| panic!("{op_name} is not a function in this module"));
        self.builder
            .build_call(op_fn, &[value.into(), dest.into()], success_name)
            .as_any_value_enum()
            .into_int_value()
    }

    /// generate code that will perform a binary math operation placing the result in the `dest` pointer
    ///
    /// Returns the `IntValue` indicating if the operation was successful
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
            .build_call(op_fn, &[lhs.into(), rhs.into(), dest.into()], success_name);
    }

    pub fn perform_print(&self, value: PointerValue<'ctx>) {
        self.builder
            .build_call(self.helpers.print, &[value.into()], "_");
    }

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
            RightShift => runtime::math::BIN_RHS,
            LeftShift => runtime::math::BIN_LHS,
            Concatenate => todo!(),
            GreaterThan => todo!(),
            GreaterThanEqual => todo!(),
            LessThan => todo!(),
            LessThanEqual => todo!(),
            Equal => todo!(),
            NotEqual => todo!(),
            And => todo!(),
            Or => todo!(),
        }
    }
}
