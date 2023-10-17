use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::ContextRef,
    module::Module,
    types::{FloatType, IntType, StructType, VoidType},
    values::{AnyValue, ArrayValue, FloatValue, FunctionValue, IntValue, PointerValue},
};

use crate::tvalue::tvalue_names;

pub struct CodeGenerator<'ctx> {
    context: ContextRef<'ctx>,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    structs: ExpectedStructs<'ctx>,
    ctors: ExpectedCtors<'ctx>,
    helpers: ExpectedHelpers<'ctx>,
}

struct ExpectedStructs<'ctx> {
    base: StructType<'ctx>,
    #[allow(unused)]
    bool: StructType<'ctx>,
    #[allow(unused)]
    number: StructType<'ctx>,
    string: StructType<'ctx>,
}

struct ExpectedCtors<'ctx> {
    nil: FunctionValue<'ctx>,
    bool: FunctionValue<'ctx>,
    number: FunctionValue<'ctx>,
    string: FunctionValue<'ctx>,
}

struct ExpectedHelpers<'ctx> {
    print: FunctionValue<'ctx>,
}

impl<'ctx> ExpectedStructs<'ctx> {
    pub fn new(module: &Module<'ctx>) -> Self {
        Self {
            base: module
                .get_struct_type(tvalue_names::types::BASE)
                .expect(tvalue_names::types::BASE),
            bool: module
                .get_struct_type(tvalue_names::types::BOOL)
                .expect(tvalue_names::types::BOOL),
            number: module
                .get_struct_type(tvalue_names::types::NUMBER)
                .expect(tvalue_names::types::NUMBER),
            string: module
                .get_struct_type(tvalue_names::types::STRING)
                .expect(tvalue_names::types::STRING),
        }
    }
}

impl<'ctx> ExpectedCtors<'ctx> {
    pub fn new(module: &Module<'ctx>) -> Self {
        Self {
            nil: module
                .get_function(tvalue_names::ctors::NIL)
                .expect(tvalue_names::ctors::NIL),
            bool: module
                .get_function(tvalue_names::ctors::BOOL)
                .expect(tvalue_names::ctors::BOOL),
            number: module
                .get_function(tvalue_names::ctors::NUMBER)
                .expect(tvalue_names::ctors::NUMBER),
            string: module
                .get_function(tvalue_names::ctors::STRING)
                .expect(tvalue_names::ctors::STRING),
        }
    }
}

impl<'ctx> ExpectedHelpers<'ctx> {
    pub fn new(module: &Module<'ctx>) -> Self {
        Self {
            print: module
                .get_function(tvalue_names::print_names::PRINT_TVALUE)
                .expect(tvalue_names::print_names::PRINT_TVALUE),
        }
    }
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn from_tvalue_module(tvalue_module: &Module<'ctx>, name: &str) -> Self {
        let context = tvalue_module.get_context();
        let module = context.create_module(name);
        for f in tvalue_module.get_functions() {
            module.add_function(
                f.get_name().to_str().unwrap(),
                f.get_type(),
                Some(f.get_linkage()),
            );
        }
        Self::new(module)
    }

    pub fn new(module: Module<'ctx>) -> Self {
        let context = module.get_context();
        let builder = context.create_builder();
        let structs = ExpectedStructs::new(&module);
        let ctors = ExpectedCtors::new(&module);
        let helpers = ExpectedHelpers::new(&module);
        Self {
            context,
            module,
            builder,
            structs,
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

    pub fn i32_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    pub fn f32_type(&self) -> FloatType<'ctx> {
        self.context.f32_type()
    }

    pub fn bool_type(&self) -> IntType<'ctx> {
        self.context.bool_type()
    }

    pub fn const_u32(&self, value: u32) -> IntValue<'ctx> {
        self.i32_type().const_int(value as _, false)
    }

    pub fn const_u8(&self, value: u8) -> IntValue<'ctx> {
        self.context.i8_type().const_int(value as _, false)
    }

    pub fn const_i32(&self, value: i32) -> IntValue<'ctx> {
        let be_bytes = value.to_be_bytes();
        let casted = u32::from_be_bytes(be_bytes);
        self.i32_type().const_int(casted as _, true)
    }

    pub fn const_f32(&self, value: f32) -> FloatValue<'ctx> {
        self.f32_type().const_float(value as _)
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

    pub fn init_tvalue_bool(&self, value: bool, name: &str) -> PointerValue<'ctx> {
        let alloca = self.builder.build_alloca(self.structs.base, name);
        let init = self.const_bool(value);
        self.builder
            .build_call(self.ctors.bool, &[alloca.into(), init.into()], "_");
        alloca
    }

    /// Generate code that will emit a single alloca for the tvalue base type setting all values
    /// to their defaults (all 0)
    pub fn alloca_tvalue(&self, name: &str) -> PointerValue<'ctx> {
        let ptr = self.builder.build_alloca(self.structs.base, name);
        self.builder.build_call(self.ctors.nil, &[ptr.into()], "_");
        ptr
    }

    /// Generate code
    pub fn init_tvalue_num(&self, value: f32, name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        let init = self.const_f32(value);
        self.builder
            .build_call(self.ctors.number, &[alloca.into(), init.into()], "_");
        alloca
    }

    pub fn init_tvalue_string(&self, value: &[u8], name: &str) -> PointerValue<'ctx> {
        let alloca = self.alloca_tvalue(name);
        let capacity = self.const_u32(value.len() as _);
        let init = self.const_string(value);
        self.builder
            .build_call(self.ctors.string, &[alloca.into(), capacity.into()], "_");
        if !value.is_empty() {
            let ptr_ptr = self
                .builder
                .build_struct_gep(self.structs.string, alloca, 4, "ptr_ptr")
                .expect("string buffer at index 4");
            let ptr = self
                .builder
                .build_load(self.i32_type().ptr_type(Default::default()), ptr_ptr, "ptr")
                .into_pointer_value();
            self.builder.build_store(ptr, init);
            let len_prt = self
                .builder
                .build_struct_gep(self.structs.string, alloca, 1, "len_prt")
                .expect("string length at index 1");
            self.builder
                .build_store(len_prt, self.const_u32(value.len() as _));
        }
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
    ) -> IntValue<'ctx> {
        let op_fn = self
            .module
            .get_function(op_name)
            .unwrap_or_else(|| panic!("{op_name} is not a function in this module"));
        self.builder
            .build_call(op_fn, &[lhs.into(), rhs.into(), dest.into()], success_name)
            .as_any_value_enum()
            .into_int_value()
    }

    pub fn perform_print(&self, value: PointerValue<'ctx>) {
        self.builder
            .build_call(self.helpers.print, &[value.into()], "_");
    }
}

#[cfg(test)]
mod test {
    use crate::tvalue::TValueModuleBuilder;

    use super::*;

    #[test]
    fn try_construct() {
        let context = inkwell::context::Context::create();
        let mut tvalue_builder = TValueModuleBuilder::new(&context);
        let tvalue_module = tvalue_builder.gen_lib();
        let _generator = CodeGenerator::from_tvalue_module(&tvalue_module, "test");
    }
}
