use analisar::{
    ast::{BinaryOperator, Expression, FunctionCall, Statement, UnaryOperator},
    Parser,
};
use bstr::ByteSlice;
use codegen::CodeGenerator;
use core::fmt;
use inkwell::{context::Context, module::Module, values::PointerValue};
use std::{backtrace::Backtrace, collections::HashMap, path::PathBuf};

pub mod codegen;

#[derive(Debug)]
pub enum Error {
    BadFileName(String),
    Other { stack: Backtrace, message: String },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::BadFileName(name) => write!(f, "Bad File Name: {name}"),
            Error::Other { stack, message } => write!(f, "{message}\n{stack}"),
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    pub fn other(msg: impl Into<String>) -> Self {
        Self::Other {
            stack: Backtrace::capture(),
            message: msg.into(),
        }
    }
}

pub fn run_on<'ctx>(context: &'ctx Context, path: PathBuf) -> Module<'ctx> {
    inkwell::support::enable_llvm_pretty_stack_trace();
    let target_module =
        context.create_module(path.file_stem().expect("file_stem").to_str().unwrap());
    let generator = codegen::CodeGenerator::new(target_module);
    let lua = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Error reading {}: {e}", path.display()));
    emit_code(&generator, lua);
    generator.into_module()
}

pub fn emit_code<'ctx>(generator: &CodeGenerator<'ctx>, lua: String) {
    let mut variables = HashMap::new();
    let mut p = Parser::new(lua.as_bytes());
    generator.emit_main_and_move_to_entry();
    let mut ret_zero = true;
    while let Some(stmt) = p.next() {
        let stmt = stmt.unwrap();
        match stmt {
            Statement::Assignment {
                local: _,
                targets,
                values,
            } => {
                for (idx, target) in targets.iter().enumerate() {
                    let value = values.get(idx);
                    emit_assignment(&generator, target, value, &mut variables);
                }
            }
            Statement::Expression(Expression::FuncCall(call)) => {
                let _success = emit_fn_call(generator, &call, &mut variables, "");
            }
            Statement::Expression(Expression::BinOp { left, op, right }) => {
                let _success = emit_bin_op(generator, &*left, &*right, op, &mut variables, "");
            }
            Statement::Return(ret) => {
                if let Some(first) = ret.0.first() {
                    match first {
                        Expression::Nil => continue,
                        Expression::False => {
                            generator.emit_main_return(0);
                            ret_zero = false;
                        }
                        Expression::True => {
                            generator.emit_main_return(1);
                            ret_zero = false;
                        }
                        Expression::Numeral(n) => {
                            if let Ok(i) = n.0.parse::<i32>() {
                                generator.emit_main_return(i);
                                ret_zero = false;
                                continue;
                            }
                            if let Ok(f) = n.0.parse::<f32>() {
                                let trimmed = f.abs();
                                generator.emit_main_return(trimmed as i32);
                                ret_zero = false;
                            }
                        }
                        Expression::LiteralString(s) => {
                            ret_zero = false;
                            let mut val = [0u8; 4];
                            val.copy_from_slice(&s.0);
                            let i = i32::from_be_bytes(val);
                            generator.emit_main_return(i);
                        }
                        Expression::Name(n) => {
                            let Some(existing) = variables.get(n.name.as_ref()) else {
                                continue;
                            };
                            ret_zero = false;
                            if existing.is_null() {
                                generator.emit_main_return(0);
                                continue;
                            }
                            let f = generator.perform_to_number(*existing);
                            let i = generator.convert_float_to_i32(f);
                            generator.emit_return(Some(&i));
                        }
                        Expression::VarArgs => todo!("var args..."),
                        Expression::FunctionDef(_) => todo!(),
                        Expression::TableCtor(_) => todo!(),
                        Expression::BinOp { left, op, right } => {
                            ret_zero = false;
                            let success =
                                emit_bin_op(generator, &*left, &*right, *op, &mut variables, "_");
                            let f = generator.perform_to_number(success);
                            let i = generator.convert_float_to_i32(f);
                            generator.emit_return(Some(&i));
                        }
                        Expression::UnaryOp { .. } => todo!("unop"),
                        Expression::FuncCall(inner) => {
                            ret_zero = false;
                            let ret = emit_fn_call(generator, inner, &mut variables, "");
                            let f = generator.perform_to_number(ret);
                            let i = generator.convert_float_to_i32(f);
                            generator.emit_return(Some(&i));
                        }
                        Expression::Suffixed(_) => todo!("Suffixed"),
                    }
                }
            }
            _ => unimplemented!("{stmt:?}"),
        }
    }
    if ret_zero {
        generator.emit_main_return(0);
    }
}

fn emit_fn_call<'ctx>(
    generator: &CodeGenerator<'ctx>,
    call: &FunctionCall,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
    name_if_const: &str,
) -> PointerValue<'ctx> {
    let name = expression_to_name(&call.prefix);
    match name.as_str() {
        "print" => emit_print_call(generator, call, vars, name_if_const),
        "assert" => emit_assert_call(generator, call, vars, name_if_const),
        "error" => emit_error_call(generator, call, vars, name_if_const),
        "tostring" => emit_to_string_call(generator, call, vars, name_if_const),
        _ => todo!("`{}` is unimplemented", name),
    }
}

fn emit_print_call<'ctx>(
    generator: &CodeGenerator<'ctx>,
    call: &FunctionCall,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
    name_if_const: &str,
) -> PointerValue<'ctx> {
    match &call.args {
        analisar::ast::Args::ExpList(exprs) => {
            let ptr = expression_to_ptr(
                &generator,
                exprs.first().unwrap_or(&Expression::Nil),
                name_if_const,
                vars,
            );
            generator.perform_print(ptr);
        }
        analisar::ast::Args::Table(_) => panic!("tables unsupported"),
        analisar::ast::Args::String(lit) => {
            let ptr = generator.init_tvalue_string(&lit.0, name_if_const);
            generator.perform_print(ptr);
        }
    }
    generator
        .i8_type()
        .ptr_type(Default::default())
        .const_null()
}

fn emit_error_call<'ctx>(
    generator: &CodeGenerator<'ctx>,
    call: &FunctionCall,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
    _name_if_const: &str,
) -> PointerValue<'ctx> {
    match &call.args {
        analisar::ast::Args::ExpList(exprs) => {
            let msg = exprs.first().unwrap_or(&Expression::Nil);
            let level = exprs.get(1).unwrap_or(&Expression::Nil);
            let msg = expression_to_ptr(&generator, msg, "_", vars);
            let level = expression_to_ptr(&generator, level, "_", vars);
            generator.perform_error(msg, level)
        }
        analisar::ast::Args::Table(_) => panic!("tables unsupported"),
        analisar::ast::Args::String(lit) => {
            let v = generator.init_tvalue_string(&lit.0, "_");
            let msg = generator
                .i8_type()
                .ptr_type(Default::default())
                .const_null();
            generator.perform_error(v, msg)
        }
    }
}

fn emit_assert_call<'ctx>(
    generator: &CodeGenerator<'ctx>,
    call: &FunctionCall,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
    name_if_const: &str,
) -> PointerValue<'ctx> {
    match &call.args {
        analisar::ast::Args::ExpList(exprs) => {
            let v = exprs.first().unwrap_or(&Expression::Nil);
            let msg = exprs.get(1).unwrap_or(&Expression::Nil);
            let v = expression_to_ptr(&generator, v, "_", vars);
            let msg = expression_to_ptr(&generator, msg, "_", vars);
            generator.perform_assert(v, msg, name_if_const)
        }
        analisar::ast::Args::Table(_) => panic!("tables unsupported"),
        analisar::ast::Args::String(lit) => {
            let v = generator.init_tvalue_string(&lit.0, "_");
            let msg = generator
                .i8_type()
                .ptr_type(Default::default())
                .const_null();
            generator.perform_assert(v, msg, name_if_const)
        }
    }
}

fn emit_to_string_call<'ctx>(
    generator: &CodeGenerator<'ctx>,
    call: &FunctionCall,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
    name_if_const: &str,
) -> PointerValue<'ctx> {
    let dest = generator.alloca_tvalue(name_if_const);
    match &call.args {
        analisar::ast::Args::ExpList(exprs) => {
            let v = exprs.first().unwrap_or(&Expression::Nil);
            let v = expression_to_ptr(&generator, v, "_", vars);
            generator.perform_to_string(v, dest);
        }
        analisar::ast::Args::Table(_) => panic!("tables unsupported"),
        analisar::ast::Args::String(lit) => {
            generator.init_tvalue_string(&lit.0, name_if_const);
        }
    }
    dest
}

fn emit_assignment<'ctx>(
    generator: &CodeGenerator<'ctx>,
    target: &Expression,
    init: Option<&Expression>,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
) -> PointerValue<'ctx> {
    tracing::debug!("emit_assignment {target:?}");
    let name = expression_to_name(target);
    tracing::debug!("emit_assignment `{name}` {init:?}");
    let Some(init) = init else {
        let ptr = generator.alloca_tvalue(&name);
        vars.insert(name, ptr);
        return ptr;
    };
    let ptr = expression_to_ptr(generator, init, &name, vars);
    vars.insert(name, ptr);
    ptr
}

pub fn expression_to_name(expr: &Expression) -> String {
    match expr {
        Expression::Name(n) => n.name.to_string(),
        _ => unimplemented!("expr_to_name: {expr:?}"),
    }
}

fn expression_to_ptr<'ctx>(
    generator: &CodeGenerator<'ctx>,
    expr: &Expression,
    name_if_const: &str,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
) -> PointerValue<'ctx> {
    match expr {
        Expression::Nil => generator.alloca_tvalue(&name_if_const),
        Expression::False => generator.init_tvalue_bool(false, &name_if_const),
        Expression::True => generator.init_tvalue_bool(true, &name_if_const),
        Expression::Numeral(n) => {
            if let Ok(i) = n.0.parse::<i64>() {
                return generator.init_tvalue_int(i, &name_if_const);
            }
            let float: f64 = n.0.parse().expect("float value");
            generator.init_tvalue_num(float, &name_if_const)
        }
        Expression::LiteralString(s) => generator.init_tvalue_string(
            &s.0.trim_with(|ch: char| ch == '"' || ch == '\''),
            &name_if_const,
        ),
        Expression::Name(n) => {
            let Some(ptr) = vars.get(&n.name[..]) else {
                panic!("unknown variable {n:?}");
            };
            *ptr
        }
        Expression::BinOp { left, op, right } => {
            emit_bin_op(generator, left, right, *op, vars, &name_if_const)
        }
        Expression::UnaryOp { op, exp } => emit_un_op(generator, exp, *op, vars, &name_if_const),
        Expression::FuncCall(inner) => emit_fn_call(generator, inner, vars, &name_if_const),
        _ => unimplemented!("expression_to_ptr: {expr:?}"),
    }
}

fn emit_bin_op<'ctx>(
    generator: &CodeGenerator<'ctx>,
    left: &Expression,
    right: &Expression,
    op: BinaryOperator,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
    name_if_const: &str,
) -> PointerValue<'ctx> {
    let op_name = CodeGenerator::binary_op_name(op);
    let lhs = expression_to_ptr(&generator, left, "lhs", vars);
    let rhs = expression_to_ptr(&generator, right, "rhs", vars);
    let dest = generator.alloca_tvalue(name_if_const);
    generator.perform_binary_op("success", op_name, lhs, rhs, dest);
    dest
}

fn emit_un_op<'ctx>(
    generator: &CodeGenerator<'ctx>,
    exp: &Expression,
    op: UnaryOperator,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
    name_if_const: &str,
) -> PointerValue<'ctx> {
    let op_name = CodeGenerator::unary_op_name(op);
    let lhs = expression_to_ptr(&generator, exp, "lhs", vars);
    let dest = generator.alloca_tvalue(name_if_const);
    generator.perform_unary_op("success", op_name, lhs, dest);
    dest
}
