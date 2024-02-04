use core::fmt;
use std::{backtrace::Backtrace, collections::HashMap, path::PathBuf};

use analisar::{
    ast::{BinaryOperator, Expression, Statement, UnaryOperator},
    Parser,
};
use codegen::CodeGenerator;
use inkwell::{context::Context, module::Module, values::PointerValue};
pub mod bytecode;
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
    let target_module =
        context.create_module(path.file_stem().expect("file_stem").to_str().unwrap());
    let generator = codegen::CodeGenerator::new(target_module);
    let lua = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Error reading {}: {e}", path.display()));
    emit_code(&generator, lua);
    generator.into_module()
}

pub fn emit_code<'ctx>(gen: &CodeGenerator<'ctx>, lua: String) {
    let mut variables = HashMap::new();
    let mut p = Parser::new(lua.as_bytes());
    gen.emit_main_and_move_to_entry();
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
                    emit_assignment(&gen, target, value, &mut variables);
                }
            }
            Statement::Expression(Expression::FuncCall(call)) => {
                let name = expression_to_name(&call.prefix);
                if name != "print" {
                    panic!("Invalid function name, only `print` is supported");
                }
                match call.args {
                    analisar::ast::Args::ExpList(exprs) => {
                        let ptr = expression_to_ptr(
                            &gen,
                            exprs.first().unwrap_or(&Expression::Nil),
                            "_",
                            &mut variables,
                        );
                        gen.perform_print(ptr);
                    }
                    analisar::ast::Args::Table(_) => panic!("tables unsupported"),
                    analisar::ast::Args::String(lit) => {
                        let ptr = gen.init_tvalue_string(&lit.0, "_");
                        gen.perform_print(ptr);
                    }
                };
            }
            Statement::Expression(Expression::BinOp { left, op, right }) => {
                let _success = emit_bin_op(gen, &*left, &*right, op, &mut variables);
            }
            Statement::Return(ret) => {
                if let Some(first) = ret.0.first() {
                    match first {
                        Expression::Nil => continue,
                        Expression::False => {
                            gen.emit_main_return(0);
                            ret_zero = false;
                        }
                        Expression::True => {
                            gen.emit_main_return(1);
                            ret_zero = false;
                        }
                        Expression::Numeral(n) => {
                            if let Ok(i) = n.0.parse::<i32>() {
                                gen.emit_main_return(i);
                                ret_zero = false;
                                continue;
                            }
                            if let Ok(f) = n.0.parse::<f32>() {
                                let trimmed = f.abs();
                                gen.emit_main_return(trimmed as i32);
                                ret_zero = false;
                            }
                        }
                        Expression::LiteralString(s) => {
                            ret_zero = false;
                            let mut val = [0u8; 4];
                            val.copy_from_slice(&s.0);
                            let i = i32::from_be_bytes(val);
                            gen.emit_main_return(i);
                        }
                        Expression::Name(n) => {
                            let Some(existing) = variables.get(n.name.as_ref()) else {
                                continue;
                            };
                            ret_zero = false;
                            if existing.is_null() {
                                gen.emit_main_return(0);
                                continue;
                            }
                            let f = gen.perform_to_number(*existing);
                            let i = gen.convert_float_to_i32(f);
                            gen.emit_return(Some(&i));
                        }
                        Expression::VarArgs => todo!("var args..."),
                        Expression::FunctionDef(_) => todo!(),
                        Expression::TableCtor(_) => todo!(),
                        Expression::BinOp { left, op, right } => {
                            ret_zero = false;
                            let success = emit_bin_op(gen, &*left, &*right, *op, &mut variables);
                            let f = gen.perform_to_number(success);
                            let i = gen.convert_float_to_i32(f);
                            gen.emit_return(Some(&i));
                        }
                        Expression::UnaryOp { .. } => todo!("unop"),
                        Expression::FuncCall(_) => todo!("FuncCall"),
                        Expression::Suffixed(_) => todo!("Suffixed"),
                    }
                }
            }
            _ => unimplemented!("{stmt:?}"),
        }
    }
    if ret_zero {
        gen.emit_main_return(0);
    }
}

fn emit_assignment<'ctx>(
    gen: &CodeGenerator<'ctx>,
    target: &Expression,
    init: Option<&Expression>,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
) -> PointerValue<'ctx> {
    let name = expression_to_name(target);
    let Some(init) = init else {
        let ptr = gen.alloca_tvalue(&name);
        vars.insert(name, ptr);
        return ptr;
    };
    let ptr = expression_to_ptr(gen, init, &name, vars);
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
    gen: &CodeGenerator<'ctx>,
    expr: &Expression,
    name_if_const: &str,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
) -> PointerValue<'ctx> {
    match expr {
        Expression::Nil => gen.alloca_tvalue(&name_if_const),
        Expression::False => gen.init_tvalue_bool(false, &name_if_const),
        Expression::True => gen.init_tvalue_bool(true, &name_if_const),
        Expression::Numeral(n) => {
            if let Ok(i) = n.0.parse::<i64>() {
                return gen.init_tvalue_int(i, &name_if_const);
            }
            let float: f64 = n.0.parse().expect("float value");
            gen.init_tvalue_num(float, &name_if_const)
        }
        Expression::LiteralString(s) => gen.init_tvalue_string(&s.0, &name_if_const),
        Expression::Name(n) => {
            let Some(ptr) = vars.get(&n.name[..]) else {
                panic!("unknown variable {n:?}");
            };
            *ptr
        }
        Expression::BinOp { left, op, right } => emit_bin_op(gen, left, right, *op, vars),
        Expression::UnaryOp { op, exp } => emit_un_op(gen, exp, *op, vars),
        _ => unimplemented!("expression_to_ptr: {expr:?}"),
    }
}

fn emit_bin_op<'ctx>(
    gen: &CodeGenerator<'ctx>,
    left: &Expression,
    right: &Expression,
    op: BinaryOperator,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
) -> PointerValue<'ctx> {
    let op_name = CodeGenerator::binary_op_name(op);
    let lhs = expression_to_ptr(&gen, left, "lhs", vars);
    let rhs = expression_to_ptr(&gen, right, "rhs", vars);
    let dest = gen.alloca_tvalue("result");
    gen.perform_binary_op("success", op_name, lhs, rhs, dest);
    dest
}

fn emit_un_op<'ctx>(
    gen: &CodeGenerator<'ctx>,
    exp: &Expression,
    op: UnaryOperator,
    vars: &mut HashMap<String, PointerValue<'ctx>>,
) -> PointerValue<'ctx> {
    let op_name = CodeGenerator::unary_op_name(op);
    let lhs = expression_to_ptr(&gen, exp, "lhs", vars);
    let dest = gen.alloca_tvalue("result");
    gen.perform_unary_op("success", op_name, lhs, dest);
    dest
}
