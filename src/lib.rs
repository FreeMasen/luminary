use core::fmt;
use std::{backtrace::Backtrace, collections::HashMap, path::PathBuf};

use analisar::{
    ast::{BinaryOperator, Expression, Statement},
    Parser,
};
use codegen::CodeGenerator;
use inkwell::{context::Context, module::Module, values::PointerValue};
use tvalue::tvalue_names;
pub mod bytecode;
pub mod codegen;
pub mod symbol_table;
pub mod tvalue;

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
    let mut std_builder = tvalue::TValueModuleBuilder::new(&context);
    let tvalue_module = std_builder.gen_lib();
    target_module
        .link_in_module(tvalue_module)
        .expect("linking should work");
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
                        if exprs.is_empty() {
                            let ptr = expression_to_ptr(
                                &gen,
                                &Expression::Nil,
                                "_",
                                &mut variables,
                            );
                            gen.perform_print(std::iter::once(ptr));
                            continue;
                        }
                        gen.perform_print(exprs.iter().map(|e| expression_to_ptr(&gen, e, "_", &mut variables)));
                    }
                    analisar::ast::Args::Table(_) => panic!("tables unsupported"),
                    analisar::ast::Args::String(lit) => {
                        let ptr = gen.init_tvalue_string(&lit.0, "_");
                        gen.perform_print(std::iter::once(ptr));
                    }
                };
            }
            Statement::Expression(Expression::BinOp { left, op, right }) => {
                use tvalue_names::math::*;
                let op_name = match op {
                    BinaryOperator::Add => ADD,
                    BinaryOperator::Subtract => SUB,
                    BinaryOperator::Multiply => MUL,
                    BinaryOperator::Divide => DIV,
                    BinaryOperator::FloorDivide => FLOOR_DIV,
                    BinaryOperator::Power => POW,
                    BinaryOperator::Modulo => MOD,
                    BinaryOperator::BitwiseAnd => AND,
                    BinaryOperator::BitwiseOr => OR,
                    BinaryOperator::RightShift => RSH,
                    BinaryOperator::LeftShift => LSH,
                    _ => unimplemented!("{op:?}"),
                };
                let lhs = expression_to_ptr(&gen, &*left, "lhs", &mut variables);
                let rhs = expression_to_ptr(&gen, &*right, "rhs", &mut variables);
                let dest = gen.alloca_tvalue("result");
                let _success = gen.perform_binary_op("success", op_name, lhs, rhs, dest);
            }
            _ => unimplemented!("{stmt:?}"),
        }
    }
    gen.emit_main_return();
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
            let float: f32 = n.0.parse().expect("float value");
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
    use tvalue_names::math::*;
    let op_name = match op {
        BinaryOperator::Add => ADD,
        BinaryOperator::Subtract => SUB,
        BinaryOperator::Multiply => MUL,
        BinaryOperator::Divide => DIV,
        BinaryOperator::FloorDivide => FLOOR_DIV,
        BinaryOperator::Power => POW,
        BinaryOperator::Modulo => MOD,
        BinaryOperator::BitwiseAnd => AND,
        BinaryOperator::BitwiseOr => OR,
        BinaryOperator::RightShift => RSH,
        BinaryOperator::LeftShift => LSH,
        _ => unimplemented!("{op:?}"),
    };
    let lhs = expression_to_ptr(&gen, &*left, "lhs", vars);
    let rhs = expression_to_ptr(&gen, &*right, "rhs", vars);
    let dest = gen.alloca_tvalue("result");
    let _success = gen.perform_binary_op("success", op_name, lhs, rhs, dest);
    dest
}
