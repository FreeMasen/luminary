use core::fmt;
use std::backtrace::Backtrace;
// use inkwell::{
//     OptimizationLevel,
//     builder::Builder,
//     context::Context,
//     module::Module,
// };
pub mod bytecode;
pub mod tvalue;

// pub struct Compiler {
//     context: Context,
//     builder: Builder,
// }

// impl Default for Compiler {
//     fn default() -> Self {
//         let context = Context::create();
//         let builder = context.create_builder();

//         Self {
//             context, builder
//         }
//     }

//     fn compile_file(&self, path: impl AsRef<Path>) -> Result<Module, Error> {
//         let lua = std::fs::read(path.as_ref()).map_err(|e| {
//             Error::other(format!("Error reading {}: {e}", path.as_ref().display()))
//         })?;
//         let parser = analisar::Parser::new(&lua);
//         let builder = self.context.create_builder();
//         let module = self.context.create_module(&resolve_file_to_module_name(path.as_ref()));
//         while let Some(part) = parser.next() {
//             let part = part.map_err(|e| {
//                 Error::other(format!("Error parsing statement: {e}"))
//             })?;
//             self.compile_stmt(&stmt, &module)?;
//         }
//         Ok(module)
//     }

//     fn compile_stmt(&self, stmt: &Statement, module: &Module) -> Result<(), Error> {
//         match stmt {
//             Statement::Empty => return Ok(()),
//             Statement::Expression(expr) => self.compile_expr(expr),
//             Statement::Assignment { local, targets, values } => todo!(),
//             Statement::Label(_) => todo!(),
//             Statement::Break => todo!(),
//             Statement::GoTo(_) => todo!(),
//             Statement::Do { block } => todo!(),
//             Statement::While { exp, block } => todo!(),
//             Statement::Repeat { block, exp } => todo!(),
//             Statement::If(_) => todo!(),
//             Statement::For(_) => todo!(),
//             Statement::ForIn(_) => todo!(),
//             Statement::Function { local, name, body } => todo!(),
//             Statement::Return(_) => todo!(),
//         }
//         Ok(())
//     }
//     fn compile_expr(&self, expr: Expression) -> Result<(), Error> {
//         let builder = self.context.create_builder();
//         match expr {
//             Expression::Nil => {},
//             Expression::False => todo!(),
//             Expression::True => todo!(),
//             Expression::Numeral(_) => todo!(),
//             Expression::LiteralString(_) => todo!(),
//             Expression::Name(_) => todo!(),
//             Expression::VarArgs => todo!(),
//             Expression::FunctionDef(_) => todo!(),
//             Expression::TableCtor(_) => todo!(),
//             Expression::BinOp { left, op, right } => todo!(),
//             Expression::UnaryOp { op, exp } => todo!(),
//             Expression::FuncCall(func) => {
//                 builder.build_call(func.prefix, args, name)
//             },
//             Expression::Suffixed(_) => todo!(),
//         }
//         Ok(())
//     }
// }

// fn resolve_file_to_module_name(file: impl AsRef<Path>) -> Result<String, Error> {
//     let gen_err = || {
//         Error::BadFileName(format!("{}", file.as_ref().display()))
//     };
//     let full = std::fs::canonicalize(file.as_ref()).map_err(|_| {
//         gen_err()
//     })?;
//     let name = full.file_name().ok_or_else(|| {
//         gen_err()
//     })?;
//     let ext = full.extension().ok_or_else(|| {
//         gen_err()
//     })?;
//     if ext != "lua" {
//         return Err(gen_err())
//     }
//     Ok(name.to_string_lossy())
// }

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
