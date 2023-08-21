use core::fmt;
use std::backtrace::Backtrace;
pub mod bytecode;
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
