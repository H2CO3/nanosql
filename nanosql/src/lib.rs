#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../README.md"))]
#![forbid(unsafe_code)]
#![deny(missing_docs, missing_debug_implementations, missing_copy_implementations)]

mod param;
mod row;
mod util;

pub mod query;
pub mod table;
pub mod stmt;
pub mod conn;
pub mod error;

pub extern crate serde;
pub extern crate rusqlite;

pub use rusqlite::Connection;
pub use query::Query;
pub use table::{Table, TableDesc, Column, Create, Insert};
pub use stmt::CompiledStatement;
pub use conn::ConnectionExt;
pub use param::ParamPrefix;
pub use error::{Error, Result};
