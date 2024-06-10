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

pub extern crate rusqlite;

pub use rusqlite::{
    Connection, Statement, Rows, Row,
    types::{self, ToSql, FromSql, Value, ValueRef, ToSqlOutput, FromSqlResult},
};
pub use query::Query;
pub use table::{Table, InsertInput, TableDesc, Column, SqlTy, TyPrim, AsSqlTy, Create, Insert};
pub use stmt::CompiledStatement;
pub use conn::ConnectionExt;
pub use param::{Param, ParamPrefix};
pub use row::{ResultRecord, ResultSet, Single};
pub use error::{Error, Result};

#[cfg(feature = "not-nan")]
pub use ordered_float::NotNan;

#[cfg(feature = "derive")]
pub use nanosql_macros::{ToSql, FromSql, AsSqlTy, Param, ResultRecord, Table, InsertInput};
