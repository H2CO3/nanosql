//! NanoSQL errors and results

use core::str::Utf8Error;
use core::fmt::{self, Display, Formatter};
use std::error::Error as StdError;
use std::borrow::Cow;
use rusqlite::{Error as SqlError, types::FromSqlError};
use thiserror::Error;


/// NanoSQL errors.
#[allow(missing_docs)]
#[derive(Debug, Error)]
pub enum Error {
    #[error("UTF-8 error: {0}")]
    Utf8Error(#[from] Utf8Error),

    #[error("formatting error")]
    Fmt(#[from] fmt::Error),

    #[error("SQLite error: {0}")]
    Sqlite(#[from] SqlError),

    #[error("SQL conversion error: {0}")]
    FromSql(#[from] FromSqlError),

    #[error("query expects {expected} parameters but {actual} were bound")]
    ParamCountMismatch {
        expected: usize,
        actual: usize,
    },

    #[error("expected {expected} rows but {actual} were returned")]
    RowCountMismatch {
        expected: RowCount,
        actual: RowCount,
    },

    #[error("expected {expected} columns but result set has {actual}")]
    ColumnCountMismatch {
        expected: usize,
        actual: usize,
    },

    #[error("unknown parameter name `{0}`")]
    UnknownParam(Cow<'static, str>),

    #[error(transparent)]
    Other(#[from] Box<dyn StdError + Send + Sync + 'static>),
}

impl Error {
    /// Creates an `Error` from any other underlying reason.
    pub fn other<E>(error: E) -> Self
    where
        E: StdError + Send + Sync + 'static
    {
        Error::Other(Box::new(error))
    }

    /// Creates an `Error` from a message.
    pub fn message<T: Display>(message: T) -> Self {
        Error::Other(message.to_string().into())
    }

    /// Creates an `UnknownParam` error from a statically-known parameter name.
    pub fn unknown_param(name: &'static str) -> Self {
        Error::UnknownParam(Cow::Borrowed(name))
    }

    /// Creates an `UnknownParam` error from a dynamic parameter name.
    pub fn unknown_param_dyn<T: Display>(message: T) -> Self {
        Error::UnknownParam(Cow::Owned(message.to_string()))
    }
}

/// Convenience type alias for NanoSQL-related results.
pub type Result<T, E = Error> = core::result::Result<T, E>;

/// A helper type for producing accurate error messages about
/// the actual and expected number of rows.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct RowCount {
    /// At least this many rows were expected or found.
    pub min: usize,
    /// At most this many rows were expected or found.
    /// If this is `None`, then there's no (required or known) upper limit.
    pub max: Option<usize>,
}

impl RowCount {
    /// Indicate that a specific number of rows in a range was expected.
    pub const fn new(min: usize, max: usize) -> Self {
        RowCount {
            min,
            max: Some(max),
        }
    }

    /// Indicate that an exact number of rows were expected or found.
    pub const fn exactly(count: usize) -> Self {
        RowCount {
            min: count,
            max: Some(count),
        }
    }

    /// Indicate that at least this many rows were expected or found.
    pub const fn at_least(min: usize) -> Self {
        RowCount {
            min,
            max: None,
        }
    }

    /// Indicate that at most this many rows were expected or found.
    pub const fn at_most(max: usize) -> Self {
        RowCount {
            min: 0,
            max: Some(max),
        }
    }
}

impl Display for RowCount {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let min = self.min;
        if let Some(max) = self.max {
            if min == max {
                write!(f, "exactly {min}")
            } else if min == 0 {
                write!(f, "at most {max}")
            } else {
                write!(f, "{min}...{max}")
            }
        } else {
            write!(f, "at least {min}")
        }
    }
}
