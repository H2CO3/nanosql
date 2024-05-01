//! NanoSQL errors and results

use core::str::Utf8Error;
use core::fmt::{self, Display};
use std::borrow::Cow;
use std::error::Error as StdError;
use serde::{ser::Error as SerError, de::Error as DeError};
use rusqlite::Error as SqlError;
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

    #[error("serialization error: {0}")]
    Ser(Cow<'static, str>),

    #[error("deserialization error: {0}")]
    De(Cow<'static, str>),

    #[error("parameters must be a flat sequence or map of scalars, nesting is not allowed")]
    NestedParam,

    #[error("enum variants with associated data are not supported")]
    EnumWithValue,

    #[error("unknown parameter name `{0}`")]
    UnknownParam(String),

    #[error("parameter name must be a string")]
    ParamNameNotString,

    #[error("query expects {expected} parameters but {actual} were bound")]
    ParamCountMismatch {
        expected: usize,
        actual: usize,
    },

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

    pub(crate) fn de(message: &'static str) -> Self {
        Error::De(Cow::Borrowed(message))
    }
}

impl SerError for Error {
    fn custom<T: Display>(message: T) -> Self {
        Error::Ser(message.to_string().into())
    }
}

impl DeError for Error {
    fn custom<T: Display>(message: T) -> Self {
        Error::De(message.to_string().into())
    }
}

/// Convenience type alias for NanoSQL-related results.
pub type Result<T, E = Error> = core::result::Result<T, E>;
