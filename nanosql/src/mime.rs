//! Newtype wrapper around [`mime::Mime`], so it can implement `FromSql` and `ToSql`.

use std::str::FromStr;
use std::ops::{Deref, DerefMut};
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Display, Debug, Formatter};
use crate::{FromSql, ToSql, ValueRef, ToSqlOutput, AsSqlTy, SqlTy, rusqlite::types::FromSqlError};


/// Newtype wrapper around [`mime::Mime`].
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Mime(pub mime::Mime);

impl Mime {
    /// Returns the wrapped [`mime::Mime`] value.
    pub fn into_inner(self) -> mime::Mime {
        self.0
    }
}

impl From<mime::Mime> for Mime {
    fn from(inner: mime::Mime) -> Self {
        Mime(inner)
    }
}

impl Display for Mime {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Debug for Mime {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl FromStr for Mime {
    type Err = <mime::Mime as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        mime::Mime::from_str(s).map(Self::from)
    }
}

impl PartialEq<mime::Mime> for Mime {
    fn eq(&self, other: &mime::Mime) -> bool {
        self.0 == *other
    }
}

impl PartialEq<str> for Mime {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl Deref for Mime {
    type Target = mime::Mime;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Mime {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Borrow<mime::Mime> for Mime {
    fn borrow(&self) -> &mime::Mime {
        &self.0
    }
}

impl BorrowMut<mime::Mime> for Mime {
    fn borrow_mut(&mut self) -> &mut mime::Mime {
        &mut self.0
    }
}

impl AsRef<mime::Mime> for Mime {
    fn as_ref(&self) -> &mime::Mime {
        &self.0
    }
}

impl AsMut<mime::Mime> for Mime {
    fn as_mut(&mut self) -> &mut mime::Mime {
        &mut self.0
    }
}

impl FromSql for Mime {
    fn column_result(value: ValueRef<'_>) -> std::result::Result<Self, FromSqlError> {
        String::column_result(value)?
            .parse()
            .map_err(FromSqlError::other)
    }
}

impl ToSql for Mime {
    fn to_sql(&self) -> Result<ToSqlOutput<'_>, rusqlite::Error> {
        Ok(self.0.to_string().into())
    }
}

impl AsSqlTy for Mime {
    type Borrowed<'p> = Self;

    const SQL_TY: SqlTy = <String as AsSqlTy>::SQL_TY;
}
