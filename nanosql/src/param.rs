//! Serializing strongly-typed arguments as bound statement parameters.

use core::num::{
    NonZeroI8,
    NonZeroU8,
    NonZeroI16,
    NonZeroU16,
    NonZeroI32,
    NonZeroU32,
    NonZeroI64,
    NonZeroU64,
    NonZeroIsize,
    NonZeroUsize,
};
use core::fmt::{self, Display, Formatter, Write};
use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;
use std::collections::{HashMap, BTreeMap};
use rusqlite::{Statement, ToSql, types::{Value, ValueRef, Null, ToSqlOutput}};
use crate::error::{Error, Result};


/// A parameter prefix character, preceding the name or index of a bound parameter.
/// One of `$`, `:`, `?`, or `@`.
///
/// The default value is `$`, because it's the most flexible one.
///
/// Variants are in ASCII/Unicode code point order.
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum ParamPrefix {
    #[default]
    Dollar = b'$',
    Colon = b':',
    Question = b'?',
    At = b'@',
}

impl ParamPrefix {
    /// Returns the underlying raw byte.
    pub const fn as_byte(self) -> u8 {
        self as u8
    }

    /// Returns the underlying raw character.
    pub const fn as_char(self) -> char {
        self as u8 as char
    }
}

impl From<ParamPrefix> for u8 {
    fn from(prefix: ParamPrefix) -> Self {
        prefix.as_byte()
    }
}

impl From<ParamPrefix> for char {
    fn from(prefix: ParamPrefix) -> Self {
        prefix.as_char()
    }
}

impl TryFrom<char> for ParamPrefix {
    type Error = Error;

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        match ch {
            '$' => Ok(ParamPrefix::Dollar),
            ':' => Ok(ParamPrefix::Colon),
            '?' => Ok(ParamPrefix::Question),
            '@' => Ok(ParamPrefix::At),
            _   => Err(Error::message(format_args!("invalid parameter prefix: `{ch}`"))),
        }
    }
}

impl TryFrom<u8> for ParamPrefix {
    type Error = Error;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        char::from(byte).try_into()
    }
}

impl Display for ParamPrefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char(self.as_char())
    }
}

/// Describes types that can be bound as parameters to a compiled statement.
///
/// The kinds of types implementing this trait include:
///
/// * Primitives (numeric types, strings, blobs, etc.)
/// * Optionals of primitives
/// * Tuples or structs of any of the above
/// * Singleton/forwarding wrappers of any of the above, e.g. `&T` and `Box`
pub trait Param {
    /// The leading symbol in parameter names. (Must be consistent across parameters.)
    const PARAM_PREFIX: ParamPrefix;

    /// Binds the primitive or the field(s) of a tuple to a raw `rusqlite::Statement`.
    fn bind(&self, statement: &mut Statement<'_>) -> Result<()>;
}

/// Private helper for ensuring that exactly 1 parameter is expected when binding a primitive
/// as a top-level parameter "list".
fn bind_primitive<T: ToSql>(statement: &mut Statement<'_>, value: T) -> Result<()> {
    let expected = statement.parameter_count();
    let actual = 1;

    if actual == expected {
        statement.raw_bind_parameter(1, value)?;
        Ok(())
    } else {
        Err(Error::ParamCountMismatch { expected, actual })
    }
}

macro_rules! impl_param_for_primitive {
    ($($ty:ty,)*) => {$(
        impl Param for $ty {
            /// Primitives are bound as positional parameters, hence the prefix is '?'
            const PARAM_PREFIX: ParamPrefix = ParamPrefix::Question;

            fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
                bind_primitive(statement, self)
            }
        }
    )*}
}

impl_param_for_primitive!{
    bool,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    isize,
    usize,
    NonZeroI8,
    NonZeroU8,
    NonZeroI16,
    NonZeroU16,
    NonZeroI32,
    NonZeroU32,
    NonZeroI64,
    NonZeroU64,
    NonZeroIsize,
    NonZeroUsize,
    f32,
    f64,
    str,
    [u8],
    String,
    Vec<u8>,
    Value,
    ToSqlOutput<'_>,
    Null,
}

impl Param for ValueRef<'_> {
    /// Primitives are bound as positional parameters, hence the prefix is '?'
    const PARAM_PREFIX: ParamPrefix = ParamPrefix::Question;

    fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
        bind_primitive(statement, ToSqlOutput::Borrowed(*self))
    }
}

impl<const N: usize> Param for [u8; N] {
    /// Primitives are bound as positional parameters, hence the prefix is '?'
    const PARAM_PREFIX: ParamPrefix = ParamPrefix::Question;

    fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
        bind_primitive(statement, self)
    }
}

macro_rules! impl_param_for_tuple {
    () => {
        impl Param for () {
            /// Tuples use positional parameters, hence the prefix is '?'
            const PARAM_PREFIX: ParamPrefix = ParamPrefix::Question;

            fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
                let expected = statement.parameter_count();
                let actual = 0;

                if actual == expected {
                    Ok(())
                } else {
                    Err(Error::ParamCountMismatch { expected, actual })
                }
            }
        }
    };
    ($head_id:ident => $head_ty:ident; $($rest_id:ident => $rest_ty:ident;)*) => {
        impl<$head_ty, $($rest_ty,)*> Param for ($head_ty, $($rest_ty,)*)
        where
            $head_ty: ToSql,
            $($rest_ty: ToSql,)*
        {
            /// Tuples use positional parameters, hence the prefix is '?'
            const PARAM_PREFIX: ParamPrefix = ParamPrefix::Question;

            fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
                let ($head_id, $($rest_id,)*) = self;

                #[allow(unused_mut)]
                let mut index = 1;
                statement.raw_bind_parameter(index, $head_id)?;

                $(
                    index += 1;
                    statement.raw_bind_parameter(index, $rest_id)?;
                )*

                let expected = statement.parameter_count();
                let actual = index;

                if actual == expected {
                    Ok(())
                } else {
                    Err(Error::ParamCountMismatch { expected, actual })
                }
            }
        }
        impl_param_for_tuple!($($rest_id => $rest_ty;)*);
    };
}

impl_param_for_tuple!{
    a => A;
    b => B;
    c => C;
    d => D;
    e => E;
    f => F;
    g => G;
    h => H;
    i => I;
    j => J;
    k => K;
    l => L;
    m => M;
    n => N;
    o => O;
    p => P;
    q => Q;
    r => R;
    s => S;
    t => T;
    u => U;
    v => V;
    w => W;
    x => X;
    y => Y;
    z => Z;
}

macro_rules! impl_param_for_wrapper {
    ($($ty:ty;)*) => {$(
        impl<T: ?Sized + Param> Param for $ty {
            const PARAM_PREFIX: ParamPrefix = T::PARAM_PREFIX;

            fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
                let body = |value: &$ty, statement| Param::bind(&**value, statement);
                body(self, statement)
            }
        }
    )*}
}

impl_param_for_wrapper! {
    &T;
    &mut T;
    Box<T>;
    Rc<T>;
    Arc<T>;
}

impl<T> Param for Cow<'_, T>
where
    T: ?Sized + ToOwned + Param,
{
    const PARAM_PREFIX: ParamPrefix = T::PARAM_PREFIX;

    fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
        Param::bind(&**self, statement)
    }
}

impl<T: ToSql> Param for Option<T> {
    /// Primitives are bound as positional parameters, hence the prefix is '?'
    const PARAM_PREFIX: ParamPrefix = ParamPrefix::Question;

    fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
        bind_primitive(statement, self)
    }
}

impl<K, V> Param for HashMap<K, V>
where
    K: Display,
    V: ToSql,
{
    /// Dynamic maps use `$` by default because it's the most flexible prefix.
    const PARAM_PREFIX: ParamPrefix = ParamPrefix::Dollar;

    fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
        let expected = statement.parameter_count();
        let actual = self.len();

        if actual != expected {
            return Err(Error::ParamCountMismatch { expected, actual });
        }

        // re-use parameter name construction buffer in order to save allocations
        let mut name_buf = String::new();

        for (key, value) in self {
            name_buf.clear();
            write!(name_buf, "{}{}", Self::PARAM_PREFIX, key)?;

            let index = statement.parameter_index(name_buf.as_str())?.ok_or_else(|| {
                Error::unknown_param_dyn(&name_buf)
            })?;

            statement.raw_bind_parameter(index, value)?;
        }

        Ok(())
    }
}

impl<K, V> Param for BTreeMap<K, V>
where
    K: Display,
    V: ToSql,
{
    /// Dynamic maps use `$` by default because it's the most flexible prefix.
    const PARAM_PREFIX: ParamPrefix = ParamPrefix::Dollar;

    fn bind(&self, statement: &mut Statement<'_>) -> Result<()> {
        let expected = statement.parameter_count();
        let actual = self.len();

        if actual != expected {
            return Err(Error::ParamCountMismatch { expected, actual });
        }

        // re-use parameter name construction buffer in order to save allocations
        let mut name_buf = String::new();

        for (key, value) in self {
            name_buf.clear();
            write!(name_buf, "{}{}", Self::PARAM_PREFIX, key)?;

            let index = statement.parameter_index(name_buf.as_str())?.ok_or_else(|| {
                Error::unknown_param_dyn(&name_buf)
            })?;

            statement.raw_bind_parameter(index, value)?;
        }

        Ok(())
    }
}
