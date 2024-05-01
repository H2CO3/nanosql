//! Serializing strongly-typed arguments as bound statement parameters.

use core::mem;
use core::num::NonZeroUsize;
use core::fmt::{self, Display, Formatter, Write};
use serde::ser::{
    Serialize,
    Serializer,
    SerializeSeq,
    SerializeTuple,
    SerializeTupleStruct,
    SerializeTupleVariant,
    SerializeMap,
    SerializeStruct,
    SerializeStructVariant,
};
use rusqlite::{Statement, ToSql};
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
    /// Returns the underlying raw character.
    pub const fn as_char(self) -> char {
        self as u8 as char
    }
}

impl From<ParamPrefix> for char {
    fn from(prefix: ParamPrefix) -> Self {
        prefix.as_char()
    }
}

impl Display for ParamPrefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char(self.as_char())
    }
}

/// A serializer that binds named (struct) or ordered (tuple) parameters
/// to a prepared statement.
#[derive(Debug)]
pub struct ParamSerializer<'a, 'b> {
    next_param_index: Option<NonZeroUsize>,
    bound_param_count: usize,
    statement: &'a mut Statement<'b>,
    key_buf: &'a mut String,
    param_prefix: ParamPrefix,
}

impl<'a, 'b> ParamSerializer<'a, 'b> {
    /// Before returning, this clears all bindings on the `statement`,
    /// so that exactly those parameters that are bound as a result of
    /// the serialization (i.e., those specified by the current invocation)
    /// are bound, and leftover parameters don't stick around.
    pub fn new(
        statement: &'a mut Statement<'b>,
        key_buf: &'a mut String,
        param_prefix: ParamPrefix,
    ) -> Self {
        statement.clear_bindings();

        ParamSerializer {
            next_param_index: None,
            bound_param_count: 0,
            statement,
            key_buf,
            param_prefix,
        }
    }

    /// Assigns the value to the parameter at the current index, then advances the index.
    /// Appropriate for both indexed *and* named parameters. (Indexed parameters are
    /// bound simply in order; named parameters will always have their index re-set before
    /// this function is called, so the incremented index will never actually be observed.)
    fn bind<T: ToSql>(&mut self, value: T) -> Result<()> {
        // the `unwrap_or(...)` part makes this work for scalar parameters
        let index = self.next_param_index.unwrap_or(NonZeroUsize::MIN);
        self.statement.raw_bind_parameter(index.get(), value)?;
        self.next_param_index = index.checked_add(1); // advance index
        self.bound_param_count += 1; // only increment bound count if bind succeeded
        Ok(())
    }

    /// Ensures that exactly as many parameters were bound as expected by the statement.
    pub fn finalize(self) -> Result<()> {
        let total_param_count = self.statement.parameter_count();

        if self.bound_param_count == total_param_count {
            Ok(())
        } else {
            Err(Error::ParamCountMismatch {
                expected: total_param_count,
                actual: self.bound_param_count,
            })
        }
    }
}

impl<'a, 'b, 'c> Serializer for &'c mut ParamSerializer<'a, 'b> {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }


    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(v.into())
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_char(self, c: char) -> Result<Self::Ok, Self::Error> {
        let mut buf = [0; 4];
        let s = c.encode_utf8(&mut buf);
        self.serialize_str(s)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.bind(v)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.bind(rusqlite::types::Null)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_none()
    }

    fn serialize_unit_struct(self, _name: &str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize
    {
        Err(Error::EnumWithValue)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        if self.next_param_index.replace(NonZeroUsize::MIN).is_none() {
            Ok(self)
        } else {
            Err(Error::NestedParam)
        }
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
       self,
       _name: &'static str,
       _variant_index: u32,
       _variant: &'static str,
       _len: usize
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::EnumWithValue)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        if self.next_param_index.is_none() {
            Ok(self)
        } else {
            Err(Error::NestedParam)
        }
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::EnumWithValue)
    }
}

impl SerializeSeq for &mut ParamSerializer<'_, '_> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl SerializeTuple for &mut ParamSerializer<'_, '_> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        SerializeSeq::end(self)
    }
}

impl SerializeTupleStruct for &mut ParamSerializer<'_, '_> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        SerializeSeq::end(self)
    }
}

impl SerializeStruct for &mut ParamSerializer<'_, '_> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(
        &mut self,
        key: &'static str,
        value: &T
    ) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        self.key_buf.clear();
        write!(self.key_buf, "{}{}", self.param_prefix, key)?;
        let index = self.statement.parameter_index(self.key_buf.as_str())?;
        let index = index.ok_or_else(|| Error::UnknownParam(mem::take(self.key_buf)))?;
        self.next_param_index = NonZeroUsize::new(index);
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl SerializeMap for &mut ParamSerializer<'_, '_> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        struct KeyStringifier<T> {
            prefix: ParamPrefix,
            key: T,
        }

        impl<T: Serialize> Display for KeyStringifier<T> {
            fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
                self.prefix.fmt(formatter)?;
                self.key.serialize(formatter)
            }
        }

        // re-use key buffer of query
        let key_stringifier = KeyStringifier {
            prefix: self.param_prefix,
            key,
        };
        self.key_buf.clear();
        write!(self.key_buf, "{}", key_stringifier).map_err(|_| Error::ParamNameNotString)?;

        let index = self.statement.parameter_index(self.key_buf.as_str())?;
        let index = index.ok_or_else(|| Error::UnknownParam(mem::take(self.key_buf)))?;

        self.next_param_index = NonZeroUsize::new(index);

        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl SerializeTupleVariant for &mut ParamSerializer<'_, '_> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, _value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        Err(Error::EnumWithValue)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::EnumWithValue)
    }
}

impl SerializeStructVariant for &mut ParamSerializer<'_, '_> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(
        &mut self,
        _key: &'static str,
        _value: &T
    ) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize
    {
        Err(Error::EnumWithValue)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::EnumWithValue)
    }
}
