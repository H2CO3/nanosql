//! Deserializing SQL rows into strongly-typed values.

use core::str;
use serde::de::{
    Deserializer,
    DeserializeSeed,
    IntoDeserializer,
    Error as DeError,
    Visitor,
    SeqAccess,
    MapAccess,
    value::UnitDeserializer,
};
use rusqlite::{Row, Rows, types::{FromSql, ValueRef}};
use crate::error::{Error, Result};


pub struct RowsDeserializer<'stmt> {
    rows: Rows<'stmt>,
}

impl<'stmt> RowsDeserializer<'stmt> {
    pub const fn new(rows: Rows<'stmt>) -> Self {
        RowsDeserializer { rows }
    }

    fn deserialize_scalar<T>(mut self) -> Result<T>
    where
        T: FromSql
    {
        if let Some(row) = self.rows.next()? {
            let (value,) = row.try_into()?;

            if self.rows.next()?.is_none() {
                Ok(value)
            } else {
                Err(Error::de("query of top-level scalar result type returned more than 1 row"))
            }
        } else {
            Err(Error::de("query of top-level scalar result type returned no rows"))
        }
    }

    fn visit_str<T, F>(mut self, visitor: F) -> Result<T>
    where
        F: FnOnce(&str) -> Result<T>
    {
        if let Some(row) = self.rows.next()? {
            if row.as_ref().column_count() != 1 {
                return Err(Error::de("query of top-level scalar type returned 0 or >1 columns"));
            }

            let ValueRef::Text(blob) = row.get_ref(0)? else {
                return Err(Error::de("query of top-level str type returned a non-string column"))
            };

            let text = str::from_utf8(blob)?;
            let result = visitor(text)?;

            if self.rows.next()?.is_none() {
                Ok(result)
            } else {
                Err(Error::de("query of top-level scalar result type returned more than 1 row"))
            }
        } else {
            Err(Error::de("query of top-level scalar result type returned no rows"))
        }
    }

    fn visit_blob<T, F>(mut self, visitor: F) -> Result<T>
    where
        F: FnOnce(&[u8]) -> Result<T>
    {
        if let Some(row) = self.rows.next()? {
            if row.as_ref().column_count() != 1 {
                return Err(Error::de("query of top-level scalar type returned 0 or >1 columns"));
            }

            let (ValueRef::Text(blob) | ValueRef::Blob(blob)) = row.get_ref(0)? else {
                return Err(Error::de("query of top-level str type returned a non-string column"))
            };

            let value = visitor(blob)?;

            if self.rows.next()?.is_none() {
                Ok(value)
            } else {
                Err(Error::de("query of top-level scalar result type returned more than 1 row"))
            }
        } else {
            Err(Error::de("query of top-level scalar result type returned no rows"))
        }
    }
}

impl<'de> Deserializer<'de> for RowsDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        // the result set is fundamentally a sequence of tuples, so unless anything
        // more specific is available, default to this very general notion.
        self.deserialize_seq(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_bool(self.deserialize_scalar()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_i8(self.deserialize_scalar()?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_i16(self.deserialize_scalar()?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_i32(self.deserialize_scalar()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_i64(self.deserialize_scalar()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_u8(self.deserialize_scalar()?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_u16(self.deserialize_scalar()?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_u32(self.deserialize_scalar()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_u64(self.deserialize_scalar()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_f32(self.deserialize_scalar()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_f64(self.deserialize_scalar()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_str(|text| {
            let mut chars = text.chars();

            let Some(ch) = chars.next() else {
                return Err(Error::de("query of top-level result type `char` returned an empty string"))
            };

            if chars.next().is_none() {
                visitor.visit_char(ch)
            } else {
                Err(Error::de("query of top-level result type `char` returned a string of >1 chars"))
            }
        })
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_str(|text| visitor.visit_str(text))
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_string(self.deserialize_scalar()?)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_blob(|blob| visitor.visit_bytes(blob))
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_byte_buf(self.deserialize_scalar()?)
    }

    fn deserialize_option<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        if let Some(row) = self.rows.next()? {
            let row_des = TupleDeserializer::new(row);
            let value = visitor.visit_some(row_des)?;

            if self.rows.next()?.is_none() {
                Ok(value)
            } else {
                Err(Error::de("query of top-level result type `Option<_>` returned more than 1 row"))
            }
        } else {
            visitor.visit_none()
        }
    }

    fn deserialize_unit<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        if self.rows.next()?.is_none() {
            visitor.visit_unit()
        } else {
            Err(Error::de("query of top-level result type `()` returned a row"))
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_seq(RowsSeqAccess::new(self.rows))
    }

    fn deserialize_tuple<V>(
        mut self,
        len: usize,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        let Some(row) = self.rows.next()? else {
            return Err(Error::de("query of top-level tuple result type returned no rows"));
        };
        let access = TupleAccess::new_sized(row, len)?;
        let value = visitor.visit_seq(access)?;

        if self.rows.next()?.is_none() {
            Ok(value)
        } else {
            Err(Error::de("query of top-level tuple result type returned >1 rows"))
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        let Some(row) = self.rows.next()? else {
            return Err(Error::de("query of top-level map result type returned no rows"));
        };
        let access = TupleAccess::new_unsized(row);
        let value = visitor.visit_seq(access)?;

        if self.rows.next()?.is_none() {
            Ok(value)
        } else {
            Err(Error::de("query of top-level map result type returned >1 rows"))
        }
    }

    fn deserialize_struct<V>(
        mut self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        let Some(row) = self.rows.next()? else {
            return Err(Error::de("query of top-level struct result type returned no rows"));
        };
        let access = TupleAccess::new_sized(row, fields.len())?;
        let value = visitor.visit_map(access)?;

        if self.rows.next()?.is_none() {
            Ok(value)
        } else {
            Err(Error::de("query of top-level struct result type returned >1 rows"))
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_unit()
    }
}

pub struct RowsSeqAccess<'stmt> {
    rows: Rows<'stmt>,
}

impl<'stmt> RowsSeqAccess<'stmt> {
    fn new(rows: Rows<'stmt>) -> Self {
        RowsSeqAccess { rows }
    }
}

impl<'stmt> SeqAccess<'stmt> for RowsSeqAccess<'stmt> {
    type Error = Error;

    fn next_element_seed<T>(
        &mut self,
        seed: T
    ) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'stmt>
    {
        if let Some(row) = self.rows.next()? {
            let deserializer = TupleDeserializer::new(row);
            seed.deserialize(deserializer).map(Some)
        } else {
            Ok(None)
        }
    }
}

pub struct TupleDeserializer<'stmt> {
    row: &'stmt Row<'stmt>,
}

impl<'stmt> TupleDeserializer<'stmt> {
    fn new(row: &'stmt Row<'stmt>) -> Self {
        TupleDeserializer { row }
    }

    fn visit_scalar<T, V, F>(self, visit: F) -> Result<V>
    where
        T: FromSql,
        F: FnOnce(T) -> Result<V>,
    {
        let (value,) = self.row.try_into()?;
        visit(value)
    }

    fn get_str(&self) -> Result<&'stmt str> {
        if self.row.as_ref().column_count() != 1 {
            return Err(Error::de("query of top-level scalar type returned 0 or >1 columns"));
        }

        let ValueRef::Text(blob) = self.row.get_ref(0)? else {
            return Err(Error::de("query of top-level str type returned a non-string column"))
        };

        let text = str::from_utf8(blob)?;

        Ok(text)
    }

    fn get_blob(&self) -> Result<&'stmt [u8]> {
        if self.row.as_ref().column_count() != 1 {
            return Err(Error::de("query of top-level scalar type returned 0 or >1 columns"));
        }

        let (ValueRef::Text(blob) | ValueRef::Blob(blob)) = self.row.get_ref(0)? else {
            return Err(Error::de("query of top-level str type returned a non-string column"))
        };

        Ok(blob)
    }
}

impl<'de> Deserializer<'de> for TupleDeserializer<'_> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        // the result set is fundamentally a tuple with named fields, so unless anything
        // more specific is available, default to this very general notion.
        self.deserialize_map(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_bool(v))
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_i8(v))
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_i16(v))
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_i32(v))
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_i64(v))
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_u8(v))
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_u16(v))
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_u32(v))
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_u64(v))
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_f32(v))
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_f64(v))
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        let text = self.get_str()?;
        let mut chars = text.chars();

        let Some(ch) = chars.next() else {
            return Err(Error::de("query of top-level result type `char` returned an empty string"))
        };

        if chars.next().is_none() {
            visitor.visit_char(ch)
        } else {
            Err(Error::de("query of top-level result type `char` returned a string of >1 chars"))
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_str(self.get_str()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_string(v))
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_bytes(self.get_blob()?)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.visit_scalar(|v| visitor.visit_byte_buf(v))
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        if self.row.as_ref().column_count() != 1 {
            return Err(Error::de("query of tuple-level result type `Option<_>` returned 0 or >1 columns"));
        }

        match self.row.get_ref(0)? {
            ValueRef::Null => visitor.visit_some(UnitDeserializer::new()),
            ValueRef::Integer(value) => visitor.visit_some(value.into_deserializer()),
            ValueRef::Real(value) => visitor.visit_some(value.into_deserializer()),
            ValueRef::Text(value) => visitor.visit_some(str::from_utf8(value)?.into_deserializer()),
            ValueRef::Blob(value) => visitor.visit_some(value.into_deserializer()),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        if self.row.as_ref().column_count() <= 1 {
            if self.row.as_ref().column_count() == 1 {
                if let ValueRef::Null = self.row.get_ref(0)? {
                    visitor.visit_unit()
                } else {
                    Err(Error::de("query of tuple-level result type `()` returned non-NULL column"))
                }
            } else {
                visitor.visit_unit()
            }
        } else {
            Err(Error::de("query of tuple-level result type `()` returned >1 column"))
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_seq(TupleAccess::new_unsized(self.row))
    }

    fn deserialize_tuple<V>(
        self,
        len: usize,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_seq(TupleAccess::new_sized(self.row, len)?)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_map(TupleAccess::new_unsized(self.row))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_map(TupleAccess::new_sized(self.row, fields.len())?)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        visitor.visit_unit()
    }
}

pub struct TupleAccess<'stmt> {
    row: &'stmt Row<'stmt>,
    len: usize,
    next_index: usize,
}

impl<'stmt> TupleAccess<'stmt> {
    fn new_sized(row: &'stmt Row<'stmt>, expected_len: usize) -> Result<Self> {
        let len = row.as_ref().column_count();

        if len == expected_len {
            Ok(TupleAccess {
                row,
                len,
                next_index: 0,
            })
        } else {
            Err(DeError::custom(format_args!(
                "expected {expected_len}-tuple due to return type, found {len} columns"
            )))
        }
    }

    fn new_unsized(row: &'stmt Row<'stmt>) -> Self {
        let len = row.as_ref().column_count();

        TupleAccess {
            row,
            len,
            next_index: 0,
        }
    }
}

impl<'de> SeqAccess<'de> for TupleAccess<'_> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>
    {
        if self.next_index >= self.len {
            return Ok(None);
        }

        let value = self.row.get_ref(self.next_index)?;
        let result = match value {
            ValueRef::Null => seed.deserialize(UnitDeserializer::new()),
            ValueRef::Integer(value) => seed.deserialize(value.into_deserializer()),
            ValueRef::Real(value) => seed.deserialize(value.into_deserializer()),
            ValueRef::Text(value) => seed.deserialize(str::from_utf8(value)?.into_deserializer()),
            ValueRef::Blob(value) => seed.deserialize(value.into_deserializer()),
        };

        self.next_index += 1;

        result.map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.len - self.next_index)
    }
}

impl<'de> MapAccess<'de> for TupleAccess<'_> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>
    {
        if self.next_index >= self.len {
            return Ok(None);
        }

        let key = self.row.as_ref().column_name(self.next_index)?;

        seed.deserialize(key.into_deserializer()).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>
    {
        let value = self.row.get_ref(self.next_index)?;

        self.next_index += 1;

        match value {
            ValueRef::Null => seed.deserialize(UnitDeserializer::new()),
            ValueRef::Integer(value) => seed.deserialize(value.into_deserializer()),
            ValueRef::Real(value) => seed.deserialize(value.into_deserializer()),
            ValueRef::Text(value) => seed.deserialize(str::from_utf8(value)?.into_deserializer()),
            ValueRef::Blob(value) => seed.deserialize(value.into_deserializer()),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        SeqAccess::size_hint(self)
    }
}
