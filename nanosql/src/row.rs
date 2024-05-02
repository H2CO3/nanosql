//! Deserializing SQL rows into strongly-typed values.

use core::str;
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
use core::hash::Hash;
use core::ops::{Deref, DerefMut};
use core::borrow::{Borrow, BorrowMut};
use std::rc::Rc;
use std::sync::Arc;
use std::collections::{VecDeque, BinaryHeap, HashSet, HashMap, BTreeSet, BTreeMap};
use rusqlite::{Statement, Row, Rows, types::{Value, ToSqlOutput, FromSql}};
use crate::error::{Error, Result, RowCount};


/// This trait describes types that deserialize from a relation (set of rows),
/// as returned by a compiled statement.
pub trait ResultSet: Sized {
    /// Convert a dynamically-typed relation to a statically-typed collection.
    fn from_rows(rows: Rows<'_>) -> Result<Self>;
}

impl ResultSet for () {
    fn from_rows(mut rows: Rows<'_>) -> Result<Self> {
        if rows.next()?.is_none() {
            Ok(())
        } else {
            Err(Error::RowCountMismatch {
                expected: RowCount::exactly(0),
                actual: RowCount::at_least(1),
            })
        }
    }
}

/// A convenience wrapper for expecting exactly one record to be returned from a query.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Single<T>(pub T);

impl<T> Single<T> {
    /// Extracts the wrapped value.
    pub fn into_inner(self) -> T {
        let Single(value) = self;
        value
    }
}

impl<T> AsRef<T> for Single<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Single<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> From<T> for Single<T> {
    fn from(value: T) -> Self {
        Single(value)
    }
}

impl<T> Deref for Single<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Single<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Borrow<T> for Single<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T> BorrowMut<T> for Single<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T: ResultRecord> ResultSet for Single<T> {
    fn from_rows(mut rows: Rows<'_>) -> Result<Self> {
        let row = rows.next()?.ok_or(Error::RowCountMismatch {
            expected: RowCount::exactly(1),
            actual: RowCount::exactly(0),
        })?;
        let value = T::from_row(row)?;

        if rows.next()?.is_none() {
            Ok(Single(value))
        } else {
            Err(Error::RowCountMismatch {
                expected: RowCount::exactly(1),
                actual: RowCount::at_least(2),
            })
        }
    }
}

impl<T: ResultRecord> ResultSet for Option<T> {
    fn from_rows(mut rows: Rows<'_>) -> Result<Self> {
        let Some(row) = rows.next()? else {
            return Ok(None);
        };

        let value = T::from_row(row)?;

        if rows.next()?.is_none() {
            Ok(Some(value))
        } else {
            Err(Error::RowCountMismatch {
                expected: RowCount::at_most(1),
                actual: RowCount::at_least(2),
            })
        }
    }
}

impl<const N: usize, T: ResultRecord> ResultSet for [T; N] {
    fn from_rows(mut rows: Rows<'_>) -> Result<Self> {
        // Instead of relying on `Vec::try_into::<[T; N]>()`,
        // we spare a dynamic allocation by building the results
        // in-place, on a stack-allocated array.
        let mut tmp = [(); N].map(|_| None);

        for (i, item) in tmp.iter_mut().enumerate() {
            let row = rows.next()?.ok_or(Error::RowCountMismatch {
                expected: RowCount::exactly(N),
                actual: RowCount::exactly(i),
            })?;

            *item = T::from_row(row)?.into();
        }

        if rows.next()?.is_none() {
            Ok(tmp.map(Option::unwrap))
        } else {
            Err(Error::RowCountMismatch {
                expected: RowCount::exactly(N),
                actual: RowCount::at_least(N + 1),
            })
        }
    }
}

impl<T: ResultRecord> ResultSet for Vec<T> {
    fn from_rows(mut rows: Rows<'_>) -> Result<Self> {
        let mut result_set = Vec::new();

        while let Some(row) = rows.next()? {
            result_set.push(T::from_row(row)?);
        }

        Ok(result_set)
    }
}

impl<T: ResultRecord> ResultSet for Box<[T]> {
    fn from_rows(rows: Rows<'_>) -> Result<Self> {
        Vec::from_rows(rows).map(Self::from)
    }
}

impl<T: ResultRecord> ResultSet for Rc<[T]> {
    fn from_rows(rows: Rows<'_>) -> Result<Self> {
        Vec::from_rows(rows).map(Self::from)
    }
}

impl<T: ResultRecord> ResultSet for Arc<[T]> {
    fn from_rows(rows: Rows<'_>) -> Result<Self> {
        Vec::from_rows(rows).map(Self::from)
    }
}

impl<T: ResultRecord> ResultSet for VecDeque<T> {
    fn from_rows(rows: Rows<'_>) -> Result<Self> {
        Vec::from_rows(rows).map(Self::from)
    }
}

impl<T> ResultSet for BinaryHeap<T>
where
    T: Ord + ResultRecord
{
    fn from_rows(rows: Rows<'_>) -> Result<Self> {
        Vec::from_rows(rows).map(Self::from)
    }
}

impl<T> ResultSet for HashSet<T>
where
    T: Eq + Hash + ResultRecord
{
    fn from_rows(mut rows: Rows<'_>) -> Result<Self> {
        let mut result_set = HashSet::new();

        while let Some(row) = rows.next()? {
            result_set.insert(T::from_row(row)?);
        }

        Ok(result_set)
    }
}

impl<T> ResultSet for BTreeSet<T>
where
    T: Ord + ResultRecord
{
    fn from_rows(mut rows: Rows<'_>) -> Result<Self> {
        let mut result_set = BTreeSet::new();

        while let Some(row) = rows.next()? {
            result_set.insert(T::from_row(row)?);
        }

        Ok(result_set)
    }
}

/// This trait describes types that deserialize from a single row (tuple).
pub trait ResultRecord: Sized {
    /// Convert a dynamically-typed relational tuple to a statically-typed record.
    fn from_row(row: &Row<'_>) -> Result<Self>;
}


/// Private helper for ensuring that exactly 1 column is found when
/// building a strongly-typed tuple from a dynamically-typed `Row`.
fn primitive_from_sql<T: FromSql>(row: &Row<'_>) -> Result<T> {
    let expected = 1;
    let actual = row.as_ref().column_count();

    if actual == expected {
        Ok(row.get(0)?)
    } else {
        Err(Error::ColumnCountMismatch { expected, actual })
    }
}

macro_rules! impl_result_record_for_primitive {
    ($($ty:ty,)*) => {$(
        impl ResultRecord for $ty {
            fn from_row(row: &Row<'_>) -> Result<Self> {
                primitive_from_sql(row)
            }
        }
    )*}
}

impl_result_record_for_primitive!{
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
    Box<str>,
    String,
    Vec<u8>,
}

macro_rules! impl_result_record_for_tuple {
    () => {
        impl ResultRecord for () {
            fn from_row(row: &Row<'_>) -> Result<Self> {
                let expected = 0;
                let actual = row.as_ref().column_count();

                if actual == expected {
                    Ok(())
                } else {
                    Err(Error::ColumnCountMismatch { expected, actual })
                }
            }
        }
    };
    ($head_id:ident => $head_ty:ident; $($rest_id:ident => $rest_ty:ident;)*) => {
        impl<$head_ty, $($rest_ty,)*> ResultRecord for ($head_ty, $($rest_ty,)*)
        where
            $head_ty: FromSql,
            $($rest_ty: FromSql,)*
        {
            fn from_row(row: &Row<'_>) -> Result<Self> {
                let mut index = 0;

                let $head_id = row.get(index)?;
                index += 1;

                $(
                    let $rest_id = row.get(index)?;
                    index += 1;
                )*

                let expected = index;
                let actual = row.as_ref().column_count();

                if actual == expected {
                    Ok(($head_id, $($rest_id,)*))
                } else {
                    Err(Error::ColumnCountMismatch { expected, actual })
                }
            }
        }
        impl_result_record_for_tuple!($($rest_id => $rest_ty;)*);
    };
}

impl_result_record_for_tuple!{
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

impl<T: FromSql> ResultRecord for Option<T> {
    fn from_row(row: &Row<'_>) -> Result<Self> {
        primitive_from_sql(row)
    }
}

impl<const N: usize> ResultRecord for [u8; N] {
    fn from_row(row: &Row<'_>) -> Result<Self> {
        primitive_from_sql(row)
    }
}

impl ResultRecord for Box<[u8]> {
    fn from_row(row: &Row<'_>) -> Result<Self> {
        primitive_from_sql(row).map(Vec::into_boxed_slice)
    }
}

impl ResultRecord for Value {
    fn from_row(row: &Row<'_>) -> Result<Self> {
        primitive_from_sql(row)
    }
}

impl ResultRecord for ToSqlOutput<'_> {
    fn from_row(row: &Row<'_>) -> Result<Self> {
        primitive_from_sql(row).map(ToSqlOutput::Owned)
    }
}

impl<K, V> ResultRecord for HashMap<K, V>
where
    K: Eq + Hash + for<'a> From<&'a str>,
    V: FromSql,
{
    fn from_row(row: &Row<'_>) -> Result<Self> {
        let stmt: &Statement<'_> = row.as_ref();
        let mut map = HashMap::new();

        for i in 0..stmt.column_count() {
            let name = stmt.column_name(i)?;
            let key = name.into();
            let value = row.get(i)?;

            map.insert(key, value);
        }

        Ok(map)
    }
}

impl<K, V> ResultRecord for BTreeMap<K, V>
where
    K: Ord + for<'a> From<&'a str>,
    V: FromSql,
{
    fn from_row(row: &Row<'_>) -> Result<Self> {
        let stmt: &Statement<'_> = row.as_ref();
        let mut map = BTreeMap::new();

        for i in 0..stmt.column_count() {
            let name = stmt.column_name(i)?;
            let key = name.into();
            let value = row.get(i)?;

            map.insert(key, value);
        }

        Ok(map)
    }
}
