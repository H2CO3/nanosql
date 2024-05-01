//! Strongly-typed queries.

use serde::{Serialize, Deserialize};
use crate::param::ParamPrefix;
use crate::error::Result;


/// Describes the input (parameter) and output (relation/row/tuple)
/// types of a query, as well as its actual SQL source text.
pub trait Query {
    /// The parameter type of the query. This must be either of the following:
    ///
    /// * a scalar (integer, floating-point number, string, blob, or null/unit);
    /// * an ordered tuple (or tuple struct) of scalars;
    /// * a struct with named fields of scalar type;
    /// * a map with string-like keys and scalar values;
    /// * or a newtype or anything that serializes like any of the items above.
    ///
    /// The lifetime parameter allows the implementor to use a type containing
    /// references, so as to avoid allocations when binding strings and blobs.
    type Input<'p>: Serialize;

    /// The result type returned by the query. This must be either of the following:
    ///
    /// * a scalar (integer, floating-point number, string, blob, or null/unit);
    /// * an ordered tuple (or tuple struct) of scalars;
    /// * a struct with named fields of scalar type;
    /// * a map with string-like keys and scalar values;
    /// * a sequence of any of the items above;
    /// * or a newtype or any other type that serializes as such.
    type Output: for<'de> Deserialize<'de>;

    /// The leading symbol in parameter names. (Must be consistent across parameters.)
    const PARAM_PREFIX: ParamPrefix = ParamPrefix::Dollar;

    /// Provides the SQL source text of the query.
    fn sql(&self) -> Result<impl AsRef<str> + '_>;
}

impl<Q> Query for &Q
where
    Q: ?Sized + Query
{
    type Input<'p> = Q::Input<'p>;
    type Output = Q::Output;

    const PARAM_PREFIX: ParamPrefix = Q::PARAM_PREFIX;

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        Q::sql(&**self)
    }
}

impl<Q> Query for &mut Q
where
    Q: ?Sized + Query
{
    type Input<'p> = Q::Input<'p>;
    type Output = Q::Output;

    const PARAM_PREFIX: ParamPrefix = Q::PARAM_PREFIX;

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        Q::sql(&**self)
    }
}

impl<Q> Query for Box<Q>
where
    Q: ?Sized + Query
{
    type Input<'p> = Q::Input<'p>;
    type Output = Q::Output;

    const PARAM_PREFIX: ParamPrefix = Q::PARAM_PREFIX;

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        Q::sql(&**self)
    }
}

