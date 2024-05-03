//! Strongly-typed queries.

use crate::param::Param;
use crate::row::ResultSet;
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
    /// * or a newtype or anything that implements [`Param`] like any of the items above.
    ///
    /// The lifetime parameter allows the implementor to use a type containing
    /// references, so as to avoid allocations when binding strings and blobs.
    type Input<'p>: Param;

    /// The result type returned by the query. This must be either of the following:
    ///
    /// * a scalar (integer, floating-point number, string, blob, or null/unit);
    /// * an ordered tuple (or tuple struct) of scalars;
    /// * a struct with named fields of scalar type;
    /// * a map with string-like keys and scalar values;
    /// * a sequence of any of the items above;
    /// * or a newtype or any other type that deserializes as such (via [`ResultSet`]).
    type Output: ResultSet;

    /// Provides the SQL source text of the query.
    fn sql(&self) -> Result<impl AsRef<str> + '_>;
}

impl<Q> Query for &Q
where
    Q: ?Sized + Query
{
    type Input<'p> = Q::Input<'p>;
    type Output = Q::Output;

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

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        Q::sql(&**self)
    }
}

/// Creates a new `struct` and implements [`Query`] for it using a function-like syntax.
/// The invocation looks like the following:
///
/// ```ignore
/// declare_query!{
///     QueryName<'lt>: InputType<'lt> => OutputType { "SQL (impl AsRef<str>)" }
/// }
/// ```
///
/// The query name may be preceded by a visibility specifier (e.g. `pub`) to control the scope.
///
/// The macro brings the lifetime `'lt` into scope when binding the input type, so
/// you can use it for defining the input type as a reference or reference-like type.
/// The SQL expression may borrow immutably from `self` and may use the `?` operator
/// to return an error when building the SQL query string.
///
/// You can declare multiple queries in the same invocation by repeating the above pattern.
///
/// Example:
///
/// ```rust
/// # use nanosql::{Result, Connection, ConnectionExt, Param, Row, ResultRecord};
/// #[derive(Clone, Copy, Debug, Param)]
/// #[nanosql(param_prefix = '@')]
/// struct YoungEmployeesByNameParams<'n> {
///     name: &'n str,
///     max_age: usize,
/// }
///
/// #[derive(Clone, Default, Debug)]
/// struct Employee {
///     id: u64,
///     name: String,
///     age: usize,
///     boss_id: u64,
/// }
/// #
/// impl ResultRecord for Employee {
///     // uninteresting details hidden
/// #     fn from_row(_: &Row<'_>) -> Result<Self> {
/// #         Ok(Employee::default())
/// #     }
/// }
///
/// nanosql::define_query! {
///     // A simple query that only uses built-in types.
///     pub PetNameById<'p>: i64 => Option<String> {
///         "SELECT name FROM pet WHERE id = ?"
///     }
///
///     // A more involved query that uses the domain types defined above.
///     pub(crate) YoungEmployeesByName<'p>: YoungEmployeesByNameParams<'p> => Vec<Employee> {
/// # /*
///         r#"
///         SELECT id, name, age, boss_id
///         FROM employee
///         WHERE name LIKE @name AND age <= @max_age
///         "#
/// # */
/// #       "VALUES (@name), (@max_age)" // so that the query returns some rows and uses 2 params
///     }
/// }
///
/// fn main() -> Result<()> {
///     let conn = Connection::open_in_memory()?;
///
///     // Compile the query
///     let mut stmt = conn.compile(YoungEmployeesByName)?;
///
///     // Get all employees named Joe under 21
///     let employees: Vec<Employee> = stmt.invoke(YoungEmployeesByNameParams {
///         name: "Jone",
///         max_age: 21,
///     })?;
///
///     // suppose there were 2 of them
///     assert_eq!(employees.len(), 2);
///
///     Ok(())
/// }
/// ```
#[macro_export]
macro_rules! define_query {
    ($(
        $vis:vis $tyname:ident<$lt:lifetime>: $input_ty:ty => $output_ty:ty { $sql:expr }
    )*) => {$(
        #[derive(Clone, Copy, Default, Debug)]
        $vis struct $tyname;

        impl ::nanosql::Query for $tyname {
            type Input<$lt> = $input_ty;
            type Output = $output_ty;

            fn sql(&self) -> ::nanosql::Result<impl ::core::convert::AsRef<::core::primitive::str> + '_> {
                ::nanosql::Result::Ok($sql)
            }
        }
    )*}
}
