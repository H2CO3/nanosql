//! Strongly-typed queries.
//!
//! Consult the official [SQLite documentation](https://www.sqlite.org/lang.html) on the
//! supported queries for an in-depth explanation of the precise SQL understood by SQLite.

use core::fmt::{self, Display, Formatter};
use crate::param::Param;
use crate::row::ResultSet;


/// Describes the input (parameter) and output (relation/row/tuple)
/// types of a query, as well as its actual SQL source text.
///
/// Consult the official [SQLite documentation](https://www.sqlite.org/lang.html) on the
/// supported queries for an in-depth explanation of the precise SQL understood by SQLite.
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
    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result;

    /// Returns a formatter object that displays the SQL text for this query.
    fn display_sql(&self) -> SqlDisplay<&Self> {
        SqlDisplay::new(self)
    }
}

impl<Q> Query for &Q
where
    Q: ?Sized + Query
{
    type Input<'p> = Q::Input<'p>;
    type Output = Q::Output;

    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Q::format_sql(&**self, formatter)
    }
}

impl<Q> Query for &mut Q
where
    Q: ?Sized + Query
{
    type Input<'p> = Q::Input<'p>;
    type Output = Q::Output;

    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Q::format_sql(&**self, formatter)
    }
}

impl<Q> Query for Box<Q>
where
    Q: ?Sized + Query
{
    type Input<'p> = Q::Input<'p>;
    type Output = Q::Output;

    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Q::format_sql(&**self, formatter)
    }
}

/// Formats the SQL source of a query (an adapter between `Query` and `Display`)
#[derive(Clone, Copy, Debug)]
pub struct SqlDisplay<Q: ?Sized> {
    /// The query of which the SQL text is to be displayed.
    pub query: Q,
}

impl<Q> SqlDisplay<Q> {
    /// Creates a new `SqlDisplay`, wrapping a specific query instance.
    pub const fn new(query: Q) -> Self {
        SqlDisplay { query }
    }

    /// Returns ownership of the wrapped query back to the caller.
    pub fn into_inner(self) -> Q {
        self.query
    }
}

impl<Q: Query + ?Sized> Display for SqlDisplay<Q> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.query.format_sql(formatter)
    }
}

/// Creates a new `struct` and implements [`Query`] for it using a function-like syntax.
/// The invocation looks like the following:
///
/// ```ignore
/// define_query!{
///     QueryName<'lt>: InputType<'lt> => OutputType { "SQL (impl Display)" }
/// }
/// ```
///
/// The query name may be preceded by a visibility specifier (e.g. `pub`) to control the scope,
/// just like normal Rust UDT declarations. Likewise, it may also be preceded by `#[attributes]`
/// such as `#[derive(Clone, Copy, Default)]` or documentation comments (which expand to such
/// an attribute). These will all be forwarded to the definition of the query type itsef.
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
/// # use nanosql::{Result, Connection, ConnectionExt, Param, ResultRecord, Table};
/// #[derive(Clone, Copy, Debug, Param)]
/// #[nanosql(param_prefix = '@')]
/// struct YoungEmployeesByNameParams<'n> {
///     name: &'n str,
///     max_age: usize,
/// }
///
/// #[derive(Clone, Default, Debug, Param, ResultRecord, Table)]
/// struct Employee {
///     id: u64,
///     name: String,
///     age: usize,
///     boss_id: u64,
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
///         r#"
///         SELECT id, name, age, boss_id
///         FROM employee
///         WHERE name LIKE @name AND age <= @max_age
///         "#
///     }
/// }
///
/// fn main() -> Result<()> {
///     let mut conn = Connection::connect_in_memory()?;
/// #
/// #   conn.create_table::<Employee>()?;
/// #   conn.insert_batch([
/// #       Employee {
/// #           id: 1,
/// #           name: "Alice".into(),
/// #           age: 18,
/// #           boss_id: 0,
/// #       },
/// #       Employee {
/// #           id: 1,
/// #           name: "Joe".into(),
/// #           age: 19,
/// #           boss_id: 0,
/// #       },
/// #       Employee {
/// #           id: 1,
/// #           name: "Joe".into(),
/// #           age: 20,
/// #           boss_id: 0,
/// #       },
/// #       Employee {
/// #           id: 1,
/// #           name: "Joe".into(),
/// #           age: 22,
/// #           boss_id: 0,
/// #       },
/// #   ])?;
///
///     // Compile the query
///     let mut stmt = conn.compile(YoungEmployeesByName)?;
///
///     // Get all employees named Joe under 21
///     // (details of creating and populating the table have been omitted)
///     let employees: Vec<Employee> = stmt.invoke(YoungEmployeesByNameParams {
///         name: "Joe",
///         max_age: 21,
///     })?;
///
///     // suppose there are 2 of them
///     assert_eq!(employees.len(), 2);
///
///     Ok(())
/// }
/// ```
#[macro_export]
macro_rules! define_query {
    ($(
        $(#[$($attrs:tt)*])*
        $vis:vis $tyname:ident<$lt:lifetime>: $input_ty:ty => $output_ty:ty { $sql:expr }
    )*) => {$(
        $(#[$($attrs)*])*
        $vis struct $tyname;

        impl $crate::Query for $tyname {
            type Input<$lt> = $input_ty;
            type Output = $output_ty;

            fn format_sql(&self, formatter: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                ::core::fmt::Display::fmt(&$sql, formatter)
            }
        }
    )*}
}
