//! A compiled ("prepared") statement.

use core::fmt::{self, Debug, Formatter};
use core::marker::PhantomData;
use std::borrow::{Borrow, Cow};
use rusqlite::CachedStatement;
use crate::query::Query;
use crate::param::Param;
use crate::row::ResultSet;
use crate::error::Result;


/// A compiled/"prepared" statement.
pub struct CompiledStatement<'conn, Q> {
    statement: CachedStatement<'conn>,
    sql: String,
    marker: PhantomData<fn() -> Q>,
}

impl<'conn, Q: Query> CompiledStatement<'conn, Q> {
    /// Constructs a `CompiledStatement` from an untyped SQLite prepared statement.
    pub(crate) fn new(statement: CachedStatement<'conn>, sql: String) -> Self {
        CompiledStatement {
            statement,
            sql,
            marker: PhantomData,
        }
    }

    /// Bind parameters, execute the statement, then deserialize the results.
    pub fn invoke<'p, P>(&mut self, params: P) -> Result<Q::Output>
    where
        P: Borrow<Q::Input<'p>>
    {
        // Bind parameters.
        // Clear bindings upfront so that params from the previous invocation
        // don't stick around accidentally.
        //
        // Oddly enough, an `EXPLAIN (QUERY PLAN)` statement retains all of
        // the parameters of the child statement being explained. Since there's
        // nothing to bind to the parameters when explaining, the parameter type
        // of `Explain` is `()`, but that upsets the `Param` impls, leading to a
        // `ParamCountMismatch` error. Therefore we only actually bind parameters
        // when this query is is _not_ an EXPLAIN or EXPLAIN QUERY PLAN statement.
        if self.statement.is_explain() == 0 {
            self.statement.clear_bindings();
            params.borrow().bind(&mut self.statement)?;
        }

        // Execute query and extract returned rows.
        // (we don't need raw_execute, as deserialization works correctly for
        // the combination of a return type of () and 0 rows being returned)
        let rows = self.statement.raw_query();
        let result = Q::Output::from_rows(rows)?;

        // Clear bindings again, so that big strings and blobs don't hog the memory.
        self.statement.clear_bindings();

        Ok(result)
    }

    /// Returns the SQL source underlying this compiled statement.
    pub fn sql(&self) -> &str {
        self.sql.as_str()
    }
}

impl<Q> Debug for CompiledStatement<'_, Q> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let n_params = self.statement.parameter_count();
        let param_names: Vec<_> = (1..=n_params)
            .map(|i| {
                match self.statement.parameter_name(i) {
                    Some(name) => Cow::Borrowed(name),
                    None => Cow::Owned(format!("?{i}")),
                }
            })
            .collect();

        let sql = if formatter.alternate() { self.sql.as_str() } else { "..." };

        formatter
            .debug_struct("CompiledStatement")
            .field("columns", &self.statement.column_names())
            .field("params", &param_names)
            .field("sql", &format_args!("{sql}")) // the SQL shouldn't be escaped
            .finish()
    }
}
