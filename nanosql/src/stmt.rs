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
    marker: PhantomData<fn() -> Q>,
}

impl<'conn, Q: Query> CompiledStatement<'conn, Q> {
    /// Constructs a `CompiledStatement` from an untyped SQLite prepared statement.
    pub(crate) fn new(statement: CachedStatement<'conn>) -> Self {
        CompiledStatement {
            statement,
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
        self.statement.clear_bindings();
        params.borrow().bind(&mut self.statement)?;

        // Execute query and extract returned rows.
        // (we don't need raw_execute, as deserialization works correctly for
        // the combination of a return type of () and 0 rows being returned)
        let rows = self.statement.raw_query();
        let result = Q::Output::from_rows(rows)?;

        // Clear bindings again, so that big strings and blobs don't hog the memory.
        self.statement.clear_bindings();

        Ok(result)
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

        let sql = if formatter.alternate() {
            self.statement.expanded_sql().map_or(Cow::Borrowed("<unavailable>"), Cow::Owned)
        } else {
            Cow::Borrowed("...")
        };

        formatter
            .debug_struct("CompiledStatement")
            .field("columns", &self.statement.column_names())
            .field("params", &param_names)
            .field("sql", &sql)
            .finish()
    }
}
