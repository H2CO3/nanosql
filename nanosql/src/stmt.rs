//! A compiled ("prepared") statement.

use core::fmt::{self, Debug, Formatter};
use core::marker::PhantomData;
use std::borrow::{Borrow, Cow};
use serde::{Serialize, Deserialize};
use rusqlite::CachedStatement;
use crate::query::Query;
use crate::param::ParamSerializer;
use crate::row::RowsDeserializer;
use crate::error::Result;


/// A compiled/"prepared" statement.
pub struct CompiledStatement<'conn, Q> {
    key_buf: String,
    statement: CachedStatement<'conn>,
    marker: PhantomData<fn() -> Q>,
}

impl<'conn, Q: Query> CompiledStatement<'conn, Q> {
    /// Constructs a `CompiledStatement` from an untyped SQLite prepared statement.
    pub(crate) fn new(statement: CachedStatement<'conn>) -> Self {
        CompiledStatement {
            key_buf: String::new(),
            statement,
            marker: PhantomData,
        }
    }

    /// Bind parameters, execute the statement, then deserialize the results.
    pub fn invoke<'p, P>(&mut self, params: P) -> Result<Q::Output>
    where
        P: Borrow<Q::Input<'p>>
    {
        // bind parameters
        let mut serializer = ParamSerializer::new(
            &mut self.statement,
            &mut self.key_buf,
            Q::PARAM_PREFIX,
        );
        params.borrow().serialize(&mut serializer)?;
        serializer.finalize()?;

        // execute query
        // (we don't need raw_execute, as our deserializer works correctly for
        // the combination of a return type of () and 0 rows being returned)
        let rows = self.statement.raw_query();

        // extract returned rows
        let deserializer = RowsDeserializer::new(rows);
        let result = Q::Output::deserialize(deserializer)?;

        // clear bindings so that big strings and blobs don't hog the memory
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
