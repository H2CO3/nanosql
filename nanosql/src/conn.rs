//! Working with top-level SQLite connections.

use core::borrow::Borrow;
use std::path::Path;
use rusqlite::{Connection, OpenFlags, Transaction, TransactionBehavior};
use crate::{
    query::Query,
    table::{Table, InsertInput, CreateTable, Insert},
    stmt::CompiledStatement,
    explain::{ExplainVdbeProgram, VdbeInstruction, ExplainQueryPlan, QueryPlan},
    error::Result,
    util::Sealed,
};


/// This extension trait defines methods on [`Connection`] that help you create
/// and manage strongly-typed prepared statements.
#[allow(private_bounds)]
pub trait ConnectionExt: Sealed {
    /// Compiles a [`Query`] into a "prepared" [`CompiledStatement`].
    fn compile<Q: Query>(&self, query: Q) -> Result<CompiledStatement<'_, Q>>;

    /// Opens a connection at the specified path, with the
    /// default flags, then applies the recommended settings.
    fn connect<P>(path: P) -> Result<Connection>
    where
        P: AsRef<Path>,
    {
        Self::connect_with_flags(path, OpenFlags::default())
    }

    /// Opens a connection to a transient in-memory database using
    /// the default flags, then applies the recommended settings.
    fn connect_in_memory() -> Result<Connection> {
        Self::connect_in_memory_with_flags(OpenFlags::default())
    }

    /// Opens a connection at the specified path with the
    /// specified flags, then applies the recommended settings.
    fn connect_with_flags<P>(path: P, flags: OpenFlags) -> Result<Connection>
    where
        P: AsRef<Path>,
    {
        let mut connection = Connection::open_with_flags(path, flags)?;
        connection.apply_recommended_settings()?;
        Ok(connection)
    }

    /// Opens a connection to a transient in-memory database using
    /// the default flags, then applies the recommended settings.
    fn connect_in_memory_with_flags(flags: OpenFlags) -> Result<Connection> {
        let mut connection = Connection::open_in_memory_with_flags(flags)?;
        connection.apply_recommended_settings()?;
        Ok(connection)
    }

    /// Applies the recommended settings to this connection.
    /// Currently, these include:
    ///
    /// * enabling foreign key integrity checks
    /// * enabling WAL mode and setting `synchronous = normal`
    /// * increasing the prepared statement cache size,
    ///   assuming a moderate number of tables and queries
    /// * running `PRAGMA optimize` in the way recommended by the SQLite docs
    ///
    /// The set of recommended settings is not considered a part of semver.
    ///
    /// This takes an exclusive `&mut self` reference so as to make the
    /// settings apply atomically.
    fn apply_recommended_settings(&mut self) -> Result<()>;

    /// Compiles a query and immediately invokes it. **This is a convenience
    /// function that should not in general be used when a query is to be
    /// executed many times.** Use [`TransactionExt::invoke_batch()`] instead for
    /// immediately invoking a query in a loop, or [`ConnectionExt::compile()`]
    /// followed by [`CompiledStatement::invoke()`] for invoking a statement
    /// potentially many times but outside a lexical loop (e.g., in a callback
    /// or an HTTP API handler).
    fn compile_invoke<'p, Q, P>(&self, query: Q, params: P) -> Result<Q::Output>
    where
        Q: Query,
        P: Borrow<Q::Input<'p>>,
    {
        self.compile(query)?.invoke(params)
    }

    /// Creates the table represented by `T` if it does not yet exist.
    ///
    /// Also creates any necessary indexes (e.g. for foreign keys).
    ///
    /// Runs `PRAGMA optimize` in the default (limited-resource) mode
    /// after creating the table and indexes, to help query planning.
    fn create_table<T: Table>(&mut self) -> Result<()>;

    /// Convenience method for inserting many rows into a table in one go.
    ///
    /// It opens a single transaction for all of the insert statements, which
    /// prevents others from observing the data in a partially-inserted state.
    ///
    /// It prepares an `INSERT` statement and calls it in a loop, so it's more
    /// efficient than re-preparing and executing the same statement in a loop.
    fn insert_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>;

    /// Explains the VDBE bytecode program generated for a query/statement.
    fn explain_vdbe_program<Q: Query>(&self, query: Q) -> Result<Vec<VdbeInstruction>> {
        self.compile_invoke(ExplainVdbeProgram::from(query), ())
    }

    /// Explains the high-level query plan generated for a query/statement.
    fn explain_query_plan<Q: Query>(&self, query: Q) -> Result<QueryPlan> {
        self.compile_invoke(ExplainQueryPlan::from(query), ())
    }
}

impl ConnectionExt for Connection {
    fn compile<Q: Query>(&self, query: Q) -> Result<CompiledStatement<'_, Q>> {
        let sql = query.display_sql().to_string();
        let statement = self.prepare_cached(&sql)?;

        Ok(CompiledStatement::new(statement, sql))
    }

    fn create_table<T: Table>(&mut self) -> Result<()> {
        let txn = self.transaction_with_behavior(TransactionBehavior::Immediate)?;

        // First, create the table itself.
        txn.compile_invoke(CreateTable::<T>::default(), ())?;

        // Then, create indexes: table-level and field-level, explicit and implicit
        T::description()
            .index_specs()
            .iter()
            .try_for_each(|spec| txn.compile_invoke(spec, ()))?;

        // re-analyze the schema if necessary, but do not try _too_ hard
        txn.pragma_update(None, "optimize", 0xfffe)?;

        // commit changes
        txn.commit()?;

        Ok(())
    }

    fn apply_recommended_settings(&mut self) -> Result<()> {
        self.pragma_update(None, "journal_mode", "WAL")?;
        self.pragma_update(None, "synchronous", "NORMAL")?;
        self.pragma_update(None, "foreign_keys", 1)?;
        self.pragma_update(None, "optimize", 0x10002)?;
        self.set_prepared_statement_cache_capacity(1024);

        Ok(())
    }

    fn insert_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>,
    {
        let txn = self.transaction_with_behavior(TransactionBehavior::Immediate)?;
        txn.insert_batch(entities)?;
        txn.commit()?;
        Ok(())
    }
}

/// This extension trait defines convenience methods on [`Transaction`].
#[allow(private_bounds)]
pub trait TransactionExt: Sealed {
    /// Convenience method for inserting many rows into a table in one go.
    ///
    /// This is an escape hatch for when you can't borrow the [`Connection`]
    /// mutably. It is recommended to use [`ConnectionExt::insert_batch()`] by
    /// default, unless you can't provide unique access to the [`Connection`].
    fn insert_batch<'p, I>(&self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>;

    /// Convenience method for invoking a query many times in one go, correctly
    /// (without the DB being modified across iterations), and efficiently
    /// (without re-compiling the query object and within a single transaction).
    fn invoke_batch<'p, Q, I>(
        &self,
        query: Q,
        param_iter: I,
    ) -> Result<impl Iterator<Item = Result<Q::Output>>>
    where
        Q: Query,
        I: IntoIterator,
        I::Item: Borrow<Q::Input<'p>>;
}

impl TransactionExt for Transaction<'_> {
    fn insert_batch<'p, I>(&self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>,
    {
        let query = Insert::<<I::Item as InsertInput<'p>>::Table>::default();
        self.invoke_batch(query, entities)?.collect()
    }

    fn invoke_batch<'p, Q, I>(
        &self,
        query: Q,
        param_iter: I,
    ) -> Result<impl Iterator<Item = Result<Q::Output>>>
    where
        Q: Query,
        I: IntoIterator,
        I::Item: Borrow<Q::Input<'p>>,
    {
        let mut stmt = self.compile(query)?;

        let iter = param_iter
            .into_iter()
            .map(move |params| stmt.invoke(params));

        Ok(iter)
    }
}

impl Sealed for Connection {}
impl Sealed for Transaction<'_> {}
