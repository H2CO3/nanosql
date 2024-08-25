//! Working with top-level SQLite connections.

use core::borrow::Borrow;
use std::path::Path;
use rusqlite::{Connection, OpenFlags, Transaction, TransactionBehavior};
use crate::{
    query::Query,
    table::{Table, InsertInput, CreateTable, Select, Insert},
    row::{ResultRecord, ResultSet},
    stmt::CompiledStatement,
    explain::{ExplainVdbeProgram, VdbeInstruction, ExplainQueryPlan, QueryPlan},
    error::Result,
    util::Sealed,
};


/// This extension trait defines methods on [`Connection`] that help you create
/// and manage strongly-typed prepared statements.
///
/// Consult the [C API Reference](https://www.sqlite.org/c3ref/intro.html) for
/// in-depth explanation of the concepts such as prepared statements, parameter
/// binding, and stepping prepared VDBE programs.
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
    ///
    /// See the [SQLite docs](https://www.sqlite.org/lang_createtable.html)
    /// for details.
    fn create_table<T: Table>(&mut self) -> Result<()>;

    /// Returns all rows of a table.
    fn select_all<T, C>(&mut self) -> Result<C>
    where
        T: Table + ResultRecord,
        C: FromIterator<T> + ResultSet,
    {
        self.compile_invoke(Select::<T, C>::all(), ())
    }

    /// Returns all **unique** rows of a table (`SELECT DISTINCT`).
    ///
    /// ```
    /// # use nanosql::{Connection, ConnectionExt, Table, Param, ResultRecord};
    /// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Table, Param, ResultRecord)]
    /// struct Point {
    ///     x: i16,
    ///     y: i16,
    /// }
    ///
    /// # fn main() -> nanosql::Result<()> {
    /// let mut conn = Connection::connect_in_memory()?;
    /// conn.create_table::<Point>()?;
    ///
    /// let mut orig_points = [
    ///     Point { x:  1, y: 2 },
    ///     Point { x:  0, y: 3 },
    ///     Point { x:  1, y: 2 }, // duplicate
    ///     Point { x: -9, y: 4 },
    /// ];
    /// conn.insert_batch(orig_points)?;
    ///
    /// let mut queried_points: Vec<Point> = conn.select_all()?;
    ///
    /// orig_points.sort_unstable();
    /// queried_points.sort_unstable();
    /// assert_eq!(queried_points, orig_points);
    ///
    /// let mut unique_orig_points: Vec<_> = orig_points.to_vec();
    /// unique_orig_points.dedup();
    ///
    /// let mut unique_queried_points: Vec<Point> = conn.select_distinct()?;
    /// unique_queried_points.sort_unstable();
    /// assert_eq!(unique_queried_points, unique_orig_points);
    /// # Ok(())
    /// # }
    /// ```
    fn select_distinct<T, C>(&mut self) -> Result<C>
    where
        T: Table + ResultRecord,
        C: FromIterator<T> + ResultSet,
    {
        self.compile_invoke(Select::<T, C>::distinct(), ())
    }

    /// Convenience method for inserting many rows into a table in one go.
    ///
    /// It opens a single transaction for all of the insert statements, which
    /// prevents others from observing the data in a partially-inserted state.
    ///
    /// It prepares an `INSERT` statement and calls it in a loop, so it's more
    /// efficient than re-preparing and executing the same statement in a loop.
    ///
    /// See the [SQLite docs](https://www.sqlite.org/lang_insert.html) for details.
    fn insert_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>;

    /// Convenience method for inserting many rows into a table in one go,
    /// ignoring rows that already exist (based on `PRIMARY KEY` and `UNIQUE`
    /// columns).
    ///
    /// It opens a single transaction for all of the insert statements, which
    /// prevents others from observing the data in a partially-inserted state.
    ///
    /// It prepares an `INSERT` statement and calls it in a loop, so it's more
    /// efficient than re-preparing and executing the same statement in a loop.
    ///
    /// See the [SQLite docs](https://sqlite.org/lang_conflict.html) for details.
    fn insert_or_ignore_batch<'p, I>(&mut self, entities: I) -> Result<Vec<bool>>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>;

    /// Convenience method for inserting many rows into a table in one go,
    /// replacing rows that already exist (based on `PRIMARY KEY` and `UNIQUE`
    /// columns).
    ///
    /// It opens a single transaction for all of the insert statements, which
    /// prevents others from observing the data in a partially-inserted state.
    ///
    /// It prepares an `INSERT` statement and calls it in a loop, so it's more
    /// efficient than re-preparing and executing the same statement in a loop.
    ///
    /// See the [SQLite docs](https://sqlite.org/lang_conflict.html) for details.
    fn insert_or_replace_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>;

    /// Inserts a single row into a table.
    ///
    /// If you are planning to insert multiple rows, consider using
    /// [`ConnectionExt::insert_batch()`] instead, **both** for
    /// correctness (transactional atomicity) **and** improved efficiency.
    /// In other words, do **NOT** call this function in a loop.
    fn insert_one<'p, T>(&mut self, entity: T) -> Result<()>
    where
        T: InsertInput<'p>
    {
        self.compile_invoke(Insert::<T::Table>::new(), entity)
            .map(drop)
    }

    /// Inserts a single row into a table if it does not yet exist.
    /// Returns `true` if a new row was inserted, and `false` if it
    /// wasn't inserted because a conflicting row already existed.
    /// Uniqueness or "existence" is determined based on `UNIQUE`
    /// and `PRIMARY KEY` fields only.
    ///
    /// If you are planning to insert multiple rows, consider using
    /// [`ConnectionExt::insert_or_ignore_batch()`] instead, **both**
    /// for correctness (transactional atomicity) **and** improved
    /// efficiency. In other words, do **NOT** call this in a loop.
    ///
    /// See the [SQLite docs](https://sqlite.org/lang_conflict.html) for details.
    fn insert_or_ignore_one<'p, T>(&mut self, entity: T) -> Result<bool>
    where
        T: InsertInput<'p>
    {
        self.compile_invoke(Insert::<T::Table>::or_ignore(), entity)
            .map(|row| row.is_some())
    }

    /// Inserts a single row into a table if it does not yet exist,
    /// or replaces the non-PK and non-`UNIQUE` columns on conflict.
    ///
    /// If you are planning to insert multiple rows, consider using
    /// [`ConnectionExt::insert_or_replace_batch()`] instead, **both**
    /// for correctness (transactional atomicity) **and** improved
    /// efficiency. In other words, do **NOT** call this in a loop.
    ///
    /// See the [SQLite docs](https://sqlite.org/lang_conflict.html) for details.
    fn insert_or_replace_one<'p, T>(&mut self, entity: T) -> Result<()>
    where
        T: InsertInput<'p>
    {
        self.compile_invoke(Insert::<T::Table>::or_replace(), entity)
            .map(drop)
    }

    /// Explains the VDBE bytecode program generated for a query/statement.
    ///
    /// See the [SQLite docs](https://www.sqlite.org/lang_explain.html) for details.
    fn explain_vdbe_program<Q: Query>(&self, query: Q) -> Result<Vec<VdbeInstruction>> {
        self.compile_invoke(ExplainVdbeProgram::from(query), ())
    }

    /// Explains the high-level query plan generated for a query/statement.
    ///
    /// See the [SQLite docs](https://www.sqlite.org/eqp.html) for details.
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

    fn insert_or_ignore_batch<'p, I>(&mut self, entities: I) -> Result<Vec<bool>>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>
    {
        let txn = self.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let flags = txn.insert_or_ignore_batch(entities)?;
        txn.commit()?;
        Ok(flags)
    }

    fn insert_or_replace_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>,
    {
        let txn = self.transaction_with_behavior(TransactionBehavior::Immediate)?;
        txn.insert_or_replace_batch(entities)?;
        txn.commit()?;
        Ok(())
    }
}

/// This extension trait defines convenience methods on [`Transaction`].
///
/// See the [SQLite docs](https://www.sqlite.org/transactional.html) for
/// an overview of the transactionality guarantees of SQLite.
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

    /// Convenience method for inserting many rows into a table in one go,
    /// skipping those that already exist (based on `PRIMARY KEY` and `UNIQUE`
    /// columns), instead of returning an error when duplicates are found.
    ///
    /// This is an escape hatch for when you can't borrow the [`Connection`]
    /// mutably. It is recommended to use [`ConnectionExt::insert_or_ignore_batch()`]
    /// by default, unless you can't provide unique access to the [`Connection`].
    ///
    /// For each row, a `bool` flag will be returned, indicating whether the
    /// insertion actually happened (`true`) or the row already existed in the
    /// database, therefore the insertion was skipped (`false`).
    ///
    /// See the [SQLite docs](https://sqlite.org/lang_conflict.html) for details.
    fn insert_or_ignore_batch<'p, I>(&self, entities: I) -> Result<Vec<bool>>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>;

    /// Convenience method for inserting many rows into a table in one go,
    /// _replacing_ those that already exist (based on `PRIMARY KEY` and `UNIQUE`
    /// columns), instead of returning an error when duplicates are found.
    ///
    /// This is an escape hatch for when you can't borrow the [`Connection`]
    /// mutably. It is recommended to use [`ConnectionExt::insert_or_replace_batch()`]
    /// by default, unless you can't provide unique access to the [`Connection`].
    ///
    /// See the [SQLite docs](https://sqlite.org/lang_conflict.html) for details.
    fn insert_or_replace_batch<'p, I>(&self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>;

    /// Convenience method for invoking a query many times in one go, correctly
    /// (without the DB being modified across iterations), and efficiently
    /// (without re-compiling the query object and within a single transaction).
    ///
    /// **Note:** unlike the other methods, you have to **drive the returned
    /// iterator to completion** by yourself if you want to ensure that the query
    /// is actually invoked with each set of parameters (i.e., each iteration).
    /// If you simply call this without exhausting the iterator, some or all of
    /// the parameters/queries may be ignored, potentially leading to data loss.
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
        let query = Insert::<<I::Item as InsertInput<'p>>::Table>::new();

        self.invoke_batch(query, entities)?
            .try_for_each(|row| row.map(drop))
    }

    fn insert_or_ignore_batch<'p, I>(&self, entities: I) -> Result<Vec<bool>>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>
    {
        let query = Insert::<<I::Item as InsertInput<'p>>::Table>::or_ignore();

        self.invoke_batch(query, entities)?
            .map(|row| row.map(|opt| opt.is_some()))
            .collect()
    }

    fn insert_or_replace_batch<'p, I>(&self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>
    {
        let query = Insert::<<I::Item as InsertInput<'p>>::Table>::or_replace();

        self.invoke_batch(query, entities)?
            .try_for_each(|row| row.map(drop))
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
