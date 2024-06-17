//! Working with top-level SQLite connections.

use core::borrow::Borrow;
use core::fmt::Write;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;
use rusqlite::{Connection, Transaction, TransactionBehavior};
use crate::{
    query::Query,
    table::{Table, InsertInput, CreateTable, Insert},
    stmt::CompiledStatement,
    error::{Error, Result},
    util::Sealed,
};


/// This extension trait defines methods on [`Connection`] that help you create
/// and manage strongly-typed prepared statements.
#[allow(private_bounds)]
pub trait ConnectionExt: Sealed {
    /// Compiles a [`Query`] into a "prepared" [`CompiledStatement`].
    fn compile<Q: Query>(&self, query: Q) -> Result<CompiledStatement<'_, Q>>;

    /// Compiles a query and immediately invokes it. **This is a convenience
    /// function that should not in general be used when a query is to be executed
    /// many times.** Use [`ConnectionExt::compile`] and then call [`CompiledStatement::invoke`]
    /// in a loop instead.
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
    fn create_table<T: Table>(&self) -> Result<()>;

    /// Convenience method for inserting many rows into a table in one go.
    /// It prepares an `INSERT` statement and calls it in a loop, so it's
    /// more efficient than preparing and executing the same statement in a
    /// loop. This does **not** open a single transaction for all records to be
    /// inserted, so it is still slower than [`ConnectionExt::insert_batch()`],
    /// and it does not lock the DB against reading between insertions. It is
    /// recommended to use the transactional method instead, unless you can't
    /// provide unique access to the [`Connection`].
    fn insert_batch_no_txn<'p, I>(&self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>,
    {
        let insert = Insert::<<I::Item as InsertInput<'p>>::Table>::default();
        let mut stmt = self.compile(insert)?;

        entities
            .into_iter()
            .try_for_each(|item| stmt.invoke(item))
    }

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
}

impl ConnectionExt for Connection {
    fn compile<Q: Query>(&self, query: Q) -> Result<CompiledStatement<'_, Q>> {
        let sql = query.sql()?;
        let statement = self.prepare_cached(sql.as_ref())?;

        Ok(CompiledStatement::new(statement))
    }

    fn create_table<T: Table>(&self) -> Result<()> {
        // First, create the table itself.
        self.compile_invoke(CreateTable::<T>::default(), ())?;

        let desc = T::description();

        // Then, create indexes: table-level and field-level, explicit and implicit
        for index_cols in desc.index_specs() {
            let index_hash = {
                let mut state = DefaultHasher::new();
                desc.name.hash(&mut state);
                index_cols.hash(&mut state);
                state.finish()
            };
            let mut sql = format!(
                r#"CREATE INDEX IF NOT EXISTS "__nanosql_index_{table}_{hash:016x}" ON "{table}"("#,
                hash = index_hash,
                table = desc.name,
            );
            let mut sep = "";

            for col in index_cols {
                write!(sql, r#"{sep}\n    "{col_name}""#, col_name = col.name)?;
                sep = ",";
            }

            sql.push_str("\n);");

            self.prepare(&sql)?.execute([])?;
        }

        Ok(())
    }

    fn insert_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>,
    {
        let txn = self.transaction_with_behavior(TransactionBehavior::Immediate)?;
        txn.insert_batch(entities)?;
        txn.commit().map_err(Error::from)
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
}

impl TransactionExt for Transaction<'_> {
    /// Convenience method for inserting many rows into a table in one go.
    ///
    /// This is an escape hatch for when you can't borrow the [`Connection`]
    /// mutably. It is recommended to use [`ConnectionExt::insert_batch()`] by
    /// default, unless you can't provide unique access to the [`Connection`].
    fn insert_batch<'p, I>(&self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: InsertInput<'p>,
    {
        let insert = Insert::<<I::Item as InsertInput<'p>>::Table>::default();
        let mut stmt = self.compile(insert)?;

        entities
            .into_iter()
            .try_for_each(|item| stmt.invoke(item))
    }
}

impl Sealed for Connection {}
impl Sealed for Transaction<'_> {}
