//! Working with top-level SQLite connections.

use core::borrow::Borrow;
use rusqlite::{Connection, TransactionBehavior};
use crate::{
    query::Query,
    table::{Table, Create, Insert},
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
    fn create_table<T: Table>(&self) -> Result<()> {
        self.compile_invoke(Create::<T>::default(), ())
    }

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
        I::Item: Table<InsertInput<'p> = I::Item>,
    {
        let mut stmt = self.compile(Insert::<I::Item>::default())?;

        entities
            .into_iter()
            .try_for_each(|item| stmt.invoke(item))
    }

    /// This is a faster and safer variant of [`ConnectionExt::insert_batch_no_txn()`].
    /// It opens a single transaction for all of the insert statements, which prevents
    /// other queries from observing the data in a partially-inserted state.
    fn insert_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: Table<InsertInput<'p> = I::Item>;
}

impl ConnectionExt for Connection {
    fn compile<Q: Query>(&self, query: Q) -> Result<CompiledStatement<'_, Q>> {
        let sql = query.sql()?;
        let statement = self.prepare_cached(sql.as_ref())?;

        Ok(CompiledStatement::new(statement))
    }

    fn insert_batch<'p, I>(&mut self, entities: I) -> Result<()>
    where
        I: IntoIterator,
        I::Item: Table<InsertInput<'p> = I::Item>,
    {
        let txn = self.transaction_with_behavior(TransactionBehavior::Immediate)?;
        txn.insert_batch_no_txn(entities)?;
        txn.commit().map_err(Error::from)
    }
}

impl Sealed for Connection {}
