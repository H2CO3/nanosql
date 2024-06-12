## NanoSQL: a tiny, strongly-typed data mapper for SQLite

NanoSQL is a small data mapper library that helps you execute SQL statements with typed
parameters and a typed result set. It does **not** attempt to typecheck your SQL code.
Rather, it only ensures that the parameters and results serialize/deserialize to/from
the correct shape.

### Architecture

The library is structured around prepared statements. First, you create a description
of the interface and implementatino of your query. This is realized by the `Query` trait
that contains the input (parameter) and output (result) types as associated types, and
a function for building the SQL text.

Next, you use a `rusqlite::Connection` object to compile the `Query` into a `CompiledStatement`.
This wraps a prepared statement and restricts its parameter and return types.

Finally, you call the `invoke()` function on your compiled statement to actually query the database.

Extremely common (basically inevitable) tasks such as creating the schema of a table and
inserting records is possible via special helper/extension methods on `Connection` objects.
These make use of the `Table` trait for preparing and executing the corresponding SQL statements
and can be called for convenience.

### Examples

The absolute basics - create a table, insert a bunch of records into it, then retrieve them:

```rust
use nanosql::{
    Result, Connection, ConnectionExt, Query,
    ToSql, FromSql, AsSqlTy, Param, ResultRecord, Table
};


/// This type is going to represent our table.
///
/// We derive the `Param` trait for it so that it can be used for
/// binding parameters to the statement when inserting new records,
/// and the `ResultRecord` trait so that we can use it to retrieve
/// results, too.
///
/// We also derive the `Table` trait so that basic operations such as
/// creating the table in the schema and bulk insertion can be performed
/// using the appropriate convenience methods in [`ConnectionExt`].
///
/// The parameter prefix is '$' by default (if not specified via the
/// param_prefix attribute); it may also be one of ':', '@', or '?',
/// the last one being allowed only for tuples and scalar parameters.
///
/// `#[nanosql(rename)]` on a struct renames the table itself, while
/// `#[nanosql(rename_all)]` applies a casing convention to all columns.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Param, ResultRecord, Table)]
#[nanosql(param_prefix = '$')] // optional
#[nanosql(rename = "MyLittlePet", rename_all = "lowerCamelCase")]
struct Pet {
    /// If you don't like the default `AsSqlTy` impl for your column's
    /// type, you can specify a different one. Here we add a non-zero
    /// constraint, but the `id` remains a plain `i64` for convenience.
    ///
    /// You can also add additional `CHECK` constraints, if necessary.
    #[nanosql(sql_ty = core::num::NonZeroI64, check = "id <= 999999")]
    id: i64,
    /// You can apply a `UNIQUE` constraint to any field.
    #[nanosql(unique)]
    nick_name: String,
    /// You can also rename fields/columns one by one
    #[nanosql(rename = "type")]
    kind: PetKind,
}

/// Collective and field-level casing/renaming also works with `enum`s
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, ToSql, FromSql, AsSqlTy)]
#[nanosql(rename_all = "UPPER_SNAKE_CASE")]
enum PetKind {
    Dog,
    #[nanosql(rename = "KITTEN")]
    Cat,
    Fish,
}

/// Our first custom query retrieves a pet by its unique ID.
///
/// If you don't want to spell out the impl by hand, you can
/// use the `define_query!{}` macro for a shorter incantation.
struct PetById;

impl Query for PetById {
    /// The type of the parameter(s). This can be a single scalar, a tuple,
    /// a tuple struct of scalars, or a struct with named fields of scalar types.
    type Input<'p> = i64;

    /// The return type of a query can be either a scalar, a single record (struct or
    /// tuple), or an optional of a scalar/record (when it returns either 0 or 1 rows),
    /// or a collection of arbitrarily many scalars/records. Here we choose an `Option`,
    /// because a given ID corresponds to at most one `Pet`.
    type Output = Option<Pet>;

    /// Finally, we create the actual SQL query.
    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        Ok("SELECT id, nickName, type FROM MyLittlePet WHERE id = ?")
    }
}

fn main() -> Result<()> {
    // First, we open a database connection.
    let mut conn = Connection::open_in_memory()?;
    
    // Then, we ensure that the table exists in the schema.
    conn.create_table::<Pet>()?;

    // Next, we insert a couple of records so we have test data to work on.
    conn.insert_batch([
        Pet {
            id: 1,
            nick_name: "Fluffy".into(),
            kind: PetKind::Dog,
        },
        Pet {
            id: 2,
            nick_name: "Hello Kitty".into(),
            kind: PetKind::Cat,
        },
        Pet {
            id: 3,
            nick_name: "Nemo".into(),
            kind: PetKind::Fish,
        },
    ])?;

    // We then compile the query into a prepared statement.
    let mut stmt = conn.compile(PetById)?;

    // Finally, we execute the compiled statement, passing parameters, and retrieve the results.
    let result = stmt.invoke(3)?;
    assert_eq!(result, Some(Pet { 
        id: 3,
        nick_name: "Nemo".into(),
        kind: PetKind::Fish,
    }));

    // We can re-use the statement and execute it multiple times
    let result = stmt.invoke(99)?;
    assert_eq!(result, None);

    drop(stmt);

    // Inserting a pet with id = 0 should fail due to the `#[nanosql(sql_ty = ...)]` attribute.
    let insert_id_0_result = conn.insert_batch([
        Pet {
            id: 0,
            nick_name: "Error".into(),
            kind: PetKind::Cat,
        }
    ]);
    assert!(insert_id_0_result.is_err(), "id = 0 violates NonZeroI64's CHECK constraint");

    // Inserting a pet with a high ID is expected to fail due to the CHECK constraint.
    let insert_id_high_result = conn.insert_batch([
        Pet {
            id: 1000000,
            nick_name: "this is unique".into(),
            kind: PetKind::Dog,
        }
    ]);
    assert!(insert_id_high_result.is_err(), "id = 1000000 violates extra CHECK constraint");

    // Inserting a pet with a duplicate name is not allowed due to `#[nanosql(unique)]`.
    let insert_dup_name_result = conn.insert_batch([
        Pet {
            id: 137731,
            nick_name: "Hello Kitty".into(),
            kind: PetKind::Dog,
        }
    ]);
    assert!(insert_dup_name_result.is_err(), "duplicate name violates uniqueness constraint");

    Ok(())
}
```

### A note about batch insertion and transactions

The `ConnectionExt::insert_batch()` method wraps the insertion statements in a transaction
for improving performance. The exclusiveness of transactions is modeled in `rusqlite` at the
type level by the `Connection` object being mutably (uniquely) borrowed for the duration of
the transaction. This in turn means that `insert_batch()` also needs to mutably borrow. However,
preparing and invoking queries needs an immutable borrow, and prepared statements borrow the
`Connection` for as long as they live. As such, you may get errors like "cannot borrow `connection`
mutably because it is also borrowed as immutable" when mixing batch insertion with other queries.
There are two basic solutions to this problem:

1. Drop the outstanding prepared statements before calling `ConnectionExt::insert_batch()`;
2. Or if you can't do that, then obtain a transaction object that is not checked at compilation
   time for exclusiveness, using `Connection::unchecked_transaction()`, then call the immutably
   borrowing `TransactionExt::insert_batch()` method on the _transaction object_ instead.

#### Notes on the test suite

1. The tests try to exercise all features of the library extensively. For this reason, they
   rely on most or all Cargo features defined in `Cargo.toml`. Consequently, for successfully
   compiling and running all tests, you must pass `cargo test --all-features` to Cargo.

2. The `compiletest_rs` crate is used for ensuring that the derive macros detect certain kinds of
   errors, such as multiple primary keys or table-level constraints that reference non-existent
   columns.

   Due to the way the `compiletest_rs` crate is structured, the tests can be somewhat flaky. If you
   encounter `E0464` errors (e.g., "multiple candidates for `rlib` dependency `nanosql` found"),
   then run `cargo clean` before running `cargo test compile_fail --all-features`.

**TL;DR:** the best "lazy" way to run tests is:

```sh
cargo clean && cargo test --all --all-features
```
