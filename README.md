# NanoSQL: a tiny, strongly-typed data mapper for SQLite

NanoSQL is a small data mapper library that helps you execute SQL statements with typed
parameters and a typed result set. It does **not** attempt to typecheck your SQL code.
Rather, it only ensures that the parameters and results serialize/deserialize to/from
the correct shape.

For an overview of SQLite, [see the official docs](https://www.sqlite.org/).

## Overview

The library is structured around prepared statements. First, you create a description
of the interface and implementation of your query. This is realized by the [`Query`] trait
that contains the input (parameter) and output (result) types as associated types, and
a function for building the SQL text. You can imeplement this trait by hand, or use the
[`define_query`] macro as a convenient shortcut.

Next, you use [`Connection::compile()`] to compile the `Query` into a [`CompiledStatement`].
This wraps an SQLite prepared statement, but restricts its parameter and return types.

Finally, you call the [`CompiledStatement::invoke()`] function on your compiled statement
to actually query the database:

* The input of a query is any type that implements the [`Param`] trait. These include primitives,
  optionals of primitives, tuples of primitives or optionals, and structs with fields of primitive
  or optional types. Nanosql can work with both positional and named arguments, and supports all
  parameter prefixes accepted by SQLite (`?`, `:`, `@`, and `$`). This trait can be derived if the
  `derive` feature of the crate is activated.
* The output of a query is a type that implements the [`ResultSet`] trait. This is most commonly
  a collection of some sort (e.g., the standard `Vec` type), or any other type that aggregates
  the rows returned from an SQL query into a meaningful data structure. Notably, `Option<T>` and
  [`Single`]`<T>` can be used for expecting at most one or exactly one record, respectively. These
  types implement `ResultSet` when the type of their wrapped value implements [`ResultRecord`].
* [`ResultRecord`] is a trait that can be implemented by tuple-like and struct-like types for
  deserializing individual rows. This trait can also be `#[derive]`d.

Extremely common (basically inevitable) tasks such as creating the schema of a table and
inserting records is possible via special helper/extension methods on `Connection` objects,
via the [`ConnectionExt`] trait. These in turn use the [`Table`] trait for preparing and
invoking the corresponding SQL statements, and can be called for convenience.

## Examples

The absolute basics - create a table, insert a bunch of records into it, then retrieve them:

```rust
use std::fmt::{self, Formatter};
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
    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str("SELECT id, nickName, type FROM MyLittlePet WHERE id = ?")
    }
}

fn main() -> Result<()> {
    // First, we open a database connection.
    let mut conn = Connection::connect_in_memory()?;
    
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

See the [`nanosql/examples`](https://github.com/H2CO3/nanosql/tree/master/nanosql/examples)
directory (and especially `realistic.rs`) for more advanced and interesting examples.

## A note about batch insertion and transactions

The [`ConnectionExt::insert_batch()`] method wraps the insertion statements in a transaction
for improving performance. The exclusiveness of transactions is modeled in `rusqlite` at the
type level by the [`Connection`] object being mutably (uniquely) borrowed for the duration of
the transaction. This in turn means that `insert_batch()` also needs to mutably borrow. However,
preparing and invoking queries needs an immutable borrow, and prepared statements borrow the
`Connection` for as long as they live. As such, you may get errors like "cannot borrow `connection`
mutably because it is also borrowed as immutable" when mixing batch insertion with other queries.
There are two basic solutions to this problem:

1. Drop the outstanding prepared statements before calling [`ConnectionExt::insert_batch()`];
2. Or if you can't do that, then obtain a transaction object that is not checked at compilation
   time for exclusiveness, using [`Connection::unchecked_transaction()`], then call the immutably
   borrowing [`TransactionExt::insert_batch()`] method on the _transaction object_ instead.

## Cargo Features

* `derive`: activates procedural macros - mostly custom `#[derive]`s for commonly-used traits.
  **Enabled by default.**
* `expr-check`: uses the `sqlparser` crate to check for syntax errors in raw SQL expressions
  in derive macro attributes at compile time. This ensures that any generated SQL will be valid
  and syntax error in user-supplied SQL code will be clearly pinpointed, instead of causing
  mysterious statement preparation errors at runtime. **Enabled by default.**
* `not-nan`: implements `Param` and `ResultRecord` for `ordered_float::NotNan`. This allows for
  a more type-safe interface in queries: since SQLite treats `NaN` as the SQL `NULL` value, you
  may run into surprising errors when binding or retrieving an `f32::NAN` or `f64::NAN` and the
  corresponding parameter needs to be `NOT NULL`, or the source column *can* be `NULL`.
* `chrono`: adds support for `DateTime<Utc | FixedOffset | Local>`, by serializing timestamps to
  and from the RFC-3339 format.
* `pretty-eqp`: use the `ptree` crate to pretty print the results of `EXPLAIN QUERY PLAN`. This
  will impl `Display` for `QueryPlan`, the return type of [`ConnectionExt::explain_query_plan()`],
  which renders the tree in a nice, human-readable format using ASCII art.

## Notes on the test suite

1. The tests try to exercise all features of the library extensively. For this reason, they
   rely on most or all Cargo features defined in `Cargo.toml`. Consequently, for successfully
   compiling and running all tests, you must pass `cargo test --all-features` to Cargo.

2. The `compiletest_rs` crate is used for ensuring that the derive macros detect certain kinds of
   errors, such as multiple primary keys or table-level constraints that reference non-existent
   columns.

   Due to the way the `compiletest_rs` crate is structured, the tests can be somewhat flaky. If you
   encounter `E0464` errors (e.g., "multiple candidates for `rlib` dependency `nanosql` found"),
   then run `cargo clean` before running `cargo test compile_fail --all-features`.

**TL;DR:** the best "lazy" way to run tests is the `./runtests.sh` script, which just does:

```sh
cargo clean
cargo test --workspace --all-features
```
