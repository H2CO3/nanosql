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
    Param, ResultRecord, Table, TableDesc, Column, TyPrim
};


/// This type is going to represent our table.
///
/// We derive the `Param` trait for it so that it can be used for
/// binding parameters to the statement when inserting new records,
/// and the `ResultRecord` trait so that we can use it to retrieve
/// results, too.
///
/// The parameter prefix is '$' by default (if not specified via the
/// param_prefix attribute); it may also be one of ':', '@', or '?',
/// the last one being allowed only for tuples and scalar parameters.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Param, ResultRecord)]
#[nanosql(param_prefix = '$')] // optional
struct Pet {
    id: i64,
    name: String,
    kind: String,
}

/// The `Table` trait informs the `ConnectionExt::create_table()` and `ConnectionExt::insert_batch()`
/// functions as to how creating the table and inserting records works.
impl Table for Pet {
    /// In the simplest case, the input of an `INSERT` is just a record of the table type.
    type InsertInput<'p> = Self;

    fn description() -> TableDesc {
        TableDesc::new("pet")
            .column(Column::new("id").ty(TyPrim::Integer).pk())
            .column(Column::new("name").ty(TyPrim::Text))
            .column(Column::new("kind").ty(TyPrim::Text))
    }
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
        Ok("SELECT id, name, kind FROM pet WHERE id = ?")
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
            name: "Fluffy".into(),
            kind: "dog".into(),
        },
        Pet {
            id: 2,
            name: "Hello Kitty".into(),
            kind: "cat".into(),
        },
        Pet {
            id: 3,
            name: "Nemo".into(),
            kind: "fish".into(),
        },
    ])?;

    // We then compile the query into a prepared statement.
    let mut stmt = conn.compile(PetById)?;

    // Finally, we execute the compiled statement, passing parameters, and retrieve the results.
    let result = stmt.invoke(3)?;
    assert_eq!(result, Some(Pet { 
        id: 3,
        name: "Nemo".into(), 
        kind: "fish".into(),
    }));

    // We can re-use the statement and execute it multiple times
    let result = stmt.invoke(99)?;
    assert_eq!(result, None);

    Ok(())
}
```
