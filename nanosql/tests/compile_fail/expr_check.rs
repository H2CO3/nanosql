//! This compile-fail test ensures that nanosql detects syntactically invalid
//! SQL expressions in plaintext attributes which are going to be interpolated
//! into the generated SQL DDL verbatim. This allows us to avoid generating
//! syntactically invalid/corrupted SQL.

use nanosql::{Result, Param, Table};


#[derive(Clone, Debug, Param, Table)]
struct Table1 {
    #[nanosql(check = "JavaScript === bad")]
    //~^ ERROR sql parser error
    checked_column: String,
}

#[derive(Clone, Debug, Param, Table)]
struct Table2 {
    #[nanosql(default = "trailing junk")]
    //~^ ERROR Expected EOF
    defaulted_column: u32,
}

#[derive(Clone, Debug, Param, Table)]
struct Table3 {
    #[nanosql(generated(virtual = "3 +"))]
    //~^ ERROR Expected an expression
    generated_column_1: i32,
}

#[derive(Clone, Debug, Param, Table)]
struct Table4 {
    #[nanosql(generated(stored = ""))]
    //~^ ERROR Expected an expression
    another_gen_col: u64,
}

#[derive(Clone, Debug, Param, Table)]
struct Table5 {
    #[nanosql(index(unique, where = "partially_indexed <"))]
    //~^ ERROR sql parser error
    partially_indexed: Box<str>,
}

#[derive(Clone, Debug, Param, Table)]
#[nanosql(index(unique, columns(unique_id), where = "unique_id BETWEEN 10"))]
//~^ ERROR Expected AND
struct Table6 {
    unique_id: i64,
}

#[derive(Clone, Debug, Param, Table)]
#[nanosql(check = "this_is < OK (syntactically)")]
#[nanosql(check = "this is not OK")]
//~^ ERROR sql parser error
struct Table7 {
    col_name_does_not_matter: String,
}

fn main() -> Result<()> {
    Ok(())
}
