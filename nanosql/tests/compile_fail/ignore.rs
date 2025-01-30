//! Ensure that structs with multiple non-ignored fields aren't
//! allowed to implement `AsSqlTy`, `ToSql`, and `FromSql`.

use std::borrow::Cow;
use nanosql::{Result, AsSqlTy, ToSql, FromSql, Table};


#[derive(AsSqlTy)]
struct AsSqlTyMultiFieldNamed {
    foo: String,
    //~^ ERROR deriving `AsSqlTy` on a struct is only allowed for a newtype with exactly one field
    bar: u32,
}

#[derive(AsSqlTy)]
struct AsSqlTyMultiFieldNamedIgnored {
    foo: String,
    //~^ ERROR deriving `AsSqlTy` on a struct is only allowed for a newtype with exactly one field
    #[nanosql(ignore)] // this is ignored, but there are still more than 1 non-ignored fields
    baz: i64,
    bar: u32,
}

#[derive(AsSqlTy)]
struct AsSqlTyMultiFieldTuple (
    Vec<u8>,
    //~^ ERROR deriving `AsSqlTy` on a struct is only allowed for a newtype with exactly one field
    i16,
);

#[derive(AsSqlTy)]
struct AsSqlTyMultiFieldTupleIgnored (
    Vec<u8>,
    //~^ ERROR deriving `AsSqlTy` on a struct is only allowed for a newtype with exactly one field
    i16,
    #[nanosql(ignore)]
    Box<str>,
);

#[derive(ToSql)]
struct ToSqlMultiFieldNamed {
    qux: f32,
    //~^ ERROR deriving `ToSql` on a struct is only allowed for a newtype with exactly one field
    lol: i32,
}

#[derive(ToSql)]
struct ToSqlMultiFieldNamedIgnored {
    #[nanosql(ignore)]
    //~^ ERROR deriving `ToSql` on a struct is only allowed for a newtype with exactly one field
    qux: f32,
    lol: i32,
    bar: Vec<u8>,
}

#[derive(ToSql)]
struct ToSqlMultiFieldTuple (
    String,
    //~^ ERROR deriving `ToSql` on a struct is only allowed for a newtype with exactly one field
    u64,
);

#[derive(ToSql)]
struct ToSqlMultiFieldTupleIgnored (
    String,
    //~^ ERROR deriving `ToSql` on a struct is only allowed for a newtype with exactly one field
    u64,
    #[nanosql(ignore)]
    [u8; 16],
);

#[derive(FromSql)]
struct FromSqlMultiFieldNamed {
    wut: Box<str>,
    //~^ ERROR deriving `FromSql` on a struct is only allowed for a newtype with exactly one field
    baz: f64,
}

#[derive(FromSql)]
struct FromSqlMultiFieldNamedIgnored {
    #[nanosql(ignore)]
    //~^ ERROR deriving `FromSql` on a struct is only allowed for a newtype with exactly one field
    ignored_1: u8,
    wut: Box<str>,
    baz: f64,
    #[nanosql(ignore)]
    ignored_2: i8,
}

#[derive(FromSql)]
struct FromSqlMultiFieldTuple (
    Box<[u8]>,
    //~^ ERROR deriving `FromSql` on a struct is only allowed for a newtype with exactly one field
    #[nanosql(ignore)]
    i32,
    #[nanosql(ignore)]
    u16,
    Cow<'static, str>,
);

#[derive(Table)]
#[nanosql(pk = [pk_field_1, pk_field_2])]
//~^ ERROR unknown column `pk_field_2` in primary key
struct TableLevelPkOnIgnored {
    pk_field_1: u32,
    #[nanosql(ignore)]
    pk_field_2: String,
    not_a_key: f64,
}

#[derive(Table)]
#[nanosql(fk(OtherTable => (connection_1 = pk_1, connection_2 = pk_2)))]
//~^ ERROR unknown column `connection_1` in foreign key
struct TableLevelFkOnIgnored {
    #[nanosql(skip)]
    connection_1: Option<Box<str>>,
    value: Option<i64>,
    connection_2: Option<[u8; 32]>,
}

#[derive(Table)]
#[nanosql(index(unique, columns(baz, qux, foo)))]
//~^ ERROR unknown column `qux` in table-level index
struct TableLevelIndexOnIgnored {
    foo: i32,
    baz: i32,
    #[nanosql(ignore)]
    qux: i32,
    nop: Vec<u8>,
}

#[derive(Table)]
#[nanosql(pk_ty = Box<str>)]
//~^ ERROR Explicit PK type given to table without a PK
struct PkTypeOnIgnoredPk {
    #[nanosql(primary_key)]
    #[nanosql(skip)]
    key: String,
    value: f64,
}

fn main() -> Result<()> {
    Ok(())
}
