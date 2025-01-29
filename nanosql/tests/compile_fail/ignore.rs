//! Ensure that structs with multiple non-ignored fields aren't
//! allowed to implement `AsSqlTy`, `ToSql`, and `FromSql`.

use std::borrow::Cow;
use nanosql::{Result, AsSqlTy, ToSql, FromSql};


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

fn main() -> Result<()> {
    Ok(())
}
