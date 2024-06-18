use nanosql::{Param, Table};


/// Only one column-level primary key annotation is allowed.
#[derive(Clone, Default, Debug, Param, Table)]
struct Address {
    #[nanosql(pk)]
    building_number: u32,

    #[nanosql(primary_key)]
    place_name: String,
    //~^^ ERROR more than one primary key column; use the table-level attribute instead

    has_street_view: bool,
}

/// The `pk` directive must respect the `rename` directive.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(pk = ["latitude", "longitude"])]
//~^ ERROR unknown column `latitude` in primary key
struct Coord {
    #[nanosql(rename = "lat")]
    latitude: f32,
    #[nanosql(rename = "lng")]
    longitude: f32,
}

/// If there is a table-level primary key annotation,
/// there must not be any on the individual columns.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(pk = ["country_code"])]
struct ZipCode {
    country_code: String,
    #[nanosql(pk)]
    value: String,
    //~^^ ERROR primary key declared at both the table and the column level
}

/// A table-level primary key must not have duplicate columns.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(pk = ["unique_id", "unique_id"])]
//~^ ERROR duplicate columns in primary key
struct DuplicatePk {
    unique_id: i32,
}

/// An empty tuple is not allowed as the primary key
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(pk = [])]
//~^ ERROR primary key may not be an empty tuple
struct EmptyTuplePk {
    id: u16,
}

/// Referencing columns of a table-level foreign key must exist in this table.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(fk("other_table" => ("qux" = "other_column")))]
//~^ ERROR unknown column `qux` in foreign key
struct NonExistentFk {
    foo: bool,
    bar: Box<[u8]>,
}

/// An empty tuple is not allowed as a foreign key
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(fk("someone_elses_table" => ()))]
//~^ ERROR foreign key may not be an empty tuple
struct EmptyTupleFk {
    my_column: Option<String>,
}

fn main() {}
