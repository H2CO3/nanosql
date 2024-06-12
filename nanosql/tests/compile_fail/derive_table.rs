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
    //~^^ primary key declared at both the table and the column level
}

fn main() {}
