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

/// Columns of a table-level primary key must exist in this table.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(pk = ["foo", "qux"])]
//~^ ERROR unknown column `qux` in primary key
struct NonExistentPk {
    foo: bool,
    bar: Box<[u8]>,
}

/// A table-level primary key must not have duplicate columns.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(pk = ["unique_id", "unique_id"])]
//~^ ERROR duplicate columns in primary key
struct DuplicatePk {
    unique_id: i32,
}

/// An empty tuple is not allowed as the primary key.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(pk = [])]
//~^ ERROR primary key may not be an empty tuple
struct EmptyTuplePk {
    id: u16,
}

/// A table-level primary key must respect `rename` and `rename_all`.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(primary_key = [orig_field_name])]
//~^ ERROR unknown column `orig_field_name` in primary key
struct RenamedPk {
    #[nanosql(rename = i_am_renamed)]
    orig_field_name: Option<bool>,
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

/// A table-level foreign key must not have duplicate columns.
/// This applies to both internal (own) and external (foreign) columns.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(fk(external_table => ("my_id" = other_1, my_id = other_2)))]
//~^ ERROR duplicate internal column in foreign key
struct DuplicateFkInternalColumn {
    my_id: u16,
}

/// A table-level foreign key must not have duplicate columns.
/// This applies to both internal (own) and external (foreign) columns.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(fk(external_table => ("my_id_1" = others_id, "my_id_2" = others_id)))]
//~^ ERROR duplicate external column in foreign key
struct DuplicateFkExternalColumn {
    my_id_2: Option<String>,
    my_id_1: String,
}

/// Referencing columns of a table-level foreign key must respect renaming attributes.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(fk("other_table" => ("MY_COL" = extern_col, "foo_bar" = "other_column")))]
#[nanosql(rename_all = "UPPER_SNAKE_CASE")]
//~^^ ERROR unknown column `foo_bar` in foreign key
struct RenamedFk {
    my_col: usize,
    foo_bar: bool,
}

/// An empty tuple is not allowed in a UNIQUE constraint.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(unique = [])]
//~^ ERROR unique constraint must refer to at least 1 column
struct EmptyTupleUnique {
    qux: Option<i32>,
    lol: String,
}

/// Columns of a unique constraint must exist in this table.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(unique = [bar, wut, foo])]
//~^ ERROR unknown column `wut` in unique constraint
struct NonExistentUnique {
    foo: bool,
    bar: Box<[u8]>,
}

/// A table-level unique constraint must not have duplicate columns.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(unique = ["my_little_column", my_little_column])]
//~^ ERROR duplicate columns in unique constraint
struct DuplicateUnique {
    #[nanosql(rename = my_little_column)]
    only_one_col: u64,
}

/// A table-level unique constraint must respect `rename` and `rename_all`.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(unique = ["not_renamed"])]
//~^ ERROR unknown column `not_renamed` in unique constraint
struct RenamedUnique {
    #[nanosql(rename = another_name)]
    not_renamed: Option<bool>,
}

/// A table-level index must not be an empty tuple of columns.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(index(columns()))]
//~^ ERROR table-level index must refer to at least 1 column
struct EmptyTupleIndex {
    has_columns: i16,
    could_choose_this_one_too: Box<str>,
}

/// A table-level index must be composed of columns in the table.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(index(unique, columns(not_my_column = asc)))]
//~^ ERROR unknown column `not_my_column` in table-level index
struct NonExistentIndex {
    my_column: String,
}

/// A table-level index must be composed of distinct columns.
/// Specifying the same column twice, with different sorting
/// orders is not allowed.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(index(columns(my_column = asc, my_column = desc)))]
//~^ ERROR duplicate columns in table-level index
struct DuplicateIndex {
    my_column: Box<isize>,
}

/// A table-level index must respect `rename` and `rename_all`.
/// Specifying the same column twice, with different sorting
/// orders is not allowed.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(index(columns(the_old_name = asc), where = "the_new_name > 0.0"))]
//~^ ERROR unknown column `the_old_name` in table-level index
struct RenamedIndex {
    #[nanosql(rename = "the_new_name")]
    the_old_name: f32,
}

fn main() {}
