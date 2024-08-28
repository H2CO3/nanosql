use std::borrow::Cow;
use nanosql::{Result, Connection, ConnectionExt};
use nanosql::{AsSqlTy, ToSql, FromSql, Param, ResultRecord, Table, InsertInput, Error};


#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, AsSqlTy, ToSql, FromSql)]
#[nanosql(rename_all = "lower_snake_case")]
enum MusicalInstrumentKind {
    Piano,
    Violin,
    Tuba,
    Harp,
    PipeOrgan,
}

#[derive(Clone, PartialEq, Eq, Debug, ResultRecord, Table)]
#[nanosql(
    rename = "musical_instrument",
    insert_input_ty = MusicalInstrumentInsertParams<'p>,
)]
struct MusicalInstrument {
    #[nanosql(unique)]
    id: u32,
    #[nanosql(default = "'Unknown Instrument'")]
    make: String,
    kind: MusicalInstrumentKind,
    #[nanosql(generated(virtual = "format('%s (%s)', make, kind)"))]
    description: String,
}

#[derive(Clone, Copy, Debug, Param, InsertInput)]
#[nanosql(table = MusicalInstrument, input_lt = 'make_lt)]
struct MusicalInstrumentInsertParams<'make_lt> {
    id: u32,
    make: &'make_lt str,
    kind: MusicalInstrumentKind,
}

#[derive(Clone, Debug, Table, Param, ResultRecord)]
struct Orchestra {
    #[nanosql(pk)]
    id: i16,
    name: String,
    #[nanosql(check = "1000 <= founded", check = "founded <= 9999")]
    founded: u16,
}

#[derive(Clone, Debug, Table, Param, ResultRecord)]
#[nanosql(pk = ["section", "other_name"])]
struct Hierarchy {
    section: Box<str>,
    /// the `primary_key` directive must respect the `rename` attribute
    #[nanosql(rename = "other_name")]
    subsection: u64,
    /// the `rename` attribute accepts either an identifier or a string literal
    #[nanosql(rename = renamed_column_ident)]
    value: f32,
}

/// This is a table that intentionally does not declare any `PRIMARY KEY`.
#[derive(Table, Param)]
struct TableWithoutPK {
    /// intentionally misleading name: this field is **NOT** marked as `#[nanosql(pk)]`.
    pk: String,
    /// this would make a fine `PRIMARY KEY`, but alas, it isn't.
    some_other_field: i64,
}

/// This table demonstrates the correct use of a custom `PRIMARY KEY` type.
#[derive(Clone, PartialEq, Debug, Table, Param, ResultRecord)]
#[nanosql(pk = [pk_first, pk_second])]
#[nanosql(input_lt = 'input, pk_ty = CorrectCustomPK<'input>)]
struct TableWithCorrectCustomPK {
    unused: String,
    #[nanosql(rename = "pk_first")]
    pk_1: usize,
    #[nanosql(rename = pk_second)]
    pk_2: Box<str>,
    irrelevant: Option<f32>,
}

/// Field order intentionally permuted
#[derive(Clone, Copy, Debug, Param)]
struct CorrectCustomPK<'a> {
    /// this field/bind parameter is not renamed
    pk_second: &'a str,
    #[nanosql(rename = pk_first)]
    pk_one: usize,
}

/// This table demonstrates an **incorrect** custom PK type: one where the
/// field names of the custom type do not match the column names of the PK.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Table, Param, ResultRecord)]
#[nanosql(pk = [field_abc, field_xyz], pk_ty = BadCustomPK)]
struct TableWithBadCustomPK {
    field_abc: u32,
    field_xyz: u64,
    field_pqr: i16,
}

#[derive(Clone, Copy, Debug, Param)]
struct BadCustomPK {
    field_abc: u32,
    field_nop: u64,
}

nanosql::define_query! {
    /// Inserts an instrument of the given kind, using the default value for its `make` column.
    #[derive(Clone, Copy, Default, Debug)]
    InsertUnknownInstrument<'p>: (u32, MusicalInstrumentKind) => () {
        "INSERT INTO musical_instrument(id, kind) VALUES (?, ?)"
    }

    /// Returns the instrument with the given ID if it exists.
    #[derive(Clone, Copy, Default, Debug)]
    InstrumentById<'p>: u32 => Option<MusicalInstrument> {
        "SELECT id, kind, make, description FROM musical_instrument WHERE id = ?"
    }
}

// This is just to verify that a table without a PK has an uninhabited
// `PrimaryKey` associated type when `Table` is automatically derived.
fn assert_uninhabited_pk_ty<T>()
where
    T: Table<PrimaryKey<'static> = nanosql::table::PrimaryKeyMissing>
{
}

fn main() -> Result<()> {
    // Perform type-level assertions.
    assert_uninhabited_pk_ty::<TableWithoutPK>();

    // First, we open a database connection.
    let mut conn = Connection::connect_in_memory()?;

    conn.create_table::<MusicalInstrument>()?;
    conn.insert_batch([
        MusicalInstrumentInsertParams {
            id: 1,
            make: "Stenway Model B".into(),
            kind: MusicalInstrumentKind::Piano,
        },
        MusicalInstrumentInsertParams {
            id: 2,
            make: "Broadwalk Hall".into(),
            kind: MusicalInstrumentKind::PipeOrgan,
        },
    ])?;

    let mut find_by_id = conn.compile(InstrumentById)?;
    let mut insert_unknown = conn.compile(InsertUnknownInstrument)?;

    let result = find_by_id.invoke(2)?;
    assert_eq!(result, Some(MusicalInstrument {
        id: 2,
        make: "Broadwalk Hall".into(),
        kind: MusicalInstrumentKind::PipeOrgan,
        description: "Broadwalk Hall (pipe_organ)".into(),
    }));
    println!("instrument ID = #2: {result:#?}");

    let result = find_by_id.invoke(137)?;
    assert_eq!(result, None);

    insert_unknown.invoke((137, MusicalInstrumentKind::Tuba))?;

    let result = find_by_id.invoke(137)?;
    assert_eq!(result, Some(MusicalInstrument {
        id: 137,
        make: "Unknown Instrument".into(),
        kind: MusicalInstrumentKind::Tuba,
        description: "Unknown Instrument (tuba)".into(),
    }));
    println!("instrument ID = #137: {result:#?}");

    drop(find_by_id);
    drop(insert_unknown);

    conn.create_table::<Orchestra>()?;
    conn.insert_batch([
        Orchestra {
            id: 7,
            name: "Seven Eleven".into(),
            founded: 1977,
        },
    ])?;

    let dup_id_result = conn.insert_batch([
        Orchestra {
            id: 7,
            name: "Some Other Name".into(),
            founded: 1985,
        },
    ]);
    assert!(dup_id_result.is_err(), "duplicate Primary Key must not be allowed");

    conn.create_table::<Hierarchy>()?;
    conn.insert_batch([
        Hierarchy {
            section: "Section One".into(),
            subsection: 42137,
            value: 9.87654321,
        },
    ])?;
    let dup_multi_col_pk_result = conn.insert_batch([
        Hierarchy {
            section: "Section One".into(),
            subsection: 42137,
            value: 1.23456789,
        },
    ]);
    assert!(dup_multi_col_pk_result.is_err(), "duplicate multi-column PK must not be allowed");

    // make sure that custom PK type is allowed when field/parameter/column names match
    conn.create_table::<TableWithCorrectCustomPK>()?;
    conn.insert_batch([
        TableWithCorrectCustomPK {
            unused: "not part of the key".into(),
            pk_1: 42,
            pk_2: "foo".into(),
            irrelevant: None,
        },
        TableWithCorrectCustomPK {
            unused: "lorem ipsum dolor sit amet".into(),
            pk_1: 42,
            pk_2: "quux".into(),
            irrelevant: Some(1.25),
        },
        TableWithCorrectCustomPK {
            unused: "consectetur adespicit elit".into(),
            pk_1: 67,
            pk_2: "thing".into(),
            irrelevant: Some(-15.4),
        },
    ])?;
    let thing: TableWithCorrectCustomPK = conn.select_by_key(CorrectCustomPK {
        pk_second: "quux",
        pk_one: 42,
    })?;
    dbg!(&thing);
    assert_eq!(thing.irrelevant, Some(1.25));
    assert_eq!(thing.unused, "lorem ipsum dolor sit amet");

    // renaming of key columns shouldn't cause any other kind of confusion
    assert_eq!(
        conn.select_by_key_opt::<TableWithCorrectCustomPK, _>(CorrectCustomPK {
            pk_second: "thing",
            pk_one: 42,
        })?,
        None
    );

    // ensure that custom PK types with incorrect field names cause an appropriate error
    conn.create_table::<TableWithBadCustomPK>()?;
    conn.insert_batch([
        TableWithBadCustomPK {
            field_abc: 1,
            field_pqr: 2,
            field_xyz: 3,
        },
        TableWithBadCustomPK {
            field_abc: 4,
            field_pqr: 5,
            field_xyz: 6,
        },
    ])?;
    let result: Result<TableWithBadCustomPK> = conn.select_by_key(BadCustomPK {
        field_abc: 1,
        field_nop: 2,
    });
    dbg!(&result);
    assert!(matches!(result, Err(Error::UnknownParam(Cow::Borrowed("$field_nop")))));

    Ok(())
}
