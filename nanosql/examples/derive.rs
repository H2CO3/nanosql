use nanosql::{Result, Connection, ConnectionExt};
use nanosql::{AsSqlTy, ToSql, FromSql, Param, ResultRecord, Table, InsertInput};

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
#[nanosql(table = MusicalInstrument, insert_input_lt = 'make_lt)]
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
    value: f32,
}

nanosql::define_query! {
    InsertUnknownInstrument<'p>: (u32, MusicalInstrumentKind) => () {
        "INSERT INTO musical_instrument(id, kind) VALUES (?, ?)"
    }
    InstrumentById<'p>: u32 => Option<MusicalInstrument> {
        "SELECT id, kind, make, description FROM musical_instrument WHERE id = ?"
    }
}

/// This is the bulk of the actual logic.
fn do_it() -> Result<()> {
    // First, we open a database connection.
    let mut conn = Connection::open_in_memory()?;

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

    Ok(())
}

// Run it both as an example as well as during testing

fn main() -> Result<()> {
    do_it()
}

#[cfg(test)]
mod tests {
    use nanosql::Result;

    #[test]
    fn do_it() -> Result<()> {
        super::do_it()
    }
}
