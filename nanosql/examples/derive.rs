use nanosql::{
    Result, Connection, ConnectionExt, AsSqlTy, ToSql, FromSql, Param, ResultRecord, Table
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, AsSqlTy, ToSql, FromSql)]
#[nanosql(rename_all = "lower_snake_case")]
enum MusicalInstrumentKind {
    Piano,
    Violin,
    Tuba,
    Harp,
    PipeOrgan,
}

#[derive(Clone, PartialEq, Eq, Debug, Param, ResultRecord, Table)]
#[nanosql(rename = "musical_instrument")]
struct MusicalInstrument {
    #[nanosql(unique)]
    id: u32,
    #[nanosql(default = "'Unknown Instrument'")]
    make: String,
    kind: MusicalInstrumentKind,
}

nanosql::define_query! {
    InsertUnknownInstrument<'p>: (u32, MusicalInstrumentKind) => () {
        "INSERT INTO musical_instrument(id, kind) VALUES (?, ?)"
    }
    InstrumentById<'p>: u32 => Option<MusicalInstrument> {
        "SELECT id, kind, make FROM musical_instrument WHERE id = ?"
    }
}

/// This is the bulk of the actual logic.
fn do_it() -> Result<()> {
    // First, we open a database connection.
    let mut conn = Connection::open_in_memory()?;

    conn.create_table::<MusicalInstrument>()?;
    conn.insert_batch([
        MusicalInstrument {
            id: 1,
            make: "Stenway Model B".into(),
            kind: MusicalInstrumentKind::Piano,
        },
        MusicalInstrument {
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
    }));
    println!("instrument ID = #137: {result:#?}");

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
