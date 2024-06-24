use std::collections::BTreeSet;
use nanosql::{define_query, Result, Error, error::RowCount};
use nanosql::{Table, Param, ResultRecord, Connection, ConnectionExt, Single};


#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Table, Param, ResultRecord)]
struct Fruit {
    #[nanosql(pk)]
    name: String,
    color: String,
    energy: u32,
}

const SQL: &str = "SELECT name, color, energy FROM fruit ORDER BY name LIMIT ?";

define_query!{
    OptQueryWithLimit<'p>: usize => Option<Fruit> {
        SQL
    }
    SingleQueryWithLimit<'p>: usize => Single<Fruit> {
        SQL
    }
    ArrayQueryWithLimit<'p>: usize => [Fruit; 4] {
        SQL
    }
    BTreeSetQueryWithLimit<'p>: usize => BTreeSet<u32> {
        "SELECT energy FROM fruit ORDER BY name LIMIT ?"
    }
}

fn setup() -> Result<Connection> {
    let mut conn = Connection::connect_in_memory()?;

    conn.create_table::<Fruit>()?;
    conn.insert_batch([
        Fruit {
            name: "banana".into(),
            color: "yellow".into(),
            energy: 89,
        },
        Fruit {
            name: "apple".into(),
            color: "red".into(),
            energy: 95,
        },
        Fruit {
            name: "avocado".into(),
            color: "green".into(),
            energy: 160,
        },
        Fruit {
            name: "orange".into(),
            color: "orange".into(),
            energy: 45,
        },
        Fruit {
            name: "plum".into(),
            color: "blue".into(),
            energy: 120,
        },
    ])?;

    Ok(conn)
}

#[test]
fn result_record_option() -> Result<()> {
    let conn = setup()?;
    let mut stmt = conn.compile(OptQueryWithLimit)?;

    assert!(stmt.invoke(0)?.is_none());

    assert_eq!(
        stmt.invoke(1)?,
        Some(Fruit {
            name: "apple".into(),
            color: "red".into(),
            energy: 95,
        })
    );

    assert!(matches!(
        stmt.invoke(2),
        Err(Error::RowCountMismatch {
            expected: RowCount { min: 0, max: Some(1) },
            actual: RowCount { min: 2, max: None },
        })
    ));
    assert!(matches!(
        stmt.invoke(3),
        Err(Error::RowCountMismatch {
            expected: RowCount { min: 0, max: Some(1) },
            actual: RowCount { min: 2, max: None },
        })
    ));

    Ok(())
}

#[test]
fn result_record_single() -> Result<()> {
    let conn = setup()?;
    let mut stmt = conn.compile(SingleQueryWithLimit)?;

    assert!(matches!(
        stmt.invoke(0),
        Err(Error::RowCountMismatch {
            expected: RowCount { min: 1, max: Some(1) },
            actual: RowCount { min: 0, max: Some(0) },
        })
    ));
    assert_eq!(
        stmt.invoke(1)?,
        Single(Fruit {
            name: "apple".into(),
            color: "red".into(),
            energy: 95,
        })
    );
    assert!(matches!(
        stmt.invoke(2),
        Err(Error::RowCountMismatch {
            expected: RowCount { min: 1, max: Some(1) },
            actual: RowCount { min: 2, max: None },
        })
    ));
    assert!(matches!(
        stmt.invoke(3),
        Err(Error::RowCountMismatch {
            expected: RowCount { min: 1, max: Some(1) },
            actual: RowCount { min: 2, max: None },
        })
    ));

    Ok(())
}

#[test]
fn result_record_array() -> Result<()> {
    let conn = setup()?;
    let mut stmt = conn.compile(ArrayQueryWithLimit)?;

    assert_eq!(
        stmt.invoke(4)?,
        [
            Fruit {
                name: "apple".into(),
                color: "red".into(),
                energy: 95,
            },
            Fruit {
                name: "avocado".into(),
                color: "green".into(),
                energy: 160,
            },
            Fruit {
                name: "banana".into(),
                color: "yellow".into(),
                energy: 89,
            },
            Fruit {
                name: "orange".into(),
                color: "orange".into(),
                energy: 45,
            },
        ]
    );
    assert!(matches!(
        stmt.invoke(3),
        Err(Error::RowCountMismatch {
            expected: RowCount { min: 4, max: Some(4) },
            actual: RowCount { min: 3, max: Some(3) },
        })
    ));
    assert!(matches!(
        stmt.invoke(5),
        Err(Error::RowCountMismatch {
            expected: RowCount { min: 4, max: Some(4) },
            actual: RowCount { min: 5, max: None },
        })
    ));

    Ok(())
}

#[test]
fn result_record_btreeset() -> Result<()> {
    let conn = setup()?;
    let mut stmt = conn.compile(BTreeSetQueryWithLimit)?;

    assert_eq!(stmt.invoke(4)?, BTreeSet::from_iter([45, 89, 95, 160]));

    Ok(())
}
