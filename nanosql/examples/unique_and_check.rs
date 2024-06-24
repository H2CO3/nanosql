use nanosql::{Error, Result, Connection, ConnectionExt, Param, Table};


#[derive(Clone, Debug, Param, Table)]
#[nanosql(unique = ["city", street_name, "building_no"])]
#[nanosql(check = "floor BETWEEN -10 AND 100", check = "building_no < 1000")]
pub struct Address {
    country: String,
    city: String,
    street_name: String,
    building_no: u32,
    floor: i16,
}

fn main() -> Result<()> {
    let mut conn = Connection::connect_in_memory()?;

    conn.create_table::<Address>()?;
    conn.insert_batch([
        Address {
            country: "Italy".into(),
            city: "Padova".into(),
            street_name: "Via Proust".into(),
            building_no: 15,
            floor: 4,
        },
        Address {
            country: "Italy".into(),
            city: "Padova".into(),
            street_name: "Via Proust".into(),
            building_no: 16,
            floor: 4,
        },
        Address {
            country: "Italy".into(),
            city: "Milano".into(),
            street_name: "Via Proust".into(),
            building_no: 16,
            floor: 4,
        },
    ])?;

    // this should violate the UNIQUE constraint
    let result = conn.insert_batch([
        Address {
            country: "Hungary".into(),
            city: "Milano".into(),
            street_name: "Via Proust".into(),
            building_no: 16,
            floor: 3,
        },
    ]);
    let error = result.unwrap_err();

    assert!(matches!(error, Error::Sqlite(_)));
    assert!(error.to_string().contains("UNIQUE constraint"));

    // this should violate the CHECK constraint on the `building_no` column
    let result = conn.insert_batch([
        Address {
            country: "Poland".into(),
            city: "Warsaw".into(),
            street_name: "Swietojanska Street".into(),
            building_no: 1001,
            floor: 9,
        },
    ]);
    let error = result.unwrap_err();
    let err_msg = error.to_string();

    assert!(matches!(error, Error::Sqlite(_)));
    assert!(err_msg.contains("CHECK constraint"));
    assert!(err_msg.contains("building_no"));

    // this should violate the CHECK constraint on the `floor` column
    let result = conn.insert_batch([
        Address {
            country: "Poland".into(),
            city: "Warsaw".into(),
            street_name: "Swietojanska Street".into(),
            building_no: 123,
            floor: 987,
        },
    ]);
    let error = result.unwrap_err();
    let err_msg = error.to_string();

    assert!(matches!(error, Error::Sqlite(_)));
    assert!(err_msg.contains("CHECK constraint"));
    assert!(err_msg.contains("floor"));

    Ok(())
}
