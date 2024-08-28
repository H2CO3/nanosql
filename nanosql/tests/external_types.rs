use nanosql::{define_query, Connection, ConnectionExt, Table, Param, ResultRecord};
use nanosql::{Error, Result, Single, Value, ValueRef, ToSqlOutput};
use nanosql::{DateTime, Utc, FixedOffset, Uuid, JsonValue};


#[derive(Clone, PartialEq, Debug, Table, Param, ResultRecord)]
struct MyTable {
    value: Value,
    json: JsonValue,
    date: DateTime<Utc>,
    uuid: Uuid,
}

define_query!{
    TestDateTimeRoundTrip<'lt>: DateTime<FixedOffset> => Single<DateTime<FixedOffset>> {
        "SELECT ?"
    }
    TestDateTimeText<'lt>: DateTime<FixedOffset> => Single<String> {
        "SELECT ?"
    }
    TestUuidRoundTrip<'lt>: Uuid => Single<Uuid> {
        "SELECT ?"
    }
    TestUuidBlob<'lt>: Uuid => Single<[u8; 16]> {
        "SELECT ?"
    }
    TestJsonValueRoundTrip<'lt>: &'lt JsonValue => Single<JsonValue> {
        "SELECT ?"
    }
    TestSqlValueRoundTrip<'val>: ValueRef<'val> => Single<Value> {
        "SELECT ?"
    }
    TestToSqlOutputRoundTrip<'sql>: ToSqlOutput<'sql> => Single<ToSqlOutput<'static>> {
        "SELECT ?"
    }
}

#[test]
fn external_types() -> Result<()> {
    let conn = Connection::connect_in_memory()?;

    let date_str = "2021-07-13 18:39:06+02:30";
    let uuid_bytes = *b"\x65\x51\x7e\x0b\x04\x72\x45\x36\xa4\x9e\xc9\x46\xcb\xd9\xa1\x6a";
    let json_str = r#"{"foo": 42, "bar": "qux"}"#;

    let date: DateTime<FixedOffset> = date_str.parse().map_err(Error::other)?;
    let uuid = Uuid::from_bytes(uuid_bytes);
    let json_value: JsonValue = serde_json::from_str(json_str).map_err(Error::other)?;

    assert!(json_value.is_object());

    assert_eq!(
        conn.compile_invoke(TestDateTimeRoundTrip, date)?.0,
        date,
    );
    assert_eq!(
        conn.compile_invoke(TestDateTimeText, date)?.0,
        date_str,
    );
    assert_eq!(
        conn.compile_invoke(TestUuidRoundTrip, uuid)?.0,
        uuid,
    );
    assert_eq!(
        conn.compile_invoke(TestUuidBlob, uuid)?.0,
        uuid_bytes,
    );
    assert_eq!(
        conn.compile_invoke(TestJsonValueRoundTrip, &JsonValue::Null)?.0,
        JsonValue::Null,
    );
    assert_eq!(
        conn.compile_invoke(TestJsonValueRoundTrip, &JsonValue::from(true))?.0,
        JsonValue::from(true),
    );
    assert_eq!(
        conn.compile_invoke(TestJsonValueRoundTrip, &JsonValue::from(1337_i64))?.0,
        JsonValue::from(1337_i64),
    );
    assert_eq!(
        conn.compile_invoke(TestJsonValueRoundTrip, &JsonValue::from(42.5_f64))?.0,
        JsonValue::from(42.5_f64),
    );
    assert_eq!(
        conn.compile_invoke(TestJsonValueRoundTrip, &JsonValue::from("some string"))?.0,
        JsonValue::from("some string"),
    );
    assert_eq!(
        conn.compile_invoke(TestJsonValueRoundTrip, &json_value)?.0,
        json_value,
    );

    Ok(())
}

#[test]
fn native_sql_value() -> Result<()> {
    let conn = Connection::connect_in_memory()?;

    assert_eq!(
        conn.compile_invoke(TestSqlValueRoundTrip, ValueRef::Null)?.0,
        Value::Null,
    );
    assert_eq!(
        conn.compile_invoke(TestSqlValueRoundTrip, ValueRef::Integer(9876543210))?.0,
        Value::Integer(9876543210),
    );
    assert_eq!(
        conn.compile_invoke(TestSqlValueRoundTrip, ValueRef::Real(1024.375))?.0,
        Value::Real(1024.375),
    );
    assert_eq!(
        conn.compile_invoke(TestSqlValueRoundTrip, ValueRef::Text(b"some text"))?.0,
        Value::Text("some text".to_owned()),
    );
    assert_eq!(
        conn.compile_invoke(TestSqlValueRoundTrip, ValueRef::Blob(b"some blob"))?.0,
        Value::Blob(b"some blob".to_vec()),
    );
    assert_eq!(
        conn.compile_invoke(TestToSqlOutputRoundTrip, ToSqlOutput::from(12_u32))?.0,
        ToSqlOutput::from(12_u32),
    );

    Ok(())
}

#[test]
fn table_with_extern_and_dynamic_types() -> Result<()> {
    let mut conn = Connection::connect_in_memory()?;
    let original_rows = [
        MyTable {
            value: Value::Text("some string".to_owned()),
            json: JsonValue::from(1994_i16),
            date: "1972-10-28 13:37:29Z".parse().unwrap(),
            uuid: "591A864D-C0B4-4658-A8C6-EEB273260F85".parse().unwrap(),
        },
        MyTable {
            value: Value::Blob(b"the blob contents".to_vec()),
            json: serde_json::from_str(r#"[2, 4, 7, -5, 0, 1]"#).unwrap(),
            date: "1994-01-11 11:45:36+00:00".parse().unwrap(),
            uuid: "3483347c-8450-4745-8c9f-766eda7f303b".parse().unwrap(),
        },
    ];

    conn.create_table::<MyTable>()?;
    let inserted_rows = conn.insert_batch(original_rows.clone())?;

    let mut retrieved_rows: Vec<MyTable> = conn.select_all()?;
    retrieved_rows.sort_by_key(|row| row.date);

    assert_eq!(inserted_rows, original_rows);
    assert_eq!(retrieved_rows, original_rows);

    Ok(())
}
