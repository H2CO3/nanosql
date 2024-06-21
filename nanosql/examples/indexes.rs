use nanosql::{Error, Result, Connection, ConnectionExt, Param, ResultRecord, Table, Query};
use nanosql::table::{SortOrder, TableIndexSpec};


/// This table has a column-level index which is partial.
#[derive(Clone, Debug, Param, Table, ResultRecord)]
#[nanosql(pk = [id])]
struct Author {
    id: i32,
    #[nanosql(index(desc, where = "name IS NOT NULL"))]
    name: Option<String>,
}

/// This one has multiple column-level indexes, one of them is explicitly ascending
/// and unique as well. Many general indexes can co-exist (unlike the PRIMARY KEY).
#[derive(Clone, Debug, Param, Table, ResultRecord)]
struct Publication {
    #[nanosql(pk)]
    id: i64,
    #[nanosql(index(unique, asc))]
    isbn: String,
    #[nanosql(index)]
    title: String,
}

/// Finally, this table has both a table-level and many column-level
/// index specifications, the latter containing explicit as well as
/// implicit indexes. All of these can peacefully co-exist as well.
///
/// The name of this table is intentionally not `AuthorPublication`
/// or something like that, in order not to be a substring of the
/// names of the other two tables. This makes testing easier and
/// more robust.
#[derive(Clone, Debug, Param, Table, ResultRecord)]
#[nanosql(
    index(
        unique,
        columns(author_id /* = asc */, publication_id = desc),
        where = "author_id IS NOT NULL",
    )
)]
struct WrittenBy {
    #[nanosql(fk = Author::id)]
    author_id: Option<i32>,
    #[nanosql(fk = Publication::id)]
    publication_id: i64,
    #[nanosql(index(unique = false, desc))]
    timestamp: f64,
}

/// This is just to exercise the ability to add multiple table-level
/// indexes, as well as to ensure that a missing `where` predicate
/// doesn't cause an inference error due to `None` missing a type.
///
/// We use a column (`big_blob`) in both indexes, and we swap the
/// indexing order of the columns compared to their declared order
/// in the struct definition.
///
/// We also make different syntactical choices in representing the
/// attributes, so we can ensure that this syntax is accepted, too.
/// (For example, no trailing comma, column names as strings, etc.)
///
/// This table intentionally doesn't contain any unique constraints,
/// primary keys, foreign keys, or anything else that would add an
/// implicit index, so we can rely on there being exactly 2 indexes.
#[derive(Clone, Debug, Param, Table, ResultRecord)]
#[nanosql(
    index(columns(big_blob, string)),
    index(
        unique = true,
        columns("number" = desc, "big_blob"),
        where = "number > 0"
    )
)]
struct ComplexTable {
    string: String,
    big_blob: Vec<u8>,
    number: i64,
}

fn do_it() -> Result<()> {
    let mut conn = Connection::open_in_memory()?;

    conn.create_table::<Author>()?;
    conn.create_table::<Publication>()?;
    conn.create_table::<WrittenBy>()?;

    conn.insert_batch([
        Author {
            id: 1,
            name: Some("Don Knuth".into()),
        },
        Author {
            id: 2,
            name: None,
        },
        Author {
            id: 3,
            name: Some("Edsger Dijkstra".into()),
        },
        Author {
            id: 5,
            name: Some("William Shakespeare".into()),
        },
        Author {
            id: 8,
            name: None,
        },
    ])?;
    conn.insert_batch([
        Publication {
            id: 1,
            isbn: "9780394530277".into(), // hard cover
            title: "Romeo and Juliet".into(),
        },
        Publication {
            id: 2,
            isbn: "0-201-03801-3".into(),
            title: "The Art of Computer Programming".into(),
        },
        Publication {
            id: 30,
            isbn: "9780140707014".into(), // paperback
            title: "Romeo and Juliet".into(),
        },
        Publication {
            id: 31,
            isbn: "9790001056564".into(),
            title: "Carmina Burana".into(),
        },
    ])?;
    conn.insert_batch([
        WrittenBy {
            author_id: Some(5),
            publication_id: 30,
            timestamp: 1718976265.0,
        },
        WrittenBy {
            author_id: Some(1),
            publication_id: 2,
            timestamp: 1718979876.2,
        },
        WrittenBy {
            author_id: Some(5),
            publication_id: 1,
            timestamp: 1718971928.9,
        },
        WrittenBy {
            author_id: None,
            publication_id: 31,
            timestamp: 1718975432.3,
        },
    ])?;

    // Inserting a duplicate ISBN violates the UNIQUE INDEX on Publication.
    let result = conn.insert_batch([
        Publication {
            id: 239,
            isbn: "0-201-03801-3".into(),
            title: "Different Title with Same ISBN".into(),
        },
    ]);
    let error = result.unwrap_err();
    let err_msg = error.to_string();

    assert!(matches!(error, Error::Sqlite(_)));
    assert!(err_msg.contains("UNIQUE"));
    assert!(err_msg.contains("Publication"));
    assert!(err_msg.contains("isbn"));

    // WrittenBy can contain duplicate entries if the author is NULL.
    // Here we pretend that Carmina Burana has multiple, unknown authors.
    // (It probably does, in fact.)
    conn.insert_batch([
        WrittenBy {
            author_id: None,
            publication_id: 31,
            timestamp: 1718971928.9,
        },
    ])?;

    // But if the author exists, the entry can't be duplicated due to the
    // UNIQUE constraint on the partial index.
    let result = conn.insert_batch([
        WrittenBy {
            author_id: Some(5),
            publication_id: 1,
            timestamp: 1718978493.0, // other indexed column is unique -> it can't cause the error
        },
    ]);
    let error = result.unwrap_err();
    let err_msg = error.to_string();

    assert!(err_msg.contains("UNIQUE"));
    assert!(err_msg.contains("WrittenBy"));
    assert!(err_msg.contains("author_id"));
    assert!(err_msg.contains("publication_id"));
    assert!(!err_msg.contains("timestamp"));

    // Check the actual generated SQL for properties that we can't
    // otherwise verify: i.e., total/partial or the sorting order.
    let author_index = Author::description()
        .index_specs()
        .into_iter()
        .find(|spec| spec.columns.len() == 1 && spec.columns[0].0 == "name")
        .expect("Author::name has no index spec");

    let author_index_sql = author_index.display_sql().to_string();

    assert_eq!(author_index.table, "Author");
    assert_eq!(author_index.unique, false);
    assert_eq!(author_index.columns[0].1, SortOrder::Descending);
    assert_eq!(author_index.predicate.as_deref(), Some("name IS NOT NULL"));

    assert!(author_index_sql.contains("CREATE INDEX"));
    assert!(author_index_sql.contains(r#""name" DESC"#));
    assert!(author_index_sql.contains("name IS NOT NULL"));

    let publication_indexes = Publication::description().index_specs();
    let publication_isbn_index = publication_indexes
        .iter()
        .find(|spec| spec.columns.len() == 1 && spec.columns[0].0 == "isbn")
        .expect("Publication::isbn has no index spec");
    let publication_title_index = publication_indexes
        .iter()
        .find(|spec| spec.columns.len() == 1 && spec.columns[0].0 == "title")
        .expect("Publication::title has no index spec");

    assert_eq!(publication_isbn_index.table, "Publication");
    assert_eq!(publication_isbn_index.unique, true);
    assert_eq!(publication_isbn_index.columns[0].1, SortOrder::Ascending);
    assert_eq!(publication_isbn_index.predicate, None);

    assert_eq!(publication_title_index.table, "Publication");
    assert_eq!(publication_title_index.unique, false);
    assert_eq!(publication_title_index.columns[0].1, SortOrder::Ascending);
    assert_eq!(publication_title_index.predicate, None);

    let written_by_indexes = WrittenBy::description().index_specs();
    let written_by_multicol_index = written_by_indexes
        .iter()
        .find(|spec| {
            spec.columns.len() == 2 &&
            spec.columns[0].0 == "author_id" &&
            spec.columns[1].0 == "publication_id"
        })
        .expect("WrittenBy has no index on (author_id, publication_id)");
    let written_by_timestamp_index = written_by_indexes
        .iter()
        .find(|spec| spec.columns.len() == 1 && spec.columns[0].0 == "timestamp")
        .expect("WrittenBy::timestamp has no index spec");

    assert_eq!(written_by_multicol_index.table, "WrittenBy");
    assert_eq!(written_by_multicol_index.unique, true);
    assert_eq!(written_by_multicol_index.columns[0].1, SortOrder::Ascending);
    assert_eq!(written_by_multicol_index.columns[1].1, SortOrder::Descending);
    assert_eq!(written_by_multicol_index.predicate.as_deref(), Some("author_id IS NOT NULL"));

    assert_eq!(written_by_timestamp_index.table, "WrittenBy");
    assert_eq!(written_by_timestamp_index.unique, false);
    assert_eq!(written_by_timestamp_index.columns[0].1, SortOrder::Descending);
    assert_eq!(written_by_timestamp_index.predicate, None);

    let complex_table_indexes = ComplexTable::description().index_specs();

    // including the index IDs is somewhat relying on an implementation
    // detail, but we can just update this test if this ever changes.
    assert_eq!(complex_table_indexes, [
        TableIndexSpec {
            table: "ComplexTable".into(),
            id: 1,
            unique: false,
            columns: vec![
                ("big_blob".into(), SortOrder::Ascending),
                ("string".into(), SortOrder::Ascending),
            ],
            predicate: None,
        },
        TableIndexSpec {
            table: "ComplexTable".into(),
            id: 2,
            unique: true,
            columns: vec![
                ("number".into(), SortOrder::Descending),
                ("big_blob".into(), SortOrder::Ascending),
            ],
            predicate: Some("number > 0".into()),
        },
    ]);

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
