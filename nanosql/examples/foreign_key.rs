use nanosql::{Result, Query, Connection, ConnectionExt};
use nanosql::{Param, ResultRecord, Table, table::{CreateTable, TableConstraint}};


/// The good ol' boring example of employees and departments in a company.
#[derive(Clone, Debug, Param, ResultRecord, Table)]
struct Department {
    #[nanosql(pk)]
    department_id: i64,
    title: String,
    established_year: u16,
}

#[derive(Clone, Debug, Param, ResultRecord, Table)]
#[nanosql(fk("bogus_table" => ("some_fk_2" = "some_col", "some_fk_1" = "other_col")))]
struct Employee {
    #[nanosql(pk)]
    employee_id: i64,
    full_name: String,
    /// The general director has no boss (its boss ID is NULL).
    ///
    /// The `foreign_key` attribute has an alias `fk` too.
    ///
    /// For consistency with table-level FKs, you can specify an FK
    /// either as `fk(foreign_table => foreign_column)`, ...
    #[nanosql(fk("Employee" => "employee_id"))]
    boss_employee_id: Option<i64>,
    /// or as a Rust path, like `fk = foreign_table::foreign_column`.
    /// Both syntaxes have the same meaning. Instead of identifiers,
    /// you can use string literals if your table or column name is
    /// not a valid Rust identifier. The string literal syntax works
    /// better with the `=>` separator (see above), and identifiers
    /// look nicer when used with the path notation.
    #[nanosql(foreign_key = Department::department_id)]
    employing_department_id: i64,
    /// Fields for testing the table-level foreign key annotations.
    /// This column is `unique` for 2 reasons:
    /// 1. to test that this does not conflict with the FK and its index, and
    /// 2. to have an `sqlite_autoindex` so we can check filtering it out below
    #[nanosql(unique)]
    some_fk_1: u32,
    some_fk_2: bool,
}

#[derive(Clone, Debug, Param, ResultRecord, Table)]
#[nanosql(pk = [some_col, other_col], rename = bogus_table)]
struct BogusTable {
    some_col: bool,
    other_col: u32,
}

nanosql::define_query!{
    /// Internal helper query for gathering information about indexes
    #[derive(Clone, Copy, Default, Debug)]
    ListTableIndexes<'p>: &'p str => Vec<Option<String>> {
        "SELECT sql FROM sqlite_schema WHERE type = 'index' AND tbl_name = ?"
    }
}

fn main() -> Result<()> {
    let query = CreateTable::<Employee>::default();
    let sql_disp = query.display_sql();
    let sql = sql_disp.to_string();

    println!("{sql_disp}");

    assert!(
        sql.contains(
            r#""employing_department_id" INTEGER NOT NULL REFERENCES "Department"("department_id") DEFERRABLE INITIALLY DEFERRED"#
        )
    );
    assert!(
        sql.contains(
            r#""boss_employee_id" INTEGER NULL REFERENCES "Employee"("employee_id") DEFERRABLE INITIALLY DEFERRED"#
        )
    );

    let desc = Employee::description();
    let fk_sql = desc.constraints
        .iter()
        .find(|c| matches!(c, TableConstraint::ForeignKey { .. }))
        .unwrap()
        .to_string();

    assert!(fk_sql.contains(
        r#"FOREIGN KEY("some_fk_2", "some_fk_1") REFERENCES "bogus_table"("some_col", "other_col") DEFERRABLE INITIALLY DEFERRED"#
    ));

    // Ensure that the correct indexes are automatically created for the FOREIGN KEY columns.
    let mut conn = Connection::connect_in_memory()?;

    conn.create_table::<Department>()?;
    conn.create_table::<BogusTable>()?;
    conn.create_table::<Employee>()?;

    let mut stmt = conn.compile(ListTableIndexes)?;
    let index_sqls = stmt.invoke("Employee")?;

    for sql in index_sqls {
        // Ignore SQLite-created automatic indexes. We must make this
        // column optional because auto-indexes have their SQL field set to NULL)
        let Some(sql) = sql else {
            println!("Found SQLite auto-index, skipping");
            continue;
        };

        println!("{sql}");

        assert!(sql.contains("CREATE INDEX \"__nanosql_index_Employee_"));
        assert!(
            (
                sql.contains(r#""boss_employee_id" ASC"#)
                &&
                !sql.contains("employing_department_id")
                &&
                !sql.contains("some_fk_1")
                &&
                !sql.contains("some_fk_2")
            ) || (
                sql.contains(r#""employing_department_id" ASC"#)
                &&
                !sql.contains("boss_employee_id")
                &&
                !sql.contains("some_fk_1")
                &&
                !sql.contains("some_fk_2")
            ) || (
                sql.contains(r#""some_fk_1" ASC"#)
                &&
                sql.contains(r#""some_fk_2" ASC"#)
                &&
                !sql.contains("boss_employee_id")
                &&
                !sql.contains("employing_department_id")
            )
        );
    }

    Ok(())
}
