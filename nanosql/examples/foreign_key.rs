use nanosql::{Result, Query, CreateTable};
use nanosql::{Param, ResultRecord, Table};


/// The good ol' boring example of employees and departments in a company.
#[derive(Clone, Debug, Param, ResultRecord, Table)]
struct Department {
    #[nanosql(pk)]
    department_id: i64,
    title: String,
    established_year: u16,
}

#[derive(Clone, Debug, Param, ResultRecord, Table)]
struct Employee {
    #[nanosql(pk)]
    employee_id: i64,
    full_name: String,
    #[nanosql(foreign_key = Department::department_id)]
    employing_department_id: i64,
    /// The general director has no boss, has a boss ID of NULL.
    /// The `foreign_key` attribute has an alias `fk` too.
    #[nanosql(fk = Employee::employee_id)]
    boss_employee_id: Option<i64>,
}

fn do_it() -> Result<()> {
    let query = CreateTable::<Employee>::default();
    let sql = query.sql()?.as_ref().to_string();

    println!("{sql}");

    assert!(
        sql.contains(
            r#""employing_department_id" INTEGER NOT NULL REFERENCES "Department"("department_id")"#
        )
    );
    assert!(
        sql.contains(
            r#""boss_employee_id" INTEGER NULL REFERENCES "Employee"("employee_id")"#
        )
    );

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
