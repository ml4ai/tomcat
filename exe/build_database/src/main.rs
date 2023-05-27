use rusqlite::{params, Connection, Result};

#[derive(Debug)]
struct Team {
    id: i32,
    score: u32,
}

fn main() -> Result<()> {
    let conn = Connection::open("test.db")?;

    conn.execute(
        "CREATE TABLE IF NOT EXISTS team (
            id   INTEGER PRIMARY KEY,
            score INTEGER
        )",
        (), // empty list of parameters.
    )?;


    //let me = Person {
        //id: 0,
        //name: "Steven".to_string(),
        //data: None,
    //};
    //conn.execute(
        //"INSERT INTO person (name, data) VALUES (?1, ?2)",
        //(&me.name, &me.data),
    //)?;

    //let mut stmt = conn.prepare("SELECT id, name, data FROM person")?;
    //let person_iter = stmt.query_map([], |row| {
        //Ok(Person {
            //id: row.get(0)?,
            //name: row.get(1)?,
            //data: row.get(2)?,
        //})
    //})?;

    //for person in person_iter {
        //println!("Found person {:?}", person.unwrap());
    //}
    Ok(())
}
