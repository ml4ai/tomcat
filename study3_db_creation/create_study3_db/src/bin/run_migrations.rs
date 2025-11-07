use create_study3_db::Migrator;
use futures::executor::block_on;
use sea_orm::{ConnectionTrait, Database, DbBackend, DbErr, Statement};
use sea_orm_migration::prelude::{MigratorTrait, SchemaManager};

// Change this according to your database implementation,
// or supply it as an environment variable.
// the whole database URL string follows the following format:
// "protocol://host:port/database"
// We put the database name (that last bit) in a separate variable simply for convenience.
const DATABASE_URL: &str = "postgres://localhost:5432/";
const DB_NAME: &str = "asist_study_3";

async fn run() -> Result<(), DbErr> {
    // let db = Database::connect(DATABASE_URL.to_owned() + DB_NAME).await?;
    let db = Database::connect(DATABASE_URL).await?;
    let db = &match db.get_database_backend() {
       DbBackend::MySql => {
           db.execute(Statement::from_string(
               db.get_database_backend(),
               format!("CREATE DATABASE IF NOT EXISTS `{}`;", DB_NAME),
           ))
           .await?;

           let url = format!("{}/{}", DATABASE_URL, DB_NAME);
           Database::connect(&url).await?
       }
       DbBackend::Postgres => {
           db.execute(Statement::from_string(
               db.get_database_backend(),
               format!("DROP DATABASE IF EXISTS \"{}\";", DB_NAME),
           ))
           .await?;
           db.execute(Statement::from_string(
               db.get_database_backend(),
               format!("CREATE DATABASE \"{}\";", DB_NAME),
           ))
           .await?;

           let url = format!("{}/{}", DATABASE_URL, DB_NAME);
           Database::connect(&url).await?
       }
       DbBackend::Sqlite => db,
   };

   let schema_manager = SchemaManager::new(db); // To investigate the schema

    Migrator::refresh(db).await?;
    assert!(schema_manager.has_table("team").await?);
    assert!(schema_manager.has_table("participant").await?);

    Ok(())
}

fn main() {
    if let Err(err) = block_on(run()) {
        panic!("{}", err);
    }
}
