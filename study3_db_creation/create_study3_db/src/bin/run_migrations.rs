use create_study3_db::Migrator;
use futures::executor::block_on;
use sea_orm::{ConnectionTrait, Database, DbBackend, DbErr, Statement};
use sea_orm_migration::prelude::{MigratorTrait, SchemaManager};

// Change this according to your database implementation,
// or supply it as an environment variable.
// the whole database URL string follows the following format:
// "protocol://host:port/database"
// We put the database name (that last bit) in a separate variable simply for convenience.
const DATABASE_URL: &str = "postgres://adarsh@localhost:5432/";
const DB_NAME: &str = "bakeries_db";

async fn run() -> Result<(), DbErr> {
    let db = Database::connect(DATABASE_URL.to_owned() + DB_NAME).await?;
    let schema_manager = SchemaManager::new(&db); // To investigate the schema

    Migrator::refresh(&db).await?;
    assert!(schema_manager.has_table("bakery").await?);
    assert!(schema_manager.has_table("chef").await?);

    Ok(())
}

fn main() {
    if let Err(err) = block_on(run()) {
        panic!("{}", err);
    }
}
