// m20251107_000002_create_participant_table.rs

use sea_orm_migration::prelude::*;

use super::m20251107_000001_create_team_table::Team;

pub struct Migration;

impl MigrationName for Migration {
    fn name(&self) -> &str {
        "m20251107_000002_create_participant_table" // Make sure this matches with the file name
    }
}

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    // Define how to apply this migration: Create the Participant table.
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Participant::Table)
                    .col(
                        ColumnDef::new(Participant::Id)
                            .string()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Participant::TeamId).string().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk-participant-team_id")
                            .from(Participant::Table, Participant::TeamId)
                            .to(Team::Table, Team::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    // Define how to rollback this migration: Drop the Participant table.
    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Participant::Table).to_owned())
            .await
    }
}

// For ease of access
#[derive(Iden)]
pub enum Participant {
    Table,
    Id,
    TeamId,
}

