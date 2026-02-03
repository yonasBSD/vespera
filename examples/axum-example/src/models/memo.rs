use sea_orm::entity::prelude::*;

#[sea_orm::model]
#[derive(Clone, Debug, PartialEq, Eq, DeriveEntityModel)]
#[sea_orm(table_name = "memo")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i32,
    #[sea_orm(indexed)]
    pub user_id: i32,
    pub title: String,
    pub content: String,
    #[sea_orm(default_value = "NOW()")]
    pub created_at: DateTimeWithTimeZone,
    #[sea_orm(default_value = "NOW()")]
    pub updated_at: DateTimeWithTimeZone,
    #[sea_orm(belongs_to, from = "user_id", to = "id")]
    pub user: HasOne<super::user::Entity>,
}

// Schema WITH user relation - has async from_model(model, db) method
// Circular refs auto-handled: when loading user, its memos field is set to vec![]
vespera::schema_type!(Schema from crate::models::memo::Model, name = "MemoSchema");

// Index definitions (SeaORM uses Statement builders externally)
// (unnamed) on [user_id]
impl ActiveModelBehavior for ActiveModel {}
