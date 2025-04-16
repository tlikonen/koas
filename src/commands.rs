use crate::{Modes, database as db};
use sqlx::PgConnection;
use std::error::Error;

pub async fn stats(_modes: &Modes, db: &mut PgConnection) -> Result<(), Box<dyn Error>> {
    let stats = db::stats(db).await?;
    println!("{:?}", stats);
    Ok(())
}
