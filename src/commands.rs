use crate::{Modes, database as db};
use sqlx::PgConnection;
use std::error::Error;

pub async fn stats(modes: &Modes, db: &mut PgConnection) -> Result<(), Box<dyn Error>> {
    let stats = db::stats(db).await?;
    stats.print(modes.output());
    Ok(())
}
