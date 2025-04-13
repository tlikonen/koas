use crate::{Modes, database as db};
use sqlx::PgConnection;

pub async fn stats(modes: &Modes, db: &mut PgConnection) -> Result<(), String> {
    let err = |e| format!("{e}");
    let stats = db::stats(db).await.map_err(err)?;
    stats.print(modes.output());
    Ok(())
}
