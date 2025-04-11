use crate::config::Config;
use futures::TryStreamExt;
use sqlx::{Connection, PgConnection as DB, Row};

pub async fn connect(config: &Config) -> Result<DB, String> {
    let client = DB::connect(
        format!(
            "postgres://{user}:{password}@{host}:{port}/{db}",
            user = config.user,
            password = config.password,
            host = config.host,
            port = config.port,
            db = config.database,
        )
        .as_str(),
    )
    .await
    .map_err(|e| format!("{e}"))?;

    Ok(client)
}

pub async fn test(db: &mut DB, line: &str) -> Result<(), sqlx::Error> {
    let mut rows = sqlx::query("SELECT id, arvo FROM foo WHERE arvo LIKE $1")
        .bind(format!("%{line}%"))
        .fetch(db);

    while let Some(row) = rows.try_next().await? {
        let id: i32 = row.try_get("id")?;
        let arvo: &str = row.try_get("arvo")?;
        println!("{id} {arvo}");
    }

    Ok(())
}
