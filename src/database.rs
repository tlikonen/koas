use crate::{Output, config::Config};
// use futures::TryStreamExt; // STREAM.try_next()
use sqlx::{Connection, Executor, PgConnection as DB, Row};

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

pub async fn begin_transaction(db: &mut DB) -> Result<(), sqlx::Error> {
    db.execute("BEGIN").await?;
    Ok(())
}

pub async fn commit_transaction(db: &mut DB) -> Result<(), sqlx::Error> {
    db.execute("COMMIT").await?;
    Ok(())
}

pub async fn rollback_transaction(db: &mut DB) -> Result<(), sqlx::Error> {
    db.execute("ROLLBACK").await?;
    Ok(())
}

// pub async fn test(db: &mut DB, line: &str) -> Result<(), sqlx::Error> {
//     let mut rows = sqlx::query("SELECT id, arvo FROM foo WHERE arvo LIKE $1")
//         .bind(format!("%{line}%"))
//         .fetch(db);
//     while let Some(row) = rows.try_next().await? {
//         let id: i32 = row.try_get("id")?;
//         let arvo: &str = row.try_get("arvo")?;
//         println!("{id} {arvo}");
//     }
//     Ok(())
// }

pub struct Stats {
    pub students: i64,
    pub groups: i64,
    pub assignments: i64,
    pub scores: i64,
}

impl Stats {
    pub fn print(&self, _output: &Output) {
        println!(
            "Oppilaita:   {o}\n\
             Ryhmiä:      {r}\n\
             Suorituksia: {s}\n\
             Arvosanoja:  {a}",
            o = self.students,
            r = self.groups,
            s = self.assignments,
            a = self.scores,
        );
    }
}

pub async fn stats(db: &mut DB) -> Result<Stats, sqlx::Error> {
    let row = sqlx::query(
        "SELECT \
         (SELECT count(*) FROM oppilaat) oppilaat, \
         (SELECT count(*) FROM ryhmat) ryhmat, \
         (SELECT count(*) FROM suoritukset) suoritukset, \
         (SELECT count(*) FROM arvosanat WHERE arvosana LIKE '_%') arvosanat",
    )
    .fetch_one(db)
    .await?;

    Ok(Stats {
        students: row.try_get("oppilaat")?,
        groups: row.try_get("ryhmat")?,
        assignments: row.try_get("suoritukset")?,
        scores: row.try_get("arvosanat")?,
    })
}
