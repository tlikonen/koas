use crate::{Modes, database as db, tools};
use sqlx::PgConnection;
use std::error::Error;

pub async fn stats(_modes: &Modes, db: &mut PgConnection) -> Result<(), Box<dyn Error>> {
    let stats = db::stats(db).await?;
    println!("{:?}", stats);
    Ok(())
}

pub async fn groups(
    _modes: &Modes,
    db: &mut PgConnection,
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    // args: /ryhmä/lisätiedot
    if args.is_empty() {
        args = "/";
    }

    let mut split = tools::split_sep(args);
    let group = split.next().unwrap_or("");
    let desc = split.next().unwrap_or("");

    let groups = db::groups(db, group, desc).await?;
    println!("{:?}", groups);
    Ok(())
}
