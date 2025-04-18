mod print;

use crate::config::Config;
use futures::TryStreamExt; // STREAM.try_next()
use sqlx::{Connection, PgConnection, Row};
use std::error::Error;

pub async fn connect(config: &Config) -> Result<PgConnection, Box<dyn Error>> {
    let client = PgConnection::connect(
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
    .await?;

    Ok(client)
}

pub struct Stats {
    pub students: i64,
    pub groups: i64,
    pub assignments: i64,
    pub scores: i64,
}

pub async fn stats(db: &mut PgConnection) -> Result<Stats, Box<dyn Error>> {
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

pub struct Groups {
    pub list: Vec<Group>,
}

pub struct Group {
    pub rid: i32,
    pub name: String,
    pub description: String,
}

pub async fn groups(
    db: &mut PgConnection,
    group: &str,
    desc: &str,
) -> Result<Groups, Box<dyn Error>> {
    let mut rows = sqlx::query(
        "SELECT rid, nimi, lisatiedot FROM ryhmat \
         WHERE nimi LIKE $1 AND lisatiedot LIKE $2 \
         ORDER BY nimi, lisatiedot, rid",
    )
    .bind(like_esc(group, true))
    .bind(like_esc(desc, true))
    .fetch(db);

    let mut list = Vec::new();
    while let Some(row) = rows.try_next().await? {
        list.push(Group {
            rid: row.try_get("rid")?,
            name: row.try_get("nimi")?,
            description: row.try_get("lisatiedot")?,
        });
    }

    if list.is_empty() {
        Err("Ryhmiä ei löytynyt.".into())
    } else {
        Ok(Groups { list })
    }
}

const LIKE_ESC_CHARS: &str = "_%\\";

fn like_esc(string: &str, wild: bool) -> String {
    let mut str = String::new();
    if wild {
        str.push('%');
    }

    for c in string.chars() {
        if LIKE_ESC_CHARS.contains(c) {
            str.push('\\');
        }
        str.push(c);
    }

    if wild {
        str.push('%');
    }

    str
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_like_exc() {
        assert_eq!("abcd", like_esc("abcd", false));
        assert_eq!("a\\%b\\_cd", like_esc("a%b_cd", false));
        assert_eq!("ab\\\\cd", like_esc("ab\\cd", false));
        assert_eq!("%abcd%", like_esc("abcd", true));
        assert_eq!("\\_\\%\\\\", like_esc(LIKE_ESC_CHARS, false));
    }
}
