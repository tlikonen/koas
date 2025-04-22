use crate::config::Config;
use futures::TryStreamExt; // STREAM.try_next()
use sqlx::{Connection, PgConnection, Row as SqlxRow};
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

pub enum EditableItem {
    None,
    Students,
    Groups(Vec<Group>),
    Assignments,
    Scores,
}

pub struct Editable {
    item: EditableItem,
}

impl Default for Editable {
    fn default() -> Self {
        Self {
            item: EditableItem::None,
        }
    }
}

impl Editable {
    pub fn clear(&mut self) {
        self.item = EditableItem::None;
    }

    pub fn is_none(&self) -> bool {
        matches!(self.item, EditableItem::None)
    }

    // pub fn is_some(&self) -> bool {
    //     !self.is_none()
    // }

    pub fn count(&self) -> usize {
        match &self.item {
            EditableItem::Groups(v) => v.len(),
            _ => 0,
        }
    }

    pub fn item_as_mut(&mut self) -> &mut EditableItem {
        &mut self.item
    }

    pub fn print_fields(&self, fields: &[&str]) {
        match self.count() {
            0 => (),
            1 => println!("Tietue: 1. Kentät: /{}", fields.join("/")),
            n => println!("Tietueet: 1–{}. Kentät: /{}", n, fields.join("/")),
        }
    }
}

pub struct Stats {
    pub students: i64,
    pub groups: i64,
    pub assignments: i64,
    pub scores: i64,
}

impl Stats {
    pub async fn query(db: &mut PgConnection) -> Result<Stats, Box<dyn Error>> {
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
}

pub struct Groups {
    pub list: Vec<Group>,
}

#[derive(Debug)]
pub struct Group {
    pub rid: i32,
    pub name: String,
    pub description: String,
}

impl Group {
    pub async fn edit(&self, db: &mut PgConnection) -> Result<(), Box<dyn Error>> {
        sqlx::query("UPDATE ryhmat SET nimi = $1, lisatiedot = $2 WHERE rid = $3")
            .bind(&self.name)
            .bind(&self.description)
            .bind(&self.rid)
            .execute(db)
            .await?;
        Ok(())
    }
}

// impl Clone for Group {
//     fn clone(&self) -> Self {
//         Self {
//             rid: self.rid,
//             name: self.name.clone(),
//             description: self.description.clone(),
//         }
//     }
// }

impl Groups {
    pub async fn query(
        db: &mut PgConnection,
        group: &str,
        desc: &str,
    ) -> Result<Groups, Box<dyn Error>> {
        let mut rows = sqlx::query(
            "SELECT rid, nimi, lisatiedot FROM ryhmat \
             WHERE nimi LIKE $1 AND lisatiedot LIKE $2 \
             ORDER BY nimi, lisatiedot, rid",
        )
        .bind(like_esc_wild(group))
        .bind(like_esc_wild(desc))
        .fetch(db);

        let mut list = Vec::new();
        while let Some(row) = rows.try_next().await? {
            list.push(Group {
                rid: row.try_get("rid")?,
                name: row.try_get("nimi")?,
                description: row.try_get("lisatiedot")?,
            });
        }

        Ok(Groups { list })
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn move_to(self, ed: &mut Editable) {
        ed.item = EditableItem::Groups(self.list);
    }
}

// if sqlx::query("SELECT 1 FROM ryhmat WHERE nimi = $1")
//     .bind("2024:suk:9a")
//     .fetch_optional(db)
//     .await?
//     .is_some()
// {
//     println!("Rivi löytyi");
// }

// Oppilashaku
// SELECT DISTINCT view_oppilaat.oid, sukunimi, etunimi, ryhmat, olt FROM view_oppilaat
// JOIN (SELECT oid, string_agg(ryhma, ' ' ORDER BY ryhma) ryhmat FROM view_oppilaat GROUP BY oid)
// ryhmat ON view_oppilaat.oid = ryhmat.oid
// WHERE ryhma LIKE '2024:%' ORDER BY sukunimi, etunimi, oid;

const LIKE_ESC_CHARS: &str = "_%\\";

fn like_esc_wild(string: &str) -> String {
    let mut str = String::with_capacity(string.len() + 2);
    str.push('%');

    for c in string.chars() {
        if LIKE_ESC_CHARS.contains(c) {
            str.push('\\');
        }
        str.push(c);
    }

    str.push('%');
    str
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_like_esc_wild() {
        assert_eq!("%abcd%", like_esc_wild("abcd"));
        assert_eq!("%a\\%b\\_cd%", like_esc_wild("a%b_cd"));
        assert_eq!("%ab\\\\cd%", like_esc_wild("ab\\cd"));
        assert_eq!("%abcd%", like_esc_wild("abcd"));
        assert_eq!("%\\_\\%\\\\%", like_esc_wild(LIKE_ESC_CHARS));
    }
}
