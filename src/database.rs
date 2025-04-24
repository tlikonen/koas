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

#[derive(Debug)]
pub enum EditableItem {
    None,
    Students(Vec<Student>),
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

    pub fn count(&self) -> usize {
        match &self.item {
            EditableItem::None => 0,
            EditableItem::Students(v) => v.len(),
            EditableItem::Groups(v) => v.len(),
            EditableItem::Assignments => todo!(),
            EditableItem::Scores => todo!(),
        }
    }

    pub fn item(&self) -> &EditableItem {
        &self.item
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
    pub async fn exists(db: &mut PgConnection, name: &str) -> Result<bool, Box<dyn Error>> {
        let result = sqlx::query("SELECT 1 FROM ryhmat WHERE nimi = $1")
            .bind(name)
            .fetch_optional(db)
            .await?
            .is_some();
        Ok(result)
    }

    pub async fn edit_name(&self, db: &mut PgConnection, name: &str) -> Result<(), Box<dyn Error>> {
        sqlx::query("UPDATE ryhmat SET nimi = $1 WHERE rid = $2")
            .bind(name)
            .bind(self.rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn edit_description(
        &self,
        db: &mut PgConnection,
        desc: &str,
    ) -> Result<(), Box<dyn Error>> {
        sqlx::query("UPDATE ryhmat SET lisatiedot = $1 WHERE rid = $2")
            .bind(desc)
            .bind(self.rid)
            .execute(db)
            .await?;
        Ok(())
    }
}

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

#[derive(Debug)]
pub struct Students {
    pub list: Vec<Student>,
}

#[derive(Debug)]
pub struct Student {
    pub oid: i32,
    pub lastname: String,
    pub firstname: String,
    pub groups: String,
    pub description: String,
}

impl Students {
    pub async fn query(
        db: &mut PgConnection,
        lastname: &str,
        firstname: &str,
        group: &str,
        desc: &str,
    ) -> Result<Students, Box<dyn Error>> {
        let mut rows = sqlx::query(
            "SELECT DISTINCT view_oppilaat.oid, sukunimi, etunimi, ryhmat, olt FROM view_oppilaat \
             JOIN (SELECT oid, string_agg(ryhma, ' ' ORDER BY ryhma) ryhmat \
             FROM view_oppilaat GROUP BY oid) ryhmat ON view_oppilaat.oid = ryhmat.oid \
             WHERE sukunimi LIKE $1 AND etunimi LIKE $2 AND ryhmat LIKE $3 and olt LIKE $4
             ORDER BY sukunimi, etunimi, oid",
        )
        .bind(like_esc_wild(lastname))
        .bind(like_esc_wild(firstname))
        .bind(like_esc_wild(group))
        .bind(like_esc_wild(desc))
        .fetch(db);

        let mut list = Vec::new();
        while let Some(row) = rows.try_next().await? {
            list.push(Student {
                oid: row.try_get("oid")?,
                lastname: row.try_get("sukunimi")?,
                firstname: row.try_get("etunimi")?,
                groups: row.try_get("ryhmat")?,
                description: row.try_get("olt")?,
            });
        }

        Ok(Students { list })
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn move_to(self, ed: &mut Editable) {
        ed.item = EditableItem::Students(self.list);
    }
}

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
