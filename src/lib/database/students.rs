use super::*;

#[derive(Default, Clone)]
pub struct Student {
    pub(crate) oid: i32,
    pub lastname: String,
    pub firstname: String,
    pub groups: String,
    pub description: String,
}

impl Student {
    /// Query for students.
    pub async fn query(
        db: &mut DBase,
        lastname: QueryMatch<'_>,
        firstname: QueryMatch<'_>,
        group: QueryMatch<'_>,
        desc: QueryMatch<'_>,
    ) -> Result<QueryList<Self>> {
        let mut rows = sqlx::query(
            "SELECT DISTINCT view_oppilaat.oid, sukunimi, etunimi, ryhmat, olt FROM view_oppilaat \
             JOIN (SELECT oid, string_agg(ryhma, ' ' ORDER BY ryhma) ryhmat \
             FROM view_oppilaat GROUP BY oid) ryhmat \
             ON view_oppilaat.oid = ryhmat.oid \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\'
             ORDER BY sukunimi, etunimi, oid",
        )
        .bind(lastname.sql_like())
        .bind(firstname.sql_like())
        .bind(group.sql_like())
        .bind(desc.sql_like())
        .fetch(db);

        let mut list = Vec::with_capacity(25);
        while let Some(row) = rows.try_next().await? {
            list.push(Self {
                oid: row.try_get("oid")?,
                lastname: row.try_get("sukunimi")?,
                firstname: row.try_get("etunimi")?,
                groups: row.try_get("ryhmat")?,
                description: row.try_get("olt")?,
            });
        }

        Ok(QueryList::new(list))
    }

    pub(crate) async fn in_group(&self, db: &mut DBase, rid: i32) -> Result<bool> {
        let result = sqlx::query("SELECT 1 FROM oppilaat_ryhmat WHERE oid = $1 AND rid = $2")
            .bind(self.oid)
            .bind(rid)
            .fetch_optional(db)
            .await?
            .is_some();
        Ok(result)
    }

    pub(crate) async fn add_to_group(&self, db: &mut DBase, rid: i32) -> Result<()> {
        sqlx::query("INSERT INTO oppilaat_ryhmat (oid, rid) VALUES ($1, $2)")
            .bind(self.oid)
            .bind(rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn remove_from_group(&self, db: &mut DBase, rid: i32) -> Result<()> {
        sqlx::query("DELETE FROM oppilaat_ryhmat WHERE oid = $1 AND rid = $2")
            .bind(self.oid)
            .bind(rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn only_one_group(&self, db: &mut DBase) -> Result<bool> {
        let row = sqlx::query("SELECT count(*) count FROM oppilaat_ryhmat WHERE oid = $1")
            .bind(self.oid)
            .fetch_one(db)
            .await?;
        let count: i64 = row.try_get("count")?;
        Ok(count <= 1)
    }

    pub(crate) async fn update_lastname(&self, db: &mut DBase, lastname: &str) -> Result<()> {
        sqlx::query("UPDATE oppilaat SET sukunimi = $1 WHERE oid = $2")
            .bind(lastname)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn update_firstname(&self, db: &mut DBase, firstname: &str) -> Result<()> {
        sqlx::query("UPDATE oppilaat SET etunimi = $1 WHERE oid = $2")
            .bind(firstname)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn update_description(&self, db: &mut DBase, desc: &str) -> Result<()> {
        sqlx::query("UPDATE oppilaat SET lisatiedot = $1 WHERE oid = $2")
            .bind(desc)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn count_grades(&self, db: &mut DBase) -> Result<i64> {
        let count: i64 = sqlx::query("SELECT count(*) AS count FROM arvosanat WHERE oid = $1")
            .bind(self.oid)
            .fetch_one(db)
            .await?
            .try_get("count")?;
        Ok(count)
    }

    pub(crate) async fn count_grades_group(&self, db: &mut DBase, rid: i32) -> Result<i64> {
        let count: i64 = sqlx::query(
            "SELECT count(*) AS count FROM arvosanat AS a \
             JOIN suoritukset AS s ON a.sid = s.sid \
             WHERE oid = $1 AND rid = $2",
        )
        .bind(self.oid)
        .bind(rid)
        .fetch_one(db)
        .await?
        .try_get("count")?;
        Ok(count)
    }

    pub(crate) async fn insert_db(&mut self, db: &mut DBase) -> Result<()> {
        let row = sqlx::query(
            "INSERT INTO oppilaat (sukunimi, etunimi, lisatiedot) \
             VALUES ($1, $2, $3) RETURNING oid",
        )
        .bind(&self.lastname)
        .bind(&self.firstname)
        .bind(&self.description)
        .fetch_one(db)
        .await?;

        self.oid = row.try_get("oid")?;
        Ok(())
    }

    pub(crate) async fn delete(&self, db: &mut DBase) -> Result<()> {
        sqlx::query("DELETE FROM oppilaat WHERE oid = $1")
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }
}

impl HasData for QueryList<Student> {
    fn is_empty(&self) -> bool {
        self.list_is_empty()
    }
}

pub enum UpdateStudentOp {
    Lastname(String),
    Firstname(String),
    GroupAdd(String),
    GroupRemove(String),
    Description(String),
    DescriptionClear,
    Delete,
}

pub type UpdateStudent<'a> = Update<'a, Student, UpdateStudentOp>;

pub struct InsertStudent {
    pub(crate) lastname: String,
    pub(crate) firstname: String,
    pub(crate) groups: Vec<String>,
    pub(crate) description: String,
}
