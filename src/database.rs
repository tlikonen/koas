mod init;

pub use self::init::PROGRAM_DB_VERSION;
use crate::prelude::*;
use futures::TryStreamExt;

pub async fn connect(config: &Config, modes: &Modes) -> Result<PgConnection, Box<dyn Error>> {
    let connect_string = format!(
        "postgres://{user}:{password}@{host}:{port}/{db}",
        user = config.user,
        password = config.password,
        host = config.host,
        port = config.port,
        db = config.database,
    );

    let mut db = PgConnection::connect(&connect_string).await?;
    init::initialize(&mut db, modes).await?;
    Ok(db)
}

impl Editable {
    pub fn clear(&mut self) {
        self.set(EditableItem::None);
    }

    pub fn is_none(&self) -> bool {
        matches!(self.item(), EditableItem::None)
    }

    pub fn is_grade(&self) -> bool {
        matches!(self.item(), EditableItem::Grades(_))
    }

    pub fn count(&self) -> usize {
        match &self.item() {
            EditableItem::None => 0,
            EditableItem::Students(v) => v.len(),
            EditableItem::Groups(v) => v.len(),
            EditableItem::Assignments(v) => v.len(),
            EditableItem::Grades(v) => v.len(),
        }
    }

    pub fn print_fields(&self, fields: &[&str]) {
        let mut s = String::with_capacity(50);
        for (n, f) in fields.iter().enumerate() {
            s.push_str(&format!(" / {}:{}", n + 1, f));
        }
        match self.count() {
            0 => (),
            1 => println!("Tietue: 1. Kentät:{s}"),
            n => println!("Tietueet: 1–{n}. Kentät:{s}"),
        }
    }
}

impl Stats {
    pub async fn query(db: &mut PgConnection) -> Result<Self, sqlx::Error> {
        let row = sqlx::query(
            "SELECT \
             (SELECT count(*) FROM oppilaat) oppilaat, \
             (SELECT count(*) FROM ryhmat) ryhmat, \
             (SELECT count(*) FROM suoritukset) suoritukset, \
             (SELECT count(*) FROM arvosanat WHERE arvosana LIKE '_%' ESCAPE '\\') arvosanat",
        )
        .fetch_one(db)
        .await?;

        Ok(Self {
            students: row.try_get("oppilaat")?,
            groups: row.try_get("ryhmat")?,
            assignments: row.try_get("suoritukset")?,
            grades: row.try_get("arvosanat")?,
        })
    }
}

impl Student {
    pub async fn in_group(&self, db: &mut PgConnection, rid: i32) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("SELECT 1 FROM oppilaat_ryhmat WHERE oid = $1 AND rid = $2")
            .bind(self.oid)
            .bind(rid)
            .fetch_optional(db)
            .await?
            .is_some();
        Ok(result)
    }

    pub async fn add_to_group(&self, db: &mut PgConnection, rid: i32) -> Result<(), sqlx::Error> {
        sqlx::query("INSERT INTO oppilaat_ryhmat (oid, rid) VALUES ($1, $2)")
            .bind(self.oid)
            .bind(rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn remove_from_group(
        &self,
        db: &mut PgConnection,
        rid: i32,
    ) -> Result<(), sqlx::Error> {
        sqlx::query("DELETE FROM oppilaat_ryhmat WHERE oid = $1 AND rid = $2")
            .bind(self.oid)
            .bind(rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn only_one_group(&self, db: &mut PgConnection) -> Result<bool, sqlx::Error> {
        let row = sqlx::query("SELECT count(*) count FROM oppilaat_ryhmat WHERE oid = $1")
            .bind(self.oid)
            .fetch_one(db)
            .await?;
        let count: i64 = row.try_get("count")?;
        Ok(count <= 1)
    }

    pub async fn update_lastname(
        &self,
        db: &mut PgConnection,
        lastname: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE oppilaat SET sukunimi = $1 WHERE oid = $2")
            .bind(lastname)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn update_firstname(
        &self,
        db: &mut PgConnection,
        firstname: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE oppilaat SET etunimi = $1 WHERE oid = $2")
            .bind(firstname)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn update_description(
        &self,
        db: &mut PgConnection,
        desc: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE oppilaat SET lisatiedot = $1 WHERE oid = $2")
            .bind(desc)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn count_grades(&self, db: &mut PgConnection) -> Result<i64, sqlx::Error> {
        let count: i64 = sqlx::query("SELECT count(*) AS count FROM arvosanat WHERE oid = $1")
            .bind(self.oid)
            .fetch_one(db)
            .await?
            .try_get("count")?;
        Ok(count)
    }

    pub async fn count_grades_group(
        &self,
        db: &mut PgConnection,
        rid: i32,
    ) -> Result<i64, sqlx::Error> {
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

    pub async fn insert(&mut self, db: &mut PgConnection) -> Result<(), sqlx::Error> {
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

    pub async fn delete(&self, db: &mut PgConnection) -> Result<(), sqlx::Error> {
        sqlx::query("DELETE FROM oppilaat WHERE oid = $1")
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }
}

impl Students {
    pub async fn query(
        db: &mut PgConnection,
        lastname: &str,
        firstname: &str,
        group: &str,
        desc: &str,
    ) -> Result<Self, sqlx::Error> {
        let mut rows = sqlx::query(
            "SELECT DISTINCT view_oppilaat.oid, sukunimi, etunimi, ryhmat, olt FROM view_oppilaat \
             JOIN (SELECT oid, string_agg(ryhma, ' ' ORDER BY ryhma) ryhmat \
             FROM view_oppilaat GROUP BY oid) ryhmat \
             ON view_oppilaat.oid = ryhmat.oid \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\'
             ORDER BY sukunimi, etunimi, oid",
        )
        .bind(like_esc_wild(lastname))
        .bind(like_esc_wild(firstname))
        .bind(like_esc_wild(group))
        .bind(like_esc_wild(desc))
        .fetch(db);

        let mut list = Vec::with_capacity(25);
        while let Some(row) = rows.try_next().await? {
            list.push(Student {
                oid: row.try_get("oid")?,
                lastname: row.try_get("sukunimi")?,
                firstname: row.try_get("etunimi")?,
                groups: row.try_get("ryhmat")?,
                description: row.try_get("olt")?,
            });
        }

        Ok(Self { list })
    }
}

impl HasData for Students {
    fn empty_data(&self) -> bool {
        self.list.is_empty()
    }
}

impl CopyToEditable for Students {
    fn item(&self) -> EditableItem {
        EditableItem::Students(self.list.clone())
    }
}

impl Group {
    pub async fn get_or_insert(db: &mut PgConnection, name: &str) -> Result<i32, sqlx::Error> {
        let rid = match Self::get_id(db, name).await? {
            Some(id) => id,
            None => {
                let row = sqlx::query("INSERT INTO ryhmat (nimi) VALUES ($1) RETURNING rid")
                    .bind(name)
                    .fetch_one(db)
                    .await?;
                let id: i32 = row.try_get("rid")?;
                id
            }
        };
        Ok(rid)
    }

    pub async fn get_id(db: &mut PgConnection, name: &str) -> Result<Option<i32>, sqlx::Error> {
        match sqlx::query("SELECT rid FROM ryhmat WHERE nimi = $1")
            .bind(name)
            .fetch_optional(db)
            .await?
        {
            None => Ok(None),
            Some(r) => {
                let id: i32 = r.try_get("rid")?;
                Ok(Some(id))
            }
        }
    }

    pub async fn update_name(&self, db: &mut PgConnection, name: &str) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE ryhmat SET nimi = $1 WHERE rid = $2")
            .bind(name)
            .bind(self.rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn update_description(
        &self,
        db: &mut PgConnection,
        desc: &str,
    ) -> Result<(), sqlx::Error> {
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
    ) -> Result<Self, sqlx::Error> {
        let mut rows = sqlx::query(
            "SELECT rid, nimi, lisatiedot FROM ryhmat \
             WHERE nimi LIKE $1 ESCAPE '\\' AND lisatiedot LIKE $2 ESCAPE '\\' \
             ORDER BY nimi, lisatiedot, rid",
        )
        .bind(like_esc_wild(group))
        .bind(like_esc_wild(desc))
        .fetch(db);

        let mut list = Vec::with_capacity(10);
        while let Some(row) = rows.try_next().await? {
            list.push(Group {
                rid: row.try_get("rid")?,
                name: row.try_get("nimi")?,
                description: row.try_get("lisatiedot")?,
            });
        }

        Ok(Self { list })
    }

    pub async fn delete_empty(db: &mut PgConnection) -> Result<(), sqlx::Error> {
        sqlx::query(
            "DELETE FROM ryhmat WHERE rid IN \
             (SELECT r.rid FROM ryhmat AS r \
             LEFT JOIN oppilaat_ryhmat AS j ON r.rid = j.rid \
             LEFT JOIN suoritukset AS s ON s.rid = r.rid \
             WHERE j.rid IS NULL AND s.rid IS NULL)",
        )
        .execute(db)
        .await?;
        Ok(())
    }
}

impl HasData for Groups {
    fn empty_data(&self) -> bool {
        self.list.is_empty()
    }
}

impl CopyToEditable for Groups {
    fn item(&self) -> EditableItem {
        EditableItem::Groups(self.list.clone())
    }
}

impl Assignment {
    pub async fn update_name(&self, db: &mut PgConnection, name: &str) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE suoritukset SET nimi = $1 WHERE sid = $2")
            .bind(name)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn update_short(
        &self,
        db: &mut PgConnection,
        short: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE suoritukset SET lyhenne = $1 WHERE sid = $2")
            .bind(short)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn update_weight(
        &self,
        db: &mut PgConnection,
        weight: Option<i32>,
    ) -> Result<(), sqlx::Error> {
        let value = match weight {
            Some(n) if n < 1 => None,
            v => v,
        };
        sqlx::query("UPDATE suoritukset SET painokerroin = $1 WHERE sid = $2")
            .bind(value)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn update_position(
        &self,
        db: &mut PgConnection,
        mut pos: i32,
    ) -> Result<(), sqlx::Error> {
        let mut other_sids = Vec::with_capacity(10);

        {
            let mut rows = sqlx::query(
                "SELECT sid FROM suoritukset \
                 WHERE rid = $1 AND NOT sid = $2 ORDER BY sija, sid DESC",
            )
            .bind(self.rid)
            .bind(self.sid)
            .fetch(&mut *db);

            while let Some(row) = rows.try_next().await? {
                let sid: i32 = row.try_get("sid")?;
                other_sids.push(sid);
            }
        }

        if pos < 1 {
            pos = 1;
        }

        let other_max: i32 = other_sids.len().try_into().unwrap();
        if pos > other_max + 1 {
            pos = other_max + 1
        }

        sqlx::query("UPDATE suoritukset SET sija = $1 WHERE sid = $2")
            .bind(pos)
            .bind(self.sid)
            .execute(&mut *db)
            .await?;

        let mut position: i32 = 0;
        for sid in other_sids {
            position += 1;
            if position == pos {
                position += 1;
            }

            sqlx::query("UPDATE suoritukset SET sija = $1 WHERE sid = $2")
                .bind(position)
                .bind(sid)
                .execute(&mut *db)
                .await?;
        }

        Ok(())
    }

    pub async fn count_grades(&self, db: &mut PgConnection) -> Result<i64, sqlx::Error> {
        let count: i64 = sqlx::query("SELECT count(*) AS count FROM arvosanat WHERE sid = $1")
            .bind(self.sid)
            .fetch_one(db)
            .await?
            .try_get("count")?;
        Ok(count)
    }

    pub async fn insert(&mut self, db: &mut PgConnection, pos: i32) -> Result<(), sqlx::Error> {
        let row = sqlx::query(
            "INSERT INTO suoritukset (rid, nimi, lyhenne, painokerroin, sija) \
             VALUES ($1, $2, $3, $4, $5) RETURNING sid",
        )
        .bind(self.rid)
        .bind(&self.assignment)
        .bind(&self.assignment_short)
        .bind(self.weight)
        .bind(pos)
        .fetch_one(&mut *db)
        .await?;

        self.sid = row.try_get("sid")?;
        self.update_position(db, pos).await?;
        Ok(())
    }

    pub async fn delete(&self, db: &mut PgConnection) -> Result<(), sqlx::Error> {
        sqlx::query("DELETE FROM suoritukset WHERE sid = $1")
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }
}

impl Assignments {
    pub async fn query(db: &mut PgConnection, group: &str) -> Result<Self, sqlx::Error> {
        let mut rows = sqlx::query(
            "SELECT rid, sid, suoritus, lyhenne, painokerroin FROM view_suoritukset \
             WHERE ryhma = $1 ORDER BY sija, sid",
        )
        .bind(group)
        .fetch(db);

        let mut list = Vec::with_capacity(10);

        while let Some(row) = rows.try_next().await? {
            list.push(Assignment {
                rid: row.try_get("rid")?,
                sid: row.try_get("sid")?,
                assignment: row.try_get("suoritus")?,
                assignment_short: row.try_get("lyhenne")?,
                weight: row.try_get("painokerroin")?,
            });
        }

        Ok(Self {
            group: group.to_string(),
            list,
        })
    }

    pub async fn reposition(db: &mut PgConnection, rid: i32) -> Result<(), sqlx::Error> {
        let mut sid_list = Vec::with_capacity(10);

        {
            let mut rows =
                sqlx::query("SELECT sid FROM suoritukset WHERE rid = $1 ORDER BY sija, sid DESC")
                    .bind(rid)
                    .fetch(&mut *db);

            while let Some(row) = rows.try_next().await? {
                let sid: i32 = row.try_get("sid")?;
                sid_list.push(sid);
            }
        }

        let mut position: i32 = 0;
        for sid in sid_list {
            position += 1;
            sqlx::query("UPDATE suoritukset SET sija = $1 WHERE sid = $2")
                .bind(position)
                .bind(sid)
                .execute(&mut *db)
                .await?;
        }

        Ok(())
    }
}

impl HasData for Assignments {
    fn empty_data(&self) -> bool {
        self.list.is_empty()
    }
}

impl CopyToEditable for Assignments {
    fn item(&self) -> EditableItem {
        EditableItem::Assignments(self.list.clone())
    }
}

impl Grade {
    async fn exists(&self, db: &mut PgConnection) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("SELECT 1 FROM arvosanat WHERE sid = $1 AND oid = $2")
            .bind(self.sid)
            .bind(self.oid)
            .fetch_optional(db)
            .await?
            .is_some();
        Ok(result)
    }

    pub async fn update_grade(
        &self,
        db: &mut PgConnection,
        grade: &str,
    ) -> Result<(), sqlx::Error> {
        let value = if grade.is_empty() { None } else { Some(grade) };

        if self.exists(db).await? {
            sqlx::query("UPDATE arvosanat SET arvosana = $1 WHERE sid = $2 AND oid = $3")
                .bind(value)
                .bind(self.sid)
                .bind(self.oid)
                .execute(db)
                .await?;
        } else {
            sqlx::query("INSERT INTO arvosanat (sid, oid, arvosana) VALUES ($1, $2, $3)")
                .bind(self.sid)
                .bind(self.oid)
                .bind(value)
                .execute(db)
                .await?;
        }
        Ok(())
    }

    pub async fn update_description(
        &self,
        db: &mut PgConnection,
        desc: &str,
    ) -> Result<(), sqlx::Error> {
        let value = if desc.is_empty() { None } else { Some(desc) };

        if self.exists(db).await? {
            sqlx::query("UPDATE arvosanat SET lisatiedot = $1 WHERE sid = $2 AND oid = $3")
                .bind(value)
                .bind(self.sid)
                .bind(self.oid)
                .execute(db)
                .await?;
        } else {
            sqlx::query("INSERT INTO arvosanat (sid, oid, lisatiedot) VALUES ($1, $2, $3)")
                .bind(self.sid)
                .bind(self.oid)
                .bind(value)
                .execute(db)
                .await?;
        }
        Ok(())
    }

    pub async fn delete(&self, db: &mut PgConnection) -> Result<(), sqlx::Error> {
        sqlx::query("DELETE FROM arvosanat WHERE sid = $1 AND oid = $2")
            .bind(self.sid)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub async fn delete_if_empty(&self, db: &mut PgConnection) -> Result<(), sqlx::Error> {
        sqlx::query(
            "DELETE FROM arvosanat \
             WHERE sid = $1 AND oid = $2 \
             AND arvosana IS NULL AND lisatiedot IS NULL",
        )
        .bind(self.sid)
        .bind(self.oid)
        .execute(db)
        .await?;
        Ok(())
    }
}

impl GradesForAssignments {
    pub async fn query(
        db: &mut PgConnection,
        group: &str,
        assign: &str,
        assign_short: &str,
    ) -> Result<Self, sqlx::Error> {
        let mut rows = sqlx::query(
            "SELECT ryhma, rid, sija, sid, suoritus, painokerroin, \
             oid, sukunimi, etunimi, arvosana, alt \
             FROM view_arvosanat \
             WHERE ryhma LIKE $1 ESCAPE '\\' AND suoritus LIKE $2 ESCAPE '\\' \
             AND lyhenne LIKE $3 ESCAPE '\\' AND oid IS NOT NULL \
             ORDER BY ryhma, rid, sija, sid, sukunimi, etunimi, oid",
        )
        .bind(like_esc_wild(group))
        .bind(like_esc_wild(assign))
        .bind(like_esc_wild(assign_short))
        .fetch(db);

        let mut row = match rows.try_next().await? {
            Some(r) => r,
            None => return Ok(Default::default()),
        };

        let mut list = Vec::with_capacity(1);
        let mut grades = Vec::with_capacity(10);

        loop {
            let sid: i32 = row.try_get("sid")?;

            grades.push(Grade {
                oid: row.try_get("oid")?,
                lastname: row.try_get("sukunimi")?,
                firstname: row.try_get("etunimi")?,
                sid,
                assignment: row.try_get("suoritus")?,
                weight: row.try_get("painokerroin")?,
                grade: row.try_get("arvosana")?,
                grade_description: row.try_get("alt")?,
            });

            row = match rows.try_next().await? {
                Some(next_row) => {
                    let next_sid: i32 = next_row.try_get("sid")?;
                    if next_sid != sid {
                        let l = grades.len();
                        list.push(GradesForAssignment {
                            assignment: row.try_get("suoritus")?,
                            group: row.try_get("ryhma")?,
                            grades,
                        });
                        grades = Vec::with_capacity(l);
                    }
                    next_row
                }

                None => {
                    list.push(GradesForAssignment {
                        assignment: row.try_get("suoritus")?,
                        group: row.try_get("ryhma")?,
                        grades,
                    });
                    break;
                }
            };
        }

        Ok(Self { list })
    }
}

impl HasData for GradesForAssignments {
    fn empty_data(&self) -> bool {
        self.list.is_empty()
    }
}

impl CopyToEditable for GradesForAssignment {
    fn item(&self) -> EditableItem {
        EditableItem::Grades(self.grades.clone())
    }
}

impl GradesForStudents {
    pub async fn query(
        db: &mut PgConnection,
        lastname: &str,
        firstname: &str,
        group: &str,
        student_desc: &str,
    ) -> Result<Self, sqlx::Error> {
        let mut rows = sqlx::query(
            "SELECT oid, sukunimi, etunimi, rid, ryhma, \
             sid, suoritus, painokerroin, arvosana, alt \
             FROM view_arvosanat \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
             AND sid IS NOT NULL \
             ORDER BY sukunimi, etunimi, oid, ryhma, rid, sija, sid",
        )
        .bind(like_esc_wild(lastname))
        .bind(like_esc_wild(firstname))
        .bind(like_esc_wild(group))
        .bind(like_esc_wild(student_desc))
        .fetch(db);

        let mut row = match rows.try_next().await? {
            Some(r) => r,
            None => return Ok(Default::default()),
        };

        let mut list = Vec::with_capacity(1);
        let mut grades = Vec::with_capacity(10);

        loop {
            let oid: i32 = row.try_get("oid")?;
            let rid: i32 = row.try_get("rid")?;

            grades.push(Grade {
                oid,
                lastname: row.try_get("sukunimi")?,
                firstname: row.try_get("etunimi")?,
                sid: row.try_get("sid")?,
                assignment: row.try_get("suoritus")?,
                weight: row.try_get("painokerroin")?,
                grade: row.try_get("arvosana")?,
                grade_description: row.try_get("alt")?,
            });

            row = match rows.try_next().await? {
                Some(next_row) => {
                    let next_oid: i32 = next_row.try_get("oid")?;
                    let next_rid: i32 = next_row.try_get("rid")?;
                    if next_oid != oid || next_rid != rid {
                        let l = grades.len();
                        list.push(GradesForStudent {
                            lastname: row.try_get("sukunimi")?,
                            firstname: row.try_get("etunimi")?,
                            group: row.try_get("ryhma")?,
                            grades,
                        });
                        grades = Vec::with_capacity(l);
                    }
                    next_row
                }

                None => {
                    list.push(GradesForStudent {
                        lastname: row.try_get("sukunimi")?,
                        firstname: row.try_get("etunimi")?,
                        group: row.try_get("ryhma")?,
                        grades,
                    });
                    break;
                }
            };
        }

        Ok(Self { list })
    }
}

impl HasData for GradesForStudents {
    fn empty_data(&self) -> bool {
        self.list.is_empty()
    }
}

impl CopyToEditable for GradesForStudent {
    fn item(&self) -> EditableItem {
        EditableItem::Grades(self.grades.clone())
    }
}

impl GradesForGroup {
    pub async fn query(db: &mut PgConnection, group: &str) -> Result<Self, sqlx::Error> {
        let mut assignments = Vec::with_capacity(10);

        {
            let mut rows = sqlx::query(
                "SELECT rid, sid, suoritus, lyhenne, painokerroin \
                 FROM view_suoritukset WHERE ryhma = $1 ORDER BY sija",
            )
            .bind(group)
            .fetch(&mut *db);

            while let Some(row) = rows.try_next().await? {
                assignments.push(Assignment {
                    rid: row.try_get("rid")?,
                    sid: row.try_get("sid")?,
                    assignment: row.try_get("suoritus")?,
                    assignment_short: row.try_get("lyhenne")?,
                    weight: row.try_get("painokerroin")?,
                });
            }
        }

        let mut rows = sqlx::query(
            "SELECT sukunimi, etunimi, oid, arvosana, painokerroin FROM view_arvosanat \
             WHERE ryhma = $1 ORDER BY sukunimi, etunimi, oid, sija",
        )
        .bind(group)
        .fetch(db);

        let mut row = match rows.try_next().await? {
            Some(r) => r,
            None => return Ok(Default::default()),
        };

        let mut students = Vec::with_capacity(25);
        let mut grades = Vec::with_capacity(10);

        loop {
            let oid: i32 = row.try_get("oid")?;

            grades.push(SimpleGrade {
                weight: row.try_get("painokerroin")?,
                grade: row.try_get("arvosana")?,
            });

            row = match rows.try_next().await? {
                Some(next_row) => {
                    let next_oid: i32 = next_row.try_get("oid")?;
                    if next_oid != oid {
                        let l = grades.len();
                        let lastname: String = row.try_get("sukunimi")?;
                        let firstname: String = row.try_get("etunimi")?;

                        students.push(SimpleStudent {
                            name: format!("{lastname}, {firstname}"),
                            grades,
                        });
                        grades = Vec::with_capacity(l);
                    }
                    next_row
                }

                None => {
                    let lastname: String = row.try_get("sukunimi")?;
                    let firstname: String = row.try_get("etunimi")?;
                    students.push(SimpleStudent {
                        name: format!("{lastname}, {firstname}"),
                        grades,
                    });
                    break;
                }
            }
        }

        Ok(Self {
            group: group.to_string(),
            students,
            assignments,
        })
    }
}

impl HasData for GradesForGroup {
    fn empty_data(&self) -> bool {
        self.assignments.is_empty()
    }
}

impl StudentRanking {
    pub fn new() -> Self {
        Self {
            data: HashMap::with_capacity(50),
        }
    }

    pub async fn query(
        &mut self,
        db: &mut PgConnection,
        all: bool,
        args: FullQuery<'_>,
    ) -> Result<(), sqlx::Error> {
        let mut rows = sqlx::query(
            "SELECT oid, sukunimi, etunimi, ryhma, arvosana, painokerroin FROM view_arvosanat \
         WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
         AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
         AND suoritus LIKE $5 ESCAPE '\\' AND lyhenne LIKE $6 ESCAPE '\\'",
        )
        .bind(like_esc_wild(args.lastname))
        .bind(like_esc_wild(args.firstname))
        .bind(like_esc_wild(args.group))
        .bind(like_esc_wild(args.description))
        .bind(like_esc_wild(args.assignment))
        .bind(like_esc_wild(args.assignment_short))
        .fetch(db);

        while let Some(row) = rows.try_next().await? {
            if let Some(gr) = row.try_get("arvosana")?
                && let Some(grade) = tools::parse_number(gr)
            {
                let weight: i32 = match row.try_get("painokerroin")? {
                    Some(w) => w,
                    None if all => 1,
                    None => continue,
                };

                let oid: i32 = row.try_get("oid")?;
                let rank = self.data.entry(oid).or_default();

                if rank.name.is_empty() {
                    rank.name.push_str(row.try_get("sukunimi")?);
                    rank.name.push_str(", ");
                    rank.name.push_str(row.try_get("etunimi")?);
                }

                let group: String = row.try_get("ryhma")?;
                if !rank.groups.contains(&group) {
                    rank.groups.push(group.to_string());
                    rank.groups.sort();
                }

                rank.sum += grade * f64::from(weight);
                rank.count += weight;
                rank.grade_count += 1;
            }
        }

        Ok(())
    }
}

impl HasData for StudentRanking {
    fn empty_data(&self) -> bool {
        self.data.is_empty()
    }
}

impl GradeDistribution {
    pub fn new(out: &Output) -> Self {
        Self {
            data: HashMap::with_capacity(28),
            output: out.clone(),
        }
    }

    pub async fn query(
        &mut self,
        db: &mut PgConnection,
        all: bool,
        args: FullQuery<'_>,
    ) -> Result<(), sqlx::Error> {
        let mut rows = sqlx::query(
            "SELECT arvosana, painokerroin FROM view_arvosanat \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
             AND suoritus LIKE $5 ESCAPE '\\' AND lyhenne LIKE $6 ESCAPE '\\'",
        )
        .bind(like_esc_wild(args.lastname))
        .bind(like_esc_wild(args.firstname))
        .bind(like_esc_wild(args.group))
        .bind(like_esc_wild(args.description))
        .bind(like_esc_wild(args.assignment))
        .bind(like_esc_wild(args.assignment_short))
        .fetch(db);

        while let Some(row) = rows.try_next().await? {
            let weight: Option<i32> = row.try_get("painokerroin")?;
            if (all || weight.is_some())
                && let Some(grade) = row.try_get("arvosana")?
            {
                let count = self.data.entry(grade).or_default();
                *count += 1;
            }
        }

        Ok(())
    }
}

impl HasData for GradeDistribution {
    fn empty_data(&self) -> bool {
        self.data.is_empty()
    }
}

fn like_esc_wild(string: &str) -> String {
    let mut new = String::with_capacity(string.len() + 3);
    new.push('%');

    for c in string.chars() {
        match c {
            '%' | '_' | '\\' => {
                new.push('\\');
                new.push(c);
            }
            '*' => new.push('%'),
            _ => new.push(c),
        }
    }

    new.push('%');
    new
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn like_esc_wild_fn() {
        assert_eq!("%abcd%", like_esc_wild("abcd"));
        assert_eq!("%a\\%b\\_cd%", like_esc_wild("a%b_cd"));
        assert_eq!("%ab\\\\cd%", like_esc_wild("ab\\cd"));
        assert_eq!("%abcd%", like_esc_wild("abcd"));
        assert_eq!("%\\_\\%\\\\%", like_esc_wild("_%\\"));
        assert_eq!("%ab%cd%", like_esc_wild("ab*cd"));
    }
}
