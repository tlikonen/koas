use super::*;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Grade {
    pub(crate) oid: i32,
    pub lastname: String,
    pub firstname: String,
    pub(crate) sid: i32,
    pub assignment: String,
    pub weight: Option<i32>,
    pub grade: Option<String>,
    pub grade_description: Option<String>,
}

#[derive(Default)]
pub struct GradesForAssignment {
    pub assignment: String,
    pub group: String,
    pub grades: Vec<Grade>,
}

#[derive(Default)]
pub struct GradesForStudent {
    pub lastname: String,
    pub firstname: String,
    pub group: String,
    pub grades: Vec<Grade>,
}

#[derive(Default)]
pub struct GradesForGroup {
    pub group: String,
    pub students: Vec<SimpleStudent>,
    pub assignments: Vec<Assignment>,
}

pub struct SimpleStudent {
    pub name: String,
    pub grades: Vec<SimpleGrade>,
}

pub struct SimpleGrade {
    pub weight: Option<i32>,
    pub grade: Option<String>,
}

#[derive(Default)]
pub(crate) struct StudentRank {
    pub(crate) name: String,
    pub(crate) groups: Vec<String>,
    pub(crate) sum: f64,
    pub(crate) count: i32,
    pub(crate) grade_count: usize,
}

pub struct StudentRanking {
    pub(crate) data: HashMap<i32, StudentRank>,
}

pub struct GradeDistribution {
    pub(crate) data: HashMap<String, i32>,
}

impl Grade {
    async fn exists(&self, db: &mut DBase) -> Result<bool> {
        let result = sqlx::query("SELECT 1 FROM arvosanat WHERE sid = $1 AND oid = $2")
            .bind(self.sid)
            .bind(self.oid)
            .fetch_optional(db)
            .await?
            .is_some();
        Ok(result)
    }

    pub(crate) async fn update_grade(&self, db: &mut DBase, mut grade: Option<&str>) -> Result<()> {
        if let Some(s) = grade
            && s.is_empty()
        {
            grade = None;
        }

        if self.exists(db).await? {
            sqlx::query("UPDATE arvosanat SET arvosana = $1 WHERE sid = $2 AND oid = $3")
                .bind(grade)
                .bind(self.sid)
                .bind(self.oid)
                .execute(db)
                .await?;
        } else {
            sqlx::query("INSERT INTO arvosanat (sid, oid, arvosana) VALUES ($1, $2, $3)")
                .bind(self.sid)
                .bind(self.oid)
                .bind(grade)
                .execute(db)
                .await?;
        }
        Ok(())
    }

    pub(crate) async fn update_description(
        &self,
        db: &mut DBase,
        mut desc: Option<&str>,
    ) -> Result<()> {
        if let Some(s) = desc
            && s.is_empty()
        {
            desc = None;
        }

        if self.exists(db).await? {
            sqlx::query("UPDATE arvosanat SET lisatiedot = $1 WHERE sid = $2 AND oid = $3")
                .bind(desc)
                .bind(self.sid)
                .bind(self.oid)
                .execute(db)
                .await?;
        } else {
            sqlx::query("INSERT INTO arvosanat (sid, oid, lisatiedot) VALUES ($1, $2, $3)")
                .bind(self.sid)
                .bind(self.oid)
                .bind(desc)
                .execute(db)
                .await?;
        }
        Ok(())
    }

    pub(crate) async fn delete(&self, db: &mut DBase) -> Result<()> {
        sqlx::query("DELETE FROM arvosanat WHERE sid = $1 AND oid = $2")
            .bind(self.sid)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn delete_if_empty(&self, db: &mut DBase) -> Result<()> {
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

impl GradesForAssignment {
    pub(crate) async fn query(
        db: &mut DBase,
        group: QueryMatch<'_>,
        assign: QueryMatch<'_>,
        assign_short: QueryMatch<'_>,
    ) -> Result<QueryList<Self>> {
        let mut rows = sqlx::query(
            "SELECT ryhma, rid, sija, sid, suoritus, painokerroin, \
             oid, sukunimi, etunimi, arvosana, alt \
             FROM view_arvosanat \
             WHERE ryhma LIKE $1 ESCAPE '\\' AND suoritus LIKE $2 ESCAPE '\\' \
             AND lyhenne LIKE $3 ESCAPE '\\' AND oid IS NOT NULL \
             ORDER BY ryhma, rid, sija, sid, sukunimi, etunimi, oid",
        )
        .bind(group.sql_like())
        .bind(assign.sql_like())
        .bind(assign_short.sql_like())
        .fetch(db);

        let mut row = match rows.try_next().await? {
            Some(r) => r,
            None => return Ok(QueryList::default()),
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
                        list.push(Self {
                            assignment: row.try_get("suoritus")?,
                            group: row.try_get("ryhma")?,
                            grades,
                        });
                        grades = Vec::with_capacity(l);
                    }
                    next_row
                }

                None => {
                    list.push(Self {
                        assignment: row.try_get("suoritus")?,
                        group: row.try_get("ryhma")?,
                        grades,
                    });
                    break;
                }
            };
        }

        Ok(QueryList::new(list))
    }
}

impl HasData for QueryList<GradesForAssignment> {
    fn is_empty(&self) -> bool {
        self.list().is_empty()
    }
}

impl GradesForStudent {
    pub(crate) async fn query(
        db: &mut DBase,
        lastname: QueryMatch<'_>,
        firstname: QueryMatch<'_>,
        group: QueryMatch<'_>,
        student_desc: QueryMatch<'_>,
    ) -> Result<QueryList<Self>> {
        let mut rows = sqlx::query(
            "SELECT oid, sukunimi, etunimi, rid, ryhma, \
             sid, suoritus, painokerroin, arvosana, alt \
             FROM view_arvosanat \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
             AND sid IS NOT NULL \
             ORDER BY sukunimi, etunimi, oid, ryhma, rid, sija, sid",
        )
        .bind(lastname.sql_like())
        .bind(firstname.sql_like())
        .bind(group.sql_like())
        .bind(student_desc.sql_like())
        .fetch(db);

        let mut row = match rows.try_next().await? {
            Some(r) => r,
            None => return Ok(QueryList::default()),
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
                        list.push(Self {
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
                    list.push(Self {
                        lastname: row.try_get("sukunimi")?,
                        firstname: row.try_get("etunimi")?,
                        group: row.try_get("ryhma")?,
                        grades,
                    });
                    break;
                }
            };
        }

        Ok(QueryList::new(list))
    }
}

impl HasData for QueryList<GradesForStudent> {
    fn is_empty(&self) -> bool {
        self.list().is_empty()
    }
}

impl GradesForGroup {
    pub(crate) async fn query(db: &mut DBase, group: QueryMatch<'_>) -> Result<QueryList<Self>> {
        let mut groups: Vec<String> = Vec::with_capacity(10);

        {
            let mut rows = sqlx::query(
                "SELECT nimi, rid FROM ryhmat \
                 WHERE nimi LIKE $1 ESCAPE '\\' ORDER BY nimi, rid",
            )
            .bind(group.sql_like())
            .fetch(&mut *db);

            while let Some(row) = rows.try_next().await? {
                let group: String = row.try_get("nimi")?;
                groups.push(group);
            }
        }

        let mut list: Vec<Self> = Vec::with_capacity(10);

        for group in groups {
            if let Ok(q) = Self::query_single(&mut *db, &group).await?.has_data() {
                list.push(q);
            }
        }

        Ok(QueryList::new(list))
    }

    async fn query_single(db: &mut DBase, group: &str) -> Result<Self> {
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
            None => return Ok(Self::default()),
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

impl HasData for QueryList<GradesForGroup> {
    fn is_empty(&self) -> bool {
        self.list().is_empty()
    }
}

impl HasData for GradesForGroup {
    fn is_empty(&self) -> bool {
        self.assignments.is_empty()
    }
}

impl StudentRanking {
    pub(crate) fn new() -> Self {
        Self {
            data: HashMap::with_capacity(50),
        }
    }

    pub(crate) async fn query(
        &mut self,
        db: &mut DBase,
        args: FullQuery<'_>,
        all: bool,
    ) -> Result<()> {
        let mut rows = sqlx::query(
            "SELECT oid, sukunimi, etunimi, ryhma, arvosana, painokerroin FROM view_arvosanat \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
             AND suoritus LIKE $5 ESCAPE '\\' AND lyhenne LIKE $6 ESCAPE '\\'",
        )
        .bind(like_esc_wild_around(args.lastname))
        .bind(like_esc_wild_around(args.firstname))
        .bind(like_esc_wild_around(args.group))
        .bind(like_esc_wild_around(args.description))
        .bind(like_esc_wild_around(args.assignment))
        .bind(like_esc_wild_around(args.assignment_short))
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
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl GradeDistribution {
    pub(crate) fn new() -> Self {
        Self {
            data: HashMap::with_capacity(28),
        }
    }

    pub(crate) async fn query(
        &mut self,
        db: &mut DBase,
        args: FullQuery<'_>,
        all: bool,
    ) -> Result<()> {
        let mut rows = sqlx::query(
            "SELECT arvosana, painokerroin FROM view_arvosanat \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
             AND suoritus LIKE $5 ESCAPE '\\' AND lyhenne LIKE $6 ESCAPE '\\'",
        )
        .bind(like_esc_wild_around(args.lastname))
        .bind(like_esc_wild_around(args.firstname))
        .bind(like_esc_wild_around(args.group))
        .bind(like_esc_wild_around(args.description))
        .bind(like_esc_wild_around(args.assignment))
        .bind(like_esc_wild_around(args.assignment_short))
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
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

pub enum UpdateGradeOp {
    Grade(String),
    GradeClear,
    Description(String),
    DescriptionClear,
    Delete,
}

pub type UpdateGrade<'a> = Update<'a, Grade, UpdateGradeOp>;
