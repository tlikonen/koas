use super::*;

#[derive(Clone)]
pub struct Grade {
    oid: i32,
    lastname: String,
    firstname: String,
    sid: i32,
    assignment: String,
    weight: Option<i32>,
    grade: Option<String>,
    grade_description: Option<String>,
}

#[derive(Default)]
pub struct GradesForAssignment {
    assignment: String,
    group: String,
    grades: Vec<Grade>,
}

#[derive(Default)]
pub struct GradesForStudent {
    lastname: String,
    firstname: String,
    group: String,
    grades: Vec<Grade>,
}

#[derive(Default)]
pub struct GradesForGroup {
    group: String,
    students: Vec<SimpleStudent>,
    assignments: Vec<Assignment>,
}

pub struct SimpleStudent {
    lastname: String,
    firstname: String,
    grades: Vec<SimpleGrade>,
}

pub struct SimpleGrade {
    weight: Option<i32>,
    grade: Option<String>,
}

pub type GradeValue = Field<ContextGrade, String>;
pub type UpdateGrade<'a> = Update<'a, Grade, UpdateGradeOp>;

#[derive(Default)]
pub struct ContextGrade;

pub enum UpdateGradeOp {
    Grade(GradeValue),
    GradeClear,
    Description(Description),
    DescriptionClear,
    Delete,
}

impl Grade {
    pub fn lastname(&self) -> &str {
        &self.lastname
    }

    pub fn firstname(&self) -> &str {
        &self.firstname
    }

    pub fn fullname(&self) -> String {
        format!("{}, {}", self.lastname, self.firstname)
    }

    pub fn assignment(&self) -> &str {
        &self.assignment
    }

    pub fn weight(&self) -> Option<i32> {
        self.weight
    }

    pub fn grade(&self) -> Option<&String> {
        self.grade.as_ref()
    }

    pub fn description(&self) -> Option<&String> {
        self.grade_description.as_ref()
    }

    /// Prepare update for grade.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_grade<'a>(&'a self, grade: GradeValue) -> UpdateGrade<'a> {
        Update::new(self, UpdateGradeOp::Grade(grade))
    }

    /// Prepare to clear grade's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_grade<'a>(&'a self) -> UpdateGrade<'a> {
        Update::new(self, UpdateGradeOp::GradeClear)
    }

    /// Prepare update for grade's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: Description) -> UpdateGrade<'a> {
        Update::new(self, UpdateGradeOp::Description(desc))
    }

    /// Prepare to clear grade's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateGrade<'a> {
        Update::new(self, UpdateGradeOp::DescriptionClear)
    }

    /// Prepare deletion of grade.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateGrade<'a> {
        Update::new(self, UpdateGradeOp::Delete)
    }

    async fn exists(&self, db: &mut DBase) -> Result<bool> {
        let result = sqlx::query("SELECT 1 FROM arvosanat WHERE sid = $1 AND oid = $2")
            .bind(self.sid)
            .bind(self.oid)
            .fetch_optional(db)
            .await?
            .is_some();
        Ok(result)
    }

    async fn update_grade(&self, db: &mut DBase, grade: Option<GradeValue>) -> Result<()> {
        let grade = grade.map(|g| g.to_string());

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

    async fn update_description(
        &self,
        db: &mut DBase,
        description: Option<Description>,
    ) -> Result<()> {
        let desc = description.map(|g| g.to_string());

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

    async fn delete(&self, db: &mut DBase) -> Result<()> {
        sqlx::query("DELETE FROM arvosanat WHERE sid = $1 AND oid = $2")
            .bind(self.sid)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn delete_if_empty(&self, db: &mut DBase) -> Result<()> {
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
    /// Query for grades associated to assignments.
    pub async fn query(
        db: &mut DBase,
        group: QueryMatch<'_>,
        assignment: QueryMatch<'_>,
        assignment_short: QueryMatch<'_>,
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
        .bind(assignment.sql_like())
        .bind(assignment_short.sql_like())
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

    pub fn assignment(&self) -> &str {
        &self.assignment
    }

    pub fn group(&self) -> &str {
        &self.group
    }

    pub fn grades(&self) -> impl Iterator<Item = &Grade> {
        self.grades.iter()
    }
}

impl<'a> IntoIterator for &'a GradesForAssignment {
    type Item = &'a Grade;
    type IntoIter = std::slice::Iter<'a, Grade>;
    fn into_iter(self) -> Self::IntoIter {
        self.grades.iter()
    }
}

impl IntoIterator for GradesForAssignment {
    type Item = Grade;
    type IntoIter = std::vec::IntoIter<Grade>;
    fn into_iter(self) -> Self::IntoIter {
        self.grades.into_iter()
    }
}

impl GradesForStudent {
    /// Query for grades associated to students.
    pub async fn query(
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

    pub fn lastname(&self) -> &str {
        &self.lastname
    }

    pub fn firstname(&self) -> &str {
        &self.firstname
    }

    pub fn fullname(&self) -> String {
        format!("{}, {}", self.lastname, self.firstname)
    }

    pub fn group(&self) -> &str {
        &self.group
    }

    pub fn grades(&self) -> impl Iterator<Item = &Grade> {
        self.grades.iter()
    }
}

impl<'a> IntoIterator for &'a GradesForStudent {
    type Item = &'a Grade;
    type IntoIter = std::slice::Iter<'a, Grade>;
    fn into_iter(self) -> Self::IntoIter {
        self.grades.iter()
    }
}

impl IntoIterator for GradesForStudent {
    type Item = Grade;
    type IntoIter = std::vec::IntoIter<Grade>;
    fn into_iter(self) -> Self::IntoIter {
        self.grades.into_iter()
    }
}

impl GradesForGroup {
    /// Query for grades associated to groups.
    pub async fn query(db: &mut DBase, group: QueryMatch<'_>) -> Result<QueryList<Self>> {
        if group.is_empty() {
            Err("Ryhmän nimi puuttuu.")?;
        }

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
                            lastname,
                            firstname,
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
                        lastname,
                        firstname,
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

    pub fn group(&self) -> &str {
        &self.group
    }

    pub fn students(&self) -> impl Iterator<Item = &SimpleStudent> {
        self.students.iter()
    }

    pub fn assignments(&self) -> impl Iterator<Item = &Assignment> {
        self.assignments.iter()
    }
}

impl SimpleStudent {
    pub fn lastname(&self) -> &str {
        &self.lastname
    }

    pub fn firstname(&self) -> &str {
        &self.firstname
    }

    pub fn fullname(&self) -> String {
        format!("{}, {}", self.lastname, self.firstname)
    }

    pub fn grades(&self) -> impl Iterator<Item = &SimpleGrade> {
        self.grades.iter()
    }
}

impl SimpleGrade {
    pub fn weight(&self) -> Option<i32> {
        self.weight
    }

    pub fn grade(&self) -> Option<&String> {
        self.grade.as_ref()
    }
}

impl<'a> IntoIterator for &'a SimpleStudent {
    type Item = &'a SimpleGrade;
    type IntoIter = std::slice::Iter<'a, SimpleGrade>;
    fn into_iter(self) -> Self::IntoIter {
        self.grades.iter()
    }
}

impl IntoIterator for SimpleStudent {
    type Item = SimpleGrade;
    type IntoIter = std::vec::IntoIter<SimpleGrade>;
    fn into_iter(self) -> Self::IntoIter {
        self.grades.into_iter()
    }
}

impl HasData for QueryList<GradesForAssignment> {
    fn is_empty(&self) -> bool {
        self.list_is_empty()
    }
}

impl HasData for QueryList<GradesForStudent> {
    fn is_empty(&self) -> bool {
        self.list_is_empty()
    }
}

impl HasData for QueryList<GradesForGroup> {
    fn is_empty(&self) -> bool {
        self.list_is_empty()
    }
}

impl HasData for GradesForGroup {
    fn is_empty(&self) -> bool {
        self.assignments.is_empty()
    }
}

impl TryFrom<&str> for GradeValue {
    type Error = Error;
    fn try_from(grade: &str) -> Result<Self> {
        match grade.normalize() {
            Some(n) => Ok(Self::new(n)),
            None => Err(Error::InvalidGrade(grade.to_string())),
        }
    }
}

impl TryFrom<String> for GradeValue {
    type Error = Error;
    fn try_from(grade: String) -> Result<Self> {
        grade.as_str().try_into()
    }
}

impl<'a> ToQueue<'a> for UpdateGrade<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::UpdateGrade(self));
    }
}

impl Commit for UpdateGrade<'_> {
    async fn commit(self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        let student_grade = self.item;

        match self.operation {
            UpdateGradeOp::Grade(g) => student_grade.update_grade(&mut ta, Some(g)).await?,

            UpdateGradeOp::GradeClear => student_grade.update_grade(&mut ta, None).await?,

            UpdateGradeOp::Description(d) => {
                student_grade.update_description(&mut ta, Some(d)).await?
            }

            UpdateGradeOp::DescriptionClear => {
                student_grade.update_description(&mut ta, None).await?
            }

            UpdateGradeOp::Delete => student_grade.delete(&mut ta).await?,
        }

        student_grade.delete_if_empty(&mut ta).await?;
        ta.commit().await?;
        Ok(())
    }
}
