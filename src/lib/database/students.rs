use super::*;

#[derive(Default)]
pub struct Student {
    oid: i32,
    lastname: String,
    firstname: String,
    groups: Vec<String>,
    description: String,
}

pub type UpdateStudent<'a> = Update<'a, Student, UpdateStudentOp>;

pub enum UpdateStudentOp {
    Lastname(Lastname),
    Firstname(Firstname),
    GroupsAdd(GroupNames),
    GroupsRemove(GroupNames),
    Description(Description),
    DescriptionClear,
    Delete,
}

pub struct InsertStudent {
    lastname: Lastname,
    firstname: Firstname,
    groups: GroupNames,
    description: Option<Description>,
}

#[derive(Default)]
pub struct Lastname(String);

#[derive(Default)]
pub struct Firstname(String);

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
                groups: {
                    let s: &str = row.try_get("ryhmat")?;
                    s.split_whitespace().map(|x| x.to_string()).collect()
                },
                description: row.try_get("olt")?,
            });
        }

        Ok(QueryList::new(list))
    }

    /// Prepare to insert new student.
    ///
    /// See [`Commit`] trait for more information.
    pub fn insert(
        lastname: Lastname,
        firstname: Firstname,
        groups: GroupNames,
        description: Option<Description>,
    ) -> InsertStudent {
        InsertStudent {
            lastname,
            firstname,
            groups,
            description,
        }
    }

    /// Return student's lastname.
    pub fn lastname(&self) -> &str {
        &self.lastname
    }

    /// Return student's firstname.
    pub fn firstname(&self) -> &str {
        &self.firstname
    }

    /// Return student's full name: constructed from lastname and firstname.
    pub fn fullname(&self) -> String {
        format!("{}, {}", self.lastname.as_str(), self.firstname.as_str())
    }

    /// Return iterator over student's group names.
    pub fn groups(&self) -> impl Iterator<Item = &str> {
        self.groups.iter().map(|s| s.as_str())
    }

    /// Return student's description.
    pub fn description(&self) -> &str {
        &self.description
    }

    /// Prepare update for student's lastname.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_lastname<'a>(&'a self, name: Lastname) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::Lastname(name))
    }

    /// Prepare update for student's firstname.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_firstname<'a>(&'a self, name: Firstname) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::Firstname(name))
    }

    /// Prepare addition for student's groups.
    ///
    /// See [`Commit`] trait for more information.
    pub fn add_groups<'a>(&'a self, groups: GroupNames) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::GroupsAdd(groups))
    }

    /// Prepare removal for student's groups.
    ///
    /// See [`Commit`] trait for more information.
    pub fn remove_groups<'a>(&'a self, groups: GroupNames) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::GroupsRemove(groups))
    }

    /// Prepare update for student's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: Description) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::Description(desc))
    }

    /// Prepare to clear student's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::DescriptionClear)
    }

    /// Prepare deletion of student.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::Delete)
    }

    async fn in_group(&self, db: &mut DBase, rid: i32) -> Result<bool> {
        let result = sqlx::query("SELECT 1 FROM oppilaat_ryhmat WHERE oid = $1 AND rid = $2")
            .bind(self.oid)
            .bind(rid)
            .fetch_optional(db)
            .await?
            .is_some();
        Ok(result)
    }

    async fn add_to_group(&self, db: &mut DBase, rid: i32) -> Result<()> {
        sqlx::query("INSERT INTO oppilaat_ryhmat (oid, rid) VALUES ($1, $2)")
            .bind(self.oid)
            .bind(rid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn remove_from_group(&self, db: &mut DBase, rid: i32) -> Result<()> {
        sqlx::query("DELETE FROM oppilaat_ryhmat WHERE oid = $1 AND rid = $2")
            .bind(self.oid)
            .bind(rid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn only_one_group(&self, db: &mut DBase) -> Result<bool> {
        let row = sqlx::query("SELECT count(*) count FROM oppilaat_ryhmat WHERE oid = $1")
            .bind(self.oid)
            .fetch_one(db)
            .await?;
        let count: i64 = row.try_get("count")?;
        Ok(count <= 1)
    }

    async fn update_lastname(&self, db: &mut DBase, lastname: &Lastname) -> Result<()> {
        sqlx::query("UPDATE oppilaat SET sukunimi = $1 WHERE oid = $2")
            .bind(lastname.as_str())
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_firstname(&self, db: &mut DBase, firstname: &Firstname) -> Result<()> {
        sqlx::query("UPDATE oppilaat SET etunimi = $1 WHERE oid = $2")
            .bind(firstname.as_str())
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_description(
        &self,
        db: &mut DBase,
        description: Option<&Description>,
    ) -> Result<()> {
        let desc = match description {
            Some(d) => d.to_string(),
            None => "".to_string(),
        };

        sqlx::query("UPDATE oppilaat SET lisatiedot = $1 WHERE oid = $2")
            .bind(desc)
            .bind(self.oid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn count_grades(&self, db: &mut DBase) -> Result<i64> {
        let count: i64 = sqlx::query("SELECT count(*) AS count FROM arvosanat WHERE oid = $1")
            .bind(self.oid)
            .fetch_one(db)
            .await?
            .try_get("count")?;
        Ok(count)
    }

    async fn count_grades_group(&self, db: &mut DBase, rid: i32) -> Result<i64> {
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

    async fn insert_db(
        db: &mut DBase,
        lastname: Lastname,
        firstname: Firstname,
        groups: GroupNames,
        description: Option<Description>,
    ) -> Result<()> {
        let desc = match description {
            Some(d) => d.to_string(),
            None => "".to_string(),
        };

        let row = sqlx::query(
            "INSERT INTO oppilaat (sukunimi, etunimi, lisatiedot) \
             VALUES ($1, $2, $3) RETURNING oid",
        )
        .bind(lastname.as_str())
        .bind(firstname.as_str())
        .bind(desc)
        .fetch_one(&mut *db)
        .await?;

        let student = Student {
            oid: row.try_get("oid")?,
            ..Student::default()
        };

        for group in &groups {
            let rid = Group::get_or_insert(&mut *db, group).await?;
            student.add_to_group(&mut *db, rid).await?;
        }

        Ok(())
    }

    async fn delete(&self, db: &mut DBase) -> Result<()> {
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

impl Lastname {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<&str> for Lastname {
    type Error = Error;
    fn try_from(name: &str) -> Result<Self> {
        match name.normalize() {
            Some(n) => Ok(Self(n)),
            None => Err(Error::InvalidLastname(name.to_string())),
        }
    }
}

impl fmt::Display for Lastname {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Firstname {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<&str> for Firstname {
    type Error = Error;
    fn try_from(name: &str) -> Result<Self> {
        match name.normalize() {
            Some(n) => Ok(Self(n)),
            None => Err(Error::InvalidFirstname(name.to_string())),
        }
    }
}

impl fmt::Display for Firstname {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> ToQueue<'a> for UpdateStudent<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::UpdateStudent(self));
    }
}

impl<'a> ToQueue<'a> for InsertStudent {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::InsertStudent(self))
    }
}

impl Commit for UpdateStudent<'_> {
    async fn commit(self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        let student = self.item;

        match &self.operation {
            UpdateStudentOp::Lastname(last) => student.update_lastname(&mut ta, last).await?,

            UpdateStudentOp::Firstname(first) => student.update_firstname(&mut ta, first).await?,

            UpdateStudentOp::GroupsAdd(groups) => {
                for name in groups {
                    let rid = Group::get_or_insert(&mut ta, name).await?;
                    if !student.in_group(&mut ta, rid).await? {
                        student.add_to_group(&mut ta, rid).await?;
                    }
                }
            }

            UpdateStudentOp::GroupsRemove(groups) => {
                for name in groups {
                    let Some(rid) = Group::get_id(&mut ta, name).await? else {
                        continue; // No such group.
                    };

                    if !student.in_group(&mut ta, rid).await? {
                        continue;
                    }

                    let count = student.count_grades_group(&mut ta, rid).await?;
                    if count > 0 {
                        return Err(Error::from(format!(
                            "Oppilaalle ”{o}” on ryhmässä ”{g}” kirjattu {c} arvosana(a).\n\
                             Säilytetään ryhmät ja perutaan toiminto.",
                            o = student.fullname(),
                            c = count,
                            g = name,
                        )));
                    }

                    if student.only_one_group(&mut ta).await? {
                        return Err(Error::from(
                            "Oppilaan pitää kuulua vähintään yhteen ryhmään.",
                        ));
                    } else {
                        student.remove_from_group(&mut ta, rid).await?;
                    }
                }

                Group::delete_empty(&mut ta).await?;
            }

            UpdateStudentOp::Description(desc) => {
                student.update_description(&mut ta, Some(desc)).await?;
            }

            UpdateStudentOp::DescriptionClear => {
                student.update_description(&mut ta, None).await?;
            }

            UpdateStudentOp::Delete => {
                let count = student.count_grades(&mut ta).await?;
                if count > 0 {
                    return Err(Error::from(format!(
                        "Oppilaalle ”{o}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                        o = student.fullname(),
                        c = count
                    )));
                }

                student.delete(&mut ta).await?;
                Group::delete_empty(&mut ta).await?;
            }
        }

        ta.commit().await?;
        Ok(())
    }
}

impl Commit for InsertStudent {
    async fn commit(self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;

        Student::insert_db(
            &mut ta,
            self.lastname,
            self.firstname,
            self.groups,
            self.description,
        )
        .await?;

        ta.commit().await?;
        Ok(())
    }
}
