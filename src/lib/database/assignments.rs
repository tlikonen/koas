use super::*;

#[derive(Clone, Default)]
pub struct Assignment {
    pub(super) rid: i32,
    pub(super) sid: i32,
    pub assignment: String,
    pub assignment_short: String,
    pub weight: Option<i32>,
}

#[derive(Default)]
pub struct AssignmentsForGroup {
    pub group: String,
    pub assignments: Vec<Assignment>,
}

pub type UpdateAssignment<'a> = Update<'a, Assignment, UpdateAssignmentOp>;

pub enum UpdateAssignmentOp {
    Name(String),
    Short(String),
    Weight(i32),
    WeightClear,
    Position(i32),
    Delete,
}

pub struct InsertAssignment {
    pub(super) group: GroupName,
    pub(super) assignment: String,
    pub(super) assignment_short: String,
    pub(super) weight: Option<i32>,
    pub(super) position: Option<i32>,
}

impl Assignment {
    /// Prepare to insert a new assignment.
    ///
    /// See [`Commit`] trait for more information.
    pub fn insert(
        group: GroupName,
        assignment: &str,
        assignment_short: &str,
        weight: Option<&str>,
        position: Option<&str>,
    ) -> Result<InsertAssignment> {
        let assignment = assignment.normalize(); // suoritus
        let assignment_short = assignment_short.normalize(); // lyhenne
        let weight = weight.filter(|x| x.has_content()); // painokerroin
        let position = position.filter(|x| x.has_content()); // sija

        if assignment.is_none() || assignment_short.is_none() {
            return Err(Error::from(
                "Pitää antaa vähintään ryhmä, suorituksen nimi ja lyhenne.",
            ));
        }

        // Convert from Option<&str> to Option<i32>.
        let weight = match weight {
            Some(s) => match s.trim().parse::<i32>() {
                Ok(n) if n >= 1 => Some(n),
                _ => {
                    return Err(Error::from(
                        "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).",
                    ));
                }
            },
            None => None,
        };

        // Convert from Option<&str> to Option<i32>.
        let position = match position {
            Some(s) => match s.trim().parse::<i32>() {
                Ok(n) => Some(n),
                _ => return Err(Error::from("Järjestysnumeron täytyy olla kokonaisluku.")),
            },
            None => None,
        };

        if let Some(long) = assignment
            && let Some(short) = assignment_short
        {
            Ok(InsertAssignment {
                group,
                assignment: long,
                assignment_short: short,
                weight,
                position,
            })
        } else {
            Err(Error::from("Suorituksen lisääminen epäonnistui."))
        }
    }

    /// Prepare update for assignment's name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_name<'a>(&'a self, name: &str) -> Result<UpdateAssignment<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!(
                "Sopimaton suorituksen nimi: ”{name}”."
            ))),
            Some(n) => Ok(Update::new(self, UpdateAssignmentOp::Name(n))),
        }
    }

    /// Prepare update for assignment's short name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_short<'a>(&'a self, name: &str) -> Result<UpdateAssignment<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!(
                "Sopimaton suorituksen lyhenne: ”{name}”."
            ))),
            Some(n) => Ok(Update::new(self, UpdateAssignmentOp::Short(n))),
        }
    }

    /// Prepare update for assignment's weight.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_weight<'a>(&'a self, number: &str) -> Result<UpdateAssignment<'a>> {
        match number.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Ok(Update::new(self, UpdateAssignmentOp::Weight(n))),
            _ => Err(Error::from(
                "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).",
            )),
        }
    }

    /// Prepare to clear assignment's weight.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_weight<'a>(&'a self) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::WeightClear)
    }

    /// Prepare update for assignment's position.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_position<'a>(&'a self, number: &str) -> Result<UpdateAssignment<'a>> {
        match number.trim().parse::<i32>() {
            Ok(n) => Ok(Update::new(self, UpdateAssignmentOp::Position(n))),
            _ => Err(Error::from("Järjestysnumeron täytyy olla kokonaisluku.")),
        }
    }

    /// Prepare deletion of assignment.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::Delete)
    }

    async fn update_name(&self, db: &mut DBase, name: &str) -> Result<()> {
        sqlx::query("UPDATE suoritukset SET nimi = $1 WHERE sid = $2")
            .bind(name)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_short(&self, db: &mut DBase, short: &str) -> Result<()> {
        sqlx::query("UPDATE suoritukset SET lyhenne = $1 WHERE sid = $2")
            .bind(short)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_weight(&self, db: &mut DBase, mut weight: Option<i32>) -> Result<()> {
        if let Some(n) = weight
            && n < 1
        {
            weight = None;
        }

        sqlx::query("UPDATE suoritukset SET painokerroin = $1 WHERE sid = $2")
            .bind(weight)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_position(&self, db: &mut DBase, mut pos: i32) -> Result<()> {
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

        let other_max: i32 = other_sids.len().try_into().unwrap_or(i32::MAX);
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

    async fn count_grades(&self, db: &mut DBase) -> Result<i64> {
        let count: i64 = sqlx::query("SELECT count(*) AS count FROM arvosanat WHERE sid = $1")
            .bind(self.sid)
            .fetch_one(db)
            .await?
            .try_get("count")?;
        Ok(count)
    }

    async fn insert_db(&mut self, db: &mut DBase, pos: i32) -> Result<()> {
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

    async fn delete(&self, db: &mut DBase) -> Result<()> {
        sqlx::query("DELETE FROM suoritukset WHERE sid = $1")
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn reposition(db: &mut DBase, rid: i32) -> Result<()> {
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

impl AssignmentsForGroup {
    /// Query for assignments.
    pub async fn query(db: &mut DBase, group: QueryMatch<'_>) -> Result<QueryList<Self>> {
        if group.is_empty() {
            Err("Ryhmän nimi puuttuu.")?;
        }

        let mut rows = sqlx::query(
            "SELECT rid, ryhma, sid, suoritus, lyhenne, painokerroin FROM view_suoritukset \
             WHERE ryhma LIKE $1 ESCAPE '\\' ORDER BY ryhma, rid, sija, sid",
        )
        .bind(group.sql_like())
        .fetch(db);

        let mut row = match rows.try_next().await? {
            Some(r) => r,
            None => return Ok(QueryList::default()),
        };

        let mut list: Vec<Self> = Vec::with_capacity(10);
        let mut assignments: Vec<Assignment> = Vec::with_capacity(15);

        loop {
            let rid: i32 = row.try_get("rid")?;

            assignments.push(Assignment {
                rid,
                sid: row.try_get("sid")?,
                assignment: row.try_get("suoritus")?,
                assignment_short: row.try_get("lyhenne")?,
                weight: row.try_get("painokerroin")?,
            });

            row = match rows.try_next().await? {
                Some(next_row) => {
                    let next_rid: i32 = next_row.try_get("rid")?;
                    if next_rid != rid {
                        let l = assignments.len();
                        list.push(Self {
                            group: row.try_get("ryhma")?,
                            assignments,
                        });
                        assignments = Vec::with_capacity(l);
                    }
                    next_row
                }

                None => {
                    list.push(Self {
                        group: row.try_get("ryhma")?,
                        assignments,
                    });
                    break;
                }
            };
        }

        Ok(QueryList::new(list))
    }
}

impl HasData for QueryList<AssignmentsForGroup> {
    fn is_empty(&self) -> bool {
        self.list_is_empty()
    }
}

impl<'a> ToQueue<'a> for UpdateAssignment<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::UpdateAssignment(self));
    }
}

impl<'a> ToQueue<'a> for InsertAssignment {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::InsertAssignment(self))
    }
}

impl Commit for UpdateAssignment<'_> {
    async fn commit(self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        let assignment = self.item;

        match &self.operation {
            UpdateAssignmentOp::Name(name) => assignment.update_name(&mut ta, name).await?,
            UpdateAssignmentOp::Short(short) => assignment.update_short(&mut ta, short).await?,
            UpdateAssignmentOp::Weight(weight) => {
                assignment.update_weight(&mut ta, Some(*weight)).await?
            }
            UpdateAssignmentOp::WeightClear => assignment.update_weight(&mut ta, None).await?,
            UpdateAssignmentOp::Position(pos) => assignment.update_position(&mut ta, *pos).await?,

            UpdateAssignmentOp::Delete => {
                let count = assignment.count_grades(&mut ta).await?;
                if count > 0 {
                    return Err(Error::from(format!(
                        "Suoritukselle ”{a}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                        a = assignment.assignment,
                        c = count,
                    )));
                }

                assignment.delete(&mut ta).await?;
                Assignment::reposition(&mut ta, assignment.rid).await?;
                Group::delete_empty(&mut ta).await?;
            }
        }

        ta.commit().await?;
        Ok(())
    }
}

impl Commit for InsertAssignment {
    async fn commit(self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;

        let mut group_assignment = Assignment {
            rid: Group::get_or_insert(&mut ta, &self.group).await?,
            assignment: self.assignment.clone(),
            assignment_short: self.assignment_short.clone(),
            weight: self.weight,
            ..Assignment::default()
        };

        let pos = self.position.unwrap_or(i32::MAX);

        group_assignment.insert_db(&mut ta, pos).await?;

        ta.commit().await?;
        Ok(())
    }
}
