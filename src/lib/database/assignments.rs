use super::*;

#[derive(Clone, Default)]
pub struct Assignment {
    pub(super) rid: i32,
    pub(super) sid: i32,
    pub(super) assignment: String,
    pub(super) assignment_short: String,
    pub(super) weight: Option<i32>,
}

#[derive(Default)]
pub struct AssignmentsForGroup {
    group: String,
    assignments: Vec<Assignment>,
}

pub type AssignmentName = Field<ContextAssignmentName, String>;
pub type AssignmentShort = Field<ContextAssignmentShort, String>;
pub type AssignmentWeight = Field<ContextAssignmentWeight, i32>;
pub type AssignmentPosition = Field<ContextAssignmentPosition, i32>;
pub type UpdateAssignment<'a> = Update<'a, Assignment, UpdateAssignmentOp>;

#[derive(Clone, Default)]
pub struct ContextAssignmentName;
#[derive(Clone, Default)]
pub struct ContextAssignmentShort;
#[derive(Clone, Default)]
pub struct ContextAssignmentWeight;
#[derive(Clone, Default)]
pub struct ContextAssignmentPosition;

pub enum UpdateAssignmentOp {
    Name(AssignmentName),
    Short(AssignmentShort),
    Weight(AssignmentWeight),
    WeightClear,
    Position(AssignmentPosition),
    Delete,
}

pub struct InsertAssignment {
    group: GroupName,
    assignment: AssignmentName,
    assignment_short: AssignmentShort,
    weight: Option<AssignmentWeight>,
    position: Option<AssignmentPosition>,
}

impl Assignment {
    /// Prepare to insert a new assignment.
    ///
    /// See [`Commit`] trait for more information.
    pub fn insert(
        group: GroupName,
        assignment: AssignmentName,
        assignment_short: AssignmentShort,
        weight: Option<AssignmentWeight>,
        position: Option<AssignmentPosition>,
    ) -> InsertAssignment {
        InsertAssignment {
            group,
            assignment,
            assignment_short,
            weight,
            position,
        }
    }

    /// Return assignment's name.
    pub fn name(&self) -> &str {
        &self.assignment
    }

    /// Return assignment's short name.
    pub fn short(&self) -> &str {
        &self.assignment_short
    }

    /// Return assignment's weight (if any).
    pub fn weight(&self) -> Option<i32> {
        self.weight
    }

    /// Prepare update for assignment's name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_name<'a>(&'a self, name: AssignmentName) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::Name(name))
    }

    /// Prepare update for assignment's short name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_short<'a>(&'a self, name: AssignmentShort) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::Short(name))
    }

    /// Prepare update for assignment's weight.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_weight<'a>(&'a self, number: AssignmentWeight) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::Weight(number))
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
    pub fn set_position<'a>(&'a self, number: AssignmentPosition) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::Position(number))
    }

    /// Prepare deletion of assignment.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::Delete)
    }

    async fn update_name(&self, db: &mut DBase, name: &AssignmentName) -> Result<()> {
        sqlx::query("UPDATE suoritukset SET nimi = $1 WHERE sid = $2")
            .bind(name.as_str())
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_short(&self, db: &mut DBase, short: &AssignmentShort) -> Result<()> {
        sqlx::query("UPDATE suoritukset SET lyhenne = $1 WHERE sid = $2")
            .bind(short.as_str())
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_weight(&self, db: &mut DBase, weight: Option<&AssignmentWeight>) -> Result<()> {
        sqlx::query("UPDATE suoritukset SET painokerroin = $1 WHERE sid = $2")
            .bind(weight.map(|w| w.value()))
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_position(&self, db: &mut DBase, position: &AssignmentPosition) -> Result<()> {
        let mut pos = position.value();
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

    async fn insert_db(
        db: &mut DBase,
        group: GroupName,
        assignment: AssignmentName,
        assignment_short: AssignmentShort,
        weight: Option<AssignmentWeight>,
        position: Option<AssignmentPosition>,
    ) -> Result<()> {
        let pos = position.unwrap_or_default();
        let rid = Group::get_or_insert(&mut *db, &group).await?;

        let row = sqlx::query(
            "INSERT INTO suoritukset (rid, nimi, lyhenne, painokerroin, sija) \
             VALUES ($1, $2, $3, $4, $5) RETURNING sid",
        )
        .bind(rid)
        .bind(assignment.as_str())
        .bind(assignment_short.as_str())
        .bind(weight.map(|w| w.value()))
        .bind(pos.value())
        .fetch_one(&mut *db)
        .await?;

        let new = Assignment {
            sid: row.try_get("sid")?,
            ..Assignment::default()
        };

        new.update_position(db, &pos).await?;
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

    /// Return group's name.
    pub fn group(&self) -> &str {
        &self.group
    }

    /// Return iterator over assignments.
    pub fn iter(&self) -> impl Iterator<Item = &Assignment> {
        self.assignments.iter()
    }
}

impl Default for AssignmentPosition {
    fn default() -> Self {
        Self::new(i32::MAX)
    }
}

impl<'a> IntoIterator for &'a AssignmentsForGroup {
    type Item = &'a Assignment;
    type IntoIter = std::slice::Iter<'a, Assignment>;
    fn into_iter(self) -> Self::IntoIter {
        self.assignments.iter()
    }
}

impl IntoIterator for AssignmentsForGroup {
    type Item = Assignment;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.assignments.into_iter()
    }
}

impl HasData for QueryList<AssignmentsForGroup> {
    fn is_empty(&self) -> bool {
        self.list_is_empty()
    }
}

impl TryFrom<&str> for AssignmentName {
    type Error = Error;
    fn try_from(name: &str) -> Result<Self> {
        match name.normalize() {
            Some(n) => Ok(Self::new(n)),
            None => Err(Error::InvalidAssignmentName(name.to_string())),
        }
    }
}

impl TryFrom<&str> for AssignmentShort {
    type Error = Error;
    fn try_from(name: &str) -> Result<Self> {
        match name.normalize() {
            Some(n) if (1..=5).contains(&n.chars().count()) => Ok(Self::new(n)),
            _ => Err(Error::InvalidAssignmentShort(name.to_string())),
        }
    }
}

impl TryFrom<&str> for AssignmentWeight {
    type Error = Error;
    fn try_from(s: &str) -> Result<Self> {
        match s.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Ok(Self::new(n)),
            _ => Err(Error::InvalidAssignmentWeight(s.to_string())),
        }
    }
}

impl TryFrom<i32> for AssignmentWeight {
    type Error = Error;
    fn try_from(v: i32) -> Result<Self> {
        if v >= 1 {
            Ok(Self::new(v))
        } else {
            Err(Error::InvalidAssignmentWeight(v.to_string()))
        }
    }
}

impl TryFrom<&str> for AssignmentPosition {
    type Error = Error;
    fn try_from(s: &str) -> Result<Self> {
        match s.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Ok(Self::new(n)),
            _ => Err(Error::InvalidAssignmentPosition(s.to_string())),
        }
    }
}

impl TryFrom<i32> for AssignmentPosition {
    type Error = Error;
    fn try_from(v: i32) -> Result<Self> {
        if v >= 1 {
            Ok(Self::new(v))
        } else {
            Err(Error::InvalidAssignmentPosition(v.to_string()))
        }
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
                assignment.update_weight(&mut ta, Some(weight)).await?
            }
            UpdateAssignmentOp::WeightClear => assignment.update_weight(&mut ta, None).await?,
            UpdateAssignmentOp::Position(pos) => assignment.update_position(&mut ta, pos).await?,

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

        Assignment::insert_db(
            &mut ta,
            self.group,
            self.assignment,
            self.assignment_short,
            self.weight,
            self.position,
        )
        .await?;

        ta.commit().await?;
        Ok(())
    }
}
