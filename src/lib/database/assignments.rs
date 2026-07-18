use super::*;

#[derive(Clone, Default)]
pub struct Assignment {
    pub(crate) rid: i32,
    pub(crate) sid: i32,
    pub assignment: String,
    pub assignment_short: String,
    pub weight: Option<i32>,
}

#[derive(Default)]
pub struct AssignmentsForGroup {
    pub group: String,
    pub assignments: Vec<Assignment>,
}

impl Assignment {
    pub(crate) async fn update_name(&self, db: &mut DBase, name: &str) -> Result<()> {
        sqlx::query("UPDATE suoritukset SET nimi = $1 WHERE sid = $2")
            .bind(name)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn update_short(&self, db: &mut DBase, short: &str) -> Result<()> {
        sqlx::query("UPDATE suoritukset SET lyhenne = $1 WHERE sid = $2")
            .bind(short)
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn update_weight(
        &self,
        db: &mut DBase,
        mut weight: Option<i32>,
    ) -> Result<()> {
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

    pub(crate) async fn update_position(&self, db: &mut DBase, mut pos: i32) -> Result<()> {
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

    pub(crate) async fn count_grades(&self, db: &mut DBase) -> Result<i64> {
        let count: i64 = sqlx::query("SELECT count(*) AS count FROM arvosanat WHERE sid = $1")
            .bind(self.sid)
            .fetch_one(db)
            .await?
            .try_get("count")?;
        Ok(count)
    }

    pub(crate) async fn insert_db(&mut self, db: &mut DBase, pos: i32) -> Result<()> {
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

    pub(crate) async fn delete(&self, db: &mut DBase) -> Result<()> {
        sqlx::query("DELETE FROM suoritukset WHERE sid = $1")
            .bind(self.sid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn reposition(db: &mut DBase, rid: i32) -> Result<()> {
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
    pub(crate) async fn query(db: &mut DBase, group: QueryMatch<'_>) -> Result<QueryList<Self>> {
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
        self.list().is_empty()
    }
}

pub enum UpdateAssignmentOp {
    Name(String),
    Short(String),
    Weight(i32),
    WeightClear,
    Position(i32),
    Delete,
}

pub type UpdateAssignment<'a> = Update<'a, Assignment, UpdateAssignmentOp>;

pub struct InsertAssignment {
    pub(crate) group: String,
    pub(crate) assignment: String,
    pub(crate) assignment_short: String,
    pub(crate) weight: Option<i32>,
    pub(crate) position: Option<i32>,
}
