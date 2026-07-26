use super::*;

#[derive(Clone)]
pub struct Group {
    pub(crate) rid: i32,
    pub name: String,
    pub description: String,
}

pub type UpdateGroup<'a> = Update<'a, Group, UpdateGroupOp>;

pub enum UpdateGroupOp {
    Name(String),
    Description(String),
    DescriptionClear,
}

impl Group {
    /// Query for groups.
    pub async fn query(
        db: &mut DBase,
        group: QueryMatch<'_>,
        desc: QueryMatch<'_>,
    ) -> Result<QueryList<Self>> {
        let mut rows = sqlx::query(
            "SELECT rid, nimi, lisatiedot FROM ryhmat \
             WHERE nimi LIKE $1 ESCAPE '\\' AND lisatiedot LIKE $2 ESCAPE '\\' \
             ORDER BY nimi, lisatiedot, rid",
        )
        .bind(group.sql_like())
        .bind(desc.sql_like())
        .fetch(db);

        let mut list = Vec::with_capacity(10);
        while let Some(row) = rows.try_next().await? {
            list.push(Self {
                rid: row.try_get("rid")?,
                name: row.try_get("nimi")?,
                description: row.try_get("lisatiedot")?,
            });
        }

        Ok(QueryList::new(list))
    }

    /// Prepare update for group's name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_name<'a>(&'a self, name: &str) -> Result<UpdateGroup<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!("Sopimaton ryhmän nimi: ”{name}”."))),
            Some(n) => {
                n.is_valid_group_name()?;
                Ok(Update::new(self, UpdateGroupOp::Name(n)))
            }
        }
    }

    /// Prepare update for group's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: &str) -> Result<UpdateGroup<'a>> {
        match desc.normalize() {
            None => Err(Error::from(format!("Sopimaton ryhmän kuvaus: ”{desc}”."))),
            Some(d) => Ok(Update::new(self, UpdateGroupOp::Description(d))),
        }
    }

    /// Prepare to clear group's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateGroup<'a> {
        Update::new(self, UpdateGroupOp::DescriptionClear)
    }

    pub(crate) async fn get_or_insert(db: &mut DBase, name: &str) -> Result<i32> {
        match Self::get_id(db, name).await? {
            Some(rid) => Ok(rid),
            None => {
                let row = sqlx::query("INSERT INTO ryhmat (nimi) VALUES ($1) RETURNING rid")
                    .bind(name)
                    .fetch_one(db)
                    .await?;
                let rid: i32 = row.try_get("rid")?;
                Ok(rid)
            }
        }
    }

    pub(crate) async fn get_id(db: &mut DBase, name: &str) -> Result<Option<i32>> {
        match sqlx::query("SELECT rid FROM ryhmat WHERE nimi = $1")
            .bind(name)
            .fetch_optional(db)
            .await?
        {
            None => Ok(None),
            Some(row) => {
                let rid: i32 = row.try_get("rid")?;
                Ok(Some(rid))
            }
        }
    }

    async fn update_name(&self, db: &mut DBase, name: &str) -> Result<()> {
        sqlx::query("UPDATE ryhmat SET nimi = $1 WHERE rid = $2")
            .bind(name)
            .bind(self.rid)
            .execute(db)
            .await?;
        Ok(())
    }

    async fn update_description(&self, db: &mut DBase, desc: &str) -> Result<()> {
        sqlx::query("UPDATE ryhmat SET lisatiedot = $1 WHERE rid = $2")
            .bind(desc)
            .bind(self.rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn delete_empty(db: &mut DBase) -> Result<()> {
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

impl HasData for QueryList<Group> {
    fn is_empty(&self) -> bool {
        self.list_is_empty()
    }
}

impl<'a> ToQueue<'a> for UpdateGroup<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::UpdateGroup(self));
    }
}

impl Commit for UpdateGroup<'_> {
    async fn commit(self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        let group = self.item;

        match &self.operation {
            UpdateGroupOp::Name(name) => group.update_name(&mut ta, name).await?,
            UpdateGroupOp::Description(desc) => group.update_description(&mut ta, desc).await?,
            UpdateGroupOp::DescriptionClear => group.update_description(&mut ta, "").await?,
        }

        ta.commit().await?;
        Ok(())
    }
}
