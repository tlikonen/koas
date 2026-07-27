use super::*;

pub struct Group {
    rid: i32,
    name: String,
    description: String,
}

pub struct GroupNames(Vec<GroupName>);

pub struct GroupName(String);

pub type UpdateGroup<'a> = Update<'a, Group, UpdateGroupOp>;

pub enum UpdateGroupOp {
    Name(GroupName),
    Description(Description),
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

    /// Return group's name,
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return group's description,
    pub fn description(&self) -> &str {
        &self.description
    }

    /// Prepare update for group's name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_name<'a>(&'a self, name: GroupName) -> UpdateGroup<'a> {
        Update::new(self, UpdateGroupOp::Name(name))
    }

    /// Prepare update for group's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: Description) -> UpdateGroup<'a> {
        Update::new(self, UpdateGroupOp::Description(desc))
    }

    /// Prepare to clear group's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateGroup<'a> {
        Update::new(self, UpdateGroupOp::DescriptionClear)
    }

    pub(super) async fn get_or_insert(db: &mut DBase, name: &GroupName) -> Result<i32> {
        match Self::get_id(db, name).await? {
            Some(rid) => Ok(rid),
            None => {
                let row = sqlx::query("INSERT INTO ryhmat (nimi) VALUES ($1) RETURNING rid")
                    .bind(name.as_str())
                    .fetch_one(db)
                    .await?;
                let rid: i32 = row.try_get("rid")?;
                Ok(rid)
            }
        }
    }

    pub(super) async fn get_id(db: &mut DBase, name: &GroupName) -> Result<Option<i32>> {
        match sqlx::query("SELECT rid FROM ryhmat WHERE nimi = $1")
            .bind(name.as_str())
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

    async fn update_name(&self, db: &mut DBase, name: &GroupName) -> Result<()> {
        sqlx::query("UPDATE ryhmat SET nimi = $1 WHERE rid = $2")
            .bind(name.as_str())
            .bind(self.rid)
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

        sqlx::query("UPDATE ryhmat SET lisatiedot = $1 WHERE rid = $2")
            .bind(desc)
            .bind(self.rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(super) async fn delete_empty(db: &mut DBase) -> Result<()> {
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

impl GroupName {
    fn is_valid(name: &str) -> bool {
        !name.has_whitespace() && name.has_content()
    }
}

impl TextField for GroupName {
    fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<&str> for GroupName {
    type Error = Error;
    fn try_from(name: &str) -> Result<Self> {
        if let Some(g) = name.normalize()
            && GroupName::is_valid(&g)
        {
            Ok(Self(g))
        } else {
            Err(Error::InvalidGroupname(name.to_string()))
        }
    }
}

impl fmt::Display for GroupName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl GroupNames {
    pub fn iter(&self) -> impl Iterator<Item = &GroupName> {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a GroupNames {
    type Item = &'a GroupName;
    type IntoIter = std::slice::Iter<'a, GroupName>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl IntoIterator for GroupNames {
    type Item = GroupName;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl TryFrom<&str> for GroupNames {
    type Error = Error;
    fn try_from(names: &str) -> Result<Self> {
        let mut v: Vec<GroupName> = Vec::new();
        for group in names.split_whitespace() {
            v.push(group.try_into()?);
        }

        if v.is_empty() {
            Err(Error::InvalidGroupname(names.to_string()))
        } else {
            Ok(Self(v))
        }
    }
}

impl TryFrom<&Vec<String>> for GroupNames {
    type Error = Error;
    fn try_from(names: &Vec<String>) -> Result<Self> {
        let mut v = Vec::new();
        for group in names {
            v.push(group.as_str().try_into()?);
        }

        if v.is_empty() {
            Err(Error::InvalidGroupname("".to_string()))
        } else {
            Ok(Self(v))
        }
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
            UpdateGroupOp::Description(desc) => {
                group.update_description(&mut ta, Some(desc)).await?
            }
            UpdateGroupOp::DescriptionClear => group.update_description(&mut ta, None).await?,
        }

        ta.commit().await?;
        Ok(())
    }
}
