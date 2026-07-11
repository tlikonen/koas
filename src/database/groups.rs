use super::*;

#[derive(Clone)]
pub struct Group {
    pub(crate) rid: i32,
    pub name: String,
    pub description: String,
}

impl Group {
    pub(crate) async fn query(db: &mut DBase, group: &str, desc: &str) -> Result<QueryList<Self>> {
        let mut rows = sqlx::query(
            "SELECT rid, nimi, lisatiedot FROM ryhmat \
             WHERE nimi LIKE $1 ESCAPE '\\' AND lisatiedot LIKE $2 ESCAPE '\\' \
             ORDER BY nimi, lisatiedot, rid",
        )
        .bind(like_esc_wild_around(group))
        .bind(like_esc_wild_around(desc))
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

    pub(crate) async fn update_name(&self, db: &mut DBase, name: &str) -> Result<()> {
        sqlx::query("UPDATE ryhmat SET nimi = $1 WHERE rid = $2")
            .bind(name)
            .bind(self.rid)
            .execute(db)
            .await?;
        Ok(())
    }

    pub(crate) async fn update_description(&self, db: &mut DBase, desc: &str) -> Result<()> {
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
        self.list().is_empty()
    }
}

pub enum UpdateGroupField {
    Name(String),
    Description(String),
    DescriptionClear,
}

pub type UpdateGroup<'a> = Update<'a, Group, UpdateGroupField>;
