use super::*;

/// Query for groups.
pub async fn groups(
    db: &mut DBase,
    name: QueryMatch<'_>,
    description: QueryMatch<'_>,
) -> Result<QueryList<Group>> {
    Group::query(db, name, description).await
}

impl Group {
    /// Prepare update for group's name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_name<'a>(&'a self, name: &str) -> Result<UpdateGroup<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton ryhmän nimi: ”{name}”.").into()),
            Some(n) => {
                n.is_valid_group_name()?;
                Ok(UpdateGroup {
                    item: self,
                    operation: UpdateGroupOp::Name(n),
                })
            }
        }
    }

    /// Prepare update for group's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: &str) -> Result<UpdateGroup<'a>> {
        match desc.normalize() {
            None => Err(format!("Sopimaton ryhmän kuvaus: ”{desc}”.").into()),
            Some(d) => Ok(UpdateGroup {
                item: self,
                operation: UpdateGroupOp::Description(d),
            }),
        }
    }

    /// Prepare to clear group's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateGroup<'a> {
        UpdateGroup {
            item: self,
            operation: UpdateGroupOp::DescriptionClear,
        }
    }
}

impl<'a> ToQueue<'a> for UpdateGroup<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push(QueueItem::UpdateGroup(self));
    }
}

impl Commit for UpdateGroup<'_> {
    async fn commit(&self, db: &mut DBase) -> Result<()> {
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
