use super::*;
use crate::prelude::*;

/// Query for groups.
///
/// Wildcard character "*" is allowed in the query strings and is
/// implicit in the start and end the strings.
pub async fn groups(db: &mut DBase, name: &str, description: &str) -> Result<QueryList<Group>> {
    Group::query(db, name, description).await
}

impl DeprecatedEdit for DeprecatedEditItems<'_, Group> {
    async fn edit(&self, db: &mut DBase) -> Result<()> {
        let name = self.field(0); // ryhmä
        let desc = self.field(1); // lisätiedot

        if !name.has_value() && desc.is_none() {
            return Err("Anna muokattavia kenttiä.".into());
        }

        if name.has_value() && self.count() > 1 {
            return Err("Usealle ryhmälle ei voi antaa samaa nimeä.".into());
        }

        if let DeprecatedField::Set(s) = &name
            && s.split_whitespace().nth(1).is_some()
        {
            return Err("Ryhmätunnuksen pitää olla yksi sana.".into());
        }

        for group in self.iter() {
            if let DeprecatedField::Set(n) = name {
                group.update_name(db, n).await?;
            }

            match desc {
                DeprecatedField::Set(d) => group.update_description(db, d).await?,
                DeprecatedField::Clear => group.update_description(db, "").await?,
                DeprecatedField::Ignore => (),
            }
        }
        Ok(())
    }
}
