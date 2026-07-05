use crate::prelude::*;

/// Query for groups.
///
/// The `fields` tuple is: 0) group name, 1) description. Wildcard
/// character "*" is allowed and is implicit in the start and end the
/// strings.
pub async fn groups(db: &mut DBase, fields: (&str, &str)) -> Result<QueryList<Group>> {
    let name = fields.0;
    let desc = fields.1;

    Group::query(db, name, desc).await
}

impl Edit for EditItems<'_, Group> {
    async fn edit(&self, db: &mut DBase) -> Result<()> {
        let name = self.field(0); // ryhmä
        let desc = self.field(1); // lisätiedot

        if !name.has_value() && desc.is_none() {
            return Err("Anna muokattavia kenttiä.".into());
        }

        if name.has_value() && self.count() > 1 {
            return Err("Usealle ryhmälle ei voi antaa samaa nimeä.".into());
        }

        if let Field::Set(s) = &name
            && s.split_whitespace().nth(1).is_some()
        {
            return Err("Ryhmätunnuksen pitää olla yksi sana.".into());
        }

        for group in self.iter() {
            if let Field::Set(n) = name {
                group.update_name(db, n).await?;
            }

            match desc {
                Field::Set(d) => group.update_description(db, d).await?,
                Field::Clear => group.update_description(db, "").await?,
                Field::Ignore => (),
            }
        }
        Ok(())
    }
}
