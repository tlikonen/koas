use crate::prelude::*;

pub async fn groups(db: &mut DBase, args: &str) -> Result<Groups> {
    let mut fields = tools::split_sep(args);
    let name = fields.next().unwrap_or(""); // ryhmä
    let desc = fields.next().unwrap_or(""); // lisätiedot

    Groups::query(db, name, desc).await
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

        if let Field::Value(s) = &name
            && s.split_whitespace().nth(1).is_some()
        {
            return Err("Ryhmätunnuksen pitää olla yksi sana.".into());
        }

        for group in self.iter() {
            if let Field::Value(n) = name {
                group.update_name(db, n).await?;
            }

            match desc {
                Field::Value(d) => group.update_description(db, d).await?,
                Field::ValueEmpty => group.update_description(db, "").await?,
                Field::None => (),
            }
        }
        Ok(())
    }
}
