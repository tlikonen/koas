use crate::prelude::*;

pub async fn groups(
    modes: &Modes,
    db: &mut DBase,
    editable: &mut Editable,
    args: &str,
) -> Result<()> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let name = fields.next().unwrap_or(""); // ryhmä
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = Groups::query(db, name, desc).await?.has_data()?;

    if modes.is_interactive() {
        query.copy_to(editable);
        query.print_num(modes.output())?;
        editable.print_fields(&["Ryhmä", "Lisätiedot"])?;
    } else {
        query.print(modes.output())?;
    }
    Ok(())
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
