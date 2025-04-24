use crate::{
    Modes,
    database::{Editable, EditableItem, Group, Groups, Stats},
    tools,
};
use sqlx::{Connection, PgConnection};
use std::error::Error;

pub async fn stats(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let query = Stats::query(db).await?;
    query.table().print(modes.output());
    Ok(())
}

pub async fn groups(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "/";
    }

    let mut fields = tools::split_sep(args);
    let name = fields.next().unwrap_or(""); // nimi
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = Groups::query(db, name, desc).await?;
    if query.is_empty() {
        print_not_found();
        return Ok(());
    }

    let mut table = query.table();
    if modes.is_interactive() {
        table.numbering();
        query.move_to(editable);
    }
    table.print(modes.output());
    editable.print_fields(&["nimi", "lisätiedot"]);
    Ok(())
}

pub async fn edit(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä muokattavia tietueita.".to_string())?;
    }

    if args.is_empty() {
        Err("Puuttuu tietueiden numerot ja muokattavat kentät.".to_string())?;
    }

    let (indexes, fields) = {
        let (first, rest) = tools::split_first(args);
        let n = tools::parse_number_list(first)?;
        let f = tools::split_sep(rest);
        (n, f)
    };

    {
        let max = editable.count();
        if !tools::is_within_limits(max, &indexes) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }
    }

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Groups(groups) => {
            edit_groups(&mut ta, indexes, groups, fields).await?;
        }
        EditableItem::Students => todo!(),
        EditableItem::Assignments => todo!(),
        EditableItem::Scores => todo!(),
        EditableItem::None => panic!("EditableItem::None"),
    }
    ta.commit().await?;
    Ok(())
}

async fn edit_groups(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    groups: &[Group],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let mut name = fields.next().unwrap_or(""); // nimi
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let mut name_set = false;
    let mut desc_set = false;

    if !name.is_empty() {
        let (first, rest) = tools::split_first(name);
        if tools::has_content(first) && rest.is_empty() {
            if indexes.len() > 1 {
                Err("Usealle ryhmälle ei voi antaa samaa nimeä.".to_string())?;
            }
            name = first;
            name_set = true;
        } else {
            Err(format!(
                "Ryhmätunnus ”{name}” ei kelpaa: pitää olla yksi sana."
            ))?;
        }
    }

    if !desc.is_empty() {
        desc_set = true;
    }
    let desc = tools::normalize_str(desc);

    for i in indexes {
        let group = match groups.get(i - 1) {
            None => continue,
            Some(g) => g,
        };
        if name_set {
            group.edit_name(db, name).await?;
        }
        if desc_set {
            group.edit_description(db, &desc).await?;
        }
    }
    Ok(())
}

pub async fn delete(
    _db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä poistettavia tietueita.".to_string())?;
    }

    if args.is_empty() {
        Err("Puuttuu tietueiden numerot.".to_string())?;
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let n = tools::parse_number_list(first)?;
        n
    };

    {
        let max = editable.count();
        if !tools::is_within_limits(max, &indexes) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }
    }

    //let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Groups(_) => {
            Err("Ryhmiä ei voi poistaa näin. Ryhmä poistuu itsestään,\n\
                 kun siltä poistaa kaikki oppilaat ja suoritukset."
                .to_string())?;
        }
        EditableItem::Students => todo!(),
        EditableItem::Assignments => todo!(),
        EditableItem::Scores => todo!(),
        EditableItem::None => panic!("EditableItem::None"),
    }
    //ta.commit().await?;
    Ok(())
}

fn print_not_found() {
    eprintln!("Ei löytynyt.");
}

pub fn help(editable: &mut Editable, args: &str) {
    editable.clear();
    println!("Tähän jotain apua: {args}");
}
