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
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if let Some(s) = args.split(' ').next() {
        print_unnecessary_arguments(s);
    }

    let stats = Stats::query(db).await?;
    stats.table().print(modes.output());
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
    if let Some(s) = fields.next() {
        print_unnecessary_arguments(s);
    }

    let groups = Groups::query(db, name, desc).await?;
    if groups.is_empty() {
        print_not_found();
        return Ok(());
    }

    let mut table = groups.table();
    if modes.is_interactive() {
        table.numbering();
        groups.move_to(editable);
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
        let f = tools::split_sep(rest); //.collect::<Vec<&str>>();
        (n, f)
    };

    {
        let max = editable.count();
        if !tools::is_within_limits(max, &indexes) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }
    }

    let mut ta = db.begin().await?;
    match editable.item_as_mut() {
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
    groups: &mut [Group],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let mut name = fields.next().unwrap_or(""); // nimi
    let desc = fields.next().unwrap_or(""); // lisätiedot
    if let Some(s) = fields.next() {
        print_unnecessary_arguments(s);
    }

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
        let index = i - 1;
        let group = &mut groups[index];
        if name_set {
            group.edit_name(db, name).await?;
        }
        if desc_set {
            group.edit_description(db, &desc).await?;
        }
    }
    Ok(())
}

fn print_not_found() {
    eprintln!("Ei löytynyt.");
}

fn print_unnecessary_arguments(s: &str) {
    eprintln!("Turhia argumentteja komennolle: ”{s}” jne.");
}

pub fn help(editable: &mut Editable, args: &str) {
    editable.clear();
    println!("Tähän jotain apua: {args}");
}
