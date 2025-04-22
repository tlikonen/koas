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
    if !args.is_empty() {
        print_unnecessary_arguments();
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

    // /ryhmä/lisätiedot
    let mut split = tools::split_sep(args);
    let group = split.next().unwrap_or("");
    let desc = split.next().unwrap_or("");
    if split.next().is_some() {
        print_unnecessary_arguments();
    }

    let groups = Groups::query(db, group, desc).await?;
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
    _db: &mut PgConnection,
    indexes: Vec<usize>,
    groups: &mut [Group],
    // _fields: &[&str],
    _fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    for i in indexes {
        let index = i - 1;
        let group = &mut groups[index];
        eprintln!("{:?}", group);
    }
    Ok(())
}

fn print_not_found() {
    eprintln!("Ei löytynyt.");
}

fn print_unnecessary_arguments() {
    eprintln!("Turhia argumentteja annettu komennolle.");
}

pub fn help(editable: &mut Editable, args: &str) {
    editable.clear();
    println!("Tähän jotain apua: {args}");
}
