use crate::{
    Modes,
    database::{Editable, EditableItem, Group, Groups, Stats, Student, Students},
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

pub async fn students(
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
    let lastname = fields.next().unwrap_or(""); // sukunimi
    let firstname = fields.next().unwrap_or(""); // etunimi
    let group = fields.next().unwrap_or(""); // ryhma
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = Students::query(db, lastname, firstname, group, desc).await?;
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
    editable.print_fields(&["sukunimi", "etunimi", "ryhmät", "lisätiedot"]);
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
        EditableItem::Students(students) => {
            edit_students(&mut ta, indexes, students, fields).await?;
        }
        EditableItem::Groups(groups) => {
            edit_groups(&mut ta, indexes, groups, fields).await?;
        }
        EditableItem::Assignments => todo!(),
        EditableItem::Scores => todo!(),
        EditableItem::None => panic!("EditableItem::None"),
    }
    ta.commit().await?;
    Ok(())
}

async fn edit_students(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    students: &[Student],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let lastname = fields.next().unwrap_or(""); // sukunimi
    let firstname = fields.next().unwrap_or(""); // etunimi
    let groups = fields.next().unwrap_or(""); // ryhmät
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let mut lastname_update = false;
    let mut firstname_update = false;
    let mut groups_update = false;
    let mut desc_update = false;

    if tools::has_content(lastname) {
        lastname_update = true;
    }

    if tools::has_content(firstname) {
        firstname_update = true;
    }

    if (lastname_update || firstname_update) && indexes.len() > 1 {
        Err("Usealle henkilölle ei voi muuttaa kerralla samaa nimeä.\n\
             Muuta yksi kerrallaan, jos se on tarkoituksena.")?;
    }

    let mut groups_add: Vec<String> = Vec::new();
    let mut groups_remove: Vec<String> = Vec::new();

    if tools::has_content(groups) {
        for g in groups.split(' ').filter(|s| !s.is_empty()) {
            let mut chars = g.chars();
            match chars.next() {
                Some('+') => groups_add.push(chars.collect()),
                Some('-') => groups_remove.push(chars.collect()),
                _ => Err(
                    "Kirjoita oppilaan ryhmätunnuksen alkuun merkki ”+” (lisää ryhmä) \
                     tai ”-” (poista ryhmä).\nErota eri ryhmät välilyönnillä.",
                )?,
            }
        }

        if groups_add.iter().any(|s| s.is_empty()) || groups_remove.iter().any(|s| s.is_empty()) {
            Err("Ryhmän nimiä puuttuu. Kirjoita merkin ”+” tai ”-” jälkeen ryhmätunnus.")?;
        }

        groups_update = true;
    }

    if !desc.is_empty() {
        desc_update = true;
    }

    let lastname = tools::normalize_str(lastname);
    let firstname = tools::normalize_str(firstname);
    let desc = tools::normalize_str(desc);

    for i in indexes {
        let student = match students.get(i - 1) {
            None => Err("Muokattavia oppilaita ei ole.")?,
            Some(v) => v,
        };

        if lastname_update {
            student.edit_lastname(db, &lastname).await?;
        }

        if firstname_update {
            student.edit_firstname(db, &firstname).await?;
        }

        if groups_update {
            for name in &groups_add {
                let rid = match Group::get_id(db, name).await? {
                    Some(id) => id,
                    None => Group::insert(db, name).await?,
                };

                if student.in_group(db, rid).await? {
                    continue;
                } else {
                    student.add_to_group(db, rid).await?;
                }
            }

            for name in &groups_remove {
                let rid = match Group::get_id(db, name).await? {
                    Some(id) => id,
                    None => continue,
                };

                if !student.in_group(db, rid).await? {
                    continue;
                }

                if student.only_one_group(db).await? {
                    Err("Oppilaan pitää kuulua vähintään yhteen ryhmään.")?;
                } else {
                    student.remove_from_group(db, rid).await?;
                }
            }
        }

        if desc_update {
            student.edit_description(db, &desc).await?;
        }
    }

    Groups::delete_empty(db).await?;
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

    let mut name_update = false;
    let mut desc_update = false;

    if !name.is_empty() {
        let (first, rest) = tools::split_first(name);
        if tools::has_content(first) && rest.is_empty() {
            if indexes.len() > 1 {
                Err("Usealle ryhmälle ei voi antaa samaa nimeä.")?;
            }
            name = first;
            name_update = true;
        } else {
            Err(format!(
                "Ryhmätunnus ”{name}” ei kelpaa: pitää olla yksi sana."
            ))?;
        }
    }

    if !desc.is_empty() {
        desc_update = true;
    }
    let desc = tools::normalize_str(desc);

    for i in indexes {
        let group = match groups.get(i - 1) {
            None => Err("Muokattavia ryhmiä ei ole.")?,
            Some(g) => g,
        };
        if name_update {
            group.edit_name(db, name).await?;
        }
        if desc_update {
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
        Err("Edellinen komento ei sisällä poistettavia tietueita.")?;
    }

    if args.is_empty() {
        Err("Puuttuu tietueiden numerot.")?;
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        tools::parse_number_list(first)?
    };

    {
        let max = editable.count();
        if !tools::is_within_limits(max, &indexes) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }
    }

    //let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Students(_) => todo!(),
        EditableItem::Groups(_) => {
            Err("Ryhmiä ei voi poistaa näin. Ryhmä poistuu itsestään,\n\
                 kun siltä poistaa kaikki oppilaat ja suoritukset.")?;
        }
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
