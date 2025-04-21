use crate::{
    Modes,
    database::{Editable, Groups, Stats},
    tools,
};
use sqlx::PgConnection;
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
    eprintln!("editable lkm: {}", editable.count());
    if editable.is_empty() {
        Err("Edellinen komento ei sisällä muokattavia tietueita.".to_string())?;
    }

    if args.is_empty() {
        Err("Pitää antaa tietueiden numerot ja muokattavat kentät.".to_string())?;
    }

    let (numbers, fields) = {
        let (first, rest) = tools::split_first(args);
        let n = tools::parse_number_list(first)?;
        let f = tools::split_sep(rest);
        (n, f)
    };

    eprintln!("Numerolista: {numbers:?}");
    eprintln!("Kentät: {:?}", fields.collect::<Vec<&str>>());

    {
        let max = editable.count();
        if !tools::is_within_limits(max, &numbers) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }
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
