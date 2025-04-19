use crate::{
    Modes,
    database::{Editable, Groups, Stats},
    tools,
};
use sqlx::PgConnection;
use std::error::Error;

pub async fn stats(modes: &Modes, db: &mut PgConnection) -> Result<(), Box<dyn Error>> {
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
    if args.is_empty() {
        args = "/";
    }

    // /ryhmä/lisätiedot
    let mut split = tools::split_sep(args);
    let group = split.next().unwrap_or("");
    let desc = split.next().unwrap_or("");

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

fn print_not_found() {
    eprintln!("Ei löytynyt.");
}

pub fn help(args: &str) {
    println!("Tähän jotain apua: {args}");
}
