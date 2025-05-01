mod commands;
pub mod config;
mod database;
pub mod modes;
mod print;
pub mod tools;

use crate::{
    config::Config,
    database::Editable,
    modes::{Mode, Modes, Output},
};
use sqlx::{Connection, PgConnection};
use std::{error::Error, io};

pub async fn command_stage(modes: Modes, config: Config) -> Result<(), Box<dyn Error>> {
    const PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
    const PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");

    let mut db = database::connect(&config).await?;
    // database::init()?;    // Check and create database item here.
    let mut editable: Editable = Default::default();

    match modes.mode() {
        Mode::Interactive => {
            println!(
                "{prg} v{ver} (postgres://{user}@{host}:{port}/{db})",
                prg = PROGRAM_NAME,
                ver = PROGRAM_VERSION,
                user = config.user,
                host = config.host,
                port = config.port,
                db = config.database,
            );

            let prompt = format!("{}> ", env!("CARGO_PKG_NAME"));
            let mut rl = rustyline::DefaultEditor::new()?;

            loop {
                let line = rl.readline(&prompt)?;
                if line.is_empty() {
                    break;
                }
                rl.add_history_entry(&line)?;

                let (cmd, args) = tools::split_first(&line);

                match interactive_commands(&modes, &mut db, &mut editable, cmd, args).await {
                    Ok(true) => (),
                    Ok(false) => eprintln!("Tuntematon komento ”{cmd}”. Apua saa ?:llä."),
                    Err(e) => eprintln!("{e}"),
                }
            }
        }

        Mode::Single(line) => {
            let (cmd, args) = tools::split_first(line);
            if !query_commands(&modes, &mut db, &mut editable, cmd, args).await? {
                Err(format!(
                    "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                ))?;
            }
        }

        Mode::Stdin => {
            let mut ta = db.begin().await?;
            for item in io::stdin().lines() {
                let line = item?;
                if !line.is_empty() {
                    let (cmd, args) = tools::split_first(&line);
                    if !stdin_commands(&modes, &mut ta, &mut editable, cmd, args).await? {
                        Err(format!(
                            "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                        ))?;
                    }
                }
            }
            ta.commit().await?;
        }
    }
    Ok(())
}

async fn interactive_commands(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    let result = query_commands(modes, db, editable, cmd, args).await?
        || edit_commands(db, editable, cmd, args).await?
        || insert_commands(db, editable, cmd, args).await?
        || help_commands(editable, cmd, args)?;
    Ok(result)
}

async fn stdin_commands(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    let result = query_commands(modes, db, editable, cmd, args).await?
        || insert_commands(db, editable, cmd, args).await?;
    Ok(result)
}

async fn query_commands(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    match cmd {
        "tk" => commands::stats(modes, db, editable).await?,
        "hr" => commands::groups(modes, db, editable, args).await?,
        "ho" => commands::students(modes, db, editable, args).await?,
        "has" => commands::scores_for_assignments(modes, db, editable, args).await?,
        "hao" => commands::scores_for_students(modes, db, editable, args).await?,
        _ => return Ok(false),
    }
    Ok(true)
}

async fn edit_commands(
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    match cmd {
        "m" => commands::edit(db, editable, args).await?,
        "ms" => commands::edit_series(db, editable, args).await?,
        "ma" => commands::convert_to_score(db, editable, args).await?,
        "md" => commands::convert_to_decimal(db, editable, args).await?,
        "poista" => commands::delete(db, editable, args).await?,
        _ => return Ok(false),
    }
    Ok(true)
}

async fn insert_commands(
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    match cmd {
        "lo" => commands::insert_student(db, editable, args).await?,
        _ => return Ok(false),
    }
    Ok(true)
}

fn help_commands(editable: &mut Editable, cmd: &str, args: &str) -> Result<bool, Box<dyn Error>> {
    match cmd {
        "?" => commands::help(editable, args),
        _ => return Ok(false),
    }
    Ok(true)
}
