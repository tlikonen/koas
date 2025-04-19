pub mod commands;
pub mod config;
mod database;
mod print;

use crate::{config::Config, database::Editable};
use sqlx::{Connection, PgConnection};
use std::{error::Error, io};

pub enum Mode {
    Interactive,
    Single(String),
    Stdin,
}

#[non_exhaustive]
#[derive(Default)]
pub enum Output {
    #[default]
    Normal,
    // Tab,
    // Csv,
    // Org,
    // Latex,
}

#[derive(Default)]
pub struct Modes {
    mode: Option<Mode>,
    output: Option<Output>,
}

impl Modes {
    pub fn output(&self) -> &Output {
        self.output.as_ref().expect("Uninitialized Modes::output.")
    }

    pub fn set_output(&mut self, v: Output) {
        self.output = Some(v);
    }

    pub fn mode(&self) -> &Mode {
        self.mode.as_ref().expect("Uninitialized Modes::mode.")
    }

    pub fn set_mode(&mut self, v: Mode) {
        self.mode = Some(v);
    }

    pub fn is_interactive(&self) -> bool {
        matches!(self.mode(), Mode::Interactive)
    }
}

pub async fn command_stage(modes: Modes, config: Config) -> Result<(), Box<dyn Error>> {
    let mut db = database::connect(&config).await?;
    let mut editable: Editable = Default::default();

    match modes.mode() {
        Mode::Interactive => {
            println!(
                "PostgreSQL (postgres://{user}@{host}:{port}/{db})",
                user = config.user,
                host = config.host,
                port = config.port,
                db = config.database,
            );

            let prompt = format!("{}> ", env!("CARGO_PKG_NAME"));
            let mut rl = rustyline::DefaultEditor::new()?;

            while let Ok(line) = rl.readline(&prompt) {
                if line.is_empty() {
                    break;
                }
                let _ = rl.add_history_entry(&line);

                command_line_interactive(&modes, &mut db, &mut editable, &line).await?;
            }
        }
        Mode::Single(line) => {
            command_line_single(&modes, &mut db, &mut editable, line).await?;
        }
        Mode::Stdin => {
            let abort_msg = || eprintln!("Perutaan muokkauskomentojen vaikutukset.");
            let mut ta = db.begin().await?;

            for item in io::stdin().lines() {
                match item {
                    Ok(line) => {
                        if !line.is_empty() {
                            match command_line_stdin(&modes, &mut ta, &mut editable, &line).await {
                                Ok(_) => (),
                                Err(e) => {
                                    abort_msg();
                                    Err(e)?;
                                }
                            }
                        }
                    }
                    Err(_) => {
                        abort_msg();
                        Err("Rivin lukeminen standardisyötteestä epäonnistui.")?;
                    }
                }
            }
            ta.commit().await?;
        }
    }
    Ok(())
}

async fn command_line_interactive(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    line: &str,
) -> Result<(), Box<dyn Error>> {
    let (cmd, args) = commands::split_first(line);
    if !commands_query(modes, db, editable, cmd, args).await?
        && !commands_help(editable, cmd, args)?
    {
        eprintln!("Tuntematon komento ”{cmd}”. Apua saa ?:llä.");
    }
    Ok(())
}

async fn command_line_single(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    line: &str,
) -> Result<(), Box<dyn Error>> {
    let (cmd, args) = commands::split_first(line);
    if !commands_query(modes, db, editable, cmd, args).await? {
        Err(format!(
            "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
        ))?;
    }
    Ok(())
}

async fn command_line_stdin(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    line: &str,
) -> Result<(), Box<dyn Error>> {
    let (cmd, args) = commands::split_first(line);
    if !commands_query(modes, db, editable, cmd, args).await? {
        Err(format!(
            "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
        ))?;
    }
    Ok(())
}

async fn commands_query(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    editable.clear();
    match cmd {
        "tk" => commands::stats(modes, db).await?,
        "hr" => commands::groups(modes, db, editable, args).await?,
        _ => return Ok(false),
    }
    Ok(true)
}

fn commands_help(editable: &mut Editable, cmd: &str, args: &str) -> Result<bool, Box<dyn Error>> {
    editable.clear();
    match cmd {
        "?" => commands::help(args),
        _ => return Ok(false),
    }
    Ok(true)
}
