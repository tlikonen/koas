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
    Unicode,
    Ascii,
    Orgmode,
    // Tab,
    // Csv,
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

                let (cmd, args) = commands::split_first(&line);
                if !match_query_commands(&modes, &mut db, &mut editable, cmd, args).await?
                    && !match_help_commands(&mut editable, cmd, args)?
                {
                    eprintln!("Tuntematon komento ”{cmd}”. Apua saa ?:llä.");
                }
            }
        }
        Mode::Single(line) => {
            let (cmd, args) = commands::split_first(line);
            if !match_query_commands(&modes, &mut db, &mut editable, cmd, args).await? {
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
                    let (cmd, args) = commands::split_first(&line);
                    if !match_query_commands(&modes, &mut ta, &mut editable, cmd, args).await? {
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

async fn match_query_commands(
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

fn match_help_commands(
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    editable.clear();
    match cmd {
        "?" => commands::help(args),
        _ => return Ok(false),
    }
    Ok(true)
}
