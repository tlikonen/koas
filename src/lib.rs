pub mod commands;
pub mod config;
pub mod database;
pub mod tools;

use crate::{commands as cmd, config::Config, database as db};
use sqlx::PgConnection as DB;
use std::io;

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
}

pub async fn command_stage(modes: Modes, config: Config) -> Result<(), String> {
    let mut db = db::connect(&config).await?;

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
            let mut rl = rustyline::DefaultEditor::new()
                .map_err(|_| "Komentorivimuokkaimen asetus epäonnistui.".to_string())?;

            while let Ok(line) = rl.readline(&prompt) {
                if line.is_empty() {
                    break;
                }
                let _ = rl.add_history_entry(&line);

                command_line_interactive(&modes, &mut db, &line).await?;
            }
        }
        Mode::Single(line) => {
            command_line_single(&modes, &mut db, line).await?;
        }
        Mode::Stdin => {
            let err = |e| format!("{e}");
            db::begin_transaction(&mut db).await.map_err(err)?;
            for item in io::stdin().lines() {
                match item {
                    Ok(line) => {
                        if !line.is_empty() {
                            match command_line_stdin(&modes, &mut db, &line).await {
                                Ok(_) => (),
                                Err(e) => {
                                    db::rollback_transaction(&mut db).await.map_err(err)?;
                                    eprintln!("Peruttiin muokkauskomentojen vaikutukset.");
                                    return Err(e);
                                }
                            }
                        }
                    }
                    Err(_) => {
                        db::rollback_transaction(&mut db).await.map_err(err)?;
                        eprintln!("Peruttiin muokkauskomentojen vaikutukset.");
                        return Err("Rivin lukeminen standardisyötteestä epäonnistui.".to_string());
                    }
                }
            }
            db::commit_transaction(&mut db).await.map_err(err)?;
        }
    }
    Ok(())
}

pub async fn command_line_interactive(
    modes: &Modes,
    db: &mut DB,
    line: &str,
) -> Result<(), String> {
    let (cmd, _args) = tools::split_first(line);
    match cmd {
        "tk" => cmd::stats(modes, db).await?,
        _ => println!("Tuntematon komento ”{cmd}”. Apua saa ?:llä."),
    }
    Ok(())
}

pub async fn command_line_single(modes: &Modes, db: &mut DB, line: &str) -> Result<(), String> {
    let (cmd, _args) = tools::split_first(line);
    match cmd {
        "tk" => cmd::stats(modes, db).await?,
        _ => {
            return Err(format!(
                "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
            ));
        }
    }
    Ok(())
}

pub async fn command_line_stdin(modes: &Modes, db: &mut DB, line: &str) -> Result<(), String> {
    let (cmd, _args) = tools::split_first(line);
    match cmd {
        "tk" => cmd::stats(modes, db).await?,
        _ => {
            return Err(format!(
                "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
            ));
        }
    }
    Ok(())
}
