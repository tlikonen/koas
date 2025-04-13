pub mod commands;
pub mod config;
pub mod database;
pub mod tools;

use crate::{commands as cmd, config::Config, database as db};
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
}

pub async fn command_stage(modes: Modes, config: Config) -> Result<(), Box<dyn Error>> {
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
            let mut rl = rustyline::DefaultEditor::new()?;

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
            //let err = |e| format!("{e}");
            let abort_msg = || eprintln!("Perutaan muokkauskomentojen vaikutukset.");
            let mut ta = db.begin().await?;

            for item in io::stdin().lines() {
                match item {
                    Ok(line) => {
                        if !line.is_empty() {
                            match command_line_stdin(&modes, &mut ta, &line).await {
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

pub async fn command_line_interactive(
    modes: &Modes,
    db: &mut PgConnection,
    line: &str,
) -> Result<(), Box<dyn Error>> {
    let (cmd, _args) = tools::split_first(line);
    match cmd {
        "tk" => cmd::stats(modes, db).await?,
        _ => println!("Tuntematon komento ”{cmd}”. Apua saa ?:llä."),
    }
    Ok(())
}

pub async fn command_line_single(
    modes: &Modes,
    db: &mut PgConnection,
    line: &str,
) -> Result<(), Box<dyn Error>> {
    let (cmd, _args) = tools::split_first(line);
    match cmd {
        "tk" => cmd::stats(modes, db).await?,
        _ => {
            Err(format!(
                "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
            ))?;
        }
    }
    Ok(())
}

pub async fn command_line_stdin(
    modes: &Modes,
    db: &mut PgConnection,
    line: &str,
) -> Result<(), Box<dyn Error>> {
    let (cmd, _args) = tools::split_first(line);
    match cmd {
        "tk" => cmd::stats(modes, db).await?,
        // "lisää" => {
        //     let (avain, loput) = tools::split_first(args);
        //     let (arvo, _) = tools::split_first(loput);
        //     sqlx::query("INSERT INTO hallinto (avain, teksti) VALUES ($1, $2)")
        //         .bind(avain)
        //         .bind(arvo)
        //         .execute(db)
        //         .await?;
        // }
        // "poista" => {
        //     let (avain, _) = tools::split_first(args);
        //     sqlx::query("DELETE FROM hallinto WHERE avain = $1")
        //         .bind(avain)
        //         .execute(db)
        //         .await?;
        // }
        _ => {
            Err(format!(
                "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
            ))?;
        }
    }
    Ok(())
}
