pub mod config;
pub mod database;
pub mod tools;

use config::Config;
use database as db;

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

pub async fn command_stage(mode: Mode, config: Config, _output: Output) -> Result<(), String> {
    match mode {
        Mode::Interactive => {
            let mut db = db::connect(&config).await?;

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
                println!("Rivi: {line:?}");

                db::test(&mut db, &line).await.map_err(|e| format!("{e}"))?;
            }
            Ok(())
        }
        Mode::Single(c) => {
            println!("command = {c:?}");
            Err("Komentorivin komentojen käsittely puuttuu.".to_string())
        }
        Mode::Stdin => Err("Standardisyötteen käsittely puuttuu.".to_string()),
    }
}
