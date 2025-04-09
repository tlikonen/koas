pub mod config;
pub mod tools;

use config::Config;

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

pub fn command_stage(mode: Mode, _config: Config, _output: Output) -> Result<(), String> {
    match mode {
        Mode::Interactive => {
            let mut rl = rustyline::DefaultEditor::new()
                .map_err(|_| "Komentorivimuokkaimen asetus epäonnistui.".to_string())?;

            let prompt = env!("CARGO_PKG_NAME").to_string() + "> ";

            while let Ok(line) = rl.readline(&prompt) {
                if line.is_empty() {
                    break;
                }
                let _ = rl.add_history_entry(&line);
                println!("Rivi: {line:?}");
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
