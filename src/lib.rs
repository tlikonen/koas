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
        Mode::Interactive => Err("Vuorovaikutteinen tila puuttuu.".to_string()),
        Mode::Single(c) => {
            println!("command = {c:?}");
            Err("Komentorivin komentojen käsittely puuttuu.".to_string())
        }
        Mode::Stdin => Err("Standardisyötteen käsittely puuttuu.".to_string()),
    }
}
