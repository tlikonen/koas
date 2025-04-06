pub mod config;
pub mod tools;

use just_getopt as jg;

pub fn interactive() -> Result<(), String> {
    Err("Vuorovaikutteinen tila puuttuu.".to_string())
}

pub fn single_command(_args: jg::Args) -> Result<(), String> {
    Err("Komentorivin komentojen käsittely puuttuu.".to_string())
}

pub fn standard_input() -> Result<(), String> {
    Err("Standardisyötteen käsittely puuttuu.".to_string())
}
