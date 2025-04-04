use std::path::{Path, PathBuf};
use std::fs;

const CONFIG_FILE: &str = env!("CARGO_PKG_NAME");

pub fn init_config_file() -> Result<PathBuf, String> {
    xdg::BaseDirectories::new()
        .map_err(|_| "Asetustiedoston alustus epäonnistui.".to_string())?
        .place_config_file(CONFIG_FILE)
        .map_err(|e| format!("Asetustiedoston alustus epäonnistui: {}", e.kind()))
}


pub struct Config {
    pub system: String,
    pub host: String,
    pub port: u16,
    pub database: String,
    pub user: String,
    pub password: String,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            system: "postgresql".to_string(),
            host: "localhost".to_string(),
            port: 5432,
            database: "".to_string(),
            user: "".to_string(),
            password: "".to_string(),
        }
    }
}

pub fn write_config_file(path: &Path, config: &Config) -> Result<(), String> {
    fs::write(
        path,
        format!(
            "järjestelmä={system}\n\
             osoite={host}\n\
             portti={port}\n\
             kanta={db}\n\
             käyttäjä={user}\n\
             salasana={pw}\n",
            system = config.system,
            host = config.host,
            port = config.port,
            db = config.database,
            user = config.user,
            pw = config.password,
        ),
    )
    .map_err(|e| {
        format!(
            "Asetustiedoston ”{}” kirjoittaminen epäonnistui: {}",
            path.to_string_lossy(),
            e.kind()
        )
    })
}
