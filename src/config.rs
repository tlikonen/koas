use std::fs;
use std::path::{Path, PathBuf};

static CONFIG_FILE: &str = env!("CARGO_PKG_NAME");
static DATABASE_SYSTEMS: [&str; 1] = ["postgresql"];

pub fn init() -> Result<PathBuf, String> {
    xdg::BaseDirectories::new()
        .map_err(|_| "Asetustiedoston alustus epäonnistui.".to_string())?
        .place_config_file(CONFIG_FILE)
        .map_err(|e| format!("Asetustiedoston alustus epäonnistui: {}", e.kind()))
}

#[derive(Debug)]
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
            database: String::new(),
            user: String::new(),
            password: String::new(),
        }
    }
}

pub fn write(path: &Path, config: &Config) -> Result<(), String> {
    fs::write(
        path,
        format!(
            "järjestelmä={system}\n\
             käyttäjä={user}\n\
             salasana={pw}\n\
             kanta={db}\n\
             osoite={host}\n\
             portti={port}\n",
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

pub fn read(path: &Path) -> Result<Config, String> {
    let contents = fs::read_to_string(path).map_err(|e| {
        format!(
            "Asetustiedoston ”{}” lukeminen epäonnistui: {}",
            path.to_string_lossy(),
            e.kind()
        )
    })?;

    let max = 6;
    for (n, line) in contents.lines().take(max + 1).enumerate() {
        if n == max {
            eprintln!("Asetustiedostosta käsitellään vain ensimmäiset {max} riviä.");
            break;
        }

        let (key, value) = match line.split_once('=') {
            Some(kv) => kv,
            None => {
                eprintln!("Rivi {} asetustiedostossa on sopimaton: hylätään.", n + 1);
                continue;
            }
        };

        println!("{key}={value}");
    }

    Ok(Default::default()) // Korjattava
}
