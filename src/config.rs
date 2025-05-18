use crate::modes::Output;
use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
};

static CONFIG_FILE: &str = env!("CARGO_PKG_NAME");

pub struct Config {
    pub host: String,
    pub port: u16,
    pub database: String,
    pub user: String,
    pub password: String,
    pub tables: String,
}

impl Config {
    pub fn file() -> Result<PathBuf, Box<dyn Error>> {
        let path = xdg::BaseDirectories::new()
            .place_config_file(CONFIG_FILE)
            .map_err(|e| format!("Asetustiedoston alustus epäonnistui: {}", e.kind()))?;
        Ok(path)
    }

    pub fn read(path: &Path) -> Result<Config, String> {
        let path_str = || path.to_string_lossy();
        let contents = fs::read_to_string(path).map_err(|e| {
            format!(
                "Asetustiedoston ”{}” lukeminen epäonnistui: {}",
                path_str(),
                e.kind()
            )
        })?;

        let mut config = Config {
            user: String::with_capacity(10),
            password: String::with_capacity(16),
            database: String::with_capacity(10),
            host: String::with_capacity(10),
            port: 0,
            tables: String::with_capacity(10),
        };
        let mut port = false;
        let max = 10;

        for (n, line) in contents.lines().enumerate() {
            if n >= max {
                eprintln!(
                    "Asetustiedostosta ”{}” käsitellään vain ensimmäiset {max} riviä.",
                    path_str()
                );
                break;
            }

            if line.chars().all(|c| c.is_whitespace()) {
                continue;
            }

            let (key, value) = match line.split_once('=') {
                Some(kv) => kv,
                None => {
                    eprintln!(
                        "Asetustiedoston ”{}” rivi {} on sopimaton.",
                        path_str(),
                        n + 1
                    );
                    continue;
                }
            };

            match key {
                "käyttäjä" => {
                    config.user.clear();
                    config.user.push_str(value);
                }
                "salasana" => {
                    config.password.clear();
                    config.password.push_str(value);
                }
                "tietokanta" => {
                    config.database.clear();
                    config.database.push_str(value);
                }
                "osoite" => {
                    config.host.clear();
                    config.host.push_str(value);
                }
                "portti" => {
                    config.port = value.parse::<u16>().map_err(|_| {
                        format!(
                            "Asetustiedostossa ”{}” kentän ”portti” arvo on\n\
                             sopimaton tietoliikenneportiksi.",
                            path_str()
                        )
                    })?;
                    port = true;
                }
                "taulukot" => {
                    config.tables.clear();
                    config.tables.push_str(value);
                }
                _ => eprintln!(
                    "Asetustiedostossa ”{}” tuntematon kenttä ”{key}”.",
                    path_str()
                ),
            }
        }

        if config.user.is_empty()
            || config.password.is_empty()
            || config.database.is_empty()
            || config.host.is_empty()
            || !port
        {
            Err(format!(
                "Asetustiedostosta ”{}” puuttuu tärkeitä asetuksia. Tarkista ohjeet.",
                path_str(),
            ))?;
        }

        if !config.tables.is_empty() && select_table_format(&config.tables).is_err() {
            eprintln!(
                "Asetustiedostossa ”{}” sopimaton kentän ”taulukot” arvo.",
                path_str()
            );
        }

        Ok(config)
    }

    pub fn write(&self, path: &Path) -> Result<(), Box<dyn Error>> {
        fs::write(
            path,
            format!(
                "käyttäjä={user}\n\
                 salasana={pw}\n\
                 tietokanta={db}\n\
                 osoite={host}\n\
                 portti={port}\n\
                 taulukot={tables}\n",
                host = self.host,
                port = self.port,
                db = self.database,
                user = self.user,
                pw = self.password,
                tables = self.tables,
            ),
        )
        .map_err(|e| {
            format!(
                "Asetustiedoston ”{}” kirjoittaminen epäonnistui: {}",
                path.to_string_lossy(),
                e.kind()
            )
        })?;
        Ok(())
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            host: "localhost".to_string(),
            port: 5432,
            database: String::with_capacity(10),
            user: String::with_capacity(10),
            password: String::with_capacity(16),
            tables: "unicode".to_string(),
        }
    }
}

pub fn select_table_format(value: &str) -> Result<Output, Box<dyn Error>> {
    let out = match value.to_lowercase().as_str() {
        "unicode" | "u" => Output::Unicode,
        "unicode-avoin" | "ua" => Output::UnicodeOpen,
        "ascii" | "a" => Output::Ascii,
        "ascii-avoin" | "aa" => Output::AsciiOpen,
        "emacs" | "e" => Output::Orgmode,
        "tab" | "t" => Output::Tab,
        "latex" | "l" => Output::Latex,
        _ => Err(value.to_string())?,
    };
    Ok(out)
}
