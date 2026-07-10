use crate::output::Output;
use crate::prelude::*;
use std::fs;
use std::io;
use std::io::Write as _;
use std::path::Path;
use std::path::PathBuf;

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
    pub fn file() -> Result<PathBuf> {
        let path = xdg::BaseDirectories::new()
            .place_config_file(CONFIG_FILE)
            .map_err(|e| format!("Asetustiedoston alustus epäonnistui: {}", e.kind()))?;
        Ok(path)
    }

    pub fn read(path: &Path) -> Result<Config> {
        let mut stderr = io::stderr();

        let contents = fs::read_to_string(path).map_err(|e| {
            format!(
                "Asetustiedoston ”{}” lukeminen epäonnistui: {}",
                path.display(),
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

        for (n, line) in (1..).zip(contents.lines()) {
            if n > max {
                writeln!(
                    stderr,
                    "Asetustiedostosta ”{}” käsitellään vain ensimmäiset {max} riviä.",
                    path.display()
                )?;
                break;
            }

            if line.chars().all(|c| c.is_whitespace()) {
                continue;
            }

            let (key, value) = match line.split_once('=') {
                Some(kv) => kv,
                None => {
                    writeln!(
                        stderr,
                        "Asetustiedoston ”{}” rivi {} on sopimaton.",
                        path.display(),
                        n
                    )?;
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
                            path.display()
                        )
                    })?;
                    port = true;
                }
                "taulukot" => {
                    config.tables.clear();
                    config.tables.push_str(value);
                }
                _ => {
                    writeln!(
                        stderr,
                        "Asetustiedostossa ”{}” tuntematon kenttä ”{key}”.",
                        path.display()
                    )?;
                }
            }
        }

        if config.user.is_empty()
            || config.password.is_empty()
            || config.database.is_empty()
            || config.host.is_empty()
            || !port
        {
            return Err(format!(
                "Asetustiedostosta ”{}” puuttuu tärkeitä asetuksia. Tarkista ohjeet.",
                path.display(),
            )
            .into());
        }

        if !config.tables.is_empty() && Output::select(&config.tables).is_err() {
            writeln!(
                stderr,
                "Asetustiedostossa ”{}” sopimaton kentän ”taulukot” arvo.",
                path.display()
            )?;
        }

        Ok(config)
    }

    pub fn write(&self, path: &Path) -> Result<()> {
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
                path.display(),
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
