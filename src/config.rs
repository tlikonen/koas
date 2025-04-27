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
    pub format: String,
}

impl Config {
    pub fn file() -> Result<PathBuf, Box<dyn Error>> {
        let path = xdg::BaseDirectories::new()
            .map_err(|_| "Asetustiedoston alustus epäonnistui.".to_string())?
            .place_config_file(CONFIG_FILE)
            .map_err(|e| format!("Asetustiedoston alustus epäonnistui: {}", e.kind()))?;
        Ok(path)
    }

    pub fn read(path: &Path) -> Result<Config, String> {
        let contents = fs::read_to_string(path).map_err(|e| {
            format!(
                "Asetustiedoston ”{}” lukeminen epäonnistui: {}",
                path.to_string_lossy(),
                e.kind()
            )
        })?;

        let mut config = Config {
            user: String::new(),
            password: String::new(),
            database: String::new(),
            host: String::new(),
            port: 0,
            format: String::new(),
        };
        let mut port = false;
        let max = 10;

        for (n, line) in contents.lines().enumerate() {
            if n >= max {
                eprintln!("Asetustiedostosta käsitellään vain ensimmäiset {max} riviä.");
                break;
            }

            let (key, value) = match line.split_once('=') {
                Some(kv) => kv,
                None => {
                    eprintln!("Asetustiedoston rivi {} on sopimaton.", n + 1);
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
                "kanta" => {
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
                            "Asetustiedostossa kentän ”portti” arvo ”{value}” on \
                             sopimaton tietoliikenneportiksi."
                        )
                    })?;
                    port = true;
                }
                "tulostusmuoto" => {
                    config.format.clear();
                    config.format.push_str(value);
                }
                _ => eprintln!("Asetustiedostossa tuntematon kenttä ”{key}”."),
            }
        }

        if config.user.is_empty()
            || config.password.is_empty()
            || config.database.is_empty()
            || config.host.is_empty()
            || !port
        {
            Err(format!(
                "Asetustiedostosta ”{}” puuttuu tärkeitä asetuksia.\n\
                 Ohjeita saa valitsimella ”-h”.",
                path.to_string_lossy(),
            ))?;
        }

        Ok(config)
    }

    pub fn write(&self, path: &Path) -> Result<(), Box<dyn Error>> {
        fs::write(
            path,
            format!(
                "käyttäjä={user}\n\
                 salasana={pw}\n\
                 kanta={db}\n\
                 osoite={host}\n\
                 portti={port}\n\
                 tulostusmuoto={format}\n",
                host = self.host,
                port = self.port,
                db = self.database,
                user = self.user,
                pw = self.password,
                format = self.format,
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
            database: String::new(),
            user: String::new(),
            password: String::new(),
            format: "unicode".to_string(),
        }
    }
}
