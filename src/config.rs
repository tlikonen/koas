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

impl Config {
    pub fn empty() -> Self {
        Self {
            system: String::new(),
            user: String::new(),
            password: String::new(),
            database: String::new(),
            host: String::new(),
            port: 0,
        }
    }
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

    let mut config = Config::empty();

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
                eprintln!("Rivi {} asetustiedostossa on sopimaton: hylätään.", n + 1);
                continue;
            }
        };

        match key {
            "järjestelmä" => {
                if DATABASE_SYSTEMS.contains(&value) {
                    config.system.clear();
                    config.system.push_str(value);
                } else {
                    return Err(format!(
                        "Asetustiedostossa sopimaton kentän ”järjestelmä” arvo: ”{value}”."
                    ));
                }
            }
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
                        "Asetustiedostossa kentän ”portti” arvo ”{value}” ei ole \
                         sopiva tietoliikenneportiksi."
                    )
                })?;
                port = true;
            }
            _ => return Err(format!("Asetustiedostossa sopimaton kenttä ”{key}”.")),
        }

        if value.is_empty() {
            return Err(format!("Asetustiedostossa kentän ”{key}” arvo puuttuu."));
        }
    }

    if config.system == "postgresql"
        && (config.user.is_empty()
            || config.password.is_empty()
            || config.database.is_empty()
            || config.host.is_empty()
            || !port)
    {
        return Err(
            "Asetustiedostosta puuttuu kenttiä. Korjaa asetukset käyttämällä \
             valitsinta ”--postgresql”."
                .to_string(),
        );
    }

    Ok(config)
}
