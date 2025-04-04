use just_getopt::{Args, OptFlags, OptSpecs, OptValue};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

const PROGRAM_NAME: &str = env!("CARGO_BIN_NAME");
const PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
const PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");
const CONFIG_FILE: &str = env!("CARGO_BIN_NAME");

fn main() -> ExitCode {
    let args = OptSpecs::new()
        .option("postgresql", "postgresql", OptValue::RequiredNonEmpty)
        .option("help", "h", OptValue::None)
        .option("version", "versio", OptValue::None)
        .flag(OptFlags::PrefixMatchLongOptions)
        .getopt(std::env::args().skip(1));

    if !args.unknown.is_empty() {
        for u in &args.unknown {
            eprintln!("Tuntematon valitsin ”{}”.", u);
        }
        eprintln!("Valitsin ”-h” tulostaa apua.");
        return ExitCode::FAILURE;
    }

    if args.required_value_missing().next().is_some() {
        for o in args.required_value_missing() {
            eprintln!("Valitsimelle ”{}” täytyy antaa arvo.", o.name);
        }
        eprintln!("Valitsin ”-h” tulostaa apua.");
        return ExitCode::FAILURE;
    }

    if args.option_exists("help") {
        print_usage();
        return ExitCode::SUCCESS;
    }

    if args.option_exists("version") {
        println!(
            "{name} v{version}\n\
             Tekijä:   {author}\n\
             Lisenssi: {license}",
            name = PROGRAM_NAME,
            version = PROGRAM_VERSION,
            author = PROGRAM_AUTHORS,
            license = PROGRAM_LICENSE
        );
        return ExitCode::SUCCESS;
    }

    match run(args) {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{}", e);
            ExitCode::FAILURE
        }
    }
}

fn print_usage() {
    println!(
        "Käyttö: {name} [valitsimet] [--] [komento]

Valitsimet

  --postgresql=/käyttäjä/salasana/osoite/portti/kanta
        Asettaa PostgreSQL-tietokantapalvelimen yhteysasetukset.

        Komentorivillä annetut asetukset ”käyttäjä” ja ”salasana” ovat
        tietokannan kirjautumistietoja. Asetukset ”osoite” ja ”portti”
        ovat palvelimen verkko-osoitetietoja. Jos tietokantapalvelin
        toimii samalla tietokoneella, sopiva osoite on ”localhost”. Sitä
        käytetään oletuksena, jos osoitteen jättää tyhjäksi. Myös
        ”portti”-asetuksen voi jättää tyhjäksi, jolloin käytetään
        PostgreSQL:n oletusporttia 5432. ”kanta” on tietokannan nimi,
        johon kirjaudutaan.

        Tietokannan täytyy olla valmiiksi olemassa, ja tällä käyttäjällä
        pitää olla CREATE-oikeus eli oikeus luoda taulukoita yms. Kaikki
        edellä mainitut asetukset tallentuvat asetustiedostoon, ja niitä
        käytetään automaattisesti seuraavilla kerroilla, ellei tätä
        valitsinta ole annettu.

        Asetusten erotinmerkkinä on yllä olevassa esimerkissä vinoviiva
        (/), mutta se voisi olla mikä tahansa muukin merkki. Valitsimen
        arvon ensimmäinen merkki määrittää, mikä merkki erottaa
        asetuskentät toisistaan.

  -h    Tulostaa tämän ohjeen.

  --versio
        Tulostaa ohjelman versionumeron ja lisenssin.\n",
        name = PROGRAM_NAME
    )
}

fn run(args: Args) -> Result<(), String> {
    let config_file = init_config_file(CONFIG_FILE)?;

    umask(0o077);

    if args.option_exists("postgresql") {
        eprintln!("puuttuu --postgresql-valitsimen käsittely.");
    } else if config_file.exists() {
        eprintln!("Puuttuu asetustiedoston lukeminen");
    } else {
        write_config_file(&config_file, &Default::default())?;
        return Err(format!(
            "Asetustiedosto ”{}” on luotu.\n\
             Muokkaa sen asetukset joko valitsimella ”--postgresql” tai tekstieditorilla.\n\
             Valitsin ”-h” tulostaa apua.",
            config_file.to_string_lossy()
        ));
    }

    Ok(())
}

fn init_config_file(name: &str) -> Result<PathBuf, String> {
    xdg::BaseDirectories::new()
        .map_err(|_| "Asetustiedoston alustus epäonnistui.".to_string())?
        .place_config_file(name)
        .map_err(|e| format!("Asetustiedoston alustus epäonnistui: {}", e.kind()))
}

fn umask(mask: u32) -> u32 {
    unsafe { libc::umask(mask) }
}

struct Config {
    system: String,
    host: String,
    port: u16,
    database: String,
    user: String,
    password: String,
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

fn write_config_file(path: &Path, config: &Config) -> Result<(), String> {
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
