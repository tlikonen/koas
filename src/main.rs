use just_getopt as jg;
use kastk::{Config, Mode, Modes, Output, tools};
use std::{error::Error, process::ExitCode};

static PROGRAM_NAME: &str = env!("CARGO_BIN_NAME");
static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

#[tokio::main]
async fn main() -> ExitCode {
    let args = jg::OptSpecs::new()
        .option("postgresql", "postgresql", jg::OptValue::RequiredNonEmpty)
        .option("help", "h", jg::OptValue::None)
        .option("version", "versio", jg::OptValue::None)
        .flag(jg::OptFlags::PrefixMatchLongOptions)
        .getopt(std::env::args().skip(1));

    let mut error = false;

    for u in &args.unknown {
        eprintln!("Tuntematon valitsin ”{u}”.");
        error = true;
    }

    for o in args.required_value_missing() {
        eprintln!("Valitsimelle ”{}” täytyy antaa arvo.", o.name);
        error = true;
    }

    if error {
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

    match config_stage(args).await {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}

fn print_usage() {
    println!(
        "Käyttö: {name} [valitsimet] [--] [komento]

Valitsimet

  --postgresql=/käyttäjä/salasana/kanta/osoite/portti
        Asettaa PostgreSQL-tietokantapalvelimen yhteysasetukset ja
        tallentaa ne asetustiedostoon. Tätä valitsinta ei enää tarvita,
        jos asetukset pysyvät samana.

        Komentorivillä annetut asetukset ”käyttäjä” ja ”salasana” ovat
        käyttäjän tunnistamistietoja, ja ”kanta” on tietokannan nimi,
        johon kirjaudutaan. Nämä asetukset ovat pakollisia.

        Asetukset ”osoite” ja ”portti” ovat palvelimen verkko-
        osoitetietoja. Jos osoitetta ei ole annettu, käytetään osoitetta
        ”localhost”. Myös ”portti”-asetuksen voi jättää tyhjäksi,
        jolloin käytetään PostgreSQL:n oletusporttia 5432.

        Tietokannan täytyy olla valmiiksi olemassa, ja tällä käyttäjällä
        pitää olla CREATE-oikeus eli oikeus luoda taulukoita yms.

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

async fn config_stage(args: jg::Args) -> Result<(), Box<dyn Error>> {
    let config_file = Config::file()?;
    let config: Config;
    let output: Output = Default::default();

    tools::umask(0o077);

    // Database configuration.
    if args.option_exists("postgresql") {
        let value = args
            .options_value_last("postgresql")
            .expect("valitsimella pitäisi olla arvo");

        let mut fields = tools::split_sep(value);
        let err =
            |field: &str| format!("Valitsimelle ”--postgresql” täytyy antaa kenttä ”{field}”.");
        let default: Config = Default::default();

        let user = fields
            .next()
            .filter(|x| !x.is_empty())
            .ok_or(err("käyttäjä"))?;

        let password = fields
            .next()
            .filter(|x| !x.is_empty())
            .ok_or(err("salasana"))?;

        let database = fields
            .next()
            .filter(|x| !x.is_empty())
            .ok_or(err("kanta"))?;

        let host = fields
            .next()
            .filter(|x| !x.is_empty())
            .unwrap_or(&default.host);

        let port = match fields.next().filter(|x| !x.is_empty()) {
            None => default.port,
            Some(field) => field.parse::<u16>().map_err(|_| {
                format!(
                    "Valitsimen ”--postgresql” kentän ”portti” arvo ”{field}” on \
                     sopimaton tietoliikenneportiksi."
                )
            })?,
        };

        config = Config {
            user: user.to_string(),
            password: password.to_string(),
            database: database.to_string(),
            host: host.to_string(),
            port,
        };

        config.write(&config_file)?;
        println!(
            "Tietokannan yhteysasetukset tallennettu asetustiedostoon ”{}”.",
            config_file.to_string_lossy()
        );
    } else if config_file.exists() {
        config = Config::read(&config_file)?;
    } else {
        return Err(format!(
            "Asetustiedosto ”{}” puuttuu. \
             Luo se käyttämällä valitsinta ”--postgresql”.\n\
             Valitsin ”-h” tulostaa apua.",
            config_file.to_string_lossy()
        )
        .into());
    }

    // Choose the command stage: stdin, single or interactive.
    let mut modes: Modes = Default::default();
    if args.other.len() == 1 && args.other[0] == "-" {
        modes.set_mode(Mode::Stdin);
        modes.set_output(output);
        kastk::command_stage(modes, config).await
    } else if !args.other.is_empty() {
        modes.set_mode(Mode::Single(args.other.join(" ")));
        modes.set_output(output);
        kastk::command_stage(modes, config).await
    } else {
        modes.set_mode(Mode::Interactive);
        modes.set_output(Default::default());
        kastk::command_stage(modes, config).await
    }
}
