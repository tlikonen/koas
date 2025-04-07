use just_getopt as jg;
use kastk::{Mode, config, tools};
use std::process::ExitCode;

const PROGRAM_NAME: &str = env!("CARGO_BIN_NAME");
const PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
const PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

fn main() -> ExitCode {
    let args = jg::OptSpecs::new()
        .option("postgresql", "postgresql", jg::OptValue::RequiredNonEmpty)
        .option("help", "h", jg::OptValue::None)
        .option("version", "versio", jg::OptValue::None)
        .flag(jg::OptFlags::PrefixMatchLongOptions)
        .getopt(std::env::args().skip(1));

    let mut error = false;

    for u in &args.unknown {
        eprintln!("Tuntematon valitsin ”{}”.", u);
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

    match config_stage(args) {
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

  --postgresql=/käyttäjä/salasana/kanta/osoite/portti
        Asettaa PostgreSQL-tietokantapalvelimen yhteysasetukset.

        Komentorivillä annetut asetukset ”käyttäjä” ja ”salasana” ovat
        käyttäjän tunnistamistietoja, ja ”kanta” on tietokannan nimi,
        johon kirjaudutaan. Nämä asetukset ovat pakollisia.

        Asetukset ”osoite” ja ”portti” ovat palvelimen verkko-
        osoitetietoja. Jos osoitetta ei ole annettu, käytetään osoitetta
        ”localhost”. Myös ”portti”-asetuksen voi jättää tyhjäksi,
        jolloin käytetään PostgreSQL:n oletusporttia 5432.

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

fn config_stage(args: jg::Args) -> Result<(), String> {
    let config_file = config::init()?;

    tools::umask(0o077);

    if args.option_exists("postgresql") {
        let value = args
            .options_value_last("postgresql")
            .expect("valitsimella pitäisi olla arvo");
        let fields = tools::split_sep(value);
        for i in fields {
            println!("{i:?}");
        }
    } else if config_file.exists() {
        eprintln!("Puuttuu asetustiedoston lukeminen");
    } else {
        config::write(&config_file, &Default::default())?;
        return Err(format!(
            "Asetustiedosto ”{}” on luotu.\n\
             Muokkaa sen asetukset joko valitsimella ”--postgresql” tai tekstieditorilla.\n\
             Valitsin ”-h” tulostaa apua.",
            config_file.to_string_lossy()
        ));
    }

    if args.other.len() == 1 && args.other[0] == "-" {
        kastk::connect_stage(Mode::Stdin, Default::default())
    } else if !args.other.is_empty() {
        kastk::connect_stage(Mode::Single(args.other.join(" ")), Default::default())
    } else {
        kastk::connect_stage(Mode::Interactive, Default::default())
    }
}
