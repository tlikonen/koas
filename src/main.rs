use just_getopt::{Args, OptFlags, OptSpecs, OptValue};
use koas::*;
use std::{error::Error, process::ExitCode};

#[tokio::main]
async fn main() -> ExitCode {
    let args = OptSpecs::new()
        .option("taulukot", "taulukot", OptValue::RequiredNonEmpty)
        .option("ohje", "ohje", OptValue::OptionalNonEmpty)
        .option("help", "h", OptValue::None)
        .option("version", "version", OptValue::None)
        .flag(OptFlags::PrefixMatchLongOptions)
        .getopt(std::env::args().skip(1));

    let mut error = false;

    for u in &args.unknown {
        eprintln!("Tuntematon valitsin ”{u}”.");
        error = true;
    }

    for o in args.required_value_missing() {
        eprintln!("Valitsimelle ”{}” täytyy antaa arvo.", o.id);
        error = true;
    }

    if error {
        eprintln!("Valitsin ”-h” tulostaa apua.");
        return ExitCode::FAILURE;
    }

    if args.option_exists("help") {
        println!(
            include_str!("../help/usage.txt"),
            ohjelma = koas::PROGRAM_NAME,
        );
        return ExitCode::SUCCESS;
    }

    if args.option_exists("ohje") {
        let topic = args.options_value_last("ohje").map_or("", |v| v);
        match koas::help(topic) {
            Ok(_) => return ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("{e}");
                return ExitCode::FAILURE;
            }
        }
    }

    if args.option_exists("version") {
        println!(
            "{name} v{version} (arvosanatietokanta v{db})\n\
             Tekijä:   {author}\n\
             Lisenssi: {license}",
            name = koas::PROGRAM_NAME,
            db = koas::PROGRAM_DB_VERSION,
            version = koas::PROGRAM_VERSION,
            author = koas::PROGRAM_AUTHORS,
            license = koas::PROGRAM_LICENSE
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

async fn config_stage(args: Args) -> Result<(), Box<dyn Error>> {
    let config_file = Config::file()?;
    let config: Config;
    let mut output: Output = Default::default();

    koas::umask(0o077);

    if config_file.exists() {
        config = Config::read(&config_file)?;
    } else {
        config = Default::default();
        config.write(&config_file)?;
        Err(format!(
            "Luotiin asetustiedosto ”{}”.\n\
             Muokkaa tiedostoa tekstieditorilla ja aseta tietokannan yhteysasetukset.\n\
             Seuraavilla valitsimilla saa apua: ”--ohje=tietokanta” ja ”--ohje=asetukset”.",
            config_file.display()
        ))?;
    }

    // Table-printing format.
    if !config.tables.is_empty() {
        output = Output::select(&config.tables).unwrap_or_default();
    }

    if let Some(value) = args.options_value_last("taulukot") {
        output = Output::select(value)
            .map_err(|e| format!("Sopimaton arvo valitsimelle: ”--taulukot={e}”."))?;
    }

    // Choose the command stage: stdin, single or interactive.
    let mut modes: Modes = Default::default();
    modes.set_output(output);
    if args.other.len() == 1 && args.other[0] == "-" {
        modes.set_mode(Mode::Stdin);
        koas::command_stage(modes, config).await
    } else if !args.other.is_empty() {
        modes.set_mode(Mode::Single(args.other.join(" ")));
        koas::command_stage(modes, config).await
    } else {
        modes.set_mode(Mode::Interactive);
        koas::command_stage(modes, config).await
    }
}
