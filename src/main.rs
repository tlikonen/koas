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
        .option("muoto", "muoto", jg::OptValue::RequiredNonEmpty)
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

  --muoto=taulukkomuoto

        Taulukoiden tulostusmuoto no oletuksena ”unicode”, mutta muita
        vaihtoehtoja ovat ”ascii”, ”org-mode”, ”tab” ja ”latex”.

  -h    Tulostaa tämän ohjeen.

  --versio
        Tulostaa ohjelman versionumeron ja lisenssin.\n",
        name = PROGRAM_NAME
    )
}

async fn config_stage(args: jg::Args) -> Result<(), Box<dyn Error>> {
    let config_file = Config::file()?;
    let config: Config;
    let mut output: Output = Default::default();

    tools::umask(0o077);

    if config_file.exists() {
        config = Config::read(&config_file)?;
    } else {
        config = Default::default();
        config.write(&config_file)?;
        Err(format!(
            "Luotiin asetustiedosto ”{}”.\n\
             Muokkaa tiedostossa tietokannan yhteysasetukset kuntoon tekstieditorilla.\n\
             Valitsin ”--ohje” tulostaa apua.",
            config_file.to_string_lossy()
        ))?;
    }

    // Print format.
    if !config.format.is_empty() {
        output = match select_output_format(&config.format) {
            Ok(f) => f,
            Err(e) => {
                eprintln!(
                    "{e}\nTarkista asetustiedosto ”{}”.",
                    config_file.to_string_lossy()
                );
                Default::default()
            }
        };
    }

    if args.option_exists("muoto") {
        let value = args
            .options_value_last("muoto")
            .expect("valitsimella pitäisi olla arvo");
        output = select_output_format(value)?;
    }

    // Choose the command stage: stdin, single or interactive.
    let mut modes: Modes = Default::default();
    modes.set_output(output);
    if args.other.len() == 1 && args.other[0] == "-" {
        modes.set_mode(Mode::Stdin);
        kastk::command_stage(modes, config).await
    } else if !args.other.is_empty() {
        modes.set_mode(Mode::Single(args.other.join(" ")));
        kastk::command_stage(modes, config).await
    } else {
        modes.set_mode(Mode::Interactive);
        kastk::command_stage(modes, config).await
    }
}

fn select_output_format(value: &str) -> Result<Output, Box<dyn Error>> {
    let out = match value.to_lowercase().as_str() {
        "unicode" => Output::Unicode,
        "ascii" => Output::Ascii,
        "org-mode" => Output::Orgmode,
        "tab" => Output::Tab,
        "latex" => Output::Latex,
        _ => Err(format!("Sopimaton tulostusmuoto ”{value}”."))?,
    };
    Ok(out)
}
