use just_getopt::{Args, OptSpecs, OptValue};
use std::path::PathBuf;
use std::process::ExitCode;

const PROGRAM_NAME: &str = env!("CARGO_BIN_NAME");
const PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
const PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");
const CONFIG_FILE: &str = env!("CARGO_BIN_NAME");

fn main() -> ExitCode {
    let args = OptSpecs::new()
        .option("tietokanta", "tietokanta", OptValue::RequiredNonEmpty)
        .option("help", "h", OptValue::None)
        .option("version", "versio", OptValue::None)
        .option("version", "version", OptValue::None)
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

  -h    Tulostaa tämän ohjeen.

  --versio
        Tulostaa ohjelman versionumeron ja lisenssin.
",
        name = PROGRAM_NAME
    )
}

fn run(_args: Args) -> Result<(), String> {
    let config_file = init_config_file(CONFIG_FILE)?;
    //println!("config_file = {:?}", config_file);
    let config_str = || config_file.to_string_lossy();

    if !config_file.exists() {
        return Err(format!(
            "Asetustiedostoa ”{}” ei ole vielä olemassa.\n\
             Luo se käyttämällä valitsinta ”--tietokanta=...”. \
             Valitsin ”-h” tulostaa apua.",
            config_str()
        ));
    }

    umask(0o077);

    Ok(())
}

fn init_config_file(file: &str) -> Result<PathBuf, String> {
    xdg::BaseDirectories::new()
        .map_err(|_| "Asetustiedoston alustus epäonnistui.".to_string())?
        .place_config_file(file)
        .map_err(|e| format!("Asetustiedoston alustus epäonnistui: {}", e.kind()))
}

fn umask(mask: u32) -> u32 {
    unsafe { libc::umask(mask) }
}
