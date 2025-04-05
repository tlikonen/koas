pub mod config;
pub mod tools;

use just_getopt as jg;

pub fn run(args: jg::Args) -> Result<(), String> {
    let config_file = config::init()?;

    umask(0o077);

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

    Ok(())
}

fn umask(mask: u32) -> u32 {
    unsafe { libc::umask(mask) }
}
