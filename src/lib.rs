mod config;

use just_getopt::Args;

pub fn run(args: Args) -> Result<(), String> {
    let config_file = config::init_config_file()?;

    umask(0o077);

    if args.option_exists("postgresql") {
        eprintln!("puuttuu --postgresql-valitsimen käsittely.");
    } else if config_file.exists() {
        eprintln!("Puuttuu asetustiedoston lukeminen");
    } else {
        config::write_config_file(&config_file, &Default::default())?;
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

fn split_sep(s: &str) -> Vec<String> {
    let mut chars = s.chars();
    let sep = chars.nth(0).expect("Tyhjä merkkijono");
    chars.collect::<String>().split(sep).map(|i| i.to_string()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_split_sep() {
        assert_eq!(vec!["eka", "toka"], split_sep("/eka/toka"));
        assert_eq!(vec![""], split_sep("/"));
    }
}
