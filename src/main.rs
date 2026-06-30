use {
    just_getopt::{Args, OptFlags, OptSpecs, OptValue},
    koas::*,
    std::{
        io::{self, Write as _},
        process::ExitCode,
    },
};

#[tokio::main]
async fn main() -> ExitCode {
    match cli().await {
        Ok(_) => ExitCode::SUCCESS,

        Err(Error::Io {
            kind: io::ErrorKind::BrokenPipe,
            ..
        }) => ExitCode::FAILURE,

        Err(other) => {
            let _ = writeln!(io::stderr(), "{other}");
            ExitCode::FAILURE
        }
    }
}

async fn cli() -> Result<()> {
    let args = OptSpecs::new()
        .option("taulukot", "taulukot", OptValue::RequiredNonEmpty)
        .option("ohje", "ohje", OptValue::OptionalNonEmpty)
        .option("help", "h", OptValue::None)
        .option("version", "version", OptValue::None)
        .flag(OptFlags::PrefixMatchLongOptions)
        .getopt(std::env::args().skip(1));

    let mut stdout = io::stdout();
    let mut stderr = io::stderr();
    let mut error = false;

    for u in args.unknown_options() {
        writeln!(stderr, "Tuntematon valitsin ”{u}”.")?;
        error = true;
    }

    for o in args.required_value_missing() {
        writeln!(stderr, "Valitsimelle ”{}” täytyy antaa arvo.", o.id)?;
        error = true;
    }

    if error {
        return Err("Valitsin ”-h” tulostaa apua.".into());
    }

    if args.option_exists("help") {
        writeln!(
            stdout,
            include_str!("../help/usage.txt"),
            ohjelma = PROGRAM_NAME,
        )?;
        return Ok(());
    }

    if args.option_exists("ohje") {
        let topic = args.options_value_last("ohje").map_or("", |v| v);
        commands::help(topic)?;
        return Ok(());
    }

    if args.option_exists("version") {
        writeln!(
            stdout,
            "{name} v{version}\n\
             Tekijä:   {author}\n\
             Lisenssi: {license}",
            name = PROGRAM_NAME,
            version = PROGRAM_VERSION,
            author = PROGRAM_AUTHORS,
            license = PROGRAM_LICENSE
        )?;
        return Ok(());
    }

    config_stage(args).await
}

async fn config_stage(args: Args) -> Result<()> {
    let config_file = Config::file()?;
    let mut output: Output = Default::default();

    tools::umask();

    let config: Config = if config_file.exists() {
        Config::read(&config_file)?
    } else {
        let c: Config = Default::default();
        c.write(&config_file)?;
        return Err(format!(
            "Luotiin asetustiedosto ”{}”.\n\
             Muokkaa tiedostoa tekstieditorilla ja aseta tietokannan yhteysasetukset.\n\
             Seuraavilla valitsimilla saa apua: ”--ohje=tietokanta” ja ”--ohje=asetukset”.",
            config_file.display()
        )
        .into());
    };

    // Table-printing format.
    if !config.tables.is_empty() {
        output = Output::select(&config.tables).unwrap_or_default();
    }

    if let Some(value) = args.options_value_last("taulukot") {
        output = Output::select(value)
            .map_err(|e| format!("Sopimaton arvo valitsimelle --taulukot: {e}"))?;
    }

    // Choose the mode for command stage: stdin, single or interactive.
    let mut modes: Modes = Default::default();
    modes.set_output(output);
    if args.other.len() == 1 && args.other[0] == "-" {
        modes.set_mode(Mode::Stdin);
    } else if !args.other.is_empty() {
        modes.set_mode(Mode::Single(args.other.join(" ")));
    } else {
        modes.set_mode(Mode::Interactive);
    }

    command_stage(modes, config).await
}

async fn command_stage(mut modes: Modes, config: Config) -> Result<()> {
    let mut db = database::connect(&config, &modes).await?;
    let mut editable: Editable = Default::default();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    match modes.mode().clone() {
        Mode::Interactive => {
            writeln!(
                stdout,
                "{prg} v{ver} (postgres://{user}@{host}:{port}/{db})",
                prg = PROGRAM_NAME,
                ver = PROGRAM_VERSION,
                user = config.user,
                host = config.host,
                port = config.port,
                db = config.database,
            )?;

            let prompt = format!("{PROGRAM_NAME}> ");
            let mut rl = rustyline::DefaultEditor::new()?;

            loop {
                let line = rl.readline(&prompt)?;
                if line.is_empty() {
                    break;
                }
                rl.add_history_entry(&line)?;

                let (cmd, args) = tools::split_first(&line);
                // if modes.upgrade() && cmd == UPGRADE_COMMAND {}

                match commands(&mut modes, &mut db, &mut editable, cmd, args).await {
                    Ok(_) => (),
                    Err(Error::UnknownCmd(cmd)) => {
                        writeln!(stderr, "Tuntematon komento ”{cmd}”. Apua saa ?:llä.")?;
                    }
                    Err(Error::UnknownTbl(tbl)) => {
                        writeln!(stderr, "Tuntematon taulukkotyyppi ”{tbl}”. Apua saa ?:llä.")?;
                    }
                    Err(e) => {
                        writeln!(stderr, "{e}")?;
                    }
                }
            }
        }

        Mode::Single(line) => {
            let (cmd, args) = tools::split_first(&line);
            match commands(&mut modes, &mut db, &mut editable, cmd, args).await {
                Ok(_) => (),
                Err(Error::UnknownCmd(cmd)) => {
                    return Err(format!(
                        "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                    )
                    .into());
                }
                err => return err,
            }
        }

        Mode::Stdin => {
            let mut ta = db.begin().await?;
            for item in io::stdin().lines() {
                let line = item?;
                if !line.is_empty() {
                    let (cmd, args) = tools::split_first(&line);
                    match commands(&mut modes, &mut ta, &mut editable, cmd, args).await {
                        Ok(_) => (),
                        Err(Error::UnknownCmd(cmd)) => {
                            return Err(format!(
                                "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                            )
                            .into());
                        }
                        err => return err,
                    }
                }
            }
            ta.commit().await?;
        }
    }
    Ok(())
}

async fn commands(
    modes: &mut Modes,
    db: &mut DBase,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<()> {
    match (cmd, modes.mode()) {
        ("ho", _) => commands::students(modes, db, editable, args).await?,
        ("hr", _) => commands::groups(modes, db, editable, args).await?,
        ("hs", _) => commands::assignments(modes, db, editable, args).await?,
        ("has", _) => commands::grades_for_assignments(modes, db, editable, args).await?,
        ("hao", _) => commands::grades_for_students(modes, db, editable, args).await?,
        ("hak", _) => commands::grades_for_group(modes, db, editable, args).await?,

        ("tp", _) => commands::student_ranking(modes, db, editable, args, false).await?,
        ("tpk", _) => commands::student_ranking(modes, db, editable, args, true).await?,
        ("tj", _) => commands::grade_distribution(modes, db, editable, args, false).await?,
        ("tjk", _) => commands::grade_distribution(modes, db, editable, args, true).await?,

        ("lo", _) => commands::insert_student(db, editable, args).await?,
        ("ls", _) => commands::insert_assignment(db, editable, args).await?,

        ("m", Mode::Interactive) => commands::edit(db, editable, args).await?,
        ("ms", Mode::Interactive) => commands::edit_series(db, editable, args).await?,
        ("ma", Mode::Interactive) => commands::convert_to_grade(db, editable, args).await?,
        ("md", Mode::Interactive) => commands::convert_to_decimal(db, editable, args).await?,
        ("poista", Mode::Interactive) => commands::delete(db, editable, args).await?,

        ("tlk", _) => commands::table_format(modes, args)?,
        ("tk", _) => commands::stats(modes, db, editable).await?,

        ("?", _) => {
            editable.clear();
            commands::help(args)?;
        }

        (c, _) => return Err(Error::unknown_cmd(c)),
    }
    Ok(())
}
