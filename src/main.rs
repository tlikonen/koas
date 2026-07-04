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
    match program().await {
        Ok(_) | Err(Error::Exit) => ExitCode::SUCCESS,

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

#[derive(Clone)]
enum Mode {
    Interactive,
    Single(String),
    Stdin,
}

#[derive(Default)]
struct Modes {
    mode: Option<Mode>,
    output: Option<Output>,
    // upgrade: bool,
}

impl Modes {
    fn output(&self) -> &Output {
        self.output.as_ref().expect("Uninitialized Modes::output.")
    }

    fn set_output(&mut self, v: Output) {
        self.output = Some(v);
    }

    fn mode(&self) -> &Mode {
        self.mode.as_ref().expect("Uninitialized Modes::mode.")
    }

    fn set_mode(&mut self, v: Mode) {
        self.mode = Some(v);
    }

    fn is_interactive(&self) -> bool {
        matches!(self.mode(), Mode::Interactive)
    }

    // pub fn upgrade(&self) -> bool {
    //     self.upgrade
    // }

    // pub fn set_upgrade(&mut self) {
    //     self.upgrade = true;
    // }
}

async fn program() -> Result<()> {
    tools::umask();
    let args = cli()?;
    let (config, modes) = config(args)?;
    command_stage(config, modes).await
}

fn cli() -> Result<Args> {
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
        return Err(Error::Exit);
    }

    if args.option_exists("ohje") {
        let topic = args.options_value_last("ohje").map_or("", |v| v);
        help(topic)?;
        return Err(Error::Exit);
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
        return Err(Error::Exit);
    }

    Ok(args)
}

fn config(args: Args) -> Result<(Config, Modes)> {
    let config_file = Config::file()?;
    let mut output: Output = Default::default();

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

    Ok((config, modes))
}

async fn command_stage(config: Config, mut modes: Modes) -> Result<()> {
    let mut db = connect(&config).await?;
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
    let out = modes.output();
    match (cmd, modes.mode()) {
        ("ho", _) => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let lastname = fields.next().unwrap_or(""); // sukunimi
            let firstname = fields.next().unwrap_or(""); // etunimi
            let group = fields.next().unwrap_or(""); // ryhma
            let desc = fields.next().unwrap_or(""); // lisätiedot

            let query = commands::students(db, (lastname, firstname, group, desc))
                .await?
                .has_data()?;

            if modes.is_interactive() {
                query.copy_to(editable);
                query.print_num(out)?;
                editable.print_fields(&["Sukunimi", "Etunimi", "Ryhmät", "Lisätiedot"])?;
            } else {
                query.print(out)?;
            }
        }

        ("hr", _) => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let name = fields.next().unwrap_or(""); // ryhmä
            let desc = fields.next().unwrap_or(""); // lisätiedot

            let query = commands::groups(db, (name, desc)).await?.has_data()?;

            if modes.is_interactive() {
                query.copy_to(editable);
                query.print_num(out)?;
                editable.print_fields(&["Ryhmä", "Lisätiedot"])?;
            } else {
                query.print(out)?;
            }
        }

        ("hs", _) => {
            editable.clear();

            let (group, _) = tools::split_first(args);
            let query = commands::assignments(db, group).await?.has_data()?;

            if modes.is_interactive()
                && query.count() == 1
                && let Some(assign) = query.get(0)
            {
                assign.copy_to(editable);
                assign.print_num(out)?;
                editable.print_fields(&[
                    "Suoritus",
                    "Lyhenne(Lyh)",
                    "Painokerroin(K)",
                    "Järjestys",
                ])?;
            } else {
                query.print(out)?;
            }
        }

        ("has", _) => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let group = fields.next().unwrap_or(""); // ryhmä
            let assign = fields.next().unwrap_or(""); // suoritus
            let assign_short = fields.next().unwrap_or(""); // lyhenne

            let query = commands::grades_for_assignments(db, (group, assign, assign_short))
                .await?
                .has_data()?;

            if modes.is_interactive()
                && query.count() == 1
                && let Some(grades) = query.get(0)
            {
                grades.copy_to(editable);
                grades.print_num(out)?;
                editable.print_fields(&["Arvosana(As)", "Lisätiedot"])?;
            } else {
                query.print(out)?;
            }
        }

        ("hao", _) => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let lastname = fields.next().unwrap_or(""); // sukunimi
            let firstname = fields.next().unwrap_or(""); // etunimi
            let group = fields.next().unwrap_or(""); // ryhmä
            let desc = fields.next().unwrap_or(""); // lisätiedot

            let query = commands::grades_for_students(db, (lastname, firstname, group, desc))
                .await?
                .has_data()?;

            if modes.is_interactive()
                && query.count() == 1
                && let Some(grades) = query.get(0)
            {
                grades.copy_to(editable);
                grades.print_num(out)?;
                editable.print_fields(&["Arvosana(As)", "Lisätiedot"])?;
            } else {
                query.print(out)?;
            }
        }

        ("hak", _) => {
            editable.clear();
            let (group, _) = tools::split_first(args);
            commands::grades_for_group(db, group)
                .await?
                .has_data()?
                .print(out)?;
        }

        ("tp", _) => {
            editable.clear();
            commands::student_ranking(db, args, false)
                .await?
                .has_data()?
                .print(out)?;
        }

        ("tpk", _) => {
            editable.clear();
            commands::student_ranking(db, args, true)
                .await?
                .has_data()?
                .print(out)?;
        }

        ("tj", _) => {
            editable.clear();
            commands::grade_distribution(db, args, false)
                .await?
                .has_data()?
                .print(out)?;
        }

        ("tjk", _) => {
            editable.clear();
            commands::grade_distribution(db, args, true)
                .await?
                .has_data()?
                .print(out)?;
        }

        ("lo", _) => {
            editable.clear();
            commands::insert_student(db, args).await?;
        }

        ("ls", _) => {
            editable.clear();
            commands::insert_assignment(db, args).await?;
        }

        ("m", Mode::Interactive) => commands::edit(db, editable, args).await?,
        ("ms", Mode::Interactive) => commands::edit_series(db, editable, args).await?,
        ("ma", Mode::Interactive) => commands::convert_to_grade(db, editable, args).await?,
        ("md", Mode::Interactive) => commands::convert_to_decimal(db, editable, args).await?,
        ("poista", Mode::Interactive) => commands::delete(db, editable, args).await?,

        ("tlk", _) => table_format(modes, args)?,

        ("tk", _) => {
            editable.clear();
            commands::stats(db).await?.print(out)?;
        }

        ("?", _) => {
            editable.clear();
            help(args)?;
        }

        (c, _) => return Err(Error::unknown_cmd(c)),
    }
    Ok(())
}

fn table_format(modes: &mut Modes, args: &str) -> Result<()> {
    let (first, _) = tools::split_first(args);
    if first.is_empty() {
        return Err("Anna argumentiksi taulukkotyyli. Apua saa ?:llä.".into());
    }

    let new = Output::select(first)?;
    modes.set_output(new);
    Ok(())
}

fn help(topic: &str) -> Result<()> {
    static HO: &str = include_str!("../help/command-ho.txt");
    static HR: &str = include_str!("../help/command-hr.txt");
    static HS: &str = include_str!("../help/command-hs.txt");
    static HAS: &str = include_str!("../help/command-has.txt");
    static HAO: &str = include_str!("../help/command-hao.txt");
    static HAK: &str = include_str!("../help/command-hak.txt");

    static M: &str = include_str!("../help/command-m.txt");
    static MS: &str = include_str!("../help/command-ms.txt");
    static MA: &str = include_str!("../help/command-ma.txt");
    static MD: &str = include_str!("../help/command-md.txt");
    static POISTA: &str = include_str!("../help/command-poista.txt");

    static LO: &str = include_str!("../help/command-lo.txt");
    static LS: &str = include_str!("../help/command-ls.txt");

    static TP: &str = include_str!("../help/command-tp.txt");
    static TJ: &str = include_str!("../help/command-tj.txt");
    static TK: &str = include_str!("../help/command-tk.txt");
    static TLK: &str = include_str!("../help/command-tlk.txt");

    static QM: &str = include_str!("../help/command-qm.txt");
    static QUICK: &str = include_str!("../help/quick.txt");

    let mut stdout = io::stdout();

    match topic {
        "" => writeln!(stdout, "\n{QUICK}")?,

        "ho" => writeln!(stdout, "\n{HO}")?,
        "hr" => writeln!(stdout, "\n{HR}")?,
        "hs" => writeln!(stdout, "\n{HS}")?,
        "has" => writeln!(stdout, "\n{HAS}")?,
        "hao" => writeln!(stdout, "\n{HAO}")?,
        "hak" => writeln!(stdout, "\n{HAK}")?,

        "m" => writeln!(stdout, "\n{M}")?,
        "ms" => writeln!(stdout, "\n{MS}")?,
        "ma" => writeln!(stdout, "\n{MA}")?,
        "md" => writeln!(stdout, "\n{MD}")?,
        "poista" => writeln!(stdout, "\n{POISTA}")?,
        "lo" => writeln!(stdout, "\n{LO}")?,
        "ls" => writeln!(stdout, "\n{LS}")?,

        "tp" | "tpk" => writeln!(stdout, "\n{TP}")?,
        "tj" | "tjk" => writeln!(stdout, "\n{TJ}")?,
        "tk" => writeln!(stdout, "\n{TK}")?,

        "tlk" => writeln!(stdout, "\n{TLK}")?,

        "?" => writeln!(stdout, "\n{QM}")?,
        "komennot" => writeln!(
            stdout,
            "\n{QUICK}\n{info}",
            info = include_str!("../help/command.txt")
        )?,
        "tietokanta" => writeln!(stdout, "\n{}", include_str!("../help/database.txt"))?,
        "asetukset" => writeln!(stdout, "\n{}", include_str!("../help/settings.txt"))?,

        u => return Err(format!("Tuntematon ohjeiden aihe: ”{u}”.").into()),
    }
    Ok(())
}
