mod commands;

use just_getopt::{Args, OptFlags, OptSpecs, OptValue};
use koas::commands as koascmd;
use koas::database;
use koas::database::*;
use koas::output::*;
use koas::tools;
use koas::{Config, Error, Result};
use std::io::{self, Write as _};
use std::process::ExitCode;

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
            include_str!("help/usage.txt"),
            ohjelma = koas::PROGRAM_NAME,
        )?;
        return Err(Error::Exit);
    }

    if args.option_exists("ohje") {
        let topic = args.options_value_last("ohje").map_or("", |v| v);
        commands::help(topic)?;
        return Err(Error::Exit);
    }

    if args.option_exists("version") {
        writeln!(
            stdout,
            "{name} v{version}\n\
             Tekijä:   {author}\n\
             Lisenssi: {license}",
            name = koas::PROGRAM_NAME,
            version = koas::PROGRAM_VERSION,
            author = koas::PROGRAM_AUTHORS,
            license = koas::PROGRAM_LICENSE
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
    use rustyline::error::ReadlineError;

    let mut db = database::connect(&config).await?;
    let mut editable: Editable = Default::default();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    match modes.mode().clone() {
        Mode::Interactive => {
            writeln!(
                stdout,
                "{prg} v{ver} (postgres://{user}@{host}:{port}/{db})",
                prg = koas::PROGRAM_NAME,
                ver = koas::PROGRAM_VERSION,
                user = config.user,
                host = config.host,
                port = config.port,
                db = config.database,
            )?;

            let prompt = format!("{}> ", koas::PROGRAM_NAME);
            let mut rl = rustyline::DefaultEditor::new()?;

            loop {
                let line = match rl.readline(&prompt) {
                    Ok(l) => l,
                    Err(err) => match err {
                        ReadlineError::Interrupted | ReadlineError::Eof => Err(Error::Exit)?,
                        _ => Err(err)?,
                    },
                };

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
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<()> {
    let out = modes.output();
    let mode = modes.mode();

    match cmd {
        "ho" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let lastname = fields.next().unwrap_or(""); // sukunimi
            let firstname = fields.next().unwrap_or(""); // etunimi
            let group = fields.next().unwrap_or(""); // ryhma
            let desc = fields.next().unwrap_or(""); // lisätiedot

            let query = koascmd::students(db, lastname, firstname, group, desc)
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

        "hr" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let name = fields.next().unwrap_or(""); // ryhmä
            let desc = fields.next().unwrap_or(""); // lisätiedot

            let query = koascmd::groups(db, name, desc).await?.has_data()?;

            if modes.is_interactive() {
                query.copy_to(editable);
                query.print_num(out)?;
                editable.print_fields(&["Ryhmä", "Lisätiedot"])?;
            } else {
                query.print(out)?;
            }
        }

        "hs" => {
            editable.clear();

            let (group, _) = tools::split_first(args);
            let query = koascmd::assignments(db, group).await?.has_data()?;

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

        "has" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let group = fields.next().unwrap_or(""); // ryhmä
            let assign = fields.next().unwrap_or(""); // suoritus
            let assign_short = fields.next().unwrap_or(""); // lyhenne

            let query = koascmd::grades_for_assignments(db, group, assign, assign_short)
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

        "hao" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let lastname = fields.next().unwrap_or(""); // sukunimi
            let firstname = fields.next().unwrap_or(""); // etunimi
            let group = fields.next().unwrap_or(""); // ryhmä
            let desc = fields.next().unwrap_or(""); // lisätiedot

            let query = koascmd::grades_for_students(db, lastname, firstname, group, desc)
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

        "hak" => {
            editable.clear();
            let (group, _) = tools::split_first(args);
            koascmd::grades_for_group(db, group)
                .await?
                .has_data()?
                .print(out)?;
        }

        c if ["tp", "tpk", "tj", "tjk"].contains(&c) => {
            editable.clear();

            let mut queries = Vec::with_capacity(3);
            let field_groups = tools::split_sep(if args.is_empty() { "@" } else { args });
            for field_string in field_groups {
                let mut fields = tools::split_sep(field_string);
                queries.push(FullQuery {
                    // Keep the order because of the next() method.
                    group: fields.next().unwrap_or(""),
                    assignment: fields.next().unwrap_or(""),
                    assignment_short: fields.next().unwrap_or(""),
                    lastname: fields.next().unwrap_or(""),
                    firstname: fields.next().unwrap_or(""),
                    description: fields.next().unwrap_or(""),
                });
            }

            let include_weightless = matches!(c, "tpk" | "tjk");

            match c {
                "tp" | "tpk" => {
                    koascmd::student_ranking(db, queries, include_weightless)
                        .await?
                        .has_data()?
                        .print(out)?;
                }

                "tj" | "tjk" => {
                    koascmd::grade_distribution(db, queries, include_weightless)
                        .await?
                        .has_data()?
                        .print(out)?;
                }

                _ => panic!(),
            }
        }

        "lo" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let lastname = fields.next().unwrap_or(""); // sukunimi
            let firstname = fields.next().unwrap_or(""); // etunimi
            let groups = fields.next().unwrap_or("").split_whitespace(); // ryhmät
            let desc = fields.next().unwrap_or(""); // lisätiedot

            koascmd::insert_student(db, lastname, firstname, groups, desc).await?;
        }

        "ls" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let groups = fields.next().unwrap_or("").split_whitespace(); // ryhmät
            let assignment = fields.next().unwrap_or(""); // suoritus
            let assignment_short = fields.next().unwrap_or(""); // lyhenne
            let weight = fields.next(); // painokerroin
            let position = fields.next(); // sija

            koascmd::insert_assignment(db, groups, assignment, assignment_short, weight, position)
                .await?;
        }

        "m" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
            }

            if args.is_empty() {
                return Err(
                    "Argumentiksi pitää antaa tietueiden numerot ja muokattavat kentät.".into(),
                );
            }

            let (indices, fields) = {
                let (first, rest) = tools::split_first(args);
                let i = tools::parse_number_list(first)?;
                let f = tools::split_sep(rest);

                let max = editable.count();
                if !tools::is_within_limits(max, &i) {
                    return Err(format!("Suurin muokattava tietue on {max}.").into());
                }

                (i, f)
            };

            match editable {
                Editable::None => (),

                Editable::Students(students) => {
                    commands::edit_students(db, students.iter_index1(indices), fields).await?
                }

                Editable::Groups(groups) => {
                    commands::edit_groups(db, groups.iter_index1(indices), fields).await?
                }

                Editable::Assignments(assignments) => {
                    commands::edit_assignments(db, assignments.iter_index1(indices), fields).await?
                }

                Editable::Grades(grades) => {
                    commands::edit_grades(db, grades.iter_index1(indices), fields).await?
                }
            }
        }

        "uusi-ms" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
            }

            if args.is_empty() {
                return Err("Argumentiksi pitää antaa tietueiden numerot ja kentän numero.".into());
            }

            let (indices, rest) = {
                let (first, rest) = tools::split_first(args);
                if rest.is_empty() {
                    return Err("Toiseksi argumentiksi täytyy antaa kentän numero.".into());
                }
                let i = tools::parse_number_list(first)?;

                let max = editable.count();
                if !tools::is_within_limits(max, &i) {
                    return Err(format!("Suurin muokattava tietue on {max}.").into());
                }

                (i, rest)
            };

            let (field_num, rest) = {
                let (f, rest) = tools::split_first(rest);
                let n = match f.parse::<usize>() {
                    Ok(n) => n,
                    Err(_) => return Err("Sopimaton kentän numero.".into()),
                };

                (n, rest)
            };

            if !rest.is_empty() {
                return Err("Vain kaksi argumenttia hyväksytään.".into());
            }

            {
                let mut stream = io::BufWriter::new(io::stdout());

                write!(
                    stream,
                    "Syötä kentän {field_num} arvot riveittäin. \
                     Tyhjä rivi jättää kentän ennalleen.\n\
                     Pelkkä välilyönti poistaa kentän arvon (paitsi eräitä pakollisia).\n\
                     Ctrl-d keskeyttää mutta tallentaa tähänastiset muutokset.\n\
                     Ctrl-c keskeyttää ja peruu kaikki muutokset\n\
                     Tietueet:"
                )?;

                for i in &indices {
                    write!(stream, " {i}")?;
                }
                writeln!(stream, "\n---")?;
                stream.flush()?;
            }

            let values = commands::read_values(&indices)?;
            if values.lines().all(|x| x.is_empty()) {
                return Err("Ei muutoksia.".into());
            }

            match editable {
                Editable::None => (),

                Editable::Students(students) => {
                    commands::edit_student_series(
                        db,
                        students.iter_index1(indices),
                        field_num,
                        values.lines(),
                    )
                    .await?;
                }

                Editable::Groups(groups) => {
                    commands::edit_group_series(
                        db,
                        groups.iter_index1(indices),
                        field_num,
                        values.lines(),
                    )
                    .await?;
                }

                Editable::Assignments(assignments) => {
                    commands::edit_assignment_series(
                        db,
                        assignments.iter_index1(indices),
                        field_num,
                        values.lines(),
                    )
                    .await?;
                }

                Editable::Grades(_) => todo!(),
            }
        }

        "ms" if matches!(mode, Mode::Interactive) => {
            koascmd::deprecated_edit_series(db, editable, args).await?
        }

        "ma" if matches!(mode, Mode::Interactive) => {
            koascmd::deprecated_convert_to_grade(db, editable, args).await?
        }

        "md" if matches!(mode, Mode::Interactive) => {
            koascmd::deprecated_convert_to_decimal(db, editable, args).await?
        }

        "poista" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err("Edellinen komento ei sisällä poistettavia tietueita.".into());
            }

            if args.is_empty() {
                return Err("Puuttuu tietueiden numerot.".into());
            }

            let indices = {
                let (first, _) = tools::split_first(args);
                let i = tools::parse_number_list(first)?;
                let max = editable.count();
                if !tools::is_within_limits(max, &i) {
                    return Err(format!("Suurin poistettava tietue on {max}.").into());
                }
                i
            };

            match editable {
                Editable::None => (),

                Editable::Students(students) => {
                    let mut updates = Queue::new();
                    for student in students.iter_index1(indices) {
                        student.mark_deleted().queue(&mut updates);
                    }
                    updates.commit(db).await?;
                }

                Editable::Groups(_) => {
                    return Err("Ryhmiä ei voi poistaa näin. Ryhmä poistuu itsestään,\n\
                                kun siltä poistaa kaikki oppilaat ja suoritukset."
                        .into());
                }

                Editable::Assignments(assignments) => {
                    let mut updates = Queue::new();
                    for assignment in assignments.iter_index1(indices) {
                        assignment.mark_deleted().queue(&mut updates);
                    }
                    updates.commit(db).await?;
                }

                Editable::Grades(grades) => {
                    let mut updates = Queue::new();
                    for student_grade in grades.iter_index1(indices) {
                        student_grade.mark_deleted().queue(&mut updates);
                    }
                    updates.commit(db).await?;
                }
            }
        }

        "tlk" => commands::table_format(modes, args)?,

        "tk" => {
            editable.clear();
            koascmd::stats(db).await?.print(out)?;
        }

        "?" => {
            editable.clear();
            commands::help(args)?;
        }

        c => return Err(Error::unknown_cmd(c)),
    }
    Ok(())
}
