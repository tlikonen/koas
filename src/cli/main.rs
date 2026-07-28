mod commands;
mod editable;

use crate::editable::{Editable, MoveToEditable};
use just_getopt::{Args, OptFlags, OptSpecs, OptValue};
use koas::database;
use koas::database::assignments::*;
use koas::database::grades::*;
use koas::database::groups::*;
use koas::database::stats::*;
use koas::database::students::*;
use koas::database::*;
use koas::output::*;
use koas::tools;
use koas::tools::StrExt;
use koas::{Config, Error, Result};
use std::io::{self, Write as _};
use std::process::ExitCode;

#[tokio::main]
async fn main() -> ExitCode {
    match program().await {
        Ok(_) | Err(Error::Exit) => ExitCode::SUCCESS,

        Err(err) => match err {
            Error::Io {
                kind: io::ErrorKind::BrokenPipe,
                ..
            } => ExitCode::FAILURE,

            Error::OldDatabase(_) => {
                let _ = writeln!(
                    io::stderr(),
                    "{err} Sen voi päivittää vuorovaikutteisessa tilassa."
                );
                ExitCode::FAILURE
            }

            other => {
                let _ = writeln!(io::stderr(), "{other}");
                ExitCode::FAILURE
            }
        },
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
        return Err(Error::from("Valitsin ”-h” tulostaa apua."));
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
    let mut output = Output::default();

    if !config_file.exists() {
        Config::default().write(&config_file)?;
        return Err(Error::from(format!(
            "Luotiin asetustiedosto ”{}”.\n\
             Muokkaa tiedostoa tekstieditorilla ja aseta tietokannan yhteysasetukset.\n\
             Seuraavilla valitsimilla saa apua: ”--ohje=tietokanta” ja ”--ohje=asetukset”.",
            config_file.display()
        )));
    }

    let config = Config::read(&config_file)?;

    // Table-printing format.
    if !config.tables.is_empty() {
        output = Output::select(&config.tables).unwrap_or_default();
    }

    if let Some(value) = args.options_value_last("taulukot") {
        output = Output::select(value)
            .map_err(|e| format!("Sopimaton arvo valitsimelle --taulukot: {e}"))?;
    }

    // Choose the mode for command stage: stdin, single or interactive.
    let mut modes = Modes::default();
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

    let mut db = match database::connect(&config).await {
        Ok(db) => db,
        Err(err) => match err {
            Error::OldDatabase(old) if matches!(modes.mode(), Mode::Interactive) => {
                maybe_upgrade_db(old).await?
            }
            e => return Err(e),
        },
    };

    let mut editable = Editable::default();
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
                        e => return Err(Error::from(e)),
                    },
                };

                if line.is_empty() {
                    break;
                }

                rl.add_history_entry(&line)?;

                let (cmd, args) = tools::split_first(&line);

                match commands(&mut modes, &mut db, &mut editable, cmd, args).await {
                    Ok(_) => (),
                    Err(err) => match err {
                        Error::UnknownCmd(cmd) => {
                            writeln!(stderr, "Tuntematon komento ”{cmd}”. Apua saa ?:llä.")?;
                        }

                        Error::UnknownTbl(tbl) => {
                            writeln!(stderr, "Tuntematon taulukkotyyppi ”{tbl}”. Apua saa ?:llä.")?;
                        }

                        e => writeln!(stderr, "{e}")?,
                    },
                }
            }
        }

        Mode::Single(line) => {
            let (cmd, args) = tools::split_first(&line);
            match commands(&mut modes, &mut db, &mut editable, cmd, args).await {
                Ok(_) => (),
                Err(err) => match err {
                    Error::UnknownCmd(cmd) => {
                        return Err(Error::from(format!(
                            "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                        )));
                    }
                    e => return Err(e),
                },
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
                        Err(err) => match err {
                            Error::UnknownCmd(cmd) => {
                                return Err(Error::from(format!(
                                    "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                                )));
                            }
                            e => return Err(e),
                        },
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
    let mode = modes.mode();

    match cmd {
        "ho" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let lastname = fields.next().unwrap_or(""); // sukunimi
            let firstname = fields.next().unwrap_or(""); // etunimi
            let group = fields.next().unwrap_or(""); // ryhma
            let desc = fields.next().unwrap_or(""); // lisätiedot
            is_too_much_fields(fields, 4)?;

            let query = Student::query(
                db,
                QueryMatch::WildAround(lastname),
                QueryMatch::WildAround(firstname),
                QueryMatch::WildAround(group),
                QueryMatch::WildAround(desc),
            )
            .await?
            .has_data()?;

            if modes.is_interactive() {
                query.print_num(out)?;
                query.move_to(editable);
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
            is_too_much_fields(fields, 2)?;

            let query = Group::query(
                db,
                QueryMatch::WildAround(name),
                QueryMatch::WildAround(desc),
            )
            .await?
            .has_data()?;

            if modes.is_interactive() {
                query.print_num(out)?;
                query.move_to(editable);
                editable.print_fields(&["Ryhmä", "Lisätiedot"])?;
            } else {
                query.print(out)?;
            }
        }

        "hs" => {
            editable.clear();

            let (group, _) = tools::split_first(args);
            let query = AssignmentsForGroup::query(db, QueryMatch::Wild(group))
                .await?
                .has_data()?;

            if modes.is_interactive() && query.count() == 1 {
                let assign = query.take(0).expect("there should be 0th element");
                assign.print_num(out)?;
                assign.move_to(editable);
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
            is_too_much_fields(fields, 3)?;

            let query = GradesForAssignment::query(
                db,
                QueryMatch::WildAround(group),
                QueryMatch::WildAround(assign),
                QueryMatch::WildAround(assign_short),
            )
            .await?
            .has_data()?;

            if modes.is_interactive() && query.count() == 1 {
                let grades = query.take(0).expect("there should be 0th element");
                grades.print_num(out)?;
                grades.move_to(editable);
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
            is_too_much_fields(fields, 4)?;

            let query = GradesForStudent::query(
                db,
                QueryMatch::WildAround(lastname),
                QueryMatch::WildAround(firstname),
                QueryMatch::WildAround(group),
                QueryMatch::WildAround(desc),
            )
            .await?
            .has_data()?;

            if modes.is_interactive() && query.count() == 1 {
                let grades = query.take(0).expect("there should be 0th element");
                grades.print_num(out)?;
                grades.move_to(editable);
                editable.print_fields(&["Arvosana(As)", "Lisätiedot"])?;
            } else {
                query.print(out)?;
            }
        }

        "hak" => {
            editable.clear();
            let (group, _) = tools::split_first(args);
            GradesForGroup::query(db, QueryMatch::Wild(group))
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
                    group: QueryMatch::WildAround(fields.next().unwrap_or("")),
                    assignment: QueryMatch::WildAround(fields.next().unwrap_or("")),
                    assignment_short: QueryMatch::WildAround(fields.next().unwrap_or("")),
                    lastname: QueryMatch::WildAround(fields.next().unwrap_or("")),
                    firstname: QueryMatch::WildAround(fields.next().unwrap_or("")),
                    description: QueryMatch::WildAround(fields.next().unwrap_or("")),
                });
            }

            let include_weightless = matches!(c, "tpk" | "tjk");

            match c {
                "tp" | "tpk" => {
                    StudentRanking::query(db, &queries, include_weightless)
                        .await?
                        .has_data()?
                        .print(out)?;
                }

                "tj" | "tjk" => {
                    GradeDistribution::query(db, &queries, include_weightless)
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
            let lastname: Lastname = fields.next().unwrap_or("").try_into()?; // sukunimi
            let firstname: Firstname = fields.next().unwrap_or("").try_into()?; // etunimi
            let groups: GroupNames = fields.next().unwrap_or("").try_into()?; // ryhmät
            let description: Option<Description> = match fields.next() {
                None | Some("") => None,
                Some(desc) => Some(desc.try_into()?),
            }; // lisätiedot
            is_too_much_fields(fields, 4)?;

            Student::insert(lastname, firstname, groups, description)
                .commit(db)
                .await?;
        }

        "ls" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let groups: GroupNames = fields.next().unwrap_or("").try_into()?; // ryhmät
            let assignment: AssignmentName = fields.next().unwrap_or("").try_into()?; // suoritus
            let assignment_short: AssignmentShort = fields.next().unwrap_or("").try_into()?; // lyhenne
            let weight: Option<AssignmentWeight> = match fields.next() {
                None | Some("") => None,
                Some(w) => Some(w.try_into()?),
            }; // painokerroin
            let position: Option<AssignmentPosition> = match fields.next() {
                None | Some("") => None,
                Some(p) => Some(p.try_into()?),
            }; // sija
            is_too_much_fields(fields, 5)?;

            let mut updates = Queue::default();
            for group in groups {
                Assignment::insert(
                    group,
                    assignment.clone(),
                    assignment_short.clone(),
                    weight.clone(),
                    position.clone(),
                )
                .queue(&mut updates);
            }
            updates.commit(db).await?;
        }

        "m" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err(Error::from(
                    "Edellinen komento ei sisällä muokattavia tietueita.",
                ));
            }

            if args.is_empty() {
                return Err(Error::from(
                    "Argumentiksi pitää antaa tietueiden numerot ja muokattavat kentät.",
                ));
            }

            let list_max = editable.count();
            let (indices, rest) = parse_next_number_list(args, list_max)?;
            let fields = tools::split_sep(rest);

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

        "ms" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err(Error::from(
                    "Edellinen komento ei sisällä muokattavia tietueita.",
                ));
            }

            if args.is_empty() {
                return Err(Error::from(
                    "Argumentiksi pitää antaa tietueiden numerot ja kentän numero.",
                ));
            }

            let list_max = editable.count();
            let (indices, rest) = parse_next_number_list(args, list_max)?;
            let (field_num, rest) = parse_next_number(rest)?;
            assert_field_num(field_num, editable)?;
            no_more_arguments(rest)?;

            commands::print_read_values_intro(field_num, &indices)?;
            let values = commands::read_values(&indices)?;
            if values.lines().all(|x| x.is_empty()) {
                return Err(Error::from("Ei muutoksia."));
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

                Editable::Grades(grades) => {
                    commands::edit_grade_series(
                        db,
                        grades.iter_index1(indices),
                        field_num,
                        values.lines(),
                    )
                    .await?;
                }
            }
        }

        "ma" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err(Error::from(
                    "Edellinen komento ei sisällä muokattavia tietueita.",
                ));
            }

            if !editable.is_grade() {
                return Err(Error::from("Vain arvosanoja voi muokata tällä komennolla."));
            }

            if args.is_empty() {
                return Err(Error::from("Puuttuu tietueiden numerot."));
            }

            let list_max = editable.count();
            let (indices, rest) = parse_next_number_list(args, list_max)?;
            no_more_arguments(rest)?;

            if let Editable::Grades(student_grades) = editable {
                let mut updates = Queue::default();
                for student_grade in student_grades.iter_index1(indices) {
                    if let Some(ss) = &student_grade.grade
                        && let Some(old) = tools::parse_number(ss)
                        && let Some(new) = tools::float_to_grade(old)
                    {
                        student_grade.set_grade(&new)?.queue(&mut updates);
                    }
                }
                updates.commit(db).await?;
            }
        }

        "md" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err(Error::from(
                    "Edellinen komento ei sisällä muokattavia tietueita.",
                ));
            }

            if !editable.is_grade() {
                return Err(Error::from("Vain arvosanoja voi muokata tällä komennolla."));
            }

            if args.is_empty() {
                return Err(Error::from("Puuttuu tietueiden numerot."));
            }

            let list_max = editable.count();
            let (indices, rest) = parse_next_number_list(args, list_max)?;
            no_more_arguments(rest)?;

            if let Editable::Grades(student_grades) = editable {
                let mut updates = Queue::default();
                for student_grade in student_grades.iter_index1(indices) {
                    if let Some(ss) = &student_grade.grade
                        && let Some(old) = tools::parse_number(ss)
                    {
                        let new = tools::format_decimal(old);
                        student_grade.set_grade(&new)?.queue(&mut updates);
                    }
                }
                updates.commit(db).await?;
            }
        }

        "poista" if matches!(mode, Mode::Interactive) => {
            if editable.is_none() {
                return Err(Error::from(
                    "Edellinen komento ei sisällä poistettavia tietueita.",
                ));
            }

            if args.is_empty() {
                return Err(Error::from("Puuttuu tietueiden numerot."));
            }

            let list_max = editable.count();
            let (indices, rest) = parse_next_number_list(args, list_max)?;
            no_more_arguments(rest)?;

            match editable {
                Editable::None => (),

                Editable::Students(students) => {
                    let mut updates = Queue::default();
                    for student in students.iter_index1(indices) {
                        student.mark_deleted().queue(&mut updates);
                    }
                    updates.commit(db).await?;
                }

                Editable::Groups(_) => {
                    return Err(Error::from(
                        "Ryhmiä ei voi poistaa näin. Ryhmä poistuu itsestään,\n\
                         kun siltä poistaa kaikki oppilaat ja suoritukset.",
                    ));
                }

                Editable::Assignments(assignments) => {
                    let mut updates = Queue::default();
                    for assignment in assignments.iter_index1(indices) {
                        assignment.mark_deleted().queue(&mut updates);
                    }
                    updates.commit(db).await?;
                }

                Editable::Grades(grades) => {
                    let mut updates = Queue::default();
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
            Stats::query(db).await?.print(out)?;
        }

        "?" => {
            editable.clear();
            commands::help(args)?;
        }

        c => return Err(Error::unknown_cmd(c)),
    }
    Ok(())
}

fn is_too_much_fields(mut fields: impl Iterator, max: usize) -> Result<()> {
    if fields.next().is_some() {
        Err(Error::from(format!(
            "Liikaa kenttiä. Vain {max} hyväksytään."
        )))
    } else {
        Ok(())
    }
}

fn parse_next_number_list(s: &str, m: usize) -> Result<(Vec<usize>, &str)> {
    let (nl, rest) = tools::split_first(s);
    let list = tools::parse_number_list(nl)?;
    if !tools::is_within_limits(m, &list) {
        return Err(Error::from(format!("Suurin muokattava tietue on {m}.")));
    }
    Ok((list, rest))
}

fn parse_next_number(s: &str) -> Result<(usize, &str)> {
    let (num, rest) = tools::split_first(s);
    let num = match num.parse::<usize>() {
        Ok(n) => n,
        Err(_) => return Err(Error::from("Sopimaton kentän numero.")),
    };

    Ok((num, rest))
}

fn no_more_arguments(s: &str) -> Result<()> {
    if s.is_empty() {
        Ok(())
    } else {
        Err(Error::from("Liikaa argumentteja."))
    }
}

fn assert_field_num(field_num: usize, editable: &Editable) -> Result<()> {
    let field_num_max: usize = match editable {
        Editable::None => return Err(Error::from("Ei muokattavia tietueita.")),
        Editable::Students(_) => 4,
        Editable::Groups(_) => 2,
        Editable::Assignments(_) => 4,
        Editable::Grades(_) => 2,
    };

    if !(1..=field_num_max).contains(&field_num) {
        return Err(Error::from(format!(
            "Kentän numeron täytyy olla 1–{field_num_max}."
        )));
    }

    Ok(())
}

async fn maybe_upgrade_db(old: OldDb) -> Result<DBase> {
    let mut stdout = io::stdout();
    let stdin = io::stdin();

    write!(
        stdout,
        "Arvosanatietokanta on vanhentunut. Päivitetäänkö? (vastaa ”kyllä”): "
    )?;
    stdout.flush()?;

    let mut line = String::with_capacity(6);
    stdin.read_line(&mut line)?;

    if line == "kyllä\n" {
        let db = old.upgrade().await?;
        writeln!(stdout, "Tietokanta päivitetty.")?;
        Ok(db)
    } else {
        Err(Error::OldDatabase(old))
    }
}
