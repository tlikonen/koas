use {
    just_getopt::{Args, OptFlags, OptSpecs, OptValue},
    koas::{database::*, output::*, tools::StrExt, *},
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
    let mut db = database::connect(&config).await?;
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

            let query = commands::students(db, lastname, firstname, group, desc)
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

            let query = commands::groups(db, name, desc).await?.has_data()?;

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

        "has" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let group = fields.next().unwrap_or(""); // ryhmä
            let assign = fields.next().unwrap_or(""); // suoritus
            let assign_short = fields.next().unwrap_or(""); // lyhenne

            let query = commands::grades_for_assignments(db, group, assign, assign_short)
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

            let query = commands::grades_for_students(db, lastname, firstname, group, desc)
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
            commands::grades_for_group(db, group)
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
                    commands::student_ranking(db, queries, include_weightless)
                        .await?
                        .has_data()?
                        .print(out)?;
                }

                "tj" | "tjk" => {
                    commands::grade_distribution(db, queries, include_weightless)
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

            commands::insert_student(db, lastname, firstname, groups, desc).await?;
        }

        "ls" => {
            editable.clear();

            let mut fields = tools::split_sep(args);
            let groups = fields.next().unwrap_or("").split_whitespace(); // ryhmät
            let assignment = fields.next().unwrap_or(""); // suoritus
            let assignment_short = fields.next().unwrap_or(""); // lyhenne
            let weight = fields.next(); // painokerroin
            let position = fields.next(); // sija

            commands::insert_assignment(db, groups, assignment, assignment_short, weight, position)
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
                let n = tools::parse_number_list(first)?;
                let f = tools::split_sep(rest);

                let max = editable.count();
                if !tools::is_within_limits(max, &n) {
                    return Err(format!("Suurin muokattava tietue on {max}.").into());
                }

                (n, f)
            };

            match editable {
                Editable::None => (),

                Editable::Students(students) => {
                    edit_students(db, students.iter_index1(indices), fields).await?
                }

                Editable::Groups(groups) => {
                    edit_groups(db, groups.iter_index1(indices), fields).await?
                }

                Editable::Assignments(assignments) => {
                    edit_assignments(db, assignments.iter_index1(indices), fields).await?
                }

                Editable::Grades(grades) => {
                    edit_grades(db, grades.iter_index1(indices), fields).await?
                }
            }
        }

        "ms" if matches!(mode, Mode::Interactive) => {
            commands::deprecated_edit_series(db, editable, args).await?
        }

        "ma" if matches!(mode, Mode::Interactive) => {
            commands::deprecated_convert_to_grade(db, editable, args).await?
        }

        "md" if matches!(mode, Mode::Interactive) => {
            commands::deprecated_convert_to_decimal(db, editable, args).await?
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

        "tlk" => table_format(modes, args)?,

        "tk" => {
            editable.clear();
            commands::stats(db).await?.print(out)?;
        }

        "?" => {
            editable.clear();
            help(args)?;
        }

        c => return Err(Error::unknown_cmd(c)),
    }
    Ok(())
}

async fn edit_students(
    db: &mut PgConnection,
    students: impl Iterator<Item = &Student>,
    mut fields: impl Iterator<Item = &str>,
) -> Result<()> {
    let lastname = fields.next().filter(|x| x.has_content()); // sukunimi
    let firstname = fields.next().filter(|x| x.has_content()); // etunimi
    let groups = fields.next().filter(|x| x.has_content()); // ryhmät
    let description = fields.next().filter(|x| !x.is_empty()); // lisätiedot
    if fields.next().is_some() {
        return Err("Liikaa kenttiä. Vain neljä hyväksytään.".into());
    }

    if lastname.is_none() && firstname.is_none() && groups.is_none() && description.is_none() {
        return Err("Anna muokattavia kenttiä.".into());
    }

    let students: Vec<&Student> = students.collect();

    if students.len() > 1 && (lastname.is_some() || firstname.is_some()) {
        return Err("Usealle henkilölle ei voi muuttaa kerralla samaa nimeä.\n\
                    Muuta yksi kerrallaan, jos se on todella tarkoituksena."
            .into());
    }

    let mut groups_add: Vec<String> = Vec::with_capacity(3);
    let mut groups_remove: Vec<String> = Vec::with_capacity(1);

    if let Some(groups) = groups {
        for g in groups.split_whitespace() {
            let mut chars = g.chars();
            match chars.next() {
                Some('+') => groups_add.push(chars.collect()),
                Some('-') => groups_remove.push(chars.collect()),
                _ => {
                    return Err(
                        "Kirjoita oppilaan ryhmätunnuksen alkuun merkki ”+” (lisää ryhmä) \
                         tai ”-” (poista ryhmä).\nErota eri ryhmät välilyönnillä."
                            .into(),
                    );
                }
            }
        }
    }

    let mut updates = Queue::new();

    for student in &students {
        if let Some(name) = lastname {
            student.set_lastname(name)?.queue(&mut updates);
        }

        if let Some(name) = firstname {
            student.set_firstname(name)?.queue(&mut updates);
        }

        for name in &groups_add {
            student.add_group(name)?.queue(&mut updates);
        }

        for name in &groups_remove {
            student.remove_group(name)?.queue(&mut updates);
        }

        if let Some(desc) = description {
            if desc.has_content() {
                student.set_description(desc)?.queue(&mut updates);
            } else {
                student.clear_description().queue(&mut updates);
            }
        }
    }

    updates.commit(db).await?;
    Ok(())
}

async fn edit_groups(
    db: &mut PgConnection,
    groups: impl Iterator<Item = &Group>,
    mut fields: impl Iterator<Item = &str>,
) -> Result<()> {
    let name = fields.next().filter(|x| x.has_content()); // ryhmä
    let description = fields.next().filter(|x| !x.is_empty()); // lisätiedot
    if fields.next().is_some() {
        return Err("Liikaa kenttiä. Vain kaksi hyväksytään.".into());
    }

    if name.is_none() && description.is_none() {
        return Err("Anna muokattavia kenttiä.".into());
    }

    let groups: Vec<&Group> = groups.collect();

    if groups.len() > 1 && name.is_some() {
        return Err("Usealle ryhmälle ei voi antaa samaa nimeä.".into());
    }

    let mut updates = Queue::new();

    for group in &groups {
        if let Some(n) = name {
            group.set_name(n)?.queue(&mut updates);
        }

        if let Some(desc) = description {
            if desc.has_content() {
                group.set_description(desc)?.queue(&mut updates);
            } else {
                group.clear_description().queue(&mut updates);
            }
        }
    }

    updates.commit(db).await?;
    Ok(())
}

async fn edit_assignments(
    db: &mut PgConnection,
    assignments: impl Iterator<Item = &Assignment>,
    mut fields: impl Iterator<Item = &str>,
) -> Result<()> {
    let name = fields.next().filter(|x| x.has_content()); // suoritus
    let short = fields.next().filter(|x| x.has_content()); // suoritus
    let weight = fields.next().filter(|x| !x.is_empty()); // painokerroin
    let position = fields.next().filter(|x| x.has_content()); // sija
    if fields.next().is_some() {
        return Err("Liikaa kenttiä. Vain neljä hyväksytään.".into());
    }

    if name.is_none() && short.is_none() && weight.is_none() && position.is_none() {
        return Err("Anna muokattavia kenttiä.".into());
    }

    let assignments: Vec<&Assignment> = assignments.collect();

    if assignments.len() > 1 && position.is_some() {
        return Err("Usealle suoritukselle ei voi asettaa samaa järjestysnumeroa.".into());
    }

    let mut updates = Queue::new();

    for assignment in &assignments {
        if let Some(n) = name {
            assignment.set_name(n)?.queue(&mut updates);
        }

        if let Some(n) = short {
            assignment.set_short(n)?.queue(&mut updates);
        }

        if let Some(w) = weight {
            if w.has_content() {
                assignment.set_weight(w)?.queue(&mut updates);
            } else {
                assignment.clear_weight().queue(&mut updates);
            }
        }

        if let Some(p) = position {
            assignment.set_position(p)?.queue(&mut updates);
        }
    }

    updates.commit(db).await?;
    Ok(())
}

async fn edit_grades(
    db: &mut PgConnection,
    grades: impl Iterator<Item = &Grade>,
    mut fields: impl Iterator<Item = &str>,
) -> Result<()> {
    let grade = fields.next().filter(|x| !x.is_empty()); // arvosana
    let description = fields.next().filter(|x| !x.is_empty()); // lisätiedot
    if fields.next().is_some() {
        return Err("Liikaa kenttiä. Vain kaksi hyväksytään.".into());
    }

    if grade.is_none() && description.is_none() {
        return Err("Anna muokattavia kenttiä.".into());
    }

    let mut updates = Queue::new();

    for student_grade in grades {
        if let Some(g) = grade {
            if g.has_content() {
                student_grade.set_grade(g)?.queue(&mut updates);
            } else {
                student_grade.clear_grade().queue(&mut updates);
            }
        }

        if let Some(d) = description {
            if d.has_content() {
                student_grade.set_description(d)?.queue(&mut updates);
            } else {
                student_grade.clear_description().queue(&mut updates);
            }
        }
    }

    updates.commit(db).await?;
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
