mod groups;
mod students;
pub use groups::*;
pub use students::*;

use crate::prelude::*;

pub async fn stats(modes: &Modes, db: &mut DBase, editable: &mut Editable) -> Result<()> {
    editable.clear();

    Stats::query(db).await?.print(modes.output())?;
    Ok(())
}

pub async fn assignments(
    modes: &Modes,
    db: &mut DBase,
    editable: &mut Editable,
    args: &str,
) -> Result<()> {
    editable.clear();

    let group = {
        let (g, _) = tools::split_first(args);
        if g.is_empty() {
            return Err("Argumentiksi pitää antaa ryhmän nimi.".into());
        }
        g
    };

    let query = AssignmentsForGroups::query(db, group).await?.has_data()?;

    match query.list.len() {
        0 => panic!(),
        1 if modes.is_interactive() => {
            let tbl = &query.list[0];
            tbl.copy_to(editable);
            tbl.print_num(modes.output())?;
            editable.print_fields(&["Suoritus", "Lyhenne(Lyh)", "Painokerroin(K)", "Järjestys"])?;
        }
        _ => query.print(modes.output())?,
    }

    Ok(())
}

pub async fn grades_for_assignments(
    modes: &Modes,
    db: &mut DBase,
    editable: &mut Editable,
    args: &str,
) -> Result<()> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let group = fields.next().unwrap_or(""); // ryhmä
    let assign = fields.next().unwrap_or(""); // suoritus
    let assign_short = fields.next().unwrap_or(""); // lyhenne

    let query = GradesForAssignments::query(db, group, assign, assign_short)
        .await?
        .has_data()?;

    match query.list.len() {
        0 => panic!(),
        1 if modes.is_interactive() => {
            let tbl = &query.list[0];
            tbl.copy_to(editable);
            tbl.print_num(modes.output())?;
            editable.print_fields(&["Arvosana(As)", "Lisätiedot"])?;
        }
        _ => query.print(modes.output())?,
    }

    Ok(())
}

pub async fn grades_for_students(
    modes: &Modes,
    db: &mut DBase,
    editable: &mut Editable,
    args: &str,
) -> Result<()> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let lastname = fields.next().unwrap_or(""); // sukunimi
    let firstname = fields.next().unwrap_or(""); // etunimi
    let group = fields.next().unwrap_or(""); // ryhmä
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = GradesForStudents::query(db, lastname, firstname, group, desc)
        .await?
        .has_data()?;

    match query.list.len() {
        0 => panic!(),
        1 if modes.is_interactive() => {
            let tbl = &query.list[0];
            tbl.copy_to(editable);
            tbl.print_num(modes.output())?;
            editable.print_fields(&["Arvosana(As)", "Lisätiedot"])?;
        }
        _ => query.print(modes.output())?,
    }

    Ok(())
}

pub async fn grades_for_group(
    modes: &Modes,
    db: &mut DBase,
    editable: &mut Editable,
    args: &str,
) -> Result<()> {
    editable.clear();

    let group = {
        let (g, _) = tools::split_first(args);
        if g.is_empty() {
            return Err("Argumentiksi pitää antaa ryhmän nimi.".into());
        }
        g
    };

    let query = GradesForGroups::query(db, group).await?.has_data()?;
    query.print(modes.output())?;
    Ok(())
}

pub async fn edit(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    if editable.is_none() {
        return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
    }

    if args.is_empty() {
        return Err("Argumentiksi pitää antaa tietueiden numerot ja muokattavat kentät.".into());
    }

    let (indexes, fields) = {
        let (first, rest) = tools::split_first(args);
        let n = tools::parse_number_list(first)?;
        let f = tools::split_sep(rest);

        let max = editable.count();
        if !tools::is_within_limits(max, &n) {
            return Err(format!("Suurin muokattava tietue on {max}.").into());
        }

        (n, f)
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Students(students) => {
            students.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        EditableItem::Groups(groups) => {
            groups.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        EditableItem::Assignments(assignments) => {
            assignments.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        EditableItem::Grades(grades) => {
            grades.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        EditableItem::None => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn edit_series(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    let mut stdout = io::stdout();

    if editable.is_none() {
        return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
    }

    if args.is_empty() {
        return Err("Argumentiksi pitää antaa tietueiden numerot ja kentän numero.".into());
    }

    let (indexes, rest) = {
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
        let field_num_max = match editable.item() {
            EditableItem::Students(_) => 4, // sukunimi, etunimi, ryhmät, lisätiedot
            EditableItem::Groups(_) => 2,   // ryhmä, lisätiedot
            EditableItem::Assignments(_) => 4, // suoritus, lyhenne, painokerroin, järjestys
            EditableItem::Grades(_) => 2,   // arvosana, lisätiedot
            EditableItem::None => panic!(),
        };

        let field_num_err =
            || format!("Kentän numeron täytyy olla kokonaisluku 1–{field_num_max}.");

        let (n, rest) = tools::split_first(rest);
        let n = n.parse::<usize>().map_err(|_| field_num_err())?;
        if n < 1 || n > field_num_max {
            return Err(field_num_err().into());
        }
        (n, rest)
    };

    let mut values: Vec<String> = Vec::with_capacity(4);
    if tools::has_content(rest) {
        for s in tools::split_sep(rest).map(|s| s.to_string()) {
            values.push(s);
        }
    } else {
        write!(
            stdout,
            "Syötä kentän {field_num} arvot riveittäin. Pelkkä välilyönti poistaa kentän arvon\n\
             (paitsi eräitä pakollisia). Tyhjä rivi jättää kentän ennalleen. Ctrl-d lopettaa.\n\
             Tietueet:"
        )?;
        for i in &indexes {
            write!(stdout, " {i}")?;
        }
        writeln!(stdout, "\n---")?;

        let mut input = io::stdin().lines();
        let mut i = 0;

        loop {
            if i >= indexes.len() {
                writeln!(stdout, "Kaikki tiedot kerätty. Lopeta Ctrl-d:llä.")?;
            }

            let line = match input.next() {
                None => break,
                Some(v) => v?,
            };

            if i < indexes.len() {
                values.push(line);
            }
            i += 1;
        }
    }

    if values.iter().all(|x| x.is_empty()) {
        return Err("Ei muutoksia.".into());
    }

    let mut ta = db.begin().await?;

    for (n, i) in indexes.into_iter().enumerate() {
        let index = vec![i];

        let value = match values.get(n) {
            None => break,
            Some(v) => v,
        };

        if value.is_empty() {
            continue;
        }

        let fields = {
            let mut v = vec![""; field_num - 1];
            v.push(value);
            v.into_iter()
        };

        match editable.item() {
            EditableItem::Students(students) => {
                students.for_edit(index, fields).edit(&mut ta).await?;
            }
            EditableItem::Groups(groups) => {
                groups.for_edit(index, fields).edit(&mut ta).await?;
            }
            EditableItem::Assignments(assignments) => {
                assignments.for_edit(index, fields).edit(&mut ta).await?;
            }
            EditableItem::Grades(grades) => {
                grades.for_edit(index, fields).edit(&mut ta).await?;
            }
            EditableItem::None => panic!(),
        }
    }

    ta.commit().await?;
    Ok(())
}

impl Edit for EditItems<'_, Assignment> {
    async fn edit(&self, db: &mut DBase) -> Result<()> {
        let name = self.field(0); // suoritus
        let short = self.field(1); // lyhenne
        let weight = self.field(2); // painokerroin
        let position = self.field(3); // sija

        if !name.has_value() && !short.has_value() && weight.is_none() && !position.has_value() {
            return Err("Anna muokattavia kenttiä.".into());
        }

        if position.has_value() && self.count() > 1 {
            return Err("Usealle suoritukselle ei voi asettaa samaa järjestysnumeroa.".into());
        }

        // Convert from &Field<String> to Field<i32>.
        let weight = match weight {
            Field::Value(s) => match s.trim().parse::<i32>() {
                Ok(n) if n >= 1 => Field::Value(n),
                _ => {
                    return Err(
                        "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).".into(),
                    );
                }
            },
            Field::ValueEmpty => Field::ValueEmpty,
            Field::None => Field::None,
        };

        // Convert from &Field<String> to Field<i32>.
        let position = match position {
            Field::Value(s) => match s.trim().parse::<i32>() {
                Ok(n) => Field::Value(n),
                _ => return Err("Järjestysnumeron täytyy olla kokonaisluku.".into()),
            },
            Field::ValueEmpty | Field::None => Field::None,
        };

        for group_assignment in self.iter() {
            if let Field::Value(n) = name {
                group_assignment.update_name(db, n).await?;
            }

            if let Field::Value(s) = short {
                group_assignment.update_short(db, s).await?;
            }

            match weight {
                Field::Value(w) => group_assignment.update_weight(db, Some(w)).await?,
                Field::ValueEmpty => group_assignment.update_weight(db, None).await?,
                Field::None => (),
            }

            if let Field::Value(p) = position {
                group_assignment.update_position(db, p).await?;
            }
        }
        Ok(())
    }
}

impl Edit for EditItems<'_, Grade> {
    async fn edit(&self, db: &mut DBase) -> Result<()> {
        let grade = self.field(0); // arvosana
        let desc = self.field(1); // lisätiedot

        if grade.is_none() && desc.is_none() {
            return Err("Anna muokattavia kenttiä.".into());
        }

        for student_grade in self.iter() {
            match grade {
                Field::Value(s) => student_grade.update_grade(db, Some(s)).await?,
                Field::ValueEmpty => student_grade.update_grade(db, None).await?,
                Field::None => (),
            }

            match desc {
                Field::Value(d) => student_grade.update_description(db, Some(d)).await?,
                Field::ValueEmpty => student_grade.update_description(db, None).await?,
                Field::None => (),
            }

            student_grade.delete_if_empty(db).await?;
        }
        Ok(())
    }
}

pub async fn convert_to_grade(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    if editable.is_none() {
        return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
    }

    if !editable.is_grade() {
        return Err("Vain arvosanoja voi muokata tällä komennolla.".into());
    }

    if args.is_empty() {
        return Err("Puuttuu tietueiden numerot.".into());
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            return Err(format!("Suurin muokattava tietue on {max}.").into());
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Grades(student_grades) => {
            let edits = student_grades.for_edit(indexes, [""; 0]); // empty fields
            for student_grade in edits.iter() {
                if let Some(ss) = &student_grade.grade
                    && let Some(old) = tools::parse_number(ss)
                    && let Some(new) = tools::float_to_grade(old)
                {
                    student_grade.update_grade(&mut ta, Some(&new)).await?;
                }
            }
        }
        _ => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn convert_to_decimal(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    if editable.is_none() {
        return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
    }

    if !editable.is_grade() {
        return Err("Vain arvosanoja voi muokata tällä komennolla.".into());
    }

    if args.is_empty() {
        return Err("Puuttuu tietueiden numerot.".into());
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            return Err(format!("Suurin muokattava tietue on {max}.").into());
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Grades(student_grades) => {
            let edits = student_grades.for_edit(indexes, [""; 0]); // empty fields
            for student_grade in edits.iter() {
                if let Some(ss) = &student_grade.grade
                    && let Some(old) = tools::parse_number(ss)
                {
                    let new = tools::format_decimal(old);
                    student_grade.update_grade(&mut ta, Some(&new)).await?;
                }
            }
        }
        _ => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn insert_assignment(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let groups = fields.next().filter(|x| tools::has_content(x)); // ryhmät

    let assignment = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // suoritus

    let assignment_short = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // lyhenne

    let weight = fields.next().filter(|x| tools::has_content(x)); // painokerroin
    let position = fields.next().filter(|x| tools::has_content(x)); // sija

    if groups.is_none() || assignment.is_none() || assignment_short.is_none() {
        return Err("Pitää antaa vähintään ryhmä, suorituksen nimi ja lyhenne.".into());
    }

    let weight = match weight {
        Some(s) => match s.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Some(n),
            _ => {
                return Err(
                    "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).".into(),
                );
            }
        },
        None => None,
    };

    let position = match position {
        Some(s) => match s.trim().parse::<i32>() {
            Ok(n) => n,
            _ => return Err("Järjestysnumeron täytyy olla kokonaisluku.".into()),
        },
        None => i32::MAX,
    };

    let mut ta = db.begin().await?;

    for g in groups.unwrap().split_whitespace() {
        let mut group_assignment = Assignment {
            rid: Group::get_or_insert(&mut ta, g).await?,
            assignment: assignment.clone().unwrap(),
            assignment_short: assignment_short.clone().unwrap(),
            weight,
            ..Default::default()
        };

        group_assignment.insert(&mut ta, position).await?;
    }

    ta.commit().await?;
    Ok(())
}

pub async fn delete(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    if editable.is_none() {
        return Err("Edellinen komento ei sisällä poistettavia tietueita.".into());
    }

    if args.is_empty() {
        return Err("Puuttuu tietueiden numerot.".into());
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            return Err(format!("Suurin poistettava tietue on {max}.").into());
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Students(students) => {
            students.for_delete(indexes).delete(&mut ta).await?;
        }
        EditableItem::Groups(_) => {
            return Err("Ryhmiä ei voi poistaa näin. Ryhmä poistuu itsestään,\n\
                        kun siltä poistaa kaikki oppilaat ja suoritukset."
                .into());
        }
        EditableItem::Assignments(assignments) => {
            assignments.for_delete(indexes).delete(&mut ta).await?;
        }
        EditableItem::Grades(grades) => {
            grades.for_delete(indexes).delete(&mut ta).await?;
        }
        EditableItem::None => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

impl Delete for DeleteItems<'_, Assignment> {
    async fn delete(&self, db: &mut DBase) -> Result<()> {
        let mut rid_list = Vec::with_capacity(1);
        for assignment in self.iter() {
            let count = assignment.count_grades(db).await?;
            if count > 0 {
                return Err(format!(
                    "Suoritukselle ”{a}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                    a = assignment.assignment,
                    c = count,
                )
                .into());
            }

            assignment.delete(db).await?;

            if !rid_list.contains(&assignment.rid) {
                rid_list.push(assignment.rid);
            }
        }

        Groups::delete_empty(db).await?;

        for rid in rid_list {
            Assignment::reposition(db, rid).await?;
        }

        Ok(())
    }
}

impl Delete for DeleteItems<'_, Grade> {
    async fn delete(&self, db: &mut DBase) -> Result<()> {
        for grade in self.iter() {
            grade.delete(db).await?;
        }
        Ok(())
    }
}

pub async fn student_ranking(
    modes: &Modes,
    db: &mut DBase,
    editable: &mut Editable,
    mut args: &str,
    all: bool,
) -> Result<()> {
    editable.clear();
    if args.is_empty() {
        args = "@";
    }

    let mut ranks = StudentRanking::new();

    let field_groups = tools::split_sep(args);
    for field_string in field_groups {
        let mut fields = tools::split_sep(field_string);
        let query_terms = FullQuery {
            // Keep the order because of the next() method.
            group: fields.next().unwrap_or(""),
            assignment: fields.next().unwrap_or(""),
            assignment_short: fields.next().unwrap_or(""),
            lastname: fields.next().unwrap_or(""),
            firstname: fields.next().unwrap_or(""),
            description: fields.next().unwrap_or(""),
            all,
        };

        ranks.query(db, query_terms).await?;
    }

    ranks.has_data()?.print(modes.output())?;
    Ok(())
}

pub async fn grade_distribution(
    modes: &Modes,
    db: &mut DBase,
    editable: &mut Editable,
    mut args: &str,
    all: bool,
) -> Result<()> {
    editable.clear();
    if args.is_empty() {
        args = "@";
    }

    let mut dist = GradeDistribution::new(modes.output());

    let field_groups = tools::split_sep(args);
    for field_string in field_groups {
        let mut fields = tools::split_sep(field_string);
        let query_terms = FullQuery {
            // Keep the order because of the next() method.
            group: fields.next().unwrap_or(""),
            assignment: fields.next().unwrap_or(""),
            assignment_short: fields.next().unwrap_or(""),
            lastname: fields.next().unwrap_or(""),
            firstname: fields.next().unwrap_or(""),
            description: fields.next().unwrap_or(""),
            all,
        };

        dist.query(db, query_terms).await?;
    }

    dist.has_data()?.print(modes.output())?;
    Ok(())
}

pub fn table_format(modes: &mut Modes, args: &str) -> Result<()> {
    let (first, _) = tools::split_first(args);
    if first.is_empty() {
        return Err("Anna argumentiksi taulukkotyyli. Apua saa ?:llä.".into());
    }

    let new = Output::select(first)?;
    modes.set_output(new);
    Ok(())
}

pub fn help(topic: &str) -> Result<()> {
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
