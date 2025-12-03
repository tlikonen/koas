use crate::prelude::*;

pub async fn stats(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    Stats::query(db).await?.print(modes.output());
    Ok(())
}

pub async fn students(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let lastname = fields.next().unwrap_or(""); // sukunimi
    let firstname = fields.next().unwrap_or(""); // etunimi
    let group = fields.next().unwrap_or(""); // ryhma
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = Students::query(db, lastname, firstname, group, desc)
        .await?
        .has_data()?;

    if modes.is_interactive() {
        query.copy_to(editable);
        query.print_numbered(modes.output());
        editable.print_fields(&["Sukunimi", "Etunimi", "Ryhmät", "Lisätiedot"]);
    } else {
        query.print(modes.output());
    }
    Ok(())
}

pub async fn groups(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let name = fields.next().unwrap_or(""); // ryhmä
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = Groups::query(db, name, desc).await?.has_data()?;

    if modes.is_interactive() {
        query.copy_to(editable);
        query.print_numbered(modes.output());
        editable.print_fields(&["Ryhmä", "Lisätiedot"]);
    } else {
        query.print(modes.output());
    }
    Ok(())
}

pub async fn assignments(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let group = {
        let (g, _) = tools::split_first(args);
        if g.is_empty() {
            Err("Argumentiksi pitää antaa ryhmän nimi.")?;
        }
        g
    };

    let query = Assignments::query(db, group).await?.has_data()?;

    if modes.is_interactive() {
        query.copy_to(editable);
        query.print_numbered(modes.output());
        editable.print_fields(&["Suoritus", "Lyhenne(Lyh)", "Painokerroin(K)", "Järjestys"]);
    } else {
        query.print(modes.output());
    }
    Ok(())
}

pub async fn grades_for_assignments(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let group = fields.next().unwrap_or(""); // ryhmä
    let assign = fields.next().unwrap_or(""); // suoritus
    let assign_short = fields.next().unwrap_or(""); // lyhenne

    let query = GradesForAssignments::query(db, group, assign, assign_short)
        .await?
        .has_data()?;

    match query.count() {
        0 => panic!(),
        1 if modes.is_interactive() => {
            query.copy_to(editable);
            query.print_numbered(modes.output());
            editable.print_fields(&["Arvosana(As)", "Lisätiedot"]);
        }
        _ => query.print(modes.output()),
    }

    Ok(())
}

pub async fn grades_for_students(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let mut fields = tools::split_sep(args);
    let lastname = fields.next().unwrap_or(""); // sukunimi
    let firstname = fields.next().unwrap_or(""); // etunimi
    let group = fields.next().unwrap_or(""); // ryhmä
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = GradesForStudents::query(db, lastname, firstname, group, desc)
        .await?
        .has_data()?;

    match query.count() {
        0 => panic!(),
        1 if modes.is_interactive() => {
            query.copy_to(editable);
            query.print_numbered(modes.output());
            editable.print_fields(&["Arvosana(As)", "Lisätiedot"]);
        }
        _ => query.print(modes.output()),
    }

    Ok(())
}

pub async fn grades_for_group(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let group = {
        let (g, _) = tools::split_first(args);
        if g.is_empty() {
            Err("Argumentiksi pitää antaa ryhmän nimi.")?;
        }
        g
    };

    let query = GradesForGroup::query(db, group).await?.has_data()?;
    query.print(modes.output());
    Ok(())
}

pub async fn edit(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä muokattavia tietueita.")?;
    }

    if args.is_empty() {
        Err("Argumentiksi pitää antaa tietueiden numerot ja muokattavat kentät.")?;
    }

    let (indexes, fields) = {
        let (first, rest) = tools::split_first(args);
        let n = tools::parse_number_list(first)?;
        let f = tools::split_sep(rest);

        let max = editable.count();
        if !tools::is_within_limits(max, &n) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }

        (n, f)
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Students(students) => {
            edit_students(&mut ta, indexes, students, fields).await?;
        }
        EditableItem::Groups(groups) => {
            edit_groups(&mut ta, indexes, groups, fields).await?;
        }
        EditableItem::Assignments(assignments) => {
            edit_assignments(&mut ta, indexes, assignments, fields).await?;
        }
        EditableItem::Grades(grades) => {
            edit_grades(&mut ta, indexes, grades, fields).await?;
        }
        EditableItem::None => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn edit_series(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä muokattavia tietueita.")?;
    }

    if args.is_empty() {
        Err("Argumentiksi pitää antaa tietueiden numerot ja kentän numero.")?;
    }

    let (indexes, rest) = {
        let (first, rest) = tools::split_first(args);
        if rest.is_empty() {
            Err("Toiseksi argumentiksi täytyy antaa kentän numero.")?;
        }
        let i = tools::parse_number_list(first)?;

        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }

        (i, rest)
    };

    let (field_num, rest) = {
        let field_num_max = match editable.item() {
            EditableItem::Students(_) => 4,
            EditableItem::Groups(_) => 2,
            EditableItem::Assignments(_) => 4,
            EditableItem::Grades(_) => 2,
            EditableItem::None => panic!(),
        };

        let field_num_err =
            || format!("Kentän numeron täytyy olla kokonaisluku 1–{field_num_max}.");

        let (n, rest) = tools::split_first(rest);
        let n = n.parse::<usize>().map_err(|_| field_num_err())?;
        if n < 1 || n > field_num_max {
            Err(field_num_err())?;
        }
        (n, rest)
    };

    let mut values: Vec<String> = Vec::with_capacity(4);
    if tools::has_content(rest) {
        for s in tools::split_sep(rest).map(|s| s.to_string()) {
            values.push(s);
        }
    } else {
        print!(
            "Syötä kentän {field_num} arvot riveittäin. Pelkkä välilyönti poistaa kentän arvon\n\
             (paitsi eräitä pakollisia). Tyhjä rivi jättää kentän ennalleen. Ctrl-d lopettaa.\n\
             Tietueet:"
        );
        for i in &indexes {
            print!(" {i}");
        }
        println!("\n---");

        let mut input = io::stdin().lines();
        let mut i = 0;

        loop {
            if i >= indexes.len() {
                println!("Kaikki tiedot kerätty. Lopeta Ctrl-d:llä.");
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
        Err("Ei tehty muutoksia.")?;
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
                edit_students(&mut ta, index, students, fields).await?;
            }
            EditableItem::Groups(groups) => {
                edit_groups(&mut ta, index, groups, fields).await?;
            }
            EditableItem::Assignments(assignments) => {
                edit_assignments(&mut ta, index, assignments, fields).await?;
            }
            EditableItem::Grades(grades) => {
                edit_grades(&mut ta, index, grades, fields).await?;
            }
            EditableItem::None => panic!(),
        }
    }

    ta.commit().await?;
    Ok(())
}

async fn edit_students(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    students: &[Student],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let lastname = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // sukunimi

    let firstname = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // etunimi

    let groups = fields.next().filter(|x| tools::has_content(x)); // ryhmät
    let desc = fields.next().map(tools::normalize_str); // lisätiedot

    if lastname.is_none() && firstname.is_none() && groups.is_none() && desc.is_none() {
        Err("Anna muokattavia kenttiä.")?;
    }

    if (lastname.is_some() || firstname.is_some()) && indexes.len() > 1 {
        Err("Usealle henkilölle ei voi muuttaa kerralla samaa nimeä.\n\
             Muuta yksi kerrallaan, jos se on tarkoituksena.")?;
    }

    let mut groups_add: Vec<String> = Vec::with_capacity(3);
    let mut groups_remove: Vec<String> = Vec::with_capacity(1);

    if let Some(groups) = groups {
        for g in groups.split_whitespace() {
            let mut chars = g.chars();
            match chars.next() {
                Some('+') => groups_add.push(chars.collect()),
                Some('-') => groups_remove.push(chars.collect()),
                _ => Err(
                    "Kirjoita oppilaan ryhmätunnuksen alkuun merkki ”+” (lisää ryhmä) \
                     tai ”-” (poista ryhmä).\nErota eri ryhmät välilyönnillä.",
                )?,
            }
        }

        if groups_add.iter().any(|s| s.is_empty()) || groups_remove.iter().any(|s| s.is_empty()) {
            Err("Ryhmän nimiä puuttuu. Kirjoita merkin ”+” tai ”-” jälkeen ryhmätunnus.")?;
        }
    }

    for i in indexes {
        let student = match students.get(i - 1) {
            None => Err("Ei muokattavia oppilaita.")?,
            Some(v) => v,
        };

        if let Some(last) = &lastname {
            student.update_lastname(db, last).await?;
        }

        if let Some(first) = &firstname {
            student.update_firstname(db, first).await?;
        }

        for name in &groups_add {
            let rid = Group::get_or_insert(db, name).await?;
            if student.in_group(db, rid).await? {
                continue;
            } else {
                student.add_to_group(db, rid).await?;
            }
        }

        for name in &groups_remove {
            let rid = match Group::get_id(db, name).await? {
                Some(id) => id,
                None => continue,
            };

            if !student.in_group(db, rid).await? {
                continue;
            }

            let count = student.count_grades_group(db, rid).await?;
            if count > 0 {
                Err(format!(
                    "Oppilaalle ”{l}, {f}” on ryhmässä ”{g}” kirjattu {c} arvosana(a).\n\
                     Säilytetään ryhmät ja perutaan toiminto.",
                    l = student.lastname,
                    f = student.firstname,
                    c = count,
                    g = name,
                ))?;
            }

            if student.only_one_group(db).await? {
                Err("Oppilaan pitää kuulua vähintään yhteen ryhmään.")?;
            } else {
                student.remove_from_group(db, rid).await?;
            }
        }

        if let Some(d) = &desc {
            student.update_description(db, d).await?;
        }
    }

    Groups::delete_empty(db).await?;
    Ok(())
}

async fn edit_groups(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    groups: &[Group],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let mut name = fields.next().filter(|x| tools::has_content(x)); // ryhmä

    let desc = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // lisätiedot

    if name.is_none() && desc.is_none() {
        Err("Anna muokattavia kenttiä.")?;
    }

    if name.is_some() && indexes.len() > 1 {
        Err("Usealle ryhmälle ei voi antaa samaa nimeä.")?;
    }

    name = match name {
        Some(s) => {
            let mut words = s.split_whitespace();
            let first = words.next().unwrap();
            if words.next().is_some() {
                Err("Ryhmätunnuksen pitää olla yksi sana.")?;
            }
            Some(first)
        }
        None => None,
    };

    for i in indexes {
        let group = match groups.get(i - 1) {
            None => Err("Muokattavia ryhmiä ei ole.")?,
            Some(g) => g,
        };

        if let Some(n) = name {
            group.update_name(db, n).await?;
        }

        if let Some(d) = &desc {
            group.update_description(db, d).await?;
        }
    }
    Ok(())
}

async fn edit_assignments(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    group_assignments: &[Assignment],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let name = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // suoritus

    let short = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // lyhenne

    let weight = fields.next().filter(|x| !x.is_empty()); // painokerroin
    let position = fields.next().filter(|x| tools::has_content(x)); // sija

    if name.is_none() && short.is_none() && weight.is_none() && position.is_none() {
        Err("Anna muokattavia kenttiä.")?;
    }

    if position.is_some() && indexes.len() > 1 {
        Err("Usealle suoritukselle ei voi asettaa samaa järjestysnumeroa.")?;
    }

    let weight = match weight {
        Some(s) if !tools::has_content(s) => Some(None), // painokerroin: NULL
        Some(s) => match s.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Some(Some(n)),
            _ => Err("Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).")?,
        },
        None => None,
    };

    let position = match position {
        Some(s) => match s.trim().parse::<i32>() {
            Ok(n) => Some(n),
            _ => Err("Järjestysnumeron täytyy olla kokonaisluku.")?,
        },
        None => None,
    };

    for i in indexes {
        let group_assignment = match group_assignments.get(i - 1) {
            None => Err("Muokattavia suorituksia ei ole.")?,
            Some(v) => v,
        };

        if let Some(n) = &name {
            group_assignment.update_name(db, n).await?;
        }

        if let Some(s) = &short {
            group_assignment.update_short(db, s).await?;
        }

        if let Some(w) = weight {
            group_assignment.update_weight(db, w).await?;
        }

        if let Some(p) = position {
            group_assignment.update_position(db, p).await?;
        }
    }
    Ok(())
}

async fn edit_grades(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    student_grades: &[Grade],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let grade = fields
        .next()
        .filter(|x| !x.is_empty())
        .map(tools::normalize_str); // arvosana

    let desc = fields
        .next()
        .filter(|x| !x.is_empty())
        .map(tools::normalize_str); // lisätiedot

    if grade.is_none() && desc.is_none() {
        Err("Anna muokattavia kenttiä.")?;
    }

    for i in indexes {
        let student_grade = match student_grades.get(i - 1) {
            None => Err("Muokattavia arvosanoja ei ole.")?,
            Some(v) => v,
        };

        if let Some(s) = &grade {
            student_grade.update_grade(db, s).await?;
        }

        if let Some(d) = &desc {
            student_grade.update_description(db, d).await?;
        }

        student_grade.delete_if_empty(db).await?;
    }
    Ok(())
}

pub async fn convert_to_grade(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä muokattavia tietueita.")?;
    }

    if !editable.is_grade() {
        Err("Vain arvosanoja voi muokata tällä komennolla.")?;
    }

    if args.is_empty() {
        Err("Puuttuu tietueiden numerot.")?;
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Grades(student_grades) => {
            for i in indexes {
                let student_grade = match student_grades.get(i - 1) {
                    Some(v) => v,
                    None => Err("Ei muokattavia tietueita.")?,
                };

                if let Some(ss) = &student_grade.grade
                    && let Some(old) = tools::parse_number(ss)
                    && let Some(new) = tools::float_to_grade(old)
                {
                    student_grade.update_grade(&mut ta, &new).await?;
                }
            }
        }
        _ => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn convert_to_decimal(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä muokattavia tietueita.")?;
    }

    if !editable.is_grade() {
        Err("Vain arvosanoja voi muokata tällä komennolla.")?;
    }

    if args.is_empty() {
        Err("Puuttuu tietueiden numerot.")?;
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            Err(format!("Suurin muokattava tietue on {max}."))?;
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Grades(student_grades) => {
            for i in indexes {
                let student_grade = match student_grades.get(i - 1) {
                    Some(v) => v,
                    None => Err("Ei muokattavia tietueita.")?,
                };

                if let Some(ss) = &student_grade.grade
                    && let Some(old) = tools::parse_number(ss)
                {
                    let new = tools::format_decimal(old);
                    student_grade.update_grade(&mut ta, &new).await?;
                }
            }
        }
        _ => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn insert_student(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();

    let mut fields = tools::split_sep(args);

    let lastname = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // sukunimi

    let firstname = fields
        .next()
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // etunimi

    let groups = fields.next().filter(|x| tools::has_content(x)); // ryhmät
    let desc = fields.next().or(Some("")).map(tools::normalize_str); // lisätiedot

    if lastname.is_none() || firstname.is_none() || groups.is_none() {
        Err("Pitää antaa vähintään sukunimi, etunimi ja ryhmä.")?;
    }

    let mut student = Student {
        lastname: lastname.unwrap(),
        firstname: firstname.unwrap(),
        description: desc.unwrap(),
        ..Default::default()
    };

    let mut ta = db.begin().await?;
    student.insert(&mut ta).await?;

    for g in groups.unwrap().split_whitespace() {
        let rid = Group::get_or_insert(&mut ta, g).await?;
        student.add_to_group(&mut ta, rid).await?;
    }

    ta.commit().await?;
    Ok(())
}

pub async fn insert_assignment(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
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
        Err("Pitää antaa vähintään ryhmä, suorituksen nimi ja lyhenne.")?;
    }

    let weight = match weight {
        Some(s) => match s.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Some(n),
            _ => Err("Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).")?,
        },
        None => None,
    };

    let position = match position {
        Some(s) => match s.trim().parse::<i32>() {
            Ok(n) => n,
            _ => Err("Järjestysnumeron täytyy olla kokonaisluku.")?,
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

pub async fn delete(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä poistettavia tietueita.")?;
    }

    if args.is_empty() {
        Err("Puuttuu tietueiden numerot.")?;
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            Err(format!("Suurin poistettava tietue on {max}."))?;
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable.item() {
        EditableItem::Students(students) => {
            delete_students(&mut ta, indexes, students).await?;
        }
        EditableItem::Groups(_) => {
            Err("Ryhmiä ei voi poistaa näin. Ryhmä poistuu itsestään,\n\
                 kun siltä poistaa kaikki oppilaat ja suoritukset.")?;
        }
        EditableItem::Assignments(assignments) => {
            delete_assignments(&mut ta, indexes, assignments).await?;
        }
        EditableItem::Grades(grades) => {
            delete_grades(&mut ta, indexes, grades).await?;
        }
        EditableItem::None => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

async fn delete_students(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    students: &[Student],
) -> Result<(), Box<dyn Error>> {
    for i in indexes {
        let student = match students.get(i - 1) {
            None => Err("Poistettavia oppilaita ei ole.")?,
            Some(v) => v,
        };

        let count = student.count_grades(db).await?;
        if count > 0 {
            Err(format!(
                "Oppilaalle ”{l}, {f}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                l = student.lastname,
                f = student.firstname,
                c = count
            ))?;
        }

        student.delete(db).await?;
    }
    Groups::delete_empty(db).await?;
    Ok(())
}

async fn delete_assignments(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    assignments: &[Assignment],
) -> Result<(), Box<dyn Error>> {
    let mut rid_list = Vec::with_capacity(1);
    for i in indexes {
        let assignment = match assignments.get(i - 1) {
            None => Err("Poistettavia suorituksia ei ole.")?,
            Some(v) => v,
        };

        let count = assignment.count_grades(db).await?;
        if count > 0 {
            Err(format!(
                "Suoritukselle ”{a}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                a = assignment.assignment,
                c = count,
            ))?;
        }

        assignment.delete(db).await?;

        if !rid_list.contains(&assignment.rid) {
            rid_list.push(assignment.rid);
        }
    }

    Groups::delete_empty(db).await?;

    for rid in rid_list {
        Assignments::reposition(db, rid).await?;
    }

    Ok(())
}

async fn delete_grades(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    grades: &[Grade],
) -> Result<(), Box<dyn Error>> {
    for i in indexes {
        let grade = match grades.get(i - 1) {
            None => Err("Poistettavia arvosanoja ei ole.")?,
            Some(v) => v,
        };
        grade.delete(db).await?;
    }
    Ok(())
}

pub async fn student_ranking(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    mut args: &str,
    all: bool,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "@";
    }

    let mut ranks = StudentRanking::new();

    let field_groups = tools::split_sep(args);
    for field_string in field_groups {
        let mut fields = tools::split_sep(field_string);
        let query_terms = FullQuery {
            // Keep the order!
            group: fields.next().unwrap_or(""),
            assignment: fields.next().unwrap_or(""),
            assignment_short: fields.next().unwrap_or(""),
            lastname: fields.next().unwrap_or(""),
            firstname: fields.next().unwrap_or(""),
            description: fields.next().unwrap_or(""),
        };

        ranks.query(db, all, query_terms).await?;
    }

    ranks.has_data()?.print(modes.output());
    Ok(())
}

pub async fn grade_distribution(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    mut args: &str,
    all: bool,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "@";
    }

    let mut dist = GradeDistribution::new(modes.output());

    let field_groups = tools::split_sep(args);
    for field_string in field_groups {
        let mut fields = tools::split_sep(field_string);
        let query_terms = FullQuery {
            // Keep the order!
            group: fields.next().unwrap_or(""),
            assignment: fields.next().unwrap_or(""),
            assignment_short: fields.next().unwrap_or(""),
            lastname: fields.next().unwrap_or(""),
            firstname: fields.next().unwrap_or(""),
            description: fields.next().unwrap_or(""),
        };

        dist.query(db, all, query_terms).await?;
    }

    dist.has_data()?.print(modes.output());
    Ok(())
}

pub fn table_format(modes: &mut Modes, args: &str) -> Result<(), Box<dyn Error>> {
    let (first, _) = tools::split_first(args);
    if first.is_empty() {
        Err("Anna argumentiksi taulukkotyyli. Apua saa ?:llä.")?;
    }

    let new = Output::select(first)
        .map_err(|e| format!("Tuntematon taulukkotyyli ”{e}”. Apua saa ?:llä."))?;
    modes.set_output(new);
    Ok(())
}

pub fn help(topic: &str) -> Result<(), String> {
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

    match topic {
        "" => println!("\n{QUICK}"),
        "ho" => println!("\n{HO}"),
        "hr" => println!("\n{HR}"),
        "hs" => println!("\n{HS}"),
        "has" => println!("\n{HAS}"),
        "hao" => println!("\n{HAO}"),
        "hak" => println!("\n{HAK}"),

        "m" => println!("\n{M}"),
        "ms" => println!("\n{MS}"),
        "ma" => println!("\n{MA}"),
        "md" => println!("\n{MD}"),
        "poista" => println!("\n{POISTA}"),

        "lo" => println!("\n{LO}"),
        "ls" => println!("\n{LS}"),

        "tp" | "tpk" => println!("\n{TP}"),
        "tj" | "tjk" => println!("\n{TJ}"),

        "tk" => println!("\n{TK}"),
        "tlk" => println!("\n{TLK}"),
        "?" => println!("\n{QM}"),

        "komennot" => println!(
            "\n{QUICK}\n{info}",
            info = include_str!("../help/command.txt")
        ),

        "tietokanta" => println!("\n{}", include_str!("../help/database.txt")),
        "asetukset" => println!("\n{}", include_str!("../help/settings.txt")),
        u => Err(format!("Tuntematon ohjeiden aihe: ”{u}”."))?,
    }
    Ok(())
}
