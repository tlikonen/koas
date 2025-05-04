use crate::{
    Modes,
    database::{
        Assignment, Assignments, Editable, EditableItem, Group, Groups, Score,
        ScoresForAssignments, ScoresForGroup, ScoresForStudents, Stats, Student, Students,
    },
    tools,
};
use sqlx::{Connection, PgConnection};
use std::{error::Error, io};

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
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "/";
    }

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
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "/";
    }

    let mut fields = tools::split_sep(args);
    let name = fields.next().unwrap_or(""); // nimi
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = Groups::query(db, name, desc).await?.has_data()?;

    if modes.is_interactive() {
        query.copy_to(editable);
        query.print_numbered(modes.output());
        editable.print_fields(&["Nimi", "Lisätiedot"]);
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
        editable.print_fields(&["Suoritus", "Lyhenne(Lyh)", "Painokerroin(K)", "Sija"]);
    } else {
        query.print(modes.output());
    }
    Ok(())
}

pub async fn scores_for_assignments(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "/";
    }

    let mut fields = tools::split_sep(args);
    let group = fields.next().unwrap_or(""); // ryhmä
    let assign = fields.next().unwrap_or(""); // suoritus
    let assign_short = fields.next().unwrap_or(""); // lyhenne

    let query = ScoresForAssignments::query(db, group, assign, assign_short)
        .await?
        .has_data()?;

    match query.count() {
        0 => panic!(),
        1 => {
            if modes.is_interactive() {
                query.copy_to(editable);
                query.print_numbered(modes.output());
                editable.print_fields(&["Arvosana(As)", "Lisätiedot"]);
            } else {
                query.print(modes.output());
            }
        }
        _ => query.print(modes.output()),
    }

    Ok(())
}

pub async fn scores_for_students(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "/";
    }

    let mut fields = tools::split_sep(args);
    let lastname = fields.next().unwrap_or(""); // sukunimi
    let firstname = fields.next().unwrap_or(""); // etunimi
    let group = fields.next().unwrap_or(""); // ryhmä
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let query = ScoresForStudents::query(db, lastname, firstname, group, desc)
        .await?
        .has_data()?;

    match query.count() {
        0 => panic!(),
        1 => {
            if modes.is_interactive() {
                query.copy_to(editable);
                query.print_numbered(modes.output());
                editable.print_fields(&["Arvosana(As)", "Lisätiedot"]);
            } else {
                query.print(modes.output());
            }
        }
        _ => query.print(modes.output()),
    }

    Ok(())
}

pub async fn scores_for_group(
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

    let query = ScoresForGroup::query(db, group).await?.has_data()?;
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
        EditableItem::Assignments(_) => todo!(),
        EditableItem::Scores(scores) => {
            edit_scores(&mut ta, indexes, scores, fields).await?;
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
            EditableItem::Scores(_) => 2,
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
            "Syötä kentän {f} arvot riveittäin. Pelkkä välilyönti poistaa kentän arvon\n\
             (paitsi eräitä pakollisia). Tyhjä rivi jättää kentän ennalleen. Ctrl-d lopettaa.\n\
             Tietueet:",
            f = field_num,
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
            EditableItem::Assignments(_) => todo!(),
            EditableItem::Scores(scores) => {
                edit_scores(&mut ta, index, scores, fields).await?;
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

    if groups.is_some() {
        for g in tools::words_iter(groups.unwrap()) {
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

        if lastname.is_some() {
            student
                .update_lastname(db, lastname.as_ref().unwrap())
                .await?;
        }

        if firstname.is_some() {
            student
                .update_firstname(db, firstname.as_ref().unwrap())
                .await?;
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

            if student.only_one_group(db).await? {
                Err("Oppilaan pitää kuulua vähintään yhteen ryhmään.")?;
            } else {
                student.remove_from_group(db, rid).await?;
            }
        }

        if desc.is_some() {
            student
                .update_description(db, desc.as_ref().unwrap())
                .await?;
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
    let mut name = fields.next().unwrap_or(""); // nimi
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let mut name_update = false;
    let mut desc_update = false;

    if name.is_empty() && desc.is_empty() {
        Err("Ei muokattavia kenttiä.")?;
    }

    if tools::has_content(name) {
        let (first, rest) = tools::split_first(name);
        if rest.is_empty() {
            if indexes.len() > 1 {
                Err("Usealle ryhmälle ei voi antaa samaa nimeä.")?;
            }
            name = first;
            name_update = true;
        } else {
            Err(format!(
                "Ryhmätunnus ”{name}” ei kelpaa: pitää olla yksi sana."
            ))?;
        }
    }

    if !desc.is_empty() {
        desc_update = true;
    }
    let desc = tools::normalize_str(desc);

    for i in indexes {
        let group = match groups.get(i - 1) {
            None => Err("Muokattavia ryhmiä ei ole.")?,
            Some(g) => g,
        };
        if name_update {
            group.update_name(db, name).await?;
        }
        if desc_update {
            group.update_description(db, &desc).await?;
        }
    }
    Ok(())
}

async fn edit_scores(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    scores: &[Score],
    mut fields: impl Iterator<Item = &str>,
) -> Result<(), Box<dyn Error>> {
    let score = fields.next().unwrap_or(""); // arvosana
    let desc = fields.next().unwrap_or(""); // lisätiedot

    let mut score_update = false;
    let mut desc_update = false;

    if score.is_empty() && desc.is_empty() {
        Err("Ei muokattavia kenttiä.")?;
    }

    if !score.is_empty() {
        score_update = true;
    }
    let new_score = tools::normalize_str(score);

    if !desc.is_empty() {
        desc_update = true;
    }
    let new_desc = tools::normalize_str(desc);

    for i in indexes {
        let score = match scores.get(i - 1) {
            None => Err("Muokattavia arvosanoja ei ole.")?,
            Some(v) => v,
        };

        if score_update && desc_update && new_score.is_empty() && new_desc.is_empty() {
            score.delete(db).await?;
            continue;
        }
        if score_update {
            score.update_score(db, &new_score).await?;
        }
        if desc_update {
            score.update_description(db, &new_desc).await?;
        }
    }
    Ok(())
}

pub async fn convert_to_score(
    db: &mut PgConnection,
    editable: &mut Editable,
    args: &str,
) -> Result<(), Box<dyn Error>> {
    if editable.is_none() {
        Err("Edellinen komento ei sisällä muokattavia tietueita.")?;
    }

    if !editable.is_score() {
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
        EditableItem::Scores(scores) => {
            for i in indexes {
                let score = match scores.get(i - 1) {
                    Some(v) => v,
                    None => Err("Ei muokattavia tietueita.")?,
                };

                if score.score.is_none() {
                    continue;
                }

                if let Some(old) = tools::parse_number(score.score.as_ref().unwrap()) {
                    if let Some(new) = tools::float_to_score(old) {
                        score.update_score(&mut ta, &new).await?;
                    }
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

    if !editable.is_score() {
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
        EditableItem::Scores(scores) => {
            for i in indexes {
                let score = match scores.get(i - 1) {
                    Some(v) => v,
                    None => Err("Ei muokattavia tietueita.")?,
                };

                if score.score.is_none() {
                    continue;
                }

                if let Some(old) = tools::parse_number(score.score.as_ref().unwrap()) {
                    let new = tools::format_decimal(old);
                    score.update_score(&mut ta, &new).await?;
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
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "/";
    }

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

    for g in tools::words_iter(groups.unwrap()) {
        let rid = Group::get_or_insert(&mut ta, g).await?;
        student.add_to_group(&mut ta, rid).await?;
    }

    ta.commit().await?;
    Ok(())
}

pub async fn insert_assignment(
    db: &mut PgConnection,
    editable: &mut Editable,
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    editable.clear();
    if args.is_empty() {
        args = "/";
    }

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

    let weight =
        match weight {
            Some(s) => Some(tools::parse_positive_int(s).map_err(|e| {
                format!("Painokertoimen ”{e}” täytyy olla positiivinen kokonaisluku.")
            })?),
            None => None,
        };

    let position = match position {
        Some(s) => tools::parse_positive_int(s)
            .map_err(|e| format!("Sijan ”{e}” täytyy olla positiivinen kokonaisluku."))?,
        None => i32::MAX,
    };

    let mut ta = db.begin().await?;

    for g in tools::words_iter(groups.unwrap()) {
        let group_assignment = Assignment {
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
        EditableItem::Scores(scores) => {
            delete_scores(&mut ta, indexes, scores).await?;
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

async fn delete_scores(
    db: &mut PgConnection,
    indexes: Vec<usize>,
    scores: &[Score],
) -> Result<(), Box<dyn Error>> {
    for i in indexes {
        let score = match scores.get(i - 1) {
            None => Err("Poistettavia arvosanoja ei ole.")?,
            Some(v) => v,
        };
        score.delete(db).await?;
    }
    Ok(())
}

pub fn help_interactive(editable: &mut Editable, _args: &str) {
    editable.clear();
    help();
}

pub fn help() {
    println!("{}", include_str!("../help/help.txt"));
}
