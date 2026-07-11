use super::*;
use std::io;
use std::io::Write as _;

pub async fn deprecated_edit(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
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
    match editable {
        Editable::Students(students) => {
            students.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        Editable::Groups(groups) => {
            groups.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        Editable::Assignments(assignments) => {
            assignments.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        Editable::Grades(grades) => {
            grades.for_edit(indexes, fields).edit(&mut ta).await?;
        }
        Editable::None => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn deprecated_edit_series(
    db: &mut DBase,
    editable: &mut Editable,
    args: &str,
) -> Result<()> {
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
        let field_num_max = match editable {
            Editable::Students(_) => 4,    // sukunimi, etunimi, ryhmät, lisätiedot
            Editable::Groups(_) => 2,      // ryhmä, lisätiedot
            Editable::Assignments(_) => 4, // suoritus, lyhenne, painokerroin, järjestys
            Editable::Grades(_) => 2,      // arvosana, lisätiedot
            Editable::None => panic!(),
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
    if rest.has_content() {
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

        match editable {
            Editable::Students(students) => {
                students.for_edit(index, fields).edit(&mut ta).await?;
            }
            Editable::Groups(groups) => {
                groups.for_edit(index, fields).edit(&mut ta).await?;
            }
            Editable::Assignments(assignments) => {
                assignments.for_edit(index, fields).edit(&mut ta).await?;
            }
            Editable::Grades(grades) => {
                grades.for_edit(index, fields).edit(&mut ta).await?;
            }
            Editable::None => panic!(),
        }
    }

    ta.commit().await?;
    Ok(())
}

pub async fn deprecated_delete(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
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
    match editable {
        Editable::Students(students) => {
            students.for_delete(indexes).delete(&mut ta).await?;
        }
        Editable::Groups(_) => {
            return Err("Ryhmiä ei voi poistaa näin. Ryhmä poistuu itsestään,\n\
                        kun siltä poistaa kaikki oppilaat ja suoritukset."
                .into());
        }
        Editable::Assignments(assignments) => {
            assignments.for_delete(indexes).delete(&mut ta).await?;
        }
        Editable::Grades(grades) => {
            grades.for_delete(indexes).delete(&mut ta).await?;
        }
        Editable::None => panic!(),
    }
    ta.commit().await?;
    Ok(())
}
