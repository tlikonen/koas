mod assignments;
mod grades;
mod groups;
mod students;

pub use assignments::*;
pub use grades::*;
pub use groups::*;
pub use students::*;

use crate::prelude::*;

pub async fn stats(modes: &Modes, db: &mut DBase, editable: &mut Editable) -> Result<()> {
    editable.clear();

    Stats::query(db).await?.print(modes.output())?;
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
