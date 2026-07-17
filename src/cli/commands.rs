use crate::Modes;
use crate::Output;
use koas::database::*;
use koas::tools::StrExt;
use koas::*;
use std::io::{self, Write as _};

pub(super) async fn edit_students(
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
        parse_add_remove_groups(groups, &mut groups_add, &mut groups_remove)?;
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

fn parse_add_remove_groups(
    input: &str,
    groups_add: &mut Vec<String>,
    groups_remove: &mut Vec<String>,
) -> Result<()> {
    for g in input.split_whitespace() {
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
    Ok(())
}

pub(super) async fn edit_groups(
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

pub(super) async fn edit_assignments(
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

pub(super) async fn edit_grades(
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

pub(super) fn read_values(indices: &[usize]) -> Result<String> {
    use rustyline::error::ReadlineError;

    let mut rl = rustyline::DefaultEditor::new()?;
    let mut buffer = String::with_capacity(50);

    let width = {
        let max = indices.iter().max().ok_or("Ei tietoja.")?;
        number_width(*max)
    };

    loop {
        let i = buffer.lines().count();

        if i >= indices.len() {
            break;
        }

        match rl.readline(&format!("{n:w$}: ", n = indices[i], w = width)) {
            Ok(s) => {
                buffer.push_str(&s);
                buffer.push('\n');
                for line in s.lines() {
                    rl.add_history_entry(line)?;
                }
            }

            Err(err) => match err {
                ReadlineError::Eof => {
                    writeln!(
                        io::stdout(),
                        "Keskeytetty. Tähänastiset muutokset tallennetaan."
                    )?;
                    break;
                }
                ReadlineError::Interrupted => Err("Keskeytetty. Kaikki muutokset perutaan.")?,
                _ => Err(err)?,
            },
        }
    }

    Ok(buffer)
}

fn number_width(mut number: usize) -> usize {
    let mut width = 1;
    while number / 10 > 0 {
        width += 1;
        number /= 10;
    }
    width
}

pub(super) async fn edit_student_series(
    db: &mut PgConnection,
    students: impl Iterator<Item = &Student>,
    field_num: usize,
    values: impl Iterator<Item = &str>,
) -> Result<()> {
    let mut updates = Queue::new();

    for (student, value) in students.zip(values) {
        if value.is_empty() {
            continue;
        }

        match field_num {
            1 => {
                // sukunimi
                if value.has_content() {
                    student.set_lastname(value)?.queue(&mut updates);
                }
            }

            2 => {
                // etunimi
                if value.has_content() {
                    student.set_firstname(value)?.queue(&mut updates);
                }
            }

            3 => {
                // ryhmät
                if value.has_content() {
                    let mut groups_add = Vec::with_capacity(3);
                    let mut groups_remove = Vec::with_capacity(1);
                    parse_add_remove_groups(value, &mut groups_add, &mut groups_remove)?;

                    for name in &groups_add {
                        student.add_group(name)?.queue(&mut updates);
                    }

                    for name in &groups_remove {
                        student.remove_group(name)?.queue(&mut updates);
                    }
                }
            }

            4 => {
                // lisätiedot
                if value.has_content() {
                    student.set_description(value)?.queue(&mut updates);
                } else {
                    student.clear_description().queue(&mut updates);
                }
            }

            _ => return Err("Kentän mumeron täytyy olla kokonaisluku 1–4.".into()),
        }
    }

    updates.commit(db).await?;
    Ok(())
}

pub(super) async fn edit_group_series(
    db: &mut PgConnection,
    groups: impl Iterator<Item = &Group>,
    field_num: usize,
    values: impl Iterator<Item = &str>,
) -> Result<()> {
    let mut updates = Queue::new();

    for (group, value) in groups.zip(values) {
        if value.is_empty() {
            continue;
        }

        match field_num {
            1 => {
                // ryhmä
                if value.has_content() {
                    group.set_name(value)?.queue(&mut updates);
                }
            }

            2 => {
                // lisätiedot
                if value.has_content() {
                    group.set_description(value)?.queue(&mut updates);
                } else {
                    group.clear_description().queue(&mut updates);
                }
            }

            _ => Err("Kentän mumeron täytyy olla kokonaisluku 1–2.")?,
        }
    }

    updates.commit(db).await?;
    Ok(())
}

pub(super) async fn edit_assignment_series(
    db: &mut PgConnection,
    assignments: impl Iterator<Item = &Assignment>,
    field_num: usize,
    values: impl Iterator<Item = &str>,
) -> Result<()> {
    let mut updates = Queue::new();

    for (assignment, value) in assignments.zip(values) {
        if value.is_empty() {
            continue;
        }

        match field_num {
            1 => {
                // suoritus
                if value.has_content() {
                    assignment.set_name(value)?.queue(&mut updates);
                }
            }

            2 => {
                // lyhenne
                if value.has_content() {
                    assignment.set_short(value)?.queue(&mut updates);
                }
            }

            3 => {
                // painokerroin
                if value.has_content() {
                    assignment.set_weight(value)?.queue(&mut updates);
                } else {
                    assignment.clear_weight().queue(&mut updates);
                }
            }

            4 => {
                // sija
                if value.has_content() {
                    assignment.set_position(value)?.queue(&mut updates);
                }
            }

            _ => Err("Kentän mumeron täytyy olla kokonaisluku 1–4.")?,
        }
    }

    updates.commit(db).await?;
    Ok(())
}

pub(super) async fn edit_grade_series(
    db: &mut PgConnection,
    grades: impl Iterator<Item = &Grade>,
    field_num: usize,
    values: impl Iterator<Item = &str>,
) -> Result<()> {
    let mut updates = Queue::new();

    for (grade, value) in grades.zip(values) {
        if value.is_empty() {
            continue;
        }

        match field_num {
            1 => {
                // arvosana
                if value.has_content() {
                    grade.set_grade(value)?.queue(&mut updates);
                } else {
                    grade.clear_grade().queue(&mut updates);
                }
            }

            2 => {
                // lisätiedot
                if value.has_content() {
                    grade.set_description(value)?.queue(&mut updates);
                } else {
                    grade.clear_description().queue(&mut updates);
                }
            }

            _ => Err("Kentän mumeron täytyy olla kokonaisluku 1–2.")?,
        }
    }

    updates.commit(db).await?;
    Ok(())
}

pub(super) fn table_format(modes: &mut Modes, args: &str) -> Result<()> {
    let (first, _) = tools::split_first(args);
    if first.is_empty() {
        return Err("Anna argumentiksi taulukkotyyli. Apua saa ?:llä.".into());
    }

    let new = Output::select(first)?;
    modes.set_output(new);
    Ok(())
}

pub(super) fn help(topic: &str) -> Result<()> {
    static HO: &str = include_str!("help/command-ho.txt");
    static HR: &str = include_str!("help/command-hr.txt");
    static HS: &str = include_str!("help/command-hs.txt");
    static HAS: &str = include_str!("help/command-has.txt");
    static HAO: &str = include_str!("help/command-hao.txt");
    static HAK: &str = include_str!("help/command-hak.txt");

    static M: &str = include_str!("help/command-m.txt");
    static MS: &str = include_str!("help/command-ms.txt");
    static MA: &str = include_str!("help/command-ma.txt");
    static MD: &str = include_str!("help/command-md.txt");
    static POISTA: &str = include_str!("help/command-poista.txt");

    static LO: &str = include_str!("help/command-lo.txt");
    static LS: &str = include_str!("help/command-ls.txt");

    static TP: &str = include_str!("help/command-tp.txt");
    static TJ: &str = include_str!("help/command-tj.txt");
    static TK: &str = include_str!("help/command-tk.txt");
    static TLK: &str = include_str!("help/command-tlk.txt");

    static QM: &str = include_str!("help/command-qm.txt");
    static QUICK: &str = include_str!("help/quick.txt");

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
            info = include_str!("help/command.txt")
        )?,
        "tietokanta" => writeln!(stdout, "\n{}", include_str!("../database.txt"))?,
        "asetukset" => writeln!(stdout, "\n{}", include_str!("../settings.txt"))?,

        u => return Err(format!("Tuntematon ohjeiden aihe: ”{u}”.").into()),
    }
    Ok(())
}
