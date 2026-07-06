use crate::prelude::*;

/// Query for students.
///
/// Wildcard character "*" is allowed in the query arguments and is
/// implicit in the start and end the strings.
pub async fn students(
    db: &mut DBase,
    lastname: &str,
    firstname: &str,
    group: &str,
    description: &str,
) -> Result<QueryList<Student>> {
    Student::query(db, lastname, firstname, group, description).await
}

/// Insert new student.
///
/// The `groups` argument is one ore more whitespace-separated group
/// names.
pub async fn insert_student(
    db: &mut DBase,
    lastname: &str,
    firstname: &str,
    groups: &str,
    description: &str,
) -> Result<()> {
    let lastname = Some(lastname)
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // sukunimi

    let firstname = Some(firstname)
        .filter(|x| tools::has_content(x))
        .map(tools::normalize_str); // etunimi

    let groups = Some(groups).filter(|x| tools::has_content(x)); // ryhmät
    let description = tools::normalize_str(description); // lisätiedot

    if lastname.is_none() || firstname.is_none() || groups.is_none() {
        return Err("Pitää antaa vähintään sukunimi, etunimi ja ryhmä.".into());
    }

    let mut student = Student {
        lastname: lastname.unwrap(),
        firstname: firstname.unwrap(),
        description,
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

impl Edit for EditItems<'_, Student> {
    async fn edit(&self, db: &mut DBase) -> Result<()> {
        let lastname = self.field(0); // sukunimi
        let firstname = self.field(1); // etunimi
        let groups = self.field(2); // ryhmät
        let desc = self.field(3); // lisätiedot

        if !lastname.has_value() && !firstname.has_value() && !groups.has_value() && desc.is_none()
        {
            return Err("Anna muokattavia kenttiä.".into());
        }

        if (lastname.has_value() || firstname.has_value()) && self.count() > 1 {
            return Err("Usealle henkilölle ei voi muuttaa kerralla samaa nimeä.\n\
                        Muuta yksi kerrallaan, jos se on tarkoituksena."
                .into());
        }

        let mut groups_add: Vec<String> = Vec::with_capacity(3);
        let mut groups_remove: Vec<String> = Vec::with_capacity(1);

        if let Field::Set(groups) = groups {
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

            if groups_add.iter().any(|s| s.is_empty()) || groups_remove.iter().any(|s| s.is_empty())
            {
                return Err(
                    "Ryhmän nimiä puuttuu. Kirjoita merkin ”+” tai ”-” jälkeen ryhmätunnus.".into(),
                );
            }
        }

        for student in self.iter() {
            if let Field::Set(last) = lastname {
                student.update_lastname(db, last).await?;
            }

            if let Field::Set(first) = firstname {
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
                    return Err(format!(
                        "Oppilaalle ”{l}, {f}” on ryhmässä ”{g}” kirjattu {c} arvosana(a).\n\
                         Säilytetään ryhmät ja perutaan toiminto.",
                        l = student.lastname,
                        f = student.firstname,
                        c = count,
                        g = name,
                    )
                    .into());
                }

                if student.only_one_group(db).await? {
                    return Err("Oppilaan pitää kuulua vähintään yhteen ryhmään.".into());
                } else {
                    student.remove_from_group(db, rid).await?;
                }
            }

            match desc {
                Field::Set(d) => student.update_description(db, d).await?,
                Field::Clear => student.update_description(db, "").await?,
                Field::Ignore => (),
            }
        }

        Group::delete_empty(db).await?;
        Ok(())
    }
}

impl Delete for DeleteItems<'_, Student> {
    async fn delete(&self, db: &mut DBase) -> Result<()> {
        for student in self.iter() {
            let count = student.count_grades(db).await?;
            if count > 0 {
                return Err(format!(
                    "Oppilaalle ”{l}, {f}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                    l = student.lastname,
                    f = student.firstname,
                    c = count
                )
                .into());
            }

            student.delete(db).await?;
        }
        Group::delete_empty(db).await?;
        Ok(())
    }
}
