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
pub async fn insert_student(
    db: &mut DBase,
    lastname: &str,
    firstname: &str,
    groups: impl IntoIterator<Item = &str>,
    description: &str,
) -> Result<()> {
    let lastname = lastname.normalize(); // sukunimi
    let firstname = firstname.normalize(); // etunimi
    let groups: Vec<String> = groups.into_iter().filter_map(|x| x.normalize()).collect(); // ryhmät
    let description = description.normalize(); // lisätiedot

    if lastname.is_none() || firstname.is_none() || groups.is_empty() {
        return Err("Pitää antaa vähintään sukunimi, etunimi ja ryhmä.".into());
    }

    tools::assert_group_names(&groups)?;

    let mut ta = db.begin().await?;

    if let Some(last) = lastname
        && let Some(first) = firstname
    {
        let mut student = Student {
            lastname: last,
            firstname: first,
            description: description.unwrap_or_default(),
            ..Default::default()
        };

        student.insert(&mut ta).await?;

        for group in groups {
            let rid = Group::get_or_insert(&mut ta, &group).await?;
            student.add_to_group(&mut ta, rid).await?;
        }
    } else {
        return Err("Oppilaan lisääminen epäonnistui.".into());
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

pub struct UpdateStudent<'a> {
    pub student: &'a Student,
    field: UpdateStudentField,
}

pub struct DeleteStudent<'a> {
    pub student: &'a Student,
}

enum UpdateStudentField {
    Lastname(String),
    Firstname(String),
    GroupAdd(String),
    GroupRemove(String),
    Description(String),
    DescriptionClear,
}

impl Student {
    /// Prepare update for student's lastname.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_lastname<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton sukunimi: ”{name}”.").into()),
            Some(n) => Ok(UpdateStudent {
                student: self,
                field: UpdateStudentField::Lastname(n),
            }),
        }
    }

    /// Prepare update for student's firstname.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_firstname<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton etunimi: ”{name}”.").into()),
            Some(n) => Ok(UpdateStudent {
                student: self,
                field: UpdateStudentField::Firstname(n),
            }),
        }
    }

    /// Prepare addition for student's groups.
    ///
    /// See [`Commit`] trait for more information.
    pub fn add_group<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton ryhmätunnus: ”{name}”.").into()),
            Some(n) => {
                n.is_valid_group_name()?;
                Ok(UpdateStudent {
                    student: self,
                    field: UpdateStudentField::GroupAdd(n),
                })
            }
        }
    }

    /// Prepare removal for student's groups.
    ///
    /// See [`Commit`] trait for more information.
    pub fn remove_group<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton ryhmätunnus: ”{name}”.").into()),
            Some(n) => {
                n.is_valid_group_name()?;
                Ok(UpdateStudent {
                    student: self,
                    field: UpdateStudentField::GroupRemove(n),
                })
            }
        }
    }

    /// Prepare update for student's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: &str) -> Result<UpdateStudent<'a>> {
        match desc.normalize() {
            None => Err(format!("Sopimaton oppilaan kuvaus: ”{desc}”.").into()),
            Some(d) => Ok(UpdateStudent {
                student: self,
                field: UpdateStudentField::Description(d),
            }),
        }
    }

    /// Prepare to clear student's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateStudent<'a> {
        UpdateStudent {
            student: self,
            field: UpdateStudentField::DescriptionClear,
        }
    }

    /// Prepare deletion of student.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> DeleteStudent<'a> {
        DeleteStudent { student: self }
    }
}

impl Commit for UpdateStudent<'_> {
    async fn commit(&self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;

        match &self.field {
            UpdateStudentField::Lastname(last) => {
                self.student.update_lastname(&mut ta, last).await?
            }

            UpdateStudentField::Firstname(first) => {
                self.student.update_firstname(&mut ta, first).await?
            }

            UpdateStudentField::GroupAdd(name) => {
                let rid = Group::get_or_insert(&mut ta, name).await?;
                if !self.student.in_group(&mut ta, rid).await? {
                    self.student.add_to_group(&mut ta, rid).await?;
                }
            }

            UpdateStudentField::GroupRemove(name) => {
                let Some(rid) = Group::get_id(&mut ta, name).await? else {
                    return Ok(()); // No such group.
                };

                if !self.student.in_group(&mut ta, rid).await? {
                    return Ok(());
                }

                let count = self.student.count_grades_group(&mut ta, rid).await?;
                if count > 0 {
                    return Err(format!(
                        "Oppilaalle ”{l}, {f}” on ryhmässä ”{g}” kirjattu {c} arvosana(a).\n\
                         Säilytetään ryhmät ja perutaan toiminto.",
                        l = self.student.lastname,
                        f = self.student.firstname,
                        c = count,
                        g = name,
                    )
                    .into());
                }

                if self.student.only_one_group(&mut ta).await? {
                    return Err("Oppilaan pitää kuulua vähintään yhteen ryhmään.".into());
                } else {
                    self.student.remove_from_group(&mut ta, rid).await?;
                }

                Group::delete_empty(&mut ta).await?;
            }

            UpdateStudentField::Description(desc) => {
                self.student.update_description(&mut ta, desc).await?
            }

            UpdateStudentField::DescriptionClear => {
                self.student.update_description(&mut ta, "").await?
            }
        }

        ta.commit().await?;
        Ok(())
    }
}

impl Commit for DeleteStudent<'_> {
    async fn commit(&self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;

        let count = self.student.count_grades(&mut ta).await?;
        if count > 0 {
            return Err(format!(
                "Oppilaalle ”{l}, {f}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                l = self.student.lastname,
                f = self.student.firstname,
                c = count
            )
            .into());
        }

        self.student.delete(&mut ta).await?;
        Group::delete_empty(&mut ta).await?;
        ta.commit().await?;
        Ok(())
    }
}
