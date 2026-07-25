use super::*;

impl Student {
    /// Prepare to insert new student.
    ///
    /// See [`Commit`] trait for more information.
    pub fn insert<'a>(
        lastname: &str,
        firstname: &str,
        groups: impl IntoIterator<Item = &'a str>,
        description: &str,
    ) -> Result<InsertStudent> {
        let lastname = lastname.normalize(); // sukunimi
        let firstname = firstname.normalize(); // etunimi
        let groups: Vec<String> = groups.into_iter().filter_map(|x| x.normalize()).collect(); // ryhmät
        let description = description.normalize(); // lisätiedot

        if lastname.is_none() || firstname.is_none() || groups.is_empty() {
            return Err(Error::from(
                "Pitää antaa vähintään sukunimi, etunimi ja ryhmä.",
            ));
        }

        tools::assert_group_names(&groups)?;

        if let Some(last) = lastname
            && let Some(first) = firstname
        {
            Ok(InsertStudent {
                lastname: last,
                firstname: first,
                groups,
                description: description.unwrap_or_default(),
            })
        } else {
            Err(Error::from("Oppilaan lisääminen epäonnistui."))
        }
    }

    /// Prepare update for student's lastname.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_lastname<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!("Sopimaton sukunimi: ”{name}”."))),
            Some(n) => Ok(Update::new(self, UpdateStudentOp::Lastname(n))),
        }
    }

    /// Prepare update for student's firstname.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_firstname<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!("Sopimaton etunimi: ”{name}”."))),
            Some(n) => Ok(Update::new(self, UpdateStudentOp::Firstname(n))),
        }
    }

    /// Prepare addition for student's groups.
    ///
    /// See [`Commit`] trait for more information.
    pub fn add_group<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!("Sopimaton ryhmätunnus: ”{name}”."))),
            Some(n) => {
                n.is_valid_group_name()?;
                Ok(Update::new(self, UpdateStudentOp::GroupAdd(n)))
            }
        }
    }

    /// Prepare removal for student's groups.
    ///
    /// See [`Commit`] trait for more information.
    pub fn remove_group<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!("Sopimaton ryhmätunnus: ”{name}”."))),
            Some(n) => {
                n.is_valid_group_name()?;
                Ok(Update::new(self, UpdateStudentOp::GroupRemove(n)))
            }
        }
    }

    /// Prepare update for student's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: &str) -> Result<UpdateStudent<'a>> {
        match desc.normalize() {
            None => Err(Error::from(format!("Sopimaton oppilaan kuvaus: ”{desc}”."))),
            Some(d) => Ok(Update::new(self, UpdateStudentOp::Description(d))),
        }
    }

    /// Prepare to clear student's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::DescriptionClear)
    }

    /// Prepare deletion of student.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateStudent<'a> {
        Update::new(self, UpdateStudentOp::Delete)
    }
}

impl<'a> ToQueue<'a> for UpdateStudent<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::UpdateStudent(self));
    }
}

impl Commit for UpdateStudent<'_> {
    async fn commit(&mut self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        let student = self.item;

        match &self.operation {
            UpdateStudentOp::Lastname(last) => student.update_lastname(&mut ta, last).await?,

            UpdateStudentOp::Firstname(first) => student.update_firstname(&mut ta, first).await?,

            UpdateStudentOp::GroupAdd(name) => {
                let rid = Group::get_or_insert(&mut ta, name).await?;
                if !student.in_group(&mut ta, rid).await? {
                    student.add_to_group(&mut ta, rid).await?;
                }
            }

            UpdateStudentOp::GroupRemove(name) => {
                let Some(rid) = Group::get_id(&mut ta, name).await? else {
                    return Ok(()); // No such group.
                };

                if !student.in_group(&mut ta, rid).await? {
                    return Ok(());
                }

                let count = student.count_grades_group(&mut ta, rid).await?;
                if count > 0 {
                    return Err(Error::from(format!(
                        "Oppilaalle ”{l}, {f}” on ryhmässä ”{g}” kirjattu {c} arvosana(a).\n\
                         Säilytetään ryhmät ja perutaan toiminto.",
                        l = student.lastname,
                        f = student.firstname,
                        c = count,
                        g = name,
                    )));
                }

                if student.only_one_group(&mut ta).await? {
                    return Err(Error::from(
                        "Oppilaan pitää kuulua vähintään yhteen ryhmään.",
                    ));
                } else {
                    student.remove_from_group(&mut ta, rid).await?;
                }

                Group::delete_empty(&mut ta).await?;
            }

            UpdateStudentOp::Description(desc) => student.update_description(&mut ta, desc).await?,

            UpdateStudentOp::DescriptionClear => student.update_description(&mut ta, "").await?,

            UpdateStudentOp::Delete => {
                let count = student.count_grades(&mut ta).await?;
                if count > 0 {
                    return Err(Error::from(format!(
                        "Oppilaalle ”{l}, {f}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                        l = student.lastname,
                        f = student.firstname,
                        c = count
                    )));
                }

                student.delete(&mut ta).await?;
                Group::delete_empty(&mut ta).await?;
            }
        }

        ta.commit().await?;
        Ok(())
    }
}

impl<'a> ToQueue<'a> for InsertStudent {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::InsertStudent(self))
    }
}

impl Commit for InsertStudent {
    async fn commit(&mut self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;

        let mut student = Student {
            lastname: self.lastname.clone(),
            firstname: self.firstname.clone(),
            description: self.description.clone(),
            ..Student::default()
        };

        student.insert_db(&mut ta).await?;

        for group in &self.groups {
            let rid = Group::get_or_insert(&mut ta, group).await?;
            student.add_to_group(&mut ta, rid).await?;
        }

        ta.commit().await?;
        Ok(())
    }
}
