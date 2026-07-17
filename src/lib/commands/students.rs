use super::*;

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
/// TODO: Korvataan funktiolla Student::new, joka valmistelee
/// InsertStudentin, joka viimeistellään commitilla tai lisätään jonoon.
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

impl Student {
    /// Prepare update for student's lastname.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_lastname<'a>(&'a self, name: &str) -> Result<UpdateStudent<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton sukunimi: ”{name}”.").into()),
            Some(n) => Ok(UpdateStudent {
                item: self,
                operation: UpdateStudentOp::Lastname(n),
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
                item: self,
                operation: UpdateStudentOp::Firstname(n),
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
                    item: self,
                    operation: UpdateStudentOp::GroupAdd(n),
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
                    item: self,
                    operation: UpdateStudentOp::GroupRemove(n),
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
                item: self,
                operation: UpdateStudentOp::Description(d),
            }),
        }
    }

    /// Prepare to clear student's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateStudent<'a> {
        UpdateStudent {
            item: self,
            operation: UpdateStudentOp::DescriptionClear,
        }
    }

    /// Prepare deletion of student.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateStudent<'a> {
        UpdateStudent {
            item: self,
            operation: UpdateStudentOp::Delete,
        }
    }
}

impl<'a> ToQueue<'a> for UpdateStudent<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push(QueueItem::UpdateStudent(self));
    }
}

impl Commit for UpdateStudent<'_> {
    async fn commit(&self, db: &mut DBase) -> Result<()> {
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

                if student.only_one_group(&mut ta).await? {
                    return Err("Oppilaan pitää kuulua vähintään yhteen ryhmään.".into());
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
                    return Err(format!(
                        "Oppilaalle ”{l}, {f}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                        l = student.lastname,
                        f = student.firstname,
                        c = count
                    )
                    .into());
                }

                student.delete(&mut ta).await?;
                Group::delete_empty(&mut ta).await?;
            }
        }

        ta.commit().await?;
        Ok(())
    }
}
