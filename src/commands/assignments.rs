use super::*;

/// Query for assignments.
///
/// Wildcard character "*" is allowed in the `group` argument.
pub async fn assignments(db: &mut DBase, group: &str) -> Result<QueryList<AssignmentsForGroup>> {
    if group.is_empty() {
        return Err("Argumentiksi pitää antaa ryhmän nimi.".into());
    }
    AssignmentsForGroup::query(db, group).await
}

/// Insert new assignment for a group.
///
/// TODO: Korvataan funktiolla Assignment::new, joka valmistelee
/// InsertAssignmentin, joka viimeistellään commitilla tai lisätään
/// jonoon.
pub async fn insert_assignment(
    db: &mut DBase,
    groups: impl IntoIterator<Item = &str>,
    assignment: &str,
    assignment_short: &str,
    weight: Option<&str>,
    position: Option<&str>,
) -> Result<()> {
    let groups: Vec<String> = groups.into_iter().filter_map(|x| x.normalize()).collect(); // ryhmät
    let assignment = assignment.normalize(); // suoritus
    let assignment_short = assignment_short.normalize(); // lyhenne
    let weight = weight.filter(|x| x.has_content()); // painokerroin
    let position = position.filter(|x| x.has_content()); // sija

    if groups.is_empty() || assignment.is_none() || assignment_short.is_none() {
        return Err("Pitää antaa vähintään ryhmä, suorituksen nimi ja lyhenne.".into());
    }

    tools::assert_group_names(&groups)?;

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

    if let Some(long) = assignment
        && let Some(short) = assignment_short
    {
        for group in groups {
            let mut group_assignment = Assignment {
                rid: Group::get_or_insert(&mut ta, &group).await?,
                assignment: long.clone(),
                assignment_short: short.clone(),
                weight,
                ..Default::default()
            };

            group_assignment.insert(&mut ta, position).await?;
        }
    } else {
        return Err("Suorituksen lisääminen epäonnistui.".into());
    }

    ta.commit().await?;
    Ok(())
}

impl DeprecatedEdit for DeprecatedEditItems<'_, Assignment> {
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
            DeprecatedField::Set(s) => match s.trim().parse::<i32>() {
                Ok(n) if n >= 1 => DeprecatedField::Set(n),
                _ => {
                    return Err(
                        "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).".into(),
                    );
                }
            },
            DeprecatedField::Clear => DeprecatedField::Clear,
            DeprecatedField::Ignore => DeprecatedField::Ignore,
        };

        // Convert from &Field<String> to Field<i32>.
        let position = match position {
            DeprecatedField::Set(s) => match s.trim().parse::<i32>() {
                Ok(n) => DeprecatedField::Set(n),
                _ => return Err("Järjestysnumeron täytyy olla kokonaisluku.".into()),
            },
            DeprecatedField::Clear | DeprecatedField::Ignore => DeprecatedField::Ignore,
        };

        for group_assignment in self.iter() {
            if let DeprecatedField::Set(n) = name {
                group_assignment.update_name(db, n).await?;
            }

            if let DeprecatedField::Set(s) = short {
                group_assignment.update_short(db, s).await?;
            }

            match weight {
                DeprecatedField::Set(w) => group_assignment.update_weight(db, Some(w)).await?,
                DeprecatedField::Clear => group_assignment.update_weight(db, None).await?,
                DeprecatedField::Ignore => (),
            }

            if let DeprecatedField::Set(p) = position {
                group_assignment.update_position(db, p).await?;
            }
        }
        Ok(())
    }
}

impl Assignment {
    /// Prepare update for assignment's name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_name<'a>(&'a self, name: &str) -> Result<UpdateAssignment<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton suorituksen nimi: ”{name}”.").into()),
            Some(n) => Ok(UpdateAssignment {
                item: self,
                operation: UpdateAssignmentOp::Name(n),
            }),
        }
    }

    /// Prepare update for assignment's short name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_short<'a>(&'a self, name: &str) -> Result<UpdateAssignment<'a>> {
        match name.normalize() {
            None => Err(format!("Sopimaton suorituksen lyhenne: ”{name}”.").into()),
            Some(n) => Ok(UpdateAssignment {
                item: self,
                operation: UpdateAssignmentOp::Short(n),
            }),
        }
    }

    /// Prepare update for assignment's weight.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_weight<'a>(&'a self, number: &str) -> Result<UpdateAssignment<'a>> {
        match number.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Ok(UpdateAssignment {
                item: self,
                operation: UpdateAssignmentOp::Weight(n),
            }),

            _ => Err("Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).".into()),
        }
    }

    /// Prepare to clear assignment's weight.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_weight<'a>(&'a self) -> UpdateAssignment<'a> {
        UpdateAssignment {
            item: self,
            operation: UpdateAssignmentOp::WeightClear,
        }
    }

    /// Prepare update for assignment's position.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_position<'a>(&'a self, number: &str) -> Result<UpdateAssignment<'a>> {
        match number.trim().parse::<i32>() {
            Ok(n) => Ok(UpdateAssignment {
                item: self,
                operation: UpdateAssignmentOp::Position(n),
            }),

            _ => Err("Järjestysnumeron täytyy olla kokonaisluku.".into()),
        }
    }

    /// Prepare deletion of assignment.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateAssignment<'a> {
        UpdateAssignment {
            item: self,
            operation: UpdateAssignmentOp::Delete,
        }
    }
}

impl<'a> ToQueue<'a> for UpdateAssignment<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push(QueueItem::UpdateAssignment(self));
    }
}

impl Commit for UpdateAssignment<'_> {
    async fn commit(&self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        let assignment = self.item;

        match &self.operation {
            UpdateAssignmentOp::Name(name) => assignment.update_name(&mut ta, name).await?,
            UpdateAssignmentOp::Short(short) => assignment.update_short(&mut ta, short).await?,
            UpdateAssignmentOp::Weight(weight) => {
                assignment.update_weight(&mut ta, Some(*weight)).await?
            }
            UpdateAssignmentOp::WeightClear => assignment.update_weight(&mut ta, None).await?,
            UpdateAssignmentOp::Position(pos) => assignment.update_position(&mut ta, *pos).await?,

            UpdateAssignmentOp::Delete => {
                let count = assignment.count_grades(&mut ta).await?;
                if count > 0 {
                    return Err(format!(
                        "Suoritukselle ”{a}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                        a = assignment.assignment,
                        c = count,
                    )
                    .into());
                }

                assignment.delete(&mut ta).await?;
                Assignment::reposition(&mut ta, assignment.rid).await?;
                Group::delete_empty(&mut ta).await?;
            }
        }

        ta.commit().await?;
        Ok(())
    }
}
