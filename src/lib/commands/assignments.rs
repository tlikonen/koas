use super::*;

impl Assignment {
    /// Prepare to insert a new assignment.
    ///
    /// See [`Commit`] trait for more information.
    pub fn insert(
        group: &str,
        assignment: &str,
        assignment_short: &str,
        weight: Option<&str>,
        position: Option<&str>,
    ) -> Result<InsertAssignment> {
        let group = group.normalize(); // ryhmä
        let assignment = assignment.normalize(); // suoritus
        let assignment_short = assignment_short.normalize(); // lyhenne
        let weight = weight.filter(|x| x.has_content()); // painokerroin
        let position = position.filter(|x| x.has_content()); // sija

        if group.is_none() || assignment.is_none() || assignment_short.is_none() {
            return Err(Error::from(
                "Pitää antaa vähintään ryhmä, suorituksen nimi ja lyhenne.",
            ));
        }

        // Convert from Option<&str> to Option<i32>.
        let weight = match weight {
            Some(s) => match s.trim().parse::<i32>() {
                Ok(n) if n >= 1 => Some(n),
                _ => {
                    return Err(Error::from(
                        "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).",
                    ));
                }
            },
            None => None,
        };

        // Convert from Option<&str> to Option<i32>.
        let position = match position {
            Some(s) => match s.trim().parse::<i32>() {
                Ok(n) => Some(n),
                _ => return Err(Error::from("Järjestysnumeron täytyy olla kokonaisluku.")),
            },
            None => None,
        };

        if let Some(gr) = group
            && let Some(long) = assignment
            && let Some(short) = assignment_short
        {
            gr.is_valid_group_name()?;
            Ok(InsertAssignment {
                group: gr,
                assignment: long,
                assignment_short: short,
                weight,
                position,
            })
        } else {
            Err(Error::from("Suorituksen lisääminen epäonnistui."))
        }
    }

    /// Prepare update for assignment's name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_name<'a>(&'a self, name: &str) -> Result<UpdateAssignment<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!(
                "Sopimaton suorituksen nimi: ”{name}”."
            ))),
            Some(n) => Ok(Update::new(self, UpdateAssignmentOp::Name(n))),
        }
    }

    /// Prepare update for assignment's short name.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_short<'a>(&'a self, name: &str) -> Result<UpdateAssignment<'a>> {
        match name.normalize() {
            None => Err(Error::from(format!(
                "Sopimaton suorituksen lyhenne: ”{name}”."
            ))),
            Some(n) => Ok(Update::new(self, UpdateAssignmentOp::Short(n))),
        }
    }

    /// Prepare update for assignment's weight.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_weight<'a>(&'a self, number: &str) -> Result<UpdateAssignment<'a>> {
        match number.trim().parse::<i32>() {
            Ok(n) if n >= 1 => Ok(Update::new(self, UpdateAssignmentOp::Weight(n))),
            _ => Err(Error::from(
                "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).",
            )),
        }
    }

    /// Prepare to clear assignment's weight.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_weight<'a>(&'a self) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::WeightClear)
    }

    /// Prepare update for assignment's position.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_position<'a>(&'a self, number: &str) -> Result<UpdateAssignment<'a>> {
        match number.trim().parse::<i32>() {
            Ok(n) => Ok(Update::new(self, UpdateAssignmentOp::Position(n))),
            _ => Err(Error::from("Järjestysnumeron täytyy olla kokonaisluku.")),
        }
    }

    /// Prepare deletion of assignment.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateAssignment<'a> {
        Update::new(self, UpdateAssignmentOp::Delete)
    }
}

impl<'a> ToQueue<'a> for UpdateAssignment<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::UpdateAssignment(self));
    }
}

impl Commit for UpdateAssignment<'_> {
    async fn commit(&mut self, db: &mut DBase) -> Result<()> {
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
                    return Err(Error::from(format!(
                        "Suoritukselle ”{a}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                        a = assignment.assignment,
                        c = count,
                    )));
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

impl<'a> ToQueue<'a> for InsertAssignment {
    fn queue(self, q: &mut Queue<'a>) {
        q.push_back(QueueItem::InsertAssignment(self))
    }
}

impl Commit for InsertAssignment {
    async fn commit(&mut self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;

        let mut group_assignment = Assignment {
            rid: Group::get_or_insert(&mut ta, &self.group).await?,
            assignment: self.assignment.clone(),
            assignment_short: self.assignment_short.clone(),
            weight: self.weight,
            ..Assignment::default()
        };

        let pos = self.position.unwrap_or(i32::MAX);

        group_assignment.insert_db(&mut ta, pos).await?;

        ta.commit().await?;
        Ok(())
    }
}
