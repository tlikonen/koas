use super::*;

impl Grade {
    /// Prepare update for grade.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_grade<'a>(&'a self, grade: &str) -> Result<UpdateGrade<'a>> {
        match grade.normalize() {
            None => Err(format!("Sopimaton arvosana: ”{grade}”.").into()),
            Some(g) => Ok(UpdateGrade {
                item: self,
                operation: UpdateGradeOp::Grade(g),
            }),
        }
    }

    /// Prepare to clear grade's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_grade<'a>(&'a self) -> UpdateGrade<'a> {
        UpdateGrade {
            item: self,
            operation: UpdateGradeOp::GradeClear,
        }
    }

    /// Prepare update for grade's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_description<'a>(&'a self, desc: &str) -> Result<UpdateGrade<'a>> {
        match desc.normalize() {
            None => Err(format!("Sopimaton arvosanan kuvaus: ”{desc}”.").into()),
            Some(d) => Ok(UpdateGrade {
                item: self,
                operation: UpdateGradeOp::Description(d),
            }),
        }
    }

    /// Prepare to clear grade's description.
    ///
    /// See [`Commit`] trait for more information.
    pub fn clear_description<'a>(&'a self) -> UpdateGrade<'a> {
        UpdateGrade {
            item: self,
            operation: UpdateGradeOp::DescriptionClear,
        }
    }

    /// Prepare deletion of grade.
    ///
    /// See [`Commit`] trait for more information.
    pub fn mark_deleted<'a>(&'a self) -> UpdateGrade<'a> {
        UpdateGrade {
            item: self,
            operation: UpdateGradeOp::Delete,
        }
    }
}

impl<'a> ToQueue<'a> for UpdateGrade<'a> {
    fn queue(self, q: &mut Queue<'a>) {
        q.push(QueueItem::UpdateGrade(self));
    }
}

impl Commit for UpdateGrade<'_> {
    async fn commit(&self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        let student_grade = self.item;

        match &self.operation {
            UpdateGradeOp::Grade(g) => student_grade.update_grade(&mut ta, Some(g)).await?,

            UpdateGradeOp::GradeClear => student_grade.update_grade(&mut ta, None).await?,

            UpdateGradeOp::Description(d) => {
                student_grade.update_description(&mut ta, Some(d)).await?
            }

            UpdateGradeOp::DescriptionClear => {
                student_grade.update_description(&mut ta, None).await?
            }

            UpdateGradeOp::Delete => student_grade.delete(&mut ta).await?,
        }

        student_grade.delete_if_empty(&mut ta).await?;
        ta.commit().await?;
        Ok(())
    }
}
