use super::*;

/// Query for grades associated to assignments.
pub async fn grades_for_assignments(
    db: &mut DBase,
    group: QueryMatch<'_>,
    assignment: QueryMatch<'_>,
    assignment_short: QueryMatch<'_>,
) -> Result<QueryList<GradesForAssignment>> {
    GradesForAssignment::query(db, group, assignment, assignment_short).await
}

/// Query for grades associated to students.
pub async fn grades_for_students(
    db: &mut DBase,
    lastname: QueryMatch<'_>,
    firstname: QueryMatch<'_>,
    group: QueryMatch<'_>,
    description: QueryMatch<'_>,
) -> Result<QueryList<GradesForStudent>> {
    GradesForStudent::query(db, lastname, firstname, group, description).await
}

/// Query for grades associated to groups.
pub async fn grades_for_group(
    db: &mut DBase,
    group: QueryMatch<'_>,
) -> Result<QueryList<GradesForGroup>> {
    if group.is_empty() {
        return Err("Argumentiksi pitää antaa ryhmän nimi.".into());
    }
    GradesForGroup::query(db, group).await
}

/// Query for student ranking.
///
/// Apply `queries` and build ranking list for students by their grades.
/// Assignments' weight is included. If `include_weightless` is `true`
/// also include assignments with no weight and count them with weight
/// 1.
pub async fn student_ranking(
    db: &mut DBase,
    queries: Vec<FullQuery<'_>>,
    include_weightless: bool,
) -> Result<StudentRanking> {
    let mut ranks = StudentRanking::new();
    for query in queries {
        ranks.query(db, query, include_weightless).await?;
    }
    Ok(ranks)
}

/// Build grade distribution graph.
///
/// Apply `queries` and build distribution graph for grades. If
/// `include_weightless` is `false` only assignments with weight are
/// included. If `include_weightless` is `true` also include assignments
/// with no weight.
pub async fn grade_distribution(
    db: &mut DBase,
    queries: Vec<FullQuery<'_>>,
    include_weightless: bool,
) -> Result<GradeDistribution> {
    let mut dist = GradeDistribution::new();
    for query in queries {
        dist.query(db, query, include_weightless).await?;
    }
    Ok(dist)
}

impl Grade {
    /// Prepare update for grade.
    ///
    /// See [`Commit`] trait for more information.
    pub fn set_grade<'a>(&'a self, grade: &str) -> Result<UpdateGrade<'a>> {
        match grade.normalize() {
            None => Err(format!("Sopimaton oppilaan kuvaus: ”{grade}”.").into()),
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
            None => Err(format!("Sopimaton oppilaan kuvaus: ”{desc}”.").into()),
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
