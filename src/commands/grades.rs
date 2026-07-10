use super::*;

/// Query for grades associated to assignments.
///
/// Wildcard character "*" is allowed in the query strings and is
/// implicit in the start and end the strings.
pub async fn grades_for_assignments(
    db: &mut DBase,
    group: &str,
    assignment: &str,
    assignment_short: &str,
) -> Result<QueryList<GradesForAssignment>> {
    GradesForAssignment::query(db, group, assignment, assignment_short).await
}

/// Query for grades associated to students.
///
/// Wildcard character "*" is allowed in the query strings and is
/// implicit in the start and end the strings.
pub async fn grades_for_students(
    db: &mut DBase,
    lastname: &str,
    firstname: &str,
    group: &str,
    description: &str,
) -> Result<QueryList<GradesForStudent>> {
    GradesForStudent::query(db, lastname, firstname, group, description).await
}

/// Query for grades associated to groups.
///
/// The wildcard character "*" is allowed in the `group` argument.
pub async fn grades_for_group(db: &mut DBase, group: &str) -> Result<QueryList<GradesForGroup>> {
    if group.is_empty() {
        return Err("Argumentiksi pitää antaa ryhmän nimi.".into());
    }
    GradesForGroup::query(db, group).await
}

pub async fn convert_to_grade(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    if editable.is_none() {
        return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
    }

    if !editable.is_grade() {
        return Err("Vain arvosanoja voi muokata tällä komennolla.".into());
    }

    if args.is_empty() {
        return Err("Puuttuu tietueiden numerot.".into());
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            return Err(format!("Suurin muokattava tietue on {max}.").into());
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable {
        Editable::Grades(student_grades) => {
            let edits = student_grades.for_edit(indexes, [""; 0]); // empty fields
            for student_grade in edits.iter() {
                if let Some(ss) = &student_grade.grade
                    && let Some(old) = tools::parse_number(ss)
                    && let Some(new) = tools::float_to_grade(old)
                {
                    student_grade.update_grade(&mut ta, Some(&new)).await?;
                }
            }
        }
        _ => panic!(),
    }
    ta.commit().await?;
    Ok(())
}

pub async fn convert_to_decimal(db: &mut DBase, editable: &mut Editable, args: &str) -> Result<()> {
    if editable.is_none() {
        return Err("Edellinen komento ei sisällä muokattavia tietueita.".into());
    }

    if !editable.is_grade() {
        return Err("Vain arvosanoja voi muokata tällä komennolla.".into());
    }

    if args.is_empty() {
        return Err("Puuttuu tietueiden numerot.".into());
    }

    let indexes = {
        let (first, _) = tools::split_first(args);
        let i = tools::parse_number_list(first)?;
        let max = editable.count();
        if !tools::is_within_limits(max, &i) {
            return Err(format!("Suurin muokattava tietue on {max}.").into());
        }
        i
    };

    let mut ta = db.begin().await?;
    match editable {
        Editable::Grades(student_grades) => {
            let edits = student_grades.for_edit(indexes, [""; 0]); // empty fields
            for student_grade in edits.iter() {
                if let Some(ss) = &student_grade.grade
                    && let Some(old) = tools::parse_number(ss)
                {
                    let new = tools::format_decimal(old);
                    student_grade.update_grade(&mut ta, Some(&new)).await?;
                }
            }
        }
        _ => panic!(),
    }
    ta.commit().await?;
    Ok(())
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

impl DeprecatedEdit for DeprecatedEditItems<'_, Grade> {
    async fn edit(&self, db: &mut DBase) -> Result<()> {
        let grade = self.field(0); // arvosana
        let desc = self.field(1); // lisätiedot

        if grade.is_none() && desc.is_none() {
            return Err("Anna muokattavia kenttiä.".into());
        }

        for student_grade in self.iter() {
            match grade {
                DeprecatedField::Set(s) => student_grade.update_grade(db, Some(s)).await?,
                DeprecatedField::Clear => student_grade.update_grade(db, None).await?,
                DeprecatedField::Ignore => (),
            }

            match desc {
                DeprecatedField::Set(d) => student_grade.update_description(db, Some(d)).await?,
                DeprecatedField::Clear => student_grade.update_description(db, None).await?,
                DeprecatedField::Ignore => (),
            }

            student_grade.delete_if_empty(db).await?;
        }
        Ok(())
    }
}

impl DeprecatedDelete for DeprecatedDeleteItems<'_, Grade> {
    async fn delete(&self, db: &mut DBase) -> Result<()> {
        for grade in self.iter() {
            grade.delete(db).await?;
        }
        Ok(())
    }
}
