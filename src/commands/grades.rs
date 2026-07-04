use crate::prelude::*;

pub async fn grades_for_assignments(db: &mut DBase, args: &str) -> Result<GradesForAssignments> {
    let mut fields = tools::split_sep(args);
    let group = fields.next().unwrap_or(""); // ryhmä
    let assign = fields.next().unwrap_or(""); // suoritus
    let assign_short = fields.next().unwrap_or(""); // lyhenne

    GradesForAssignments::query(db, group, assign, assign_short).await
}

pub async fn grades_for_students(db: &mut DBase, args: &str) -> Result<GradesForStudents> {
    let mut fields = tools::split_sep(args);
    let lastname = fields.next().unwrap_or(""); // sukunimi
    let firstname = fields.next().unwrap_or(""); // etunimi
    let group = fields.next().unwrap_or(""); // ryhmä
    let desc = fields.next().unwrap_or(""); // lisätiedot

    GradesForStudents::query(db, lastname, firstname, group, desc).await
}

pub async fn grades_for_group(db: &mut DBase, args: &str) -> Result<GradesForGroups> {
    let group = {
        let (g, _) = tools::split_first(args);
        if g.is_empty() {
            return Err("Argumentiksi pitää antaa ryhmän nimi.".into());
        }
        g
    };

    GradesForGroups::query(db, group).await
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
    match editable.item() {
        EditableItem::Grades(student_grades) => {
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
    match editable.item() {
        EditableItem::Grades(student_grades) => {
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

pub async fn student_ranking(db: &mut DBase, mut args: &str, all: bool) -> Result<StudentRanking> {
    if args.is_empty() {
        args = "@";
    }

    let mut ranks = StudentRanking::new();

    let field_groups = tools::split_sep(args);
    for field_string in field_groups {
        let mut fields = tools::split_sep(field_string);
        let query_terms = FullQuery {
            // Keep the order because of the next() method.
            group: fields.next().unwrap_or(""),
            assignment: fields.next().unwrap_or(""),
            assignment_short: fields.next().unwrap_or(""),
            lastname: fields.next().unwrap_or(""),
            firstname: fields.next().unwrap_or(""),
            description: fields.next().unwrap_or(""),
            all,
        };

        ranks.query(db, query_terms).await?;
    }

    Ok(ranks)
}

pub async fn grade_distribution(
    db: &mut DBase,
    mut args: &str,
    all: bool,
) -> Result<GradeDistribution> {
    if args.is_empty() {
        args = "@";
    }

    let mut dist = GradeDistribution::new();

    let field_groups = tools::split_sep(args);
    for field_string in field_groups {
        let mut fields = tools::split_sep(field_string);
        let query_terms = FullQuery {
            // Keep the order because of the next() method.
            group: fields.next().unwrap_or(""),
            assignment: fields.next().unwrap_or(""),
            assignment_short: fields.next().unwrap_or(""),
            lastname: fields.next().unwrap_or(""),
            firstname: fields.next().unwrap_or(""),
            description: fields.next().unwrap_or(""),
            all,
        };

        dist.query(db, query_terms).await?;
    }

    Ok(dist)
}

impl Edit for EditItems<'_, Grade> {
    async fn edit(&self, db: &mut DBase) -> Result<()> {
        let grade = self.field(0); // arvosana
        let desc = self.field(1); // lisätiedot

        if grade.is_none() && desc.is_none() {
            return Err("Anna muokattavia kenttiä.".into());
        }

        for student_grade in self.iter() {
            match grade {
                Field::Set(s) => student_grade.update_grade(db, Some(s)).await?,
                Field::Clear => student_grade.update_grade(db, None).await?,
                Field::Ignore => (),
            }

            match desc {
                Field::Set(d) => student_grade.update_description(db, Some(d)).await?,
                Field::Clear => student_grade.update_description(db, None).await?,
                Field::Ignore => (),
            }

            student_grade.delete_if_empty(db).await?;
        }
        Ok(())
    }
}

impl Delete for DeleteItems<'_, Grade> {
    async fn delete(&self, db: &mut DBase) -> Result<()> {
        for grade in self.iter() {
            grade.delete(db).await?;
        }
        Ok(())
    }
}
