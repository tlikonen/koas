use super::*;
use crate::prelude::*;

/// Query for assignments.
///
/// Wildcard character "*" is allowed in the `group` argument.
pub async fn assignments(db: &mut DBase, group: &str) -> Result<QueryList<AssignmentsForGroup>> {
    if group.is_empty() {
        return Err("Argumentiksi pitää antaa ryhmän nimi.".into());
    }
    AssignmentsForGroup::query(db, group).await
}

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

impl Edit for EditItems<'_, Assignment> {
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
            Field::Set(s) => match s.trim().parse::<i32>() {
                Ok(n) if n >= 1 => Field::Set(n),
                _ => {
                    return Err(
                        "Painokertoimen täytyy olla positiivinen kokonaisluku (tai tyhjä).".into(),
                    );
                }
            },
            Field::Clear => Field::Clear,
            Field::Ignore => Field::Ignore,
        };

        // Convert from &Field<String> to Field<i32>.
        let position = match position {
            Field::Set(s) => match s.trim().parse::<i32>() {
                Ok(n) => Field::Set(n),
                _ => return Err("Järjestysnumeron täytyy olla kokonaisluku.".into()),
            },
            Field::Clear | Field::Ignore => Field::Ignore,
        };

        for group_assignment in self.iter() {
            if let Field::Set(n) = name {
                group_assignment.update_name(db, n).await?;
            }

            if let Field::Set(s) = short {
                group_assignment.update_short(db, s).await?;
            }

            match weight {
                Field::Set(w) => group_assignment.update_weight(db, Some(w)).await?,
                Field::Clear => group_assignment.update_weight(db, None).await?,
                Field::Ignore => (),
            }

            if let Field::Set(p) = position {
                group_assignment.update_position(db, p).await?;
            }
        }
        Ok(())
    }
}

impl Delete for DeleteItems<'_, Assignment> {
    async fn delete(&self, db: &mut DBase) -> Result<()> {
        let mut rid_list = Vec::with_capacity(1);
        for assignment in self.iter() {
            let count = assignment.count_grades(db).await?;
            if count > 0 {
                return Err(format!(
                    "Suoritukselle ”{a}” on kirjattu {c} arvosana(a). Poista ne ensin.",
                    a = assignment.assignment,
                    c = count,
                )
                .into());
            }

            assignment.delete(db).await?;

            if !rid_list.contains(&assignment.rid) {
                rid_list.push(assignment.rid);
            }
        }

        Group::delete_empty(db).await?;

        for rid in rid_list {
            Assignment::reposition(db, rid).await?;
        }

        Ok(())
    }
}
