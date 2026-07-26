use super::*;
use std::collections::HashMap;

pub struct Stats {
    pub students: i64,
    pub groups: i64,
    pub assignments: i64,
    pub grades: i64,
}

#[derive(Default)]
pub(crate) struct StudentRank {
    pub(crate) name: String,
    pub(crate) groups: Vec<String>,
    pub(crate) sum: f64,
    pub(crate) count: i32,
    pub(crate) grade_count: usize,
}

#[derive(Default)]
pub struct StudentRanking {
    pub(crate) data: HashMap<i32, StudentRank>,
}

#[derive(Default)]
pub struct GradeDistribution {
    pub(crate) data: HashMap<String, i32>,
}

impl Stats {
    /// Return statistics about the database.
    pub async fn query(db: &mut DBase) -> Result<Self> {
        let row = sqlx::query(
            "SELECT \
             (SELECT count(*) FROM oppilaat) oppilaat, \
             (SELECT count(*) FROM ryhmat) ryhmat, \
             (SELECT count(*) FROM suoritukset) suoritukset, \
             (SELECT count(*) FROM arvosanat WHERE arvosana LIKE '_%' ESCAPE '\\') arvosanat",
        )
        .fetch_one(db)
        .await?;

        Ok(Self {
            students: row.try_get("oppilaat")?,
            groups: row.try_get("ryhmat")?,
            assignments: row.try_get("suoritukset")?,
            grades: row.try_get("arvosanat")?,
        })
    }
}

impl StudentRanking {
    /// Query for student ranking.
    ///
    /// Apply `queries` and build ranking list for students by their grades.
    /// Assignments' weight is included. If `include_weightless` is `true`
    /// also include assignments with no weight and count them with weight
    /// 1.
    pub async fn query<'a>(
        db: &mut DBase,
        queries: impl IntoIterator<Item = &'a FullQuery<'a>>,
        include_weightless: bool,
    ) -> Result<Self> {
        let mut ranks = Self::default();
        for query in queries.into_iter() {
            ranks.query_db(db, query, include_weightless).await?;
        }
        Ok(ranks)
    }

    async fn query_db(&mut self, db: &mut DBase, args: &FullQuery<'_>, all: bool) -> Result<()> {
        let mut rows = sqlx::query(
            "SELECT oid, sukunimi, etunimi, ryhma, arvosana, painokerroin FROM view_arvosanat \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
             AND suoritus LIKE $5 ESCAPE '\\' AND lyhenne LIKE $6 ESCAPE '\\'",
        )
        .bind(args.lastname.sql_like())
        .bind(args.firstname.sql_like())
        .bind(args.group.sql_like())
        .bind(args.description.sql_like())
        .bind(args.assignment.sql_like())
        .bind(args.assignment_short.sql_like())
        .fetch(db);

        while let Some(row) = rows.try_next().await? {
            if let Some(gr) = row.try_get("arvosana")?
                && let Some(grade) = tools::parse_number(gr)
            {
                let weight: i32 = match row.try_get("painokerroin")? {
                    Some(w) => w,
                    None if all => 1,
                    None => continue,
                };

                let oid: i32 = row.try_get("oid")?;
                let rank = self.data.entry(oid).or_default();

                if rank.name.is_empty() {
                    rank.name.push_str(row.try_get("sukunimi")?);
                    rank.name.push_str(", ");
                    rank.name.push_str(row.try_get("etunimi")?);
                }

                let group: String = row.try_get("ryhma")?;
                if !rank.groups.contains(&group) {
                    rank.groups.push(group.to_string());
                    rank.groups.sort();
                }

                rank.sum += grade * f64::from(weight);
                rank.count += weight;
                rank.grade_count += 1;
            }
        }

        Ok(())
    }
}

impl GradeDistribution {
    /// Build grade distribution graph.
    ///
    /// Apply `queries` and build distribution graph for grades. If
    /// `include_weightless` is `false` only assignments with weight are
    /// included. If `include_weightless` is `true` also include assignments
    /// with no weight.
    pub async fn query<'a>(
        db: &mut DBase,
        queries: impl IntoIterator<Item = &'a FullQuery<'a>>,
        include_weightless: bool,
    ) -> Result<Self> {
        let mut dist = Self::default();
        for query in queries.into_iter() {
            dist.query_db(db, query, include_weightless).await?;
        }
        Ok(dist)
    }

    async fn query_db(&mut self, db: &mut DBase, args: &FullQuery<'_>, all: bool) -> Result<()> {
        let mut rows = sqlx::query(
            "SELECT arvosana, painokerroin FROM view_arvosanat \
             WHERE sukunimi LIKE $1 ESCAPE '\\' AND etunimi LIKE $2 ESCAPE '\\' \
             AND ryhma LIKE $3 ESCAPE '\\' AND olt LIKE $4 ESCAPE '\\' \
             AND suoritus LIKE $5 ESCAPE '\\' AND lyhenne LIKE $6 ESCAPE '\\'",
        )
        .bind(args.lastname.sql_like())
        .bind(args.firstname.sql_like())
        .bind(args.group.sql_like())
        .bind(args.description.sql_like())
        .bind(args.assignment.sql_like())
        .bind(args.assignment_short.sql_like())
        .fetch(db);

        while let Some(row) = rows.try_next().await? {
            let weight: Option<i32> = row.try_get("painokerroin")?;
            if (all || weight.is_some())
                && let Some(grade) = row.try_get("arvosana")?
            {
                let count = self.data.entry(grade).or_default();
                *count += 1;
            }
        }

        Ok(())
    }
}

impl HasData for StudentRanking {
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl HasData for GradeDistribution {
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}
