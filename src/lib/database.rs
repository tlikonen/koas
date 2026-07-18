mod assignments;
mod grades;
mod groups;
mod init;
mod students;

use crate::config::Config;
use crate::prelude::*;
use crate::tools;
use futures::TryStreamExt;
use std::io;
use std::io::Write as _;

pub(crate) use self::assignments::UpdateAssignmentOp;
pub(crate) use self::grades::UpdateGradeOp;
pub(crate) use self::groups::UpdateGroupOp;
pub(crate) use self::students::UpdateStudentOp;
pub(crate) use sqlx::{Connection as _, PgConnection as DBase, Row as _};

pub use self::assignments::{Assignment, AssignmentsForGroup, InsertAssignment, UpdateAssignment};
pub use self::grades::{
    Grade, GradeDistribution, GradesForAssignment, GradesForGroup, GradesForStudent, SimpleGrade,
    SimpleStudent, StudentRanking, UpdateGrade,
};
pub use self::groups::{Group, UpdateGroup};
pub use self::students::{InsertStudent, Student, UpdateStudent};
pub use sqlx::{Connection, PgConnection};

pub async fn connect(config: &Config) -> Result<DBase> {
    let connect_string = format!(
        "postgres://{user}:{password}@{host}:{port}/{db}",
        user = config.user,
        password = config.password,
        host = config.host,
        port = config.port,
        db = config.database,
    );

    let mut db = DBase::connect(&connect_string).await?;
    init::initialize(&mut db).await?;
    Ok(db)
}

/// Query field match variants.
pub enum QueryMatch<'a> {
    /// The query string must match exactly.
    Exact(&'a str),
    /// Wildcard character "*" can be used in the query string. It
    /// matches any characters zero or more times.
    Wild(&'a str),
    /// Wildcard character "*" can be used in the query string. It
    /// matches any characters zero or more times. Wildcard characters
    /// are automatically inserted around the string.
    WildAround(&'a str),
}

impl QueryMatch<'_> {
    fn sql_like(&self) -> String {
        match self {
            Self::Exact(s) => like_esc(s),
            Self::Wild(s) => like_esc_wild(s),
            Self::WildAround(s) => like_esc_wild_around(s),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        match self {
            Self::Exact(s) => s.is_empty(),
            Self::Wild(s) => s.is_empty(),
            Self::WildAround(s) => s.is_empty(),
        }
    }
}

pub trait HasData {
    fn has_data(self) -> Result<Self>
    where
        Self: Sized,
    {
        match self.is_empty() {
            false => Ok(self),
            true => Err("Ei löytynyt.".into()),
        }
    }

    fn is_empty(&self) -> bool;
}

#[derive(Default, Clone)]
pub struct QueryList<T>(Vec<T>);

impl<T> QueryList<T> {
    pub fn new(v: Vec<T>) -> Self {
        Self(v)
    }

    pub fn list(&self) -> &Vec<T> {
        &self.0
    }

    /// Return iterator over items by 1-based indices.
    pub fn iter_index1<I>(&self, indices: I) -> impl Iterator<Item = &T>
    where
        I: IntoIterator<Item = usize>,
    {
        indices.into_iter().filter_map(|i| self.0.get(i - 1))
    }

    pub fn count(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, n: usize) -> Option<&T> {
        self.0.get(n)
    }
}

pub struct Stats {
    pub students: i64,
    pub groups: i64,
    pub assignments: i64,
    pub grades: i64,
}

impl Stats {
    pub(crate) async fn query(db: &mut DBase) -> Result<Self> {
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

/// Commit prepared updates to the database.
///
/// Updates are prepared with methods of [`Student`], [`Group`],
/// [`Assignment`], [`Grade`].
#[allow(async_fn_in_trait)]
pub trait Commit {
    /// Commit the database update.
    async fn commit(&self, db: &mut DBase) -> Result<()>;
}

/// A queue for updates.
pub struct Queue<'a>(Vec<QueueItem<'a>>);

pub enum QueueItem<'a> {
    UpdateStudent(UpdateStudent<'a>),
    UpdateGroup(UpdateGroup<'a>),
    UpdateAssignment(UpdateAssignment<'a>),
    UpdateGrade(UpdateGrade<'a>),
    InsertStudent(InsertStudent),
    InsertAssignment(InsertAssignment),
}

impl Commit for QueueItem<'_> {
    async fn commit(&self, db: &mut DBase) -> Result<()> {
        match self {
            Self::UpdateStudent(s) => s.commit(db).await?,
            Self::UpdateGroup(g) => g.commit(db).await?,
            Self::UpdateAssignment(a) => a.commit(db).await?,
            Self::UpdateGrade(g) => g.commit(db).await?,
            Self::InsertStudent(s) => s.commit(db).await?,
            Self::InsertAssignment(a) => a.commit(db).await?,
        }
        Ok(())
    }
}

impl<'a> Queue<'a> {
    /// Create a new empty queue for updates.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Iterate over the queue.
    pub fn iter(&self) -> impl Iterator<Item = &QueueItem<'_>> {
        self.0.iter()
    }

    /// Return how many items are in the queue.
    pub fn count(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn push(&mut self, item: QueueItem<'a>) {
        self.0.push(item);
    }
}

impl Default for Queue<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl Commit for Queue<'_> {
    /// Commit a queue of updates.
    ///
    /// The whole queue is committed as a single database transaction.
    /// It is faster than several separate commits. If anything fails in
    /// the transaction then the whole queue of changes is rolled back
    /// (canceled).
    async fn commit(&self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        for item in self.iter() {
            item.commit(&mut ta).await?;
        }
        ta.commit().await?;
        Ok(())
    }
}

pub struct Update<'a, I, O> {
    pub(crate) item: &'a I,
    pub(crate) operation: O,
}

pub trait ToQueue<'a> {
    fn queue(self, q: &mut Queue<'a>);
}

pub struct FullQuery<'a> {
    pub group: QueryMatch<'a>,
    pub assignment: QueryMatch<'a>,
    pub assignment_short: QueryMatch<'a>,
    pub lastname: QueryMatch<'a>,
    pub firstname: QueryMatch<'a>,
    pub description: QueryMatch<'a>,
}

fn like_esc_wild_around(string: &str) -> String {
    let mut new = String::with_capacity(string.len() + 3);
    new.push('%');

    for c in string.chars() {
        match c {
            '%' | '_' | '\\' => {
                new.push('\\');
                new.push(c);
            }
            '*' => new.push('%'),
            _ => new.push(c),
        }
    }

    new.push('%');
    new
}

fn like_esc_wild(string: &str) -> String {
    let mut new = String::with_capacity(string.len() + 3);

    for c in string.chars() {
        match c {
            '%' | '_' | '\\' => {
                new.push('\\');
                new.push(c);
            }
            '*' => new.push('%'),
            _ => new.push(c),
        }
    }

    new
}

fn like_esc(string: &str) -> String {
    let mut new = String::with_capacity(string.len() + 3);

    for c in string.chars() {
        match c {
            '%' | '_' | '\\' => {
                new.push('\\');
                new.push(c);
            }
            _ => new.push(c),
        }
    }

    new
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn like_esc_wild_around_fn() {
        assert_eq!("%abcd%", like_esc_wild_around("abcd"));
        assert_eq!("%a\\%b\\_cd%", like_esc_wild_around("a%b_cd"));
        assert_eq!("%ab\\\\cd%", like_esc_wild_around("ab\\cd"));
        assert_eq!("%\\_\\%\\\\%", like_esc_wild_around("_%\\"));
        assert_eq!("%ab%cd%", like_esc_wild_around("ab*cd"));
    }

    #[test]
    fn like_esc_wild_fn() {
        assert_eq!("abcd", like_esc_wild("abcd"));
        assert_eq!("a\\%b\\_cd", like_esc_wild("a%b_cd"));
        assert_eq!("ab\\\\cd", like_esc_wild("ab\\cd"));
        assert_eq!("\\_\\%\\\\", like_esc_wild("_%\\"));
        assert_eq!("ab%cd", like_esc_wild("ab*cd"));
        assert_eq!("%ab%cd%", like_esc_wild("*ab*cd*"));
    }

    #[test]
    fn like_esc_fn() {
        assert_eq!("abcd", like_esc("abcd"));
        assert_eq!("a\\%b\\_cd", like_esc("a%b_cd"));
        assert_eq!("ab\\\\cd", like_esc("ab\\cd"));
        assert_eq!("\\_\\%\\\\", like_esc("_%\\"));
        assert_eq!("ab*cd", like_esc("ab*cd"));
        assert_eq!("*ab*cd*", like_esc("*ab*cd*"));
    }
}
