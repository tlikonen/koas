mod assignments;
mod deprecated;
mod grades;
mod groups;
mod init;
mod students;

use crate::config::Config;
use crate::prelude::*;
use crate::tools;
use crate::tools::Normalize;
use crate::tools::StrExt;
use futures::TryStreamExt;
use std::io;
use std::io::Write as _;

pub(crate) use self::deprecated::{
    DeprecatedDelete, DeprecatedDeleteItems, DeprecatedEdit, DeprecatedEditItems, DeprecatedField,
};
pub(crate) use self::groups::UpdateGroupField;
pub(crate) use self::students::UpdateStudentField;
pub(crate) use sqlx::{Connection as _, PgConnection as DBase, Row as _};

pub use {
    self::{
        assignments::{Assignment, AssignmentsForGroup},
        deprecated::{CopyToEditable, Editable},
        grades::{
            Grade, GradeDistribution, GradesForAssignment, GradesForGroup, GradesForStudent,
            SimpleGrade, SimpleStudent, StudentRanking,
        },
        groups::{Group, UpdateGroup},
        students::{DeleteStudent, Student, UpdateStudent},
    },
    sqlx::{Connection, PgConnection},
};

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
    pub(crate) fn new(v: Vec<T>) -> Self {
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

/// Commit prepared changes to the database.
///
/// The [`Commit::commit`] method commits prepared updates to the
/// database. Updates can be prepared with methods of [`Student`],
/// [`Group`], [`Assignment`] and [`Grade`], as well as methods of
/// [`Updates`] which represents a queue of updates.
#[allow(async_fn_in_trait)]
pub trait Commit {
    /// Commit the database update.
    async fn commit(&self, db: &mut DBase) -> Result<()>;
}

/// A queue for updates.
///
/// # Examples
/// ```compile_fail
/// let mut updates = Updates::new();
/// updates.push(/* item */);
/// updates.push(/* item */);
/// updates.commit(/* &mut PgConnection */).await?;
/// ```
pub struct Updates<T: Commit>(Vec<T>);

impl<T: Commit> Updates<T> {
    /// Create a new empty queue for updates.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Push new item to the queue of updates.
    pub fn push(&mut self, item: T) {
        self.0.push(item);
    }

    /// Iterate over the queue.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    /// Return how many items are in the queue.
    pub fn count(&self) -> usize {
        self.0.len()
    }

    /// Return an immutable reference to the queue vector.
    pub fn as_vec(&self) -> &Vec<T> {
        &self.0
    }

    /// Return a mutable reference to the queue vector.
    pub fn as_vec_mut(&mut self) -> &mut Vec<T> {
        &mut self.0
    }
}

impl<T: Commit> Default for Updates<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Commit> Commit for Updates<T> {
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

pub struct Update<'a, I, F> {
    pub(crate) item: &'a I,
    pub(crate) field: F,
}

pub struct Delete<'a, T> {
    pub(crate) item: &'a T,
}

pub struct FullQuery<'a> {
    pub group: &'a str,
    pub assignment: &'a str,
    pub assignment_short: &'a str,
    pub lastname: &'a str,
    pub firstname: &'a str,
    pub description: &'a str,
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
}
