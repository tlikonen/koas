//! # Database
//!
//! ## Connect
//!
//! Call [`connect`] function for connecting to the database server.
//!
//! ## Insert Data
//!
//!   - [`Student::insert`]
//!   - [`Assignment::insert`]
//!
//! ## Query Data
//!
//!   - [`Stats::query`]
//!   - [`Student::query`]
//!   - [`Group::query`]
//!   - [`AssignmentsForGroup::query`]
//!   - [`GradesForAssignment::query`]
//!   - [`GradesForStudent::query`]
//!   - [`GradesForGroup::query`]
//!   - [`StudentRanking::query`]
//!   - [`GradeDistribution::query`]
//!
//! Query functions' return value can be printed with each type's
//! respective [`print`](crate::output::PrintQuery::print) method from
//! [`PrintQuery`](crate::output::PrintQuery) trait or
//! [`print_num`](crate::output::PrintQueryNum::print_num) method from
//! [`PrintQueryNum`](crate::output::PrintQueryNum) trait.
//!
//! A [`Table`](crate::output::Table) can be constructed with
//! [`table`](crate::output::MakeTable::table) method which again can be
//! printed with [`print`](crate::output::PrintQuery::print) method.
//!
//! ## Update Data
//!
//! Query functions return a type which may contain editable database
//! types. They are:
//!
//!   - [`Student`]
//!   - [`Group`]
//!   - [`Assignment`]
//!   - [`Grade`]
//!
//! Methods of those types are used to update or delete the database
//! data.

pub mod assignments;
pub mod grades;
pub mod groups;
mod init;
pub mod stats;
pub mod students;

use crate::config::Config;
use crate::prelude::*;
use crate::tools::StrExt;
use futures::TryStreamExt;
use sqlx::Row as _;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Display;
use std::io;
use std::io::Write as _;

pub(crate) use self::assignments::*;
pub(crate) use self::grades::*;
pub(crate) use self::groups::*;
pub(crate) use self::stats::*;
pub(crate) use self::students::*;

pub use self::init::OldDb;
pub use self::init::connect;
pub use sqlx::Connection as _;

pub type DBase = sqlx::PgConnection;

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

#[derive(Default, Clone)]
pub struct QueryList<T>(Vec<T>);

pub type Description = Field<ContextDescription, String>;

#[derive(Default)]
pub struct ContextDescription;

#[derive(Clone)]
pub struct Field<C, T> {
    field: T,
    #[allow(unused)]
    context: C,
}

pub struct Update<'a, I, O> {
    pub(crate) item: &'a I,
    pub(crate) operation: O,
}

pub struct FullQuery<'a> {
    pub group: QueryMatch<'a>,
    pub assignment: QueryMatch<'a>,
    pub assignment_short: QueryMatch<'a>,
    pub lastname: QueryMatch<'a>,
    pub firstname: QueryMatch<'a>,
    pub description: QueryMatch<'a>,
}

pub trait HasData {
    fn has_data(self) -> Result<Self>
    where
        Self: Sized,
    {
        match self.is_empty() {
            false => Ok(self),
            true => Err(Error::QueryEmpty),
        }
    }

    fn is_empty(&self) -> bool;
}

/// Commit prepared updates to the database.
///
/// Updates are prepared with methods of [`Student`], [`Group`],
/// [`Assignment`], [`Grade`]. Usually it is preferable to queue several
/// updates and commit the whole queue instead. A [`Queue`] of updates
/// is constructed with [`queue`](ToQueue::queue) method from
/// [`ToQueue`] trait.
#[allow(async_fn_in_trait)]
pub trait Commit {
    /// Commit the database update.
    async fn commit(self, db: &mut DBase) -> Result<()>;
}

/// A queue for updates.
///
/// Several updates can be committed in a single database transaction.
/// It is faster than several individual commits. The queue commit is
/// also atomic: if there is a failure in the transaction the whole
/// queue of updates is canceled.
///
/// ```compile_fail
/// let mut updates = Queue::default();
/// let student1 = /* ... */
/// let student2 = /* ... */
/// student1.set_description(/* ... */)?.queue(&mut updates);
/// student2.set_description(/* ... */)?.queue(&mut updates);
/// updates.commit(/* &mut DBase */).await?;
#[derive(Default)]
pub struct Queue<'a> {
    queue: VecDeque<QueueItem<'a>>,
}

pub enum QueueItem<'a> {
    UpdateStudent(UpdateStudent<'a>),
    UpdateGroup(UpdateGroup<'a>),
    UpdateAssignment(UpdateAssignment<'a>),
    UpdateGrade(UpdateGrade<'a>),
    InsertStudent(InsertStudent),
    InsertAssignment(InsertAssignment),
}

pub trait ToQueue<'a> {
    fn queue(self, q: &mut Queue<'a>);
}

impl<C, T> Field<C, T>
where
    C: Default,
{
    fn new(field: T) -> Self {
        Self {
            field,
            context: Default::default(),
        }
    }
}

impl<C> Field<C, String> {
    pub fn as_str(&self) -> &str {
        &self.field
    }
}

impl<C> Field<C, i32> {
    pub fn value(&self) -> i32 {
        self.field
    }
}

fn unwrap_or_empty<C>(desc: Option<Field<C, String>>) -> String {
    desc.map_or("".to_string(), |x| x.to_string())
}

impl<C, T> Display for Field<C, T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.field)
    }
}

impl QueryMatch<'_> {
    fn sql_like(&self) -> String {
        match self {
            Self::Exact(s) => like_esc(s),
            Self::Wild(s) => like_esc_wild(s),
            Self::WildAround(s) => like_esc_wild_around(s),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::Exact(s) | Self::Wild(s) | Self::WildAround(s) => s.is_empty(),
        }
    }
}

impl<T> QueryList<T> {
    /// Construct a new list.
    pub fn new(v: Vec<T>) -> Self {
        Self(v)
    }

    /// Return iterator over items.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    /// Return iterator over items by 1-based indices.
    pub fn iter_index1<I>(&self, indices: I) -> impl Iterator<Item = &T>
    where
        I: IntoIterator<Item = usize>,
    {
        indices.into_iter().filter_map(|i| self.0.get(i - 1))
    }

    /// Return the count of elements in the list.
    pub fn count(&self) -> usize {
        self.0.len()
    }

    /// Return the `n`th element.
    pub fn get(&self, n: usize) -> Option<&T> {
        self.0.get(n)
    }

    /// Take and return the `n`th element and consume the list.
    pub fn take(self, n: usize) -> Option<T> {
        self.0.into_iter().nth(n)
    }

    pub(crate) fn list_is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'a, T> IntoIterator for &'a QueryList<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> IntoIterator for QueryList<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl TryFrom<&str> for Description {
    type Error = Error;
    fn try_from(desc: &str) -> Result<Self> {
        match desc.normalize() {
            Some(d) => Ok(Self::new(d)),
            None => Err(Error::InvalidDescription(desc.to_string())),
        }
    }
}

impl Commit for QueueItem<'_> {
    async fn commit(self, db: &mut DBase) -> Result<()> {
        match self {
            Self::UpdateStudent(s) => s.commit(db).await,
            Self::UpdateGroup(g) => g.commit(db).await,
            Self::UpdateAssignment(a) => a.commit(db).await,
            Self::UpdateGrade(g) => g.commit(db).await,
            Self::InsertStudent(s) => s.commit(db).await,
            Self::InsertAssignment(a) => a.commit(db).await,
        }
    }
}

impl<'a> Queue<'a> {
    pub(crate) fn push_back(&mut self, item: QueueItem<'a>) {
        self.queue.push_back(item);
    }

    /// Pop the first item from the queue.
    pub fn pop_front(&mut self) -> Option<QueueItem<'_>> {
        self.queue.pop_front()
    }
}

impl Commit for Queue<'_> {
    /// Commit a queue of updates.
    async fn commit(mut self, db: &mut DBase) -> Result<()> {
        let mut ta = db.begin().await?;
        while let Some(item) = self.pop_front() {
            item.commit(&mut ta).await?;
        }
        ta.commit().await?;
        Ok(())
    }
}

impl<'a, I, O> Update<'a, I, O> {
    pub(crate) fn new(item: &'a I, operation: O) -> Self {
        Self { item, operation }
    }
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

    #[test]
    fn unwrap_or_empty_fn() {
        let empty: Option<Description> = None;
        assert_eq!("", unwrap_or_empty(empty));
        assert_eq!(
            "foo bar",
            unwrap_or_empty(Some(Description::try_from("  foo  bar  ").unwrap()))
        );

        // Lastname won't be empty but test this generic function anyway.
        let empty: Option<Lastname> = None;
        assert_eq!("", unwrap_or_empty(empty));
        assert_eq!(
            "foo bar",
            unwrap_or_empty(Some(Lastname::try_from("  foo  bar  ").unwrap()))
        );
    }
}
