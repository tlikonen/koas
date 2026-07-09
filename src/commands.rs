mod assignments;
mod edit;
mod grades;
mod groups;
mod students;

pub use crate::objects::FullQuery;
pub use assignments::*;
pub use edit::*;
pub use grades::*;
pub use groups::*;
pub use students::*;

use crate::prelude::*;

/// Print statistics about the database.
pub async fn stats(db: &mut DBase) -> Result<Stats> {
    Stats::query(db).await
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
pub struct Updates<T: Commit>(Vec<T>);

impl<T: Commit> Updates<T> {
    /// Create a new queue for updates.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Push new item to a queue of updates.
    pub fn push(&mut self, item: T) {
        self.0.push(item);
    }

    /// Iterate over a queue.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
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
