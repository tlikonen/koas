mod assignments;
mod edit;
mod grades;
mod groups;
mod students;

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
