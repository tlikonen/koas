mod assignments;
mod grades;
mod groups;
mod students;

pub use assignments::*;
pub use grades::*;
pub use groups::*;
pub use students::*;

use crate::database::*;
use crate::prelude::*;
use crate::tools;
use crate::tools::Normalize;
use crate::tools::StrExt;

/// Return statistics about the database.
pub async fn stats(db: &mut DBase) -> Result<Stats> {
    Stats::query(db).await
}
