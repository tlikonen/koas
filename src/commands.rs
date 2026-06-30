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

pub async fn stats(modes: &Modes, db: &mut DBase, editable: &mut Editable) -> Result<()> {
    editable.clear();

    Stats::query(db).await?.print(modes.output())?;
    Ok(())
}
