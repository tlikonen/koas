use crate::prelude::*;

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

pub trait CopyToEditable {
    fn copy_to(&self, ed: &mut Editable);
}

pub(crate) enum EditableItem {
    None,
    Students(EditableValue<Student>),
    Groups(EditableValue<Group>),
    Assignments(EditableValue<Assignment>),
    Grades(EditableValue<Grade>),
}

pub(crate) struct EditableValue<T>(Vec<T>);

impl<T> EditableValue<T> {
    pub(crate) fn from(value: Vec<T>) -> Self {
        Self(value)
    }

    fn value(&self) -> &Vec<T> {
        &self.0
    }

    pub(crate) fn count(&self) -> usize {
        self.0.len()
    }
}

pub struct Editable(EditableItem);

impl Editable {
    pub(crate) fn set(&mut self, value: EditableItem) {
        self.0 = value;
    }

    pub(crate) fn item(&self) -> &EditableItem {
        &self.0
    }
}

impl Default for Editable {
    fn default() -> Self {
        Self(EditableItem::None)
    }
}

pub struct QueryList<T>(Vec<T>);

impl<T> QueryList<T> {
    pub fn from(v: Vec<T>) -> Self {
        Self(v)
    }

    pub fn list(&self) -> &Vec<T> {
        &self.0
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

#[derive(Default, Clone)]
pub struct Student {
    pub(crate) oid: i32,
    pub lastname: String,
    pub firstname: String,
    pub groups: String,
    pub description: String,
}

#[derive(Clone)]
pub struct Group {
    pub(crate) rid: i32,
    pub name: String,
    pub description: String,
}

pub struct Groups(pub(crate) Vec<Group>);

impl Groups {
    pub fn list(&self) -> &Vec<Group> {
        &self.0
    }
}

#[derive(Clone, Default)]
pub struct Assignment {
    pub(crate) rid: i32,
    pub(crate) sid: i32,
    pub assignment: String,
    pub assignment_short: String,
    pub weight: Option<i32>,
}

pub struct AssignmentsForGroup {
    pub group: String,
    pub assignments: Vec<Assignment>,
}

#[derive(Default)]
pub struct AssignmentsForGroups(pub(crate) Vec<AssignmentsForGroup>);

impl AssignmentsForGroups {
    pub fn count(&self) -> usize {
        self.0.len()
    }

    pub fn list(&self) -> &Vec<AssignmentsForGroup> {
        &self.0
    }

    pub fn get(&self, n: usize) -> Option<&AssignmentsForGroup> {
        self.0.get(n)
    }
}

#[derive(Clone)]
pub struct Grade {
    pub(crate) oid: i32,
    pub lastname: String,
    pub firstname: String,
    pub(crate) sid: i32,
    pub assignment: String,
    pub weight: Option<i32>,
    pub grade: Option<String>,
    pub grade_description: Option<String>,
}

pub struct GradesForAssignment {
    pub assignment: String,
    pub group: String,
    pub grades: Vec<Grade>,
}

#[derive(Default)]
pub struct GradesForAssignments(pub(crate) Vec<GradesForAssignment>);

impl GradesForAssignments {
    pub fn count(&self) -> usize {
        self.0.len()
    }

    pub fn list(&self) -> &Vec<GradesForAssignment> {
        &self.0
    }

    pub fn get(&self, n: usize) -> Option<&GradesForAssignment> {
        self.0.get(n)
    }
}

pub struct GradesForStudent {
    pub lastname: String,
    pub firstname: String,
    pub group: String,
    pub grades: Vec<Grade>,
}

#[derive(Default)]
pub struct GradesForStudents(pub(crate) Vec<GradesForStudent>);

impl GradesForStudents {
    pub fn count(&self) -> usize {
        self.0.len()
    }

    pub fn list(&self) -> &Vec<GradesForStudent> {
        &self.0
    }

    pub fn get(&self, n: usize) -> Option<&GradesForStudent> {
        self.0.get(n)
    }
}

#[derive(Default)]
pub struct GradesForGroup {
    pub group: String,
    pub students: Vec<SimpleStudent>,
    pub assignments: Vec<Assignment>,
}

#[derive(Default)]
pub struct GradesForGroups(pub(crate) Vec<GradesForGroup>);

impl GradesForGroups {
    pub fn list(&self) -> &Vec<GradesForGroup> {
        &self.0
    }
}

pub struct SimpleStudent {
    pub name: String,
    pub grades: Vec<SimpleGrade>,
}

pub struct SimpleGrade {
    pub weight: Option<i32>,
    pub grade: Option<String>,
}

#[derive(Default)]
pub(crate) struct StudentRank {
    pub(crate) name: String,
    pub(crate) groups: Vec<String>,
    pub(crate) sum: f64,
    pub(crate) count: i32,
    pub(crate) grade_count: usize,
}

pub struct StudentRanking {
    pub(crate) data: HashMap<i32, StudentRank>,
}

pub struct GradeDistribution {
    pub(crate) data: HashMap<String, i32>,
}

pub(crate) struct FullQuery<'a> {
    pub(crate) group: &'a str,
    pub(crate) assignment: &'a str,
    pub(crate) assignment_short: &'a str,
    pub(crate) lastname: &'a str,
    pub(crate) firstname: &'a str,
    pub(crate) description: &'a str,
    pub(crate) all: bool,
}

pub(crate) struct EditItems<'a, T> {
    items: &'a Vec<T>,
    indexes: Vec<usize>,
    fields: Vec<Field<String>>,
}

impl<'a, T> EditItems<'a, T> {
    pub(crate) fn count(&self) -> usize {
        self.indexes.len()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.indexes.iter().filter_map(|i| self.items.get(i - 1))
    }

    pub(crate) fn field(&self, n: usize) -> &Field<String> {
        match self.fields.get(n) {
            Some(f) => f,
            None => &Field::Ignore,
        }
    }
}

pub(crate) enum Field<T> {
    Ignore,
    Clear,
    Set(T),
}

impl<T> Field<T> {
    pub(crate) fn is_none(&self) -> bool {
        matches!(self, Field::Ignore)
    }

    pub(crate) fn has_value(&self) -> bool {
        matches!(self, Field::Set(_))
    }
}

pub(crate) trait ForEdit<T> {
    fn for_edit<'a, I, S>(&'a self, indexes: Vec<usize>, fields: I) -> EditItems<'a, T>
    where
        I: IntoIterator<Item = S>,
        S: ToString,
    {
        let mut normalized = Vec::with_capacity(4);
        for field in fields.into_iter().map(|x| x.to_string()) {
            normalized.push(if field.is_empty() {
                Field::Ignore
            } else if !tools::has_content(&field) {
                Field::Clear
            } else {
                Field::Set(tools::normalize_str(&field))
            });
        }

        EditItems {
            items: self.items(),
            indexes,
            fields: normalized,
        }
    }

    fn items(&self) -> &Vec<T>;
}

impl<T> ForEdit<T> for EditableValue<T> {
    fn items(&self) -> &Vec<T> {
        self.value()
    }
}

pub(crate) trait Edit {
    async fn edit(&self, db: &mut DBase) -> Result<()>;
}

pub(crate) struct DeleteItems<'a, T> {
    items: &'a Vec<T>,
    indexes: Vec<usize>,
}

impl<'a, T> DeleteItems<'a, T> {
    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.indexes.iter().filter_map(|i| self.items.get(i - 1))
    }
}

pub(crate) trait ForDelete<T> {
    fn for_delete<'a>(&'a self, indexes: Vec<usize>) -> DeleteItems<'a, T> {
        DeleteItems {
            items: self.items(),
            indexes,
        }
    }

    fn items(&self) -> &Vec<T>;
}

impl<T> ForDelete<T> for EditableValue<T> {
    fn items(&self) -> &Vec<T> {
        self.value()
    }
}

pub(crate) trait Delete {
    async fn delete(&self, db: &mut DBase) -> Result<()>;
}
