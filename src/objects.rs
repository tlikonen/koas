use crate::prelude::*;

pub trait HasData {
    fn has_data(self) -> Result<Self, String>
    where
        Self: Sized,
    {
        match self.empty_data() {
            false => Ok(self),
            true => Err("Ei löytynyt.")?,
        }
    }

    fn empty_data(&self) -> bool;
}

pub trait CopyToEditable {
    fn copy_to(&self, ed: &mut Editable) {
        ed.set(self.item());
    }

    fn item(&self) -> EditableItem;
}

pub enum EditableItem {
    None,
    Students(Vec<Student>),
    Groups(Vec<Group>),
    Assignments(Vec<Assignment>),
    Grades(Vec<Grade>),
}

pub struct Editable(EditableItem);

impl Editable {
    pub fn set(&mut self, value: EditableItem) {
        self.0 = value;
    }

    pub fn item(&self) -> &EditableItem {
        &self.0
    }
}

impl Default for Editable {
    fn default() -> Self {
        Self(EditableItem::None)
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
    pub oid: i32,
    pub lastname: String,
    pub firstname: String,
    pub groups: String,
    pub description: String,
}

pub struct Students {
    pub list: Vec<Student>,
}

#[derive(Clone)]
pub struct Group {
    pub rid: i32,
    pub name: String,
    pub description: String,
}

pub struct Groups {
    pub list: Vec<Group>,
}

#[derive(Clone, Default)]
pub struct Assignment {
    pub rid: i32,
    pub sid: i32,
    pub assignment: String,
    pub assignment_short: String,
    pub weight: Option<i32>,
}

pub struct Assignments {
    pub group: String,
    pub list: Vec<Assignment>,
}

#[derive(Clone)]
pub struct Grade {
    pub oid: i32,
    pub lastname: String,
    pub firstname: String,
    pub sid: i32,
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
pub struct GradesForAssignments {
    pub list: Vec<GradesForAssignment>,
}

pub struct GradesForStudent {
    pub lastname: String,
    pub firstname: String,
    pub group: String,
    pub grades: Vec<Grade>,
}

#[derive(Default)]
pub struct GradesForStudents {
    pub list: Vec<GradesForStudent>,
}

#[derive(Default)]
pub struct GradesForGroup {
    pub group: String,
    pub students: Vec<SimpleStudent>,
    pub assignments: Vec<Assignment>,
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
pub struct StudentRank {
    pub name: String,
    pub groups: Vec<String>,
    pub sum: f64,
    pub count: i32,
    pub grade_count: usize,
}

pub struct GradeDistribution {
    pub data: HashMap<String, i32>,
    pub output: Output,
}

pub struct FullQuery<'a> {
    pub group: &'a str,
    pub assignment: &'a str,
    pub assignment_short: &'a str,
    pub lastname: &'a str,
    pub firstname: &'a str,
    pub description: &'a str,
}
