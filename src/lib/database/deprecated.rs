use super::*;

pub trait CopyToEditable {
    fn copy_to(&self, ed: &mut Editable);
}

#[derive(Default)]
pub enum Editable {
    #[default]
    None,
    Students(QueryList<Student>),
    Groups(QueryList<Group>),
    Assignments(QueryList<Assignment>),
    Grades(QueryList<Grade>),
}

impl Editable {
    pub(crate) fn set(&mut self, value: Self) {
        *self = value;
    }

    pub fn clear(&mut self) {
        self.set(Self::None);
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn is_grade(&self) -> bool {
        matches!(self, Self::Grades(_))
    }

    pub fn count(&self) -> usize {
        match self {
            Self::None => 0,
            Self::Students(v) => v.count(),
            Self::Groups(v) => v.count(),
            Self::Assignments(v) => v.count(),
            Self::Grades(v) => v.count(),
        }
    }

    pub fn print_fields(&self, fields: &[&str]) -> Result<()> {
        let mut s = String::with_capacity(50);
        let mut stdout = io::stdout();

        for (n, f) in (1..).zip(fields) {
            s.push_str(&format!(" / {}:{}", n, f));
        }

        match self.count() {
            0 => (),
            1 => writeln!(stdout, "Tietue: 1. Kentät:{s}")?,
            n => writeln!(stdout, "Tietueet: 1–{n}. Kentät:{s}")?,
        }
        Ok(())
    }
}

impl CopyToEditable for QueryList<Student> {
    fn copy_to(&self, ed: &mut Editable) {
        ed.set(Editable::Students(self.clone()));
    }
}

impl CopyToEditable for QueryList<Group> {
    fn copy_to(&self, ed: &mut Editable) {
        ed.set(Editable::Groups(self.clone()));
    }
}

impl CopyToEditable for AssignmentsForGroup {
    fn copy_to(&self, ed: &mut Editable) {
        ed.set(Editable::Assignments(QueryList::new(
            self.assignments.clone(),
        )));
    }
}

impl CopyToEditable for GradesForAssignment {
    fn copy_to(&self, ed: &mut Editable) {
        ed.set(Editable::Grades(QueryList::new(self.grades.clone())));
    }
}

impl CopyToEditable for GradesForStudent {
    fn copy_to(&self, ed: &mut Editable) {
        ed.set(Editable::Grades(QueryList::new(self.grades.clone())));
    }
}
