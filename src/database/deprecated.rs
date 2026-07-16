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

impl<T> QueryList<T> {
    pub(crate) fn for_edit<'a, I, S>(
        &'a self,
        indexes: Vec<usize>,
        fields: I,
    ) -> DeprecatedEditItems<'a, T>
    where
        I: IntoIterator<Item = S>,
        S: ToString,
    {
        let mut normalized = Vec::with_capacity(4);
        for field in fields.into_iter().map(|x| x.to_string()) {
            normalized.push(if field.is_empty() {
                DeprecatedField::Ignore
            } else if !field.has_content() {
                DeprecatedField::Clear
            } else if let Some(f) = field.normalize() {
                DeprecatedField::Set(f)
            } else {
                DeprecatedField::Ignore
            });
        }

        DeprecatedEditItems {
            items: self.list(),
            indexes,
            fields: normalized,
        }
    }
}

pub(crate) struct DeprecatedEditItems<'a, T> {
    items: &'a Vec<T>,
    indexes: Vec<usize>,
    fields: Vec<DeprecatedField<String>>,
}

impl<'a, T> DeprecatedEditItems<'a, T> {
    pub(crate) fn count(&self) -> usize {
        self.indexes.len()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.indexes.iter().filter_map(|i| self.items.get(i - 1))
    }

    pub(crate) fn field(&self, n: usize) -> &DeprecatedField<String> {
        match self.fields.get(n) {
            Some(f) => f,
            None => &DeprecatedField::Ignore,
        }
    }
}

pub(crate) enum DeprecatedField<T> {
    Ignore,
    Clear,
    Set(T),
}

impl<T> DeprecatedField<T> {
    pub(crate) fn is_none(&self) -> bool {
        matches!(self, Self::Ignore)
    }

    pub(crate) fn has_value(&self) -> bool {
        matches!(self, Self::Set(_))
    }
}

pub(crate) trait DeprecatedEdit {
    async fn edit(&self, db: &mut DBase) -> Result<()>;
}
