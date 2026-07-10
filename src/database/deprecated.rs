use crate::prelude::*;

impl<T> QueryList<T> {
    pub(crate) fn for_edit<'a, I, S>(&'a self, indexes: Vec<usize>, fields: I) -> EditItems<'a, T>
    where
        I: IntoIterator<Item = S>,
        S: ToString,
    {
        let mut normalized = Vec::with_capacity(4);
        for field in fields.into_iter().map(|x| x.to_string()) {
            normalized.push(if field.is_empty() {
                Field::Ignore
            } else if !field.has_content() {
                Field::Clear
            } else if let Some(f) = field.normalize() {
                Field::Set(f)
            } else {
                Field::Ignore
            });
        }

        EditItems {
            items: self.list(),
            indexes,
            fields: normalized,
        }
    }

    pub(crate) fn for_delete<'a>(&'a self, indexes: Vec<usize>) -> DeleteItems<'a, T> {
        DeleteItems {
            items: self.list(),
            indexes,
        }
    }
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
        matches!(self, Self::Ignore)
    }

    pub(crate) fn has_value(&self) -> bool {
        matches!(self, Self::Set(_))
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

pub(crate) trait Delete {
    async fn delete(&self, db: &mut DBase) -> Result<()>;
}
