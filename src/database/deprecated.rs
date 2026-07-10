use super::*;

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

    pub(crate) fn for_delete<'a>(&'a self, indexes: Vec<usize>) -> DeprecatedDeleteItems<'a, T> {
        DeprecatedDeleteItems {
            items: self.list(),
            indexes,
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

pub(crate) struct DeprecatedDeleteItems<'a, T> {
    items: &'a Vec<T>,
    indexes: Vec<usize>,
}

impl<'a, T> DeprecatedDeleteItems<'a, T> {
    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.indexes.iter().filter_map(|i| self.items.get(i - 1))
    }
}

pub(crate) trait DeprecatedDelete {
    async fn delete(&self, db: &mut DBase) -> Result<()>;
}
