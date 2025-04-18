use super::*;

#[derive(Debug)]
pub struct Table {
    pub rows: Vec<Row>,
}

impl Table {
    fn widths(&self) -> Option<Vec<usize>> {
        let mut vec = Vec::new();
        for row in &self.rows {
            match row.widths() {
                None => continue,
                Some(widths) => {
                    if vec.is_empty() {
                        for e in widths {
                            vec.push(e);
                        }
                    } else {
                        for (i, e) in widths.iter().enumerate() {
                            if *e > vec[i] {
                                vec[i] = *e;
                            }
                        }
                    }
                }
            }
        }
        if vec.is_empty() { None } else { Some(vec) }
    }
}

#[derive(Debug)]
pub enum Row {
    Toprule,
    Midrule,
    Bottomrule,
    Head(Vec<Cell>),
    Data(Vec<Cell>),
    Total(Vec<Cell>),
}

impl Row {
    fn widths(&self) -> Option<Vec<usize>> {
        let mut vec = Vec::new();
        match self {
            Row::Head(v) | Row::Data(v) | Row::Total(v) => {
                for cell in v {
                    vec.push(cell.width());
                }
                Some(vec)
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Cell {
    Empty,
    Left(String),
    Right(String),
    Multi(Vec<String>),
}

impl Cell {
    fn width(&self) -> usize {
        match &self {
            Cell::Empty => 0,
            Cell::Left(s) | Cell::Right(s) => s.chars().count(),
            Cell::Multi(v) => {
                let mut width = 0;
                for s in v {
                    let count = s.chars().count();
                    if count > width {
                        width = count;
                    }
                }
                width
            }
        }
    }
}

impl Groups {
    pub fn table(&self) -> Table {
        let mut rows: Vec<Row> = Vec::new();

        rows.push(Row::Toprule);
        rows.push(Row::Head(vec![
            Cell::Left("Nimi".to_string()),
            Cell::Left("Lisätiedot".to_string()),
        ]));

        rows.push(Row::Midrule);

        for group in &self.list {
            rows.push(Row::Data(vec![
                Cell::Left(group.name.clone()),
                Cell::Left(group.description.clone()),
            ]));
        }

        rows.push(Row::Bottomrule);

        Table { rows }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_row_widths() {
        assert_eq!(
            vec![5, 4, 3],
            Row::Data(vec![
                Cell::Left("12345".to_string()),
                Cell::Left("1234".to_string()),
                Cell::Right("123".to_string())
            ])
            .widths()
            .unwrap()
        );

        assert_eq!(
            vec![5, 4, 3],
            Row::Data(vec![
                Cell::Left("€€€€€".to_string()),
                Cell::Left("€€€€".to_string()),
                Cell::Right("€€€".to_string())
            ])
            .widths()
            .unwrap()
        );

        assert_eq!(
            vec![4],
            Row::Data(vec![Cell::Multi(vec![
                "1".to_string(),
                "1234".to_string(),
                "12".to_string(),
            ]),])
            .widths()
            .unwrap()
        );
    }

    #[test]
    fn t_table_widths() {
        let table = Table {
            rows: vec![
                Row::Toprule,
                Row::Head(vec![
                    Cell::Left("12".to_string()),
                    Cell::Left("1".to_string()),
                    Cell::Right("1234".to_string()),
                ]),
                Row::Data(vec![
                    Cell::Left("€".to_string()),
                    Cell::Left("€€".to_string()),
                    Cell::Right("€€€".to_string()),
                ]),
                Row::Data(vec![
                    Cell::Left("€".to_string()),
                    Cell::Left("€€€".to_string()),
                    Cell::Right("€€€€".to_string()),
                ]),
            ],
        };

        assert_eq!(vec![2, 3, 4], table.widths().unwrap());
    }

    #[test]
    fn t_cell_width() {
        assert_eq!(0, Cell::Empty.width());
        assert_eq!(3, Cell::Left("123".to_string()).width());
        assert_eq!(4, Cell::Right("1234".to_string()).width());
        assert_eq!(
            5,
            Cell::Multi(vec!["123".to_string(), "12345".to_string()]).width()
        );
    }
}
