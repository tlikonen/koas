use crate::{
    Output,
    database::{Groups, Stats},
};

pub struct Table {
    rows: Vec<Row>,
}

impl Table {
    fn widths(&self) -> Vec<usize> {
        let mut vec = Vec::new();
        for row in &self.rows {
            match row.widths() {
                None => continue,
                Some(widths) => {
                    for (i, e) in widths.iter().enumerate() {
                        if vec.get(i).is_none() {
                            vec.push(*e);
                        } else if *e > vec[i] {
                            vec[i] = *e;
                        }
                    }
                }
            }
        }
        vec
    }

    pub fn numbering(&mut self) {
        let mut n = 1;
        for row in &mut self.rows {
            match row {
                Row::Head(v) | Row::Foot(v) => v.insert(0, Cell::Empty),
                Row::Data(v) => {
                    v.insert(0, Cell::Right(n.to_string()));
                    n += 1;
                }
                _ => (),
            }
        }
    }

    pub fn print(&self, output: &Output) {
        if self.rows.is_empty() {
            return;
        }
        match output {
            Output::Unicode => print_table(self, BOX_UNICODE),
            Output::Ascii => print_table(self, BOX_ASCII),
            Output::Orgmode => print_table(self, BOX_ORGMODE),
            Output::Tab => print_table_tab(self),
        }
    }
}

enum Row {
    Toprule,
    Midrule,
    Bottomrule,
    Head(Vec<Cell>),
    Data(Vec<Cell>),
    Foot(Vec<Cell>),
}

impl Row {
    fn widths(&self) -> Option<Vec<usize>> {
        let mut vec = Vec::new();
        match self {
            Row::Head(v) | Row::Data(v) | Row::Foot(v) => {
                for cell in v {
                    vec.push(cell.width());
                }
                Some(vec)
            }
            _ => None,
        }
    }
}

enum Cell {
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

impl Stats {
    pub fn table(&self) -> Table {
        let rows = vec![
            Row::Toprule,
            Row::Data(vec![
                Cell::Left("Oppilaita:".to_string()),
                Cell::Right(self.students.to_string()),
            ]),
            Row::Data(vec![
                Cell::Left("Ryhmiä:".to_string()),
                Cell::Right(self.groups.to_string()),
            ]),
            Row::Data(vec![
                Cell::Left("Suorituksia:".to_string()),
                Cell::Right(self.assignments.to_string()),
            ]),
            Row::Data(vec![
                Cell::Left("Arvosanoja:".to_string()),
                Cell::Right(self.scores.to_string()),
            ]),
            Row::Bottomrule,
        ];

        Table { rows }
    }
}

impl Groups {
    pub fn table(&self) -> Table {
        const DESCRIPTION_WIDTH: usize = 70;

        let mut rows = vec![
            Row::Toprule,
            Row::Head(vec![
                Cell::Left("Nimi".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ]),
            Row::Midrule,
        ];

        for group in &self.list {
            rows.push(Row::Data(vec![
                Cell::Left(group.name.clone()),
                Cell::Multi(line_split(&group.description, DESCRIPTION_WIDTH)),
            ]));
        }

        rows.push(Row::Bottomrule);
        Table { rows }
    }
}

static BOX_UNICODE: [char; 13] = [
    '╒', '═', '╤', '╕', // top
    '├', '─', '┼', '┤', // mid
    '╘', '═', '╧', '╛', // bottom
    '│', // vert
];

static BOX_ASCII: [char; 13] = [
    '+', '-', '+', '+', // top
    '+', '-', '+', '+', // mid
    '+', '-', '+', '+', // bottom
    '|', // vert
];

static BOX_ORGMODE: [char; 13] = [
    '|', '-', '+', '|', // top
    '|', '-', '+', '|', // mid
    '|', '-', '+', '|', // bottom
    '|', // vert
];

fn print_table(tbl: &Table, boxes: [char; 13]) {
    let top_left = boxes[0];
    let top_line = boxes[1];
    let top_mid = boxes[2];
    let top_right = boxes[3];

    let mid_left = boxes[4];
    let mid_line = boxes[5];
    let mid_mid = boxes[6];
    let mid_right = boxes[7];

    let bottom_left = boxes[8];
    let bottom_line = boxes[9];
    let bottom_mid = boxes[10];
    let bottom_right = boxes[11];

    let vert_line = boxes[12];

    let series = |c, n| {
        for _ in 0..(n + 2) {
            print!("{c}");
        }
    };

    let widths = tbl.widths();
    for row in &tbl.rows {
        match row {
            Row::Toprule => {
                print!("{top_left}");
                for i in 0..widths.len() {
                    series(top_line, widths[i]);
                    if widths.get(i + 1).is_some() {
                        print!("{top_mid}");
                    } else {
                        print!("{top_right}");
                    }
                }
                println!();
            }
            Row::Midrule => {
                print!("{mid_left}");
                for i in 0..widths.len() {
                    series(mid_line, widths[i]);
                    if widths.get(i + 1).is_some() {
                        print!("{mid_mid}");
                    } else {
                        print!("{mid_right}");
                    }
                }
                println!();
            }
            Row::Bottomrule => {
                print!("{bottom_left}");
                for i in 0..widths.len() {
                    series(bottom_line, widths[i]);
                    if widths.get(i + 1).is_some() {
                        print!("{bottom_mid}");
                    } else {
                        print!("{bottom_right}");
                    }
                }
                println!();
            }
            Row::Data(v) | Row::Head(v) | Row::Foot(v) => {
                let empty_cell = |w| {
                    series(' ', w);
                    print!("{vert_line}");
                };
                let mut multi_max = 0;
                let mut multi = 0;
                loop {
                    print!("{vert_line}");
                    for (w, cell) in v.iter().enumerate() {
                        let width = widths[w];
                        match multi {
                            0 => match cell {
                                Cell::Empty => empty_cell(width),
                                Cell::Left(s) => print!(" {s:<width$} {vert_line}"),
                                Cell::Right(s) => print!(" {s:>width$} {vert_line}"),
                                Cell::Multi(v) => {
                                    print!(" {:<width$} {vert_line}", v[multi]);
                                    if v.len() > multi_max {
                                        multi_max = v.len();
                                    }
                                }
                            },
                            _ => match cell {
                                Cell::Multi(v) => {
                                    if let Some(s) = v.get(multi) {
                                        print!(" {s:<width$} {vert_line}");
                                    } else {
                                        empty_cell(width);
                                    }
                                }
                                _ => empty_cell(width),
                            },
                        }
                    }
                    println!();
                    multi += 1;
                    if multi >= multi_max {
                        break;
                    }
                }
            }
        }
    }
}

fn print_table_tab(tbl: &Table) {
    for row in &tbl.rows {
        match row {
            Row::Head(v) | Row::Data(v) | Row::Foot(v) => {
                for (n, cell) in v.iter().enumerate() {
                    if n > 0 {
                        print!("\t");
                    }
                    match cell {
                        Cell::Left(s) | Cell::Right(s) => print!("{s}"),
                        Cell::Multi(v) => print!("{}", v.join(" ")),
                        _ => (),
                    }
                }
                println!();
            }
            _ => (),
        }
    }
}

fn line_split(s: &str, max: usize) -> Vec<String> {
    let words: Vec<&str> = s.split(' ').filter(|x| !x.is_empty()).collect();
    let mut lines = Vec::new();
    let mut line = Vec::new();
    let mut i = 0;

    loop {
        if words.get(i).is_none() {
            break;
        }

        if line.is_empty() || line.join(" ").chars().count() + words[i].chars().count() < max {
            line.push(words[i]);
            if words.get(i + 1).is_none() {
                lines.push(line.join(" "));
            }
            i += 1;
        } else {
            lines.push(line.join(" "));
            line.clear();
        }
    }

    lines
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

        assert_eq!(vec![2, 3, 4], table.widths());
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

    #[test]
    fn t_line_split() {
        for i in 0..8 {
            assert_eq!(
                vec!["€ka", "tøka", "kølmas"],
                line_split("€ka tøka kølmas", i)
            );
        }

        for i in 8..15 {
            assert_eq!(vec!["€ka tøka", "kølmas"], line_split("€ka tøka kølmas", i));
        }
        assert_eq!(vec!["€ka tøka kølmas"], line_split("€ka tøka kølmas", 15));
    }
}
