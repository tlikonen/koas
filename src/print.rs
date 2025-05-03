use crate::{
    Output,
    database::{
        Groups, ScoresForAssignment, ScoresForAssignments, ScoresForGroup, ScoresForStudent,
        ScoresForStudents, Stats, Students,
    },
    tools,
};

struct Table {
    rows: Vec<Row>,
}

impl Table {
    fn widths(&self) -> Vec<usize> {
        let mut vec = Vec::with_capacity(10);
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

    fn numbering(mut self) -> Self {
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
        self
    }

    fn print(&self, output: &Output) {
        if self.rows.is_empty() {
            return;
        }
        match output {
            Output::Unicode => print_table(self, TBL_UNICODE),
            Output::UnicodeOpen => print_table(self, TBL_UNICODE_OPEN),
            Output::Ascii => print_table(self, TBL_ASCII),
            Output::AsciiOpen => print_table(self, TBL_ASCII_OPEN),
            Output::Orgmode => print_table(self, TBL_ORGMODE),
            Output::Tab => print_table_tab(self),
            Output::Latex => print_table_latex(self),
        }
    }
}

enum Row {
    Title(String),
    Toprule,
    Midrule,
    Bottomrule,
    Head(Vec<Cell>),
    Data(Vec<Cell>),
    Foot(Vec<Cell>),
}

impl Row {
    fn widths(&self) -> Option<Vec<usize>> {
        let mut vec = Vec::with_capacity(10);
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
        match self {
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
    pub fn print(&self, out: &Output) {
        self.table().print(out);
    }

    fn table(&self) -> Table {
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

impl Students {
    pub fn print(&self, out: &Output) {
        self.table().print(out);
    }

    pub fn print_numbered(&self, out: &Output) {
        self.table().numbering().print(out);
    }

    fn table(&self) -> Table {
        const GROUPS_WIDTH: usize = 36;
        const DESC_WIDTH: usize = 36;

        let mut rows = vec![
            Row::Toprule,
            Row::Head(vec![
                Cell::Left("Sukunimi".to_string()),
                Cell::Left("Etunimi".to_string()),
                Cell::Left("Ryhmät".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ]),
            Row::Midrule,
        ];

        for student in &self.list {
            rows.push(Row::Data(vec![
                Cell::Left(student.lastname.clone()),
                Cell::Left(student.firstname.clone()),
                Cell::Multi(line_split(&student.groups, GROUPS_WIDTH)),
                Cell::Multi(line_split(&student.description, DESC_WIDTH)),
            ]));
        }

        rows.push(Row::Bottomrule);
        Table { rows }
    }
}

impl Groups {
    pub fn print(&self, out: &Output) {
        self.table().print(out);
    }

    pub fn print_numbered(&self, out: &Output) {
        self.table().numbering().print(out);
    }

    fn table(&self) -> Table {
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

impl ScoresForAssignment {
    pub fn print(&self, out: &Output) {
        self.table().print(out);
    }

    pub fn print_numbered(&self, out: &Output) {
        self.table().numbering().print(out);
    }

    fn table(&self) -> Table {
        const DESC_WIDTH: usize = 50;

        let mut rows = vec![
            Row::Title(format!("{s} ({r})", r = self.group, s = self.assignment,)),
            Row::Toprule,
            Row::Head(vec![
                Cell::Left("Oppilas".to_string()),
                Cell::Left("As".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ]),
            Row::Midrule,
        ];

        let mut sum = 0.0;
        let mut count = 0;

        for score in &self.scores {
            rows.push(Row::Data(vec![
                Cell::Left(format!("{}, {}", score.lastname, score.firstname)),
                match &score.score {
                    Some(s) => {
                        if let Some(f) = tools::parse_number(s) {
                            sum += f;
                            count += 1;
                        }
                        Cell::Left(s.clone())
                    }
                    None => Cell::Empty,
                },
                match &score.score_description {
                    Some(s) => Cell::Multi(line_split(s, DESC_WIDTH)),
                    None => Cell::Empty,
                },
            ]));
        }

        let average = if count > 0 {
            Cell::Left(tools::format_decimal(sum / f64::from(count)))
        } else {
            Cell::Empty
        };

        rows.push(Row::Midrule);
        rows.push(Row::Foot(vec![
            Cell::Left("Keskiarvo".to_string()),
            average,
            Cell::Empty,
        ]));
        rows.push(Row::Bottomrule);
        Table { rows }
    }
}

impl ScoresForAssignments {
    pub fn print(&self, out: &Output) {
        for t in &self.list {
            t.print(out);
        }
    }

    pub fn print_numbered(&self, out: &Output) {
        assert!(self.count() == 1);
        self.list[0].print_numbered(out);
    }
}

impl ScoresForStudent {
    pub fn print(&self, out: &Output) {
        self.table().print(out);
    }

    pub fn print_numbered(&self, out: &Output) {
        self.table().numbering().print(out);
    }

    fn table(&self) -> Table {
        const DESC_WIDTH: usize = 50;

        let mut rows = vec![
            Row::Title(format!(
                "{s}, {e} ({r})",
                r = self.group,
                s = self.lastname,
                e = self.firstname,
            )),
            Row::Toprule,
            Row::Head(vec![
                Cell::Left("Suoritus".to_string()),
                Cell::Left("As".to_string()),
                Cell::Left("K".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ]),
            Row::Midrule,
        ];

        let mut sum = 0.0;
        let mut count = 0;

        for score in &self.scores {
            rows.push(Row::Data(vec![
                Cell::Left(score.assignment.clone()),
                match &score.score {
                    Some(s) => {
                        if let Some(f) = tools::parse_number(s) {
                            if let Some(w) = score.weight {
                                sum += f * f64::from(w);
                                count += w;
                            }
                        }
                        Cell::Left(s.clone())
                    }
                    None => Cell::Empty,
                },
                match &score.weight {
                    Some(w) => Cell::Left(w.to_string()),
                    None => Cell::Empty,
                },
                match &score.score_description {
                    Some(s) => Cell::Multi(line_split(s, DESC_WIDTH)),
                    None => Cell::Empty,
                },
            ]));
        }

        let average = if count > 0 {
            Cell::Left(tools::format_decimal(sum / f64::from(count)))
        } else {
            Cell::Empty
        };

        rows.push(Row::Midrule);
        rows.push(Row::Foot(vec![
            Cell::Left("Keskiarvo".to_string()),
            average,
            Cell::Empty,
            Cell::Empty,
        ]));
        rows.push(Row::Bottomrule);
        Table { rows }
    }
}

impl ScoresForStudents {
    pub fn print(&self, out: &Output) {
        for t in &self.list {
            t.print(out);
        }
    }

    pub fn print_numbered(&self, out: &Output) {
        assert!(self.count() == 1);
        self.list[0].print_numbered(out);
    }
}

impl ScoresForGroup {
    pub fn print(&self, out: &Output) {
        self.table().print(out);
        println!();
        self.table_assignments().print(out);
    }

    fn table(&self) -> Table {
        let mut rows = vec![Row::Title(self.group.clone()), Row::Toprule];

        let mut assigns = vec![Cell::Left("Suoritus".to_string())];
        let mut weigths = vec![Cell::Left("Painokerroin".to_string())];
        for assign in &self.assignments {
            assigns.push(Cell::Left(assign.assignment_short.clone()));
            weigths.push(match assign.weight {
                Some(w) => Cell::Left(w.to_string()),
                None => Cell::Empty,
            });
        }

        assigns.push(Cell::Right("ka".to_string()));
        rows.push(Row::Head(assigns));
        weigths.push(Cell::Empty);
        rows.push(Row::Head(weigths));

        rows.push(Row::Midrule);

        let mut total_sum = 0.0;
        let mut total_count = 0;

        let mut vert_sums = Vec::with_capacity(10);
        let mut vert_counts = Vec::with_capacity(10);

        for student in &self.students {
            let mut line = Vec::with_capacity(10);
            line.push(Cell::Left(student.name.clone()));

            let mut horiz_sum = 0.0;
            let mut horiz_count = 0;

            for (c, simple_score) in student.scores.iter().enumerate() {
                if vert_sums.get(c).is_none() {
                    vert_sums.push(0.0);
                    vert_counts.push(0);
                }

                match &simple_score.score {
                    Some(s) => {
                        if let Some(f) = tools::parse_number(s) {
                            if let Some(w) = simple_score.weight {
                                horiz_sum += f * f64::from(w);
                                horiz_count += w;
                            }
                            vert_sums[c] += f;
                            vert_counts[c] += 1;
                        }
                        line.push(Cell::Left(s.clone()));
                    }
                    None => line.push(Cell::Empty),
                }
            }

            let average = if horiz_count > 0 {
                let avg = horiz_sum / f64::from(horiz_count);
                total_sum += avg;
                total_count += 1;
                Cell::Right(tools::format_decimal(avg))
            } else {
                Cell::Empty
            };

            line.push(average);
            rows.push(Row::Data(line));
        }

        rows.push(Row::Midrule);

        let mut totals = Vec::with_capacity(10);
        totals.push(Cell::Left("Keskiarvo".to_string()));

        for (n, sum) in vert_sums.iter().enumerate() {
            let c = vert_counts[n];
            totals.push(if c > 0 {
                Cell::Left(tools::format_decimal(sum / f64::from(c)))
            } else {
                Cell::Empty
            });
        }

        totals.push(if total_count > 0 {
            Cell::Right(tools::format_decimal(total_sum / f64::from(total_count)))
        } else {
            Cell::Empty
        });

        rows.push(Row::Foot(totals));
        rows.push(Row::Bottomrule);
        Table { rows }
    }

    fn table_assignments(&self) -> Table {
        let mut rows = vec![
            Row::Toprule,
            Row::Head(vec![
                Cell::Left("Lyh".to_string()),
                Cell::Left("Suoritus".to_string()),
            ]),
            Row::Midrule,
        ];

        for assign in &self.assignments {
            rows.push(Row::Data(vec![
                Cell::Left(assign.assignment_short.clone()),
                Cell::Left(assign.assignment.clone()),
            ]));
        }

        rows.push(Row::Data(vec![
            Cell::Left("ka".to_string()),
            Cell::Left("Keskiarvo".to_string()),
        ]));

        rows.push(Row::Bottomrule);
        Table { rows }
    }
}

#[rustfmt::skip]
static TBL_UNICODE: [&str; 15] = [
    "╒═", "═", "═╤═", "═╕", // top
    "├─", "─", "─┼─", "─┤", // mid
    "╘═", "═", "═╧═", "═╛", // bottom
    "│ ", " │ ", " │", // vert: left mid right
];

#[rustfmt::skip]
static TBL_UNICODE_OPEN: [&str; 15] = [
    "═", "═", "══", "═", // top
    "─", "─", "──", "─", // mid
    "═", "═", "══", "═", // bottom
    " ", "  ", " ", // vert: left mid right
];

#[rustfmt::skip]
static TBL_ASCII: [&str; 15] = [
    "+-", "-", "-+-", "-+", // top
    "+-", "-", "-+-", "-+", // mid
    "+-", "-", "-+-", "-+", // bottom
    "| ", " | ", " |", // vert: left mid right
];

#[rustfmt::skip]
static TBL_ASCII_OPEN: [&str; 15] = [
    "=", "=", "==", "=", // top
    "-", "-", "--", "-", // mid
    "=", "=", "==", "=", // bottom
    " ", "  ", " ", // vert: left mid right
];

#[rustfmt::skip]
static TBL_ORGMODE: [&str; 15] = [
    "|-", "-", "-+-", "-|", // top
    "|-", "-", "-+-", "-|", // mid
    "|-", "-", "-+-", "-|", // bottom
    "| ", " | ", " |", // vert: left mid right
];

fn print_table(tbl: &Table, tbl_chars: [&str; 15]) {
    let top_left = tbl_chars[0];
    let top_line = tbl_chars[1];
    let top_mid = tbl_chars[2];
    let top_right = tbl_chars[3];

    let mid_left = tbl_chars[4];
    let mid_line = tbl_chars[5];
    let mid_mid = tbl_chars[6];
    let mid_right = tbl_chars[7];

    let bottom_left = tbl_chars[8];
    let bottom_line = tbl_chars[9];
    let bottom_mid = tbl_chars[10];
    let bottom_right = tbl_chars[11];

    let vert_left = tbl_chars[12];
    let vert_mid = tbl_chars[13];
    let vert_right = tbl_chars[14];

    let series = |c, n| {
        for _ in 0..n {
            print!("{c}");
        }
    };

    let empty_cell = |w| {
        series(" ", w);
    };

    let widths = tbl.widths();
    for row in &tbl.rows {
        match row {
            Row::Title(s) => println!("\n{s}\n"),
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
                let mut multi_max = 0;
                let mut multi = 0;
                loop {
                    print!("{vert_left}");
                    for (col, cell) in v.iter().enumerate() {
                        let width = widths[col];
                        match multi {
                            0 => match cell {
                                Cell::Empty => empty_cell(width),
                                Cell::Left(s) => print!("{s:<width$}"),
                                Cell::Right(s) => print!("{s:>width$}"),
                                Cell::Multi(v) => {
                                    if let Some(s) = v.get(multi) {
                                        print!("{s:<width$}");
                                    } else {
                                        empty_cell(width);
                                    }
                                    if v.len() > multi_max {
                                        multi_max = v.len();
                                    }
                                }
                            },
                            _ => match cell {
                                Cell::Multi(v) => {
                                    if let Some(s) = v.get(multi) {
                                        print!("{s:<width$}");
                                    } else {
                                        empty_cell(width);
                                    }
                                }
                                _ => empty_cell(width),
                            },
                        }
                        if widths.get(col + 1).is_some() {
                            print!("{vert_mid}");
                        } else {
                            print!("{vert_right}");
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
            Row::Title(s) => println!("\n{s}\n"),
            Row::Head(v) | Row::Data(v) | Row::Foot(v) => {
                for (col, cell) in v.iter().enumerate() {
                    if col > 0 {
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

fn print_table_latex(tbl: &Table) {
    for row in &tbl.rows {
        match row {
            Row::Title(s) => println!("\n{s}\n"),
            Row::Head(v) | Row::Data(v) | Row::Foot(v) => {
                print!("\\rivi");
                for cell in v {
                    match cell {
                        Cell::Empty => print!("{{}}"),
                        Cell::Left(s) | Cell::Right(s) => print!("{{{s}}}"),
                        Cell::Multi(v) => print!("{{{}}}", v.join(" ")),
                    }
                }
                println!();
            }
            _ => (),
        }
    }
}

fn line_split(s: &str, max: usize) -> Vec<String> {
    let mut lines = Vec::with_capacity(20);
    let mut line = String::with_capacity(60);

    for word in tools::words_iter(s) {
        if line.is_empty() {
            line.push_str(word);
        } else if line.chars().count() + word.chars().count() < max {
            line.push(' ');
            line.push_str(word);
        } else {
            let l = line.len();
            lines.push(line);
            line = String::with_capacity(l);
            line.push_str(word);
        }
    }

    lines.push(line);
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
        assert_eq!(vec![""], line_split("", 15));
    }
}
