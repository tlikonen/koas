use crate::database::*;
use crate::prelude::*;
use crate::tools::FloatExt;
use crate::tools::StrExt;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::io;
use std::io::BufWriter;
use std::io::Write as _;

type OutBuf = BufWriter<io::Stdout>;

fn output_buffer() -> OutBuf {
    BufWriter::new(io::stdout())
}

const GROUPS_WIDTH: usize = 42;

#[non_exhaustive]
#[derive(Default)]
pub enum Output {
    #[default]
    Unicode,
    UnicodeOpen,
    Ascii,
    AsciiOpen,
    Orgmode,
    Tab,
    Csv,
    Latex,
    Html,
}

impl Output {
    pub fn select(value: &str) -> Result<Self> {
        let out = match value {
            "unicode" | "u" => Self::Unicode,
            "unicode-avoin" | "ua" => Self::UnicodeOpen,
            "ascii" | "a" => Self::Ascii,
            "ascii-avoin" | "aa" => Self::AsciiOpen,
            "emacs" => Self::Orgmode,
            "tab" => Self::Tab,
            "csv" => Self::Csv,
            "latex" => Self::Latex,
            "html" => Self::Html,
            _ => return Err(Error::unknown_tbl(value)),
        };
        Ok(out)
    }
}

pub trait MakeTable {
    fn table(&self) -> Table;
}

pub trait PrintQuery {
    fn print(&self, out: &Output) -> Result<()>;
}

impl<T: MakeTable> PrintQuery for T {
    fn print(&self, out: &Output) -> Result<()> {
        let mut stream = output_buffer();
        self.table().print_tbl(out, &mut stream)?;
        stream.flush()?;
        Ok(())
    }
}

pub trait PrintQueryNum {
    fn print_num(&self, out: &Output) -> Result<()>;
}

impl<T: MakeTable> PrintQueryNum for T {
    fn print_num(&self, out: &Output) -> Result<()> {
        let mut stream = output_buffer();
        self.table().numbering().print_tbl(out, &mut stream)?;
        stream.flush()?;
        Ok(())
    }
}

#[derive(Default)]
pub struct Table {
    title: Option<String>,
    rows: Vec<Row>,
}

impl Table {
    pub fn title(&self) -> Option<&String> {
        self.title.as_ref()
    }

    pub fn rows(&self) -> impl Iterator<Item = &Row> {
        self.rows.iter()
    }

    pub fn rows_mut(&mut self) -> impl Iterator<Item = &mut Row> {
        self.rows.iter_mut()
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    fn from(title: impl ToString, rows: Vec<Row>) -> Self {
        Self {
            title: Some(title.to_string()),
            rows,
        }
    }

    fn from_rows(rows: Vec<Row>) -> Self {
        Self {
            rows,
            ..Self::default()
        }
    }

    fn count_columns(&self) -> usize {
        let mut columns = 0;
        for row in self.rows() {
            match row {
                Row::Head(cells) | Row::Data(cells) | Row::Foot(cells) if cells.len() > columns => {
                    columns = cells.len();
                }
                _ => (),
            }
        }
        columns
    }

    pub fn widths(&self) -> Vec<usize> {
        let mut vec = Vec::with_capacity(10);
        for row in self.rows() {
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
        for row in self.rows_mut() {
            match row {
                Row::Head(v) | Row::Foot(v) => v.push_front(Cell::Empty),
                Row::Data(v) => {
                    v.push_front(Cell::Right(n.to_string()));
                    n += 1;
                }
                _ => (),
            }
        }
        self
    }

    pub fn print(&self, output: &Output) -> Result<()> {
        let mut stream = output_buffer();
        self.print_tbl(output, &mut stream)?;
        stream.flush()?;
        Ok(())
    }

    fn print_tbl(&self, output: &Output, stream: &mut OutBuf) -> Result<()> {
        if self.is_empty() {
            return Ok(());
        }
        match output {
            Output::Unicode => print_table(self, stream, TBL_UNICODE),
            Output::UnicodeOpen => print_table(self, stream, TBL_UNICODE_OPEN),
            Output::Ascii => print_table(self, stream, TBL_ASCII),
            Output::AsciiOpen => print_table(self, stream, TBL_ASCII_OPEN),
            Output::Orgmode => print_table(self, stream, TBL_ORGMODE),
            Output::Tab => print_table_tab(self, stream),
            Output::Csv => print_table_csv(self, stream),
            Output::Latex => print_table_latex(self, stream),
            Output::Html => print_table_html(self, stream),
        }
    }
}

pub enum Row {
    Toprule,
    Midrule,
    Bottomrule,
    Head(VecDeque<Cell>),
    Data(VecDeque<Cell>),
    Foot(VecDeque<Cell>),
}

impl Row {
    pub fn widths(&self) -> Option<Vec<usize>> {
        let mut vec = Vec::with_capacity(10);
        match self {
            Self::Head(v) | Self::Data(v) | Self::Foot(v) => {
                for cell in v {
                    vec.push(cell.width());
                }
                Some(vec)
            }
            _ => None,
        }
    }
}

pub enum Cell {
    Empty,
    Left(String),
    Right(String),
    Multi(Vec<String>),
    Proportion {
        proportion: f64,
        width: usize,
        width_max: usize,
    },
}

impl Cell {
    pub fn width(&self) -> usize {
        match self {
            Self::Empty => 0,
            Self::Left(s) | Self::Right(s) => s.chars().count(),
            Self::Multi(v) => {
                let mut width = 0;
                for s in v {
                    let count = s.chars().count();
                    if count > width {
                        width = count;
                    }
                }
                width
            }
            Self::Proportion { width, .. } => *width,
        }
    }
}

impl MakeTable for Stats {
    fn table(&self) -> Table {
        let rows = vec![
            Row::Toprule,
            Row::Data(VecDeque::from([
                Cell::Left("Oppilaita:".to_string()),
                Cell::Right(self.students.to_string()),
            ])),
            Row::Data(VecDeque::from([
                Cell::Left("Ryhmiä:".to_string()),
                Cell::Right(self.groups.to_string()),
            ])),
            Row::Data(VecDeque::from([
                Cell::Left("Suorituksia:".to_string()),
                Cell::Right(self.assignments.to_string()),
            ])),
            Row::Data(VecDeque::from([
                Cell::Left("Arvosanoja:".to_string()),
                Cell::Right(self.grades.to_string()),
            ])),
            Row::Bottomrule,
        ];

        Table::from_rows(rows)
    }
}

impl MakeTable for QueryList<Student> {
    fn table(&self) -> Table {
        const DESC_WIDTH: usize = 36;

        let mut rows = vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Left("Sukunimi".to_string()),
                Cell::Left("Etunimi".to_string()),
                Cell::Left("Ryhmät".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ])),
            Row::Midrule,
        ];

        for student in self.iter() {
            rows.push(Row::Data(VecDeque::from([
                Cell::Left(student.lastname().to_string()),
                Cell::Left(student.firstname().to_string()),
                Cell::Multi(column_lines(student.groups(), GROUPS_WIDTH)),
                Cell::Multi(column_lines(
                    student.description().split_whitespace(),
                    DESC_WIDTH,
                )),
            ])));
        }

        rows.push(Row::Bottomrule);
        Table::from_rows(rows)
    }
}

impl MakeTable for QueryList<Group> {
    fn table(&self) -> Table {
        const DESCRIPTION_WIDTH: usize = 70;

        let mut rows = vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Left("Ryhmä".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ])),
            Row::Midrule,
        ];

        for group in self.iter() {
            rows.push(Row::Data(VecDeque::from([
                Cell::Left(group.name().to_string()),
                Cell::Multi(column_lines(
                    group.description().split_whitespace(),
                    DESCRIPTION_WIDTH,
                )),
            ])));
        }

        rows.push(Row::Bottomrule);
        Table::from_rows(rows)
    }
}

impl MakeTable for AssignmentsForGroup {
    fn table(&self) -> Table {
        let mut rows = vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Left("Suoritus".to_string()),
                Cell::Left("Lyh".to_string()),
                Cell::Right("K".to_string()),
            ])),
            Row::Midrule,
        ];

        for assign in self {
            rows.push(Row::Data(VecDeque::from([
                Cell::Left(assign.name().to_string()),
                Cell::Left(assign.short().to_string()),
                match assign.weight() {
                    Some(w) => Cell::Right(w.to_string()),
                    None => Cell::Empty,
                },
            ])));
        }

        rows.push(Row::Bottomrule);
        Table::from(self.group(), rows)
    }
}

impl PrintQuery for QueryList<AssignmentsForGroup> {
    fn print(&self, out: &Output) -> Result<()> {
        let mut stream = output_buffer();
        for t in self.iter() {
            t.table().print_tbl(out, &mut stream)?;
        }
        stream.flush()?;
        Ok(())
    }
}

impl MakeTable for GradesForAssignment {
    fn table(&self) -> Table {
        const DESC_WIDTH: usize = 50;

        let mut rows = vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Left("Oppilas".to_string()),
                Cell::Left("As".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ])),
            Row::Midrule,
        ];

        let mut sum = 0.0;
        let mut count = 0;

        for grade in self {
            rows.push(Row::Data(VecDeque::from([
                Cell::Left(grade.fullname()),
                match grade.grade() {
                    Some(s) => {
                        if let Some(f) = s.float() {
                            sum += f;
                            count += 1;
                        }
                        Cell::Left(s.clone())
                    }
                    None => Cell::Empty,
                },
                match grade.description() {
                    Some(s) => Cell::Multi(column_lines(s.split_whitespace(), DESC_WIDTH)),
                    None => Cell::Empty,
                },
            ])));
        }

        let average = if count > 0 {
            Cell::Left((sum / f64::from(count)).format_decimal())
        } else {
            Cell::Empty
        };

        rows.push(Row::Midrule);
        rows.push(Row::Foot(VecDeque::from([
            Cell::Left("Keskiarvo".to_string()),
            average,
            Cell::Empty,
        ])));
        rows.push(Row::Bottomrule);

        Table::from(
            format!("{s} ({r})", r = self.group(), s = self.assignment()),
            rows,
        )
    }
}

impl PrintQuery for QueryList<GradesForAssignment> {
    fn print(&self, out: &Output) -> Result<()> {
        let mut stream = output_buffer();
        for t in self.iter() {
            t.table().print_tbl(out, &mut stream)?;
        }
        stream.flush()?;
        Ok(())
    }
}

impl MakeTable for GradesForStudent {
    fn table(&self) -> Table {
        const DESC_WIDTH: usize = 50;

        let mut rows = vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Left("Suoritus".to_string()),
                Cell::Left("As".to_string()),
                Cell::Left("K".to_string()),
                Cell::Left("Lisätiedot".to_string()),
            ])),
            Row::Midrule,
        ];

        let mut sum = 0.0;
        let mut count = 0;

        for grade in self {
            rows.push(Row::Data(VecDeque::from([
                Cell::Left(grade.assignment().to_string()),
                match grade.grade() {
                    Some(s) => {
                        if let Some(f) = s.float()
                            && let Some(w) = grade.weight()
                        {
                            sum += f * f64::from(w);
                            count += w;
                        }
                        Cell::Left(s.clone())
                    }
                    None => Cell::Empty,
                },
                match grade.weight() {
                    Some(w) => Cell::Left(w.to_string()),
                    None => Cell::Empty,
                },
                match grade.description() {
                    Some(s) => Cell::Multi(column_lines(s.split_whitespace(), DESC_WIDTH)),
                    None => Cell::Empty,
                },
            ])));
        }

        let average = if count > 0 {
            Cell::Left((sum / f64::from(count)).format_decimal())
        } else {
            Cell::Empty
        };

        rows.push(Row::Midrule);
        rows.push(Row::Foot(VecDeque::from([
            Cell::Left("Keskiarvo".to_string()),
            average,
            Cell::Empty,
            Cell::Empty,
        ])));
        rows.push(Row::Bottomrule);
        Table::from(format!("{} ({})", self.fullname(), self.group()), rows)
    }
}

impl PrintQuery for QueryList<GradesForStudent> {
    fn print(&self, out: &Output) -> Result<()> {
        let mut stream = output_buffer();
        for t in self.iter() {
            t.table().print_tbl(out, &mut stream)?;
        }
        stream.flush()?;
        Ok(())
    }
}

impl MakeTable for GradesForGroup {
    fn table(&self) -> Table {
        let mut rows = vec![Row::Toprule];

        let mut assigns = vec![Cell::Left("Suoritus".to_string())];
        let mut weigths = vec![Cell::Left("Painokerroin".to_string())];
        for assign in self.assignments() {
            assigns.push(Cell::Left(assign.short().to_string()));
            weigths.push(match assign.weight() {
                Some(w) => Cell::Left(w.to_string()),
                None => Cell::Empty,
            });
        }

        assigns.push(Cell::Left("ka".to_string()));
        rows.push(Row::Head(VecDeque::from(assigns)));
        weigths.push(Cell::Empty);
        rows.push(Row::Head(VecDeque::from(weigths)));

        rows.push(Row::Midrule);

        let mut total_sum = 0.0;
        let mut total_count = 0;

        let mut vert_sums = Vec::with_capacity(10);
        let mut vert_counts = Vec::with_capacity(10);

        for student in self.students() {
            let mut line = Vec::with_capacity(10);
            line.push(Cell::Left(student.fullname()));

            let mut horiz_sum = 0.0;
            let mut horiz_count = 0;

            for (c, simple_grade) in student.grades().enumerate() {
                if vert_sums.get(c).is_none() {
                    vert_sums.push(0.0);
                    vert_counts.push(0);
                }

                match simple_grade.grade() {
                    Some(s) => {
                        if let Some(f) = s.float() {
                            if let Some(w) = simple_grade.weight() {
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
                Cell::Right(avg.format_decimal())
            } else {
                Cell::Empty
            };

            line.push(average);
            rows.push(Row::Data(VecDeque::from(line)));
        }

        rows.push(Row::Midrule);

        let mut totals = Vec::with_capacity(10);
        totals.push(Cell::Left("Keskiarvo".to_string()));

        for (n, sum) in vert_sums.iter().enumerate() {
            let c = vert_counts[n];
            totals.push(if c > 0 {
                Cell::Left((sum / f64::from(c)).format_decimal())
            } else {
                Cell::Empty
            });
        }

        totals.push(if total_count > 0 {
            Cell::Right((total_sum / f64::from(total_count)).format_decimal())
        } else {
            Cell::Empty
        });

        rows.push(Row::Foot(VecDeque::from(totals)));
        rows.push(Row::Bottomrule);
        Table::from(self.group(), rows)
    }
}

impl PrintQuery for QueryList<GradesForGroup> {
    fn print(&self, out: &Output) -> Result<()> {
        let mut stream = output_buffer();

        for tbl in self.iter() {
            tbl.table().print_tbl(out, &mut stream)?;
            writeln!(stream)?;

            // Table of assignments
            let mut rows = vec![
                Row::Toprule,
                Row::Head(VecDeque::from([
                    Cell::Left("Lyh".to_string()),
                    Cell::Left("Suoritus".to_string()),
                ])),
                Row::Midrule,
            ];

            for assign in tbl.assignments() {
                rows.push(Row::Data(VecDeque::from([
                    Cell::Left(assign.short().to_string()),
                    Cell::Left(assign.name().to_string()),
                ])));
            }

            rows.push(Row::Data(VecDeque::from([
                Cell::Left("ka".to_string()),
                Cell::Left("Keskiarvo".to_string()),
            ])));

            rows.push(Row::Bottomrule);
            Table::from_rows(rows).print_tbl(out, &mut stream)?;
        }

        stream.flush()?;
        Ok(())
    }
}

struct Rankline {
    name: String,
    groups: Vec<String>,
    average: f64,
    count: usize,
}

impl MakeTable for StudentRanking {
    fn table(&self) -> Table {
        let mut rows: Vec<Row> = vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Empty,
                Cell::Left("Oppilas".to_string()),
                Cell::Left("Ryhmät".to_string()),
                Cell::Left("Ka".to_string()),
                Cell::Left("Lkm".to_string()),
            ])),
            Row::Midrule,
        ];

        let mut total_sum = 0.0;
        let mut total_count = 0;

        let mut list: Vec<Rankline> = Vec::with_capacity(30);
        for student in self.data.values() {
            let avg = student.sum / f64::from(student.count);

            list.push(Rankline {
                name: student.name.clone(),
                groups: student.groups.clone(),
                average: avg,
                count: student.grade_count,
            });

            total_sum += avg;
            total_count += 1;
        }

        list.sort_by(|left, right| match right.average.total_cmp(&left.average) {
            Ordering::Equal => left.name.cmp(&right.name),
            ord => ord,
        });

        let mut average_last = 0.0;
        for (n, student) in (1..).zip(list) {
            rows.push(Row::Data(VecDeque::from([
                if student.average == average_last {
                    Cell::Empty
                } else {
                    Cell::Right(format!("{}.", n))
                },
                Cell::Left(student.name.clone()),
                Cell::Multi(column_lines(student.groups, GROUPS_WIDTH)),
                Cell::Right(student.average.format_decimal()),
                Cell::Right(student.count.to_string()),
            ])));
            average_last = student.average;
        }

        rows.push(Row::Midrule);
        rows.push(Row::Foot(VecDeque::from([
            Cell::Empty,
            Cell::Left("Keskiarvo".to_string()),
            Cell::Empty,
            if total_count > 0 {
                Cell::Right((total_sum / f64::from(total_count)).format_decimal())
            } else {
                Cell::Empty
            },
            Cell::Empty,
        ])));
        rows.push(Row::Bottomrule);

        Table::from_rows(rows)
    }
}

impl MakeTable for GradeDistribution {
    fn table(&self) -> Table {
        const BAR_WIDTH: f64 = 40.0;

        static GRADE_SERIES_1: [&str; 7] = ["4", "5", "6", "7", "8", "9", "10"];
        static GRADE_SERIES_4: [&str; 25] = [
            "4", "4+", "4½", "5−", "5", "5+", "5½", "6−", "6", "6+", "6½", "7−", "7", "7+", "7½",
            "8−", "8", "8+", "8½", "9−", "9", "9+", "9½", "10−", "10",
        ];

        let mut rows = vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Left("As".to_string()),
                Cell::Left("Lkm".to_string()),
                Cell::Empty,
            ])),
            Row::Midrule,
        ];

        let mut integer_only = true;
        let mut highest_count: i32 = 0;
        for (grade, count) in &self.data {
            let gr = grade.as_str();
            if !GRADE_SERIES_4.contains(&gr) {
                continue;
            }

            if !GRADE_SERIES_1.contains(&gr) {
                integer_only = false;
            }

            if *count > highest_count {
                highest_count = *count;
            }
        }

        for grade in GRADE_SERIES_4 {
            if integer_only && !GRADE_SERIES_1.contains(&grade) {
                continue;
            }

            if let Some(count) = self.data.get(grade) {
                let prop = f64::from(*count) / f64::from(highest_count);
                let char_count = (prop * BAR_WIDTH).round() as usize;

                rows.push(Row::Data(VecDeque::from([
                    Cell::Left(grade.to_string()),
                    Cell::Right(count.to_string()),
                    Cell::Proportion {
                        proportion: prop,
                        width: char_count,
                        width_max: BAR_WIDTH as usize,
                    },
                ])));
            } else {
                rows.push(Row::Data(VecDeque::from([
                    Cell::Left(grade.to_string()),
                    Cell::Right("0".to_string()),
                    Cell::Empty,
                ])));
            }
        }

        rows.push(Row::Bottomrule);
        Table::from_rows(rows)
    }
}

const TBL_ARRAY_LENGTH: usize = 16;

#[rustfmt::skip]
static TBL_UNICODE: [&str; TBL_ARRAY_LENGTH] = [
    "╒═", "═", "═╤═", "═╕", // top
    "├─", "─", "─┼─", "─┤", // mid
    "╘═", "═", "═╧═", "═╛", // bottom
    "│ ", " │ ", " │", // vert: left mid right
    "◼", // box
];

#[rustfmt::skip]
static TBL_UNICODE_OPEN: [&str; TBL_ARRAY_LENGTH] = [
    "═", "═", "══", "═", // top
    "─", "─", "──", "─", // mid
    "═", "═", "══", "═", // bottom
    " ", "  ", " ", // vert: left mid right
    "◼", // box
];

#[rustfmt::skip]
static TBL_ASCII: [&str; TBL_ARRAY_LENGTH] = [
    "+-", "-", "-+-", "-+", // top
    "+-", "-", "-+-", "-+", // mid
    "+-", "-", "-+-", "-+", // bottom
    "| ", " | ", " |", // vert: left mid right
    "#", // box
];

#[rustfmt::skip]
static TBL_ASCII_OPEN: [&str; TBL_ARRAY_LENGTH] = [
    "=", "=", "==", "=", // top
    "-", "-", "--", "-", // mid
    "=", "=", "==", "=", // bottom
    " ", "  ", " ", // vert: left mid right
    "#", // box
];

#[rustfmt::skip]
static TBL_ORGMODE: [&str; TBL_ARRAY_LENGTH] = [
    "|-", "-", "-+-", "-|", // top
    "|-", "-", "-+-", "-|", // mid
    "|-", "-", "-+-", "-|", // bottom
    "| ", " | ", " |", // vert: left mid right
    "#", // box
];

fn print_table(
    tbl: &Table,
    stream: &mut OutBuf,
    tbl_chars: [&str; TBL_ARRAY_LENGTH],
) -> Result<()> {
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

    let box_char = tbl_chars[15];

    let series = |stream: &mut OutBuf, s: &str, n: usize| -> Result<()> {
        for _ in 0..n {
            write!(stream, "{s}")?;
        }
        Ok(())
    };

    let empty_cell = |stream: &mut OutBuf, w: usize| -> Result<()> { series(stream, " ", w) };

    let widths = tbl.widths();

    if let Some(title) = tbl.title() {
        writeln!(stream, "\n{title}\n")?;
    }

    for row in tbl.rows() {
        match row {
            Row::Toprule => {
                write!(stream, "{top_left}")?;
                for i in 0..widths.len() {
                    series(stream, top_line, widths[i])?;
                    if widths.get(i + 1).is_some() {
                        write!(stream, "{top_mid}")?;
                    } else {
                        write!(stream, "{top_right}")?;
                    }
                }
                writeln!(stream)?;
            }

            Row::Midrule => {
                write!(stream, "{mid_left}")?;
                for i in 0..widths.len() {
                    series(stream, mid_line, widths[i])?;
                    if widths.get(i + 1).is_some() {
                        write!(stream, "{mid_mid}")?;
                    } else {
                        write!(stream, "{mid_right}")?;
                    }
                }
                writeln!(stream)?;
            }

            Row::Bottomrule => {
                write!(stream, "{bottom_left}")?;
                for i in 0..widths.len() {
                    series(stream, bottom_line, widths[i])?;
                    if widths.get(i + 1).is_some() {
                        write!(stream, "{bottom_mid}")?;
                    } else {
                        write!(stream, "{bottom_right}")?;
                    }
                }
                writeln!(stream)?;
            }

            Row::Data(v) | Row::Head(v) | Row::Foot(v) => {
                let mut multi_max = 0;
                let mut multi = 0;
                loop {
                    write!(stream, "{vert_left}")?;
                    for (col, cell) in v.iter().enumerate() {
                        let width = widths[col];
                        match multi {
                            0 => match cell {
                                Cell::Empty => empty_cell(stream, width)?,

                                Cell::Left(s) => {
                                    write!(stream, "{s:<width$}")?;
                                }

                                Cell::Right(s) => {
                                    write!(stream, "{s:>width$}")?;
                                }

                                Cell::Multi(v) => {
                                    if let Some(s) = v.get(multi) {
                                        write!(stream, "{s:<width$}")?;
                                    } else {
                                        empty_cell(stream, width)?;
                                    }
                                    if v.len() > multi_max {
                                        multi_max = v.len();
                                    }
                                }

                                Cell::Proportion { width: n, .. } => {
                                    series(stream, box_char, *n)?;
                                    empty_cell(stream, width - n)?;
                                }
                            },

                            _ => match cell {
                                Cell::Multi(v) => {
                                    if let Some(s) = v.get(multi) {
                                        write!(stream, "{s:<width$}")?;
                                    } else {
                                        empty_cell(stream, width)?;
                                    }
                                }
                                _ => empty_cell(stream, width)?,
                            },
                        }
                        if widths.get(col + 1).is_some() {
                            write!(stream, "{vert_mid}")?;
                        } else {
                            write!(stream, "{vert_right}")?;
                        }
                    }
                    writeln!(stream)?;
                    multi += 1;
                    if multi >= multi_max {
                        break;
                    }
                }
            }
        }
    }
    Ok(())
}

fn print_table_tab(tbl: &Table, stream: &mut OutBuf) -> Result<()> {
    if let Some(title) = tbl.title() {
        writeln!(stream, "\n{title}\n")?;
    }

    for row in tbl.rows() {
        match row {
            Row::Head(v) | Row::Data(v) | Row::Foot(v) => {
                for (col, cell) in v.iter().enumerate() {
                    if col > 0 {
                        write!(stream, "\t")?;
                    }
                    match cell {
                        Cell::Empty => (),
                        Cell::Left(s) | Cell::Right(s) => {
                            write!(stream, "{s}")?;
                        }
                        Cell::Multi(v) => {
                            write!(stream, "{}", v.join(" "))?;
                        }
                        Cell::Proportion { width: n, .. } => write!(stream, "{}", "#".repeat(*n))?,
                    }
                }
                writeln!(stream)?;
            }
            _ => (),
        }
    }
    Ok(())
}

fn print_table_csv(tbl: &Table, stream: &mut OutBuf) -> Result<()> {
    if let Some(title) = &tbl.title {
        writeln!(stream, "\n{title}\n")?;
    }

    for row in tbl.rows() {
        match row {
            Row::Head(v) | Row::Data(v) | Row::Foot(v) => {
                for (col, cell) in v.iter().enumerate() {
                    if col > 0 {
                        write!(stream, ",")?;
                    }
                    match cell {
                        Cell::Empty => (),

                        Cell::Left(s) | Cell::Right(s) => {
                            if s.chars().all(|c| c.is_ascii_digit()) {
                                write!(stream, "{s}")?;
                            } else {
                                write!(stream, "{s:?}")?;
                            }
                        }

                        Cell::Multi(v) => {
                            let s = v.join(" ");
                            if s.chars().all(|c| c.is_ascii_digit()) {
                                write!(stream, "{s}")?;
                            } else {
                                write!(stream, "{s:?}")?;
                            }
                        }

                        Cell::Proportion { width: n, .. } => {
                            write!(stream, "{:?}", "#".repeat(*n))?
                        }
                    }
                }
                writeln!(stream)?;
            }
            _ => (),
        }
    }
    Ok(())
}

fn print_table_latex(tbl: &Table, stream: &mut OutBuf) -> Result<()> {
    if let Some(title) = tbl.title() {
        write!(stream, "\n\\section{{")?;
        write_latex_esc(stream, title)?;
        writeln!(stream, "}}\n")?;
    }

    writeln!(
        stream,
        "\\begin{{tabular}}{{*{{{cols}}}{{l}}}}",
        cols = tbl.count_columns()
    )?;

    for row in tbl.rows() {
        match row {
            Row::Toprule | Row::Midrule | Row::Bottomrule => writeln!(stream, "  \\hline")?,

            Row::Head(cells) | Row::Foot(cells) => {
                write_latex_cells(stream, cells, true)?;
            }

            Row::Data(cells) => {
                write_latex_cells(stream, cells, false)?;
            }
        }
    }

    writeln!(stream, "\\end{{tabular}}\n\\clearpage")?;
    Ok(())
}

fn write_latex_cells(stream: &mut OutBuf, cells: &VecDeque<Cell>, head: bool) -> Result<()> {
    let mut multi_max = 0;
    let mut multi = 0;
    loop {
        for (n, cell) in (0..).zip(cells) {
            match n {
                0 => write!(stream, "  ")?,
                _ => write!(stream, " & ")?,
            }

            match multi {
                0 => match cell {
                    Cell::Left(s) | Cell::Right(s) => {
                        if head {
                            write!(stream, "\\textbf{{")?;
                            write_latex_esc(stream, s)?;
                            write!(stream, "}}")?;
                        } else {
                            write_latex_esc(stream, s)?;
                        }
                    }

                    Cell::Multi(v) => {
                        if let Some(s) = v.get(multi) {
                            if head {
                                write!(stream, "\\textbf{{")?;
                                write_latex_esc(stream, s)?;
                                write!(stream, "}}")?;
                            } else {
                                write_latex_esc(stream, s)?;
                            }
                        }

                        if v.len() > multi_max {
                            multi_max = v.len();
                        }
                    }

                    Cell::Proportion { proportion, .. } => {
                        write!(stream, "\\rule{{{proportion:.3}\\mitta}}{{.9em}}")?;
                    }
                    _ => (),
                },

                _ => {
                    // multi > 0
                    if let Cell::Multi(v) = cell
                        && let Some(s) = v.get(multi)
                    {
                        if head {
                            write!(stream, "\\textbf{{")?;
                            write_latex_esc(stream, s)?;
                            write!(stream, "}}")?;
                        } else {
                            write_latex_esc(stream, s)?;
                        }
                    }
                }
            }
        }

        writeln!(stream, " \\\\")?;
        multi += 1;
        if multi >= multi_max {
            break;
        }
    }
    Ok(())
}

fn print_table_html(tbl: &Table, stream: &mut OutBuf) -> Result<()> {
    if let Some(title) = tbl.title() {
        write!(stream, "\n<h2>")?;
        write_html_esc(stream, title)?;
        writeln!(stream, "</h2>\n")?;
    }

    writeln!(stream, "<table style=\"text-align:left;\">")?;
    for row in tbl.rows() {
        match row {
            Row::Head(v) | Row::Foot(v) => {
                writeln!(stream, "  <tr>")?;
                for cell in v {
                    write!(stream, "    <th>")?;
                    match cell {
                        Cell::Empty => (),
                        Cell::Left(s) => {
                            write_html_esc(stream, s)?;
                        }
                        Cell::Right(s) => {
                            write!(stream, "<div style=\"text-align:right;\">")?;
                            write_html_esc(stream, s)?;
                            write!(stream, "</div>")?;
                        }
                        Cell::Multi(v) => {
                            write_html_esc(stream, &v.join(" "))?;
                        }
                        Cell::Proportion {
                            proportion,
                            width_max,
                            ..
                        } => {
                            write_html_proportion(stream, *proportion, *width_max)?;
                        }
                    }
                    writeln!(stream, "</th>")?;
                }
                writeln!(stream, "  </tr>")?;
            }

            Row::Data(v) => {
                writeln!(stream, "  <tr>")?;
                for cell in v {
                    write!(stream, "    <td>")?;
                    match cell {
                        Cell::Empty => (),
                        Cell::Left(s) => {
                            write_html_esc(stream, s)?;
                        }
                        Cell::Right(s) => {
                            write!(stream, "<div style=\"text-align:right;\">")?;
                            write_html_esc(stream, s)?;
                            write!(stream, "</div>")?;
                        }
                        Cell::Multi(v) => {
                            write_html_esc(stream, &v.join(" "))?;
                        }
                        Cell::Proportion {
                            proportion,
                            width_max,
                            ..
                        } => {
                            write_html_proportion(stream, *proportion, *width_max)?;
                        }
                    }
                    writeln!(stream, "</td>")?;
                }
                writeln!(stream, "  </tr>")?;
            }

            _ => (),
        }
    }

    writeln!(stream, "</table>")?;
    Ok(())
}

fn write_html_proportion(stream: &mut OutBuf, proportion: f64, width_max: usize) -> Result<()> {
    write!(
        stream,
        "<div style=\"width:{max}em;\">\
         <div style=\"\
         width:{prop:.2}%;\
         height:1em;\
         background-color:#444;\
         \"></div></div>",
        prop = proportion * 100.0,
        max = width_max,
    )?;
    Ok(())
}

fn write_latex_esc(stream: &mut OutBuf, s: &str) -> Result<()> {
    for character in s.chars() {
        match character {
            '^' => write!(stream, "\\textasciicircum{{}}")?,
            '\\' => write!(stream, "\\textbackslash{{}}")?,
            '~' => write!(stream, "\\textasciitilde{{}}")?,
            c if "%$_#&{}".contains(c) => write!(stream, "\\{c}")?,
            c => write!(stream, "{c}")?,
        }
    }
    Ok(())
}

fn write_html_esc(stream: &mut OutBuf, s: &str) -> Result<()> {
    for character in s.chars() {
        match character {
            '<' => write!(stream, "&lt;")?,
            '>' => write!(stream, "&gt;")?,
            '&' => write!(stream, "&amp;")?,
            c => write!(stream, "{c}")?,
        }
    }
    Ok(())
}

fn column_lines<I>(its: I, max: usize) -> Vec<String>
where
    I: IntoIterator,
    I::Item: ToString,
{
    let mut lines = Vec::with_capacity(20);
    let mut line = String::with_capacity(60);

    for word in its.into_iter().map(|x| x.to_string()) {
        if line.is_empty() {
            line.push_str(&word);
        } else if line.chars().count() + word.chars().count() < max {
            line.push(' ');
            line.push_str(&word);
        } else {
            let l = line.len();
            lines.push(line);
            line = String::with_capacity(l);
            line.push_str(&word);
        }
    }

    lines.push(line);
    lines
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn row_widths() {
        assert_eq!(
            vec![5, 4, 3],
            Row::Data(VecDeque::from([
                Cell::Left("12345".to_string()),
                Cell::Left("1234".to_string()),
                Cell::Right("123".to_string())
            ]))
            .widths()
            .unwrap()
        );

        assert_eq!(
            vec![5, 4, 3],
            Row::Data(VecDeque::from([
                Cell::Left("€€€€€".to_string()),
                Cell::Left("€€€€".to_string()),
                Cell::Right("€€€".to_string())
            ]))
            .widths()
            .unwrap()
        );

        assert_eq!(
            vec![4],
            Row::Data(VecDeque::from([Cell::Multi(vec![
                "1".to_string(),
                "1234".to_string(),
                "12".to_string(),
            ]),]))
            .widths()
            .unwrap()
        );
    }

    #[test]
    fn table_widths() {
        let table = Table::from_rows(vec![
            Row::Toprule,
            Row::Head(VecDeque::from([
                Cell::Left("12".to_string()),
                Cell::Left("1".to_string()),
                Cell::Right("1234".to_string()),
            ])),
            Row::Data(VecDeque::from([
                Cell::Left("€".to_string()),
                Cell::Left("€€".to_string()),
                Cell::Right("€€€".to_string()),
            ])),
            Row::Data(VecDeque::from([
                Cell::Left("€".to_string()),
                Cell::Left("€€€".to_string()),
                Cell::Right("€€€€".to_string()),
            ])),
        ]);

        assert_eq!(vec![2, 3, 4], table.widths());
    }

    #[test]
    fn cell_width() {
        assert_eq!(0, Cell::Empty.width());
        assert_eq!(3, Cell::Left("123".to_string()).width());
        assert_eq!(4, Cell::Right("1234".to_string()).width());
        assert_eq!(
            5,
            Cell::Multi(vec!["123".to_string(), "12345".to_string()]).width()
        );
    }

    #[test]
    fn column_lines_fn() {
        fn test(s: &str, max: usize) -> Vec<String> {
            column_lines(s.split_whitespace(), max)
        }

        for i in 0..8 {
            assert_eq!(vec!["€ka", "tøka", "kølmas"], test("€ka tøka kølmas", i));
        }

        for i in 8..15 {
            assert_eq!(vec!["€ka tøka", "kølmas"], test("€ka tøka kølmas", i));
        }

        assert_eq!(vec!["€ka tøka kølmas"], test("€ka tøka kølmas", 15));
        assert_eq!(vec![""], test("", 15));
    }
}
