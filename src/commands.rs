use crate::{
    Mode, Modes,
    database::{Groups, Stats},
};
use sqlx::PgConnection;
use std::error::Error;

pub async fn stats(modes: &Modes, db: &mut PgConnection) -> Result<(), Box<dyn Error>> {
    let stats = Stats::query(db).await?;
    stats.table().print(modes.output());
    Ok(())
}

pub async fn groups(
    modes: &Modes,
    db: &mut PgConnection,
    mut args: &str,
) -> Result<(), Box<dyn Error>> {
    // args: /ryhmä/lisätiedot
    if args.is_empty() {
        args = "/";
    }

    let mut split = split_sep(args);
    let group = split.next().unwrap_or("");
    let desc = split.next().unwrap_or("");

    let groups = Groups::query(db, group, desc).await?;
    if groups.is_empty() {
        print_not_found();
        return Ok(());
    }

    let mut table = groups.table();
    if let Mode::Interactive = modes.mode() {
        table.numbering();
    }
    table.print(modes.output());
    Ok(())
}

fn print_not_found() {
    eprintln!("Ei löytynyt.");
}

pub fn split_sep(s: &str) -> impl Iterator<Item = &str> {
    let sep = s.chars().next().unwrap_or('/');
    s.split(sep).skip(1)
}

pub fn split_first(s: &str) -> (&str, &str) {
    let trimmed = s.trim_start();
    match trimmed.split_once(' ') {
        Some((first, rest)) => (first, rest.trim_start()),
        None => (trimmed, ""),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_split_sep() {
        assert_eq!(
            vec!["eka", "toka"],
            split_sep("/eka/toka").collect::<Vec<&str>>()
        );
        assert_eq!(
            vec!["äiti", "öljy", ""],
            split_sep("/äiti/öljy/").collect::<Vec<&str>>()
        );
        assert_eq!(
            vec!["äiti", "", "öljy"],
            split_sep("/äiti//öljy").collect::<Vec<&str>>()
        );
        assert_eq!(
            vec!["äiti", "", "öljy"],
            split_sep("–äiti––öljy").collect::<Vec<&str>>()
        );
        assert_eq!(None, split_sep("").next());
        assert_eq!(vec![""], split_sep("/").collect::<Vec<&str>>());
        assert_eq!(vec![""], split_sep("–").collect::<Vec<&str>>());
        assert_eq!(vec!["", "", ""], split_sep("///").collect::<Vec<&str>>());
        assert_eq!(vec!["", "", ""], split_sep("–––").collect::<Vec<&str>>());
        assert_eq!(
            vec![" ", "  ", " "],
            split_sep("/ /  / ").collect::<Vec<&str>>()
        );
        assert_eq!(
            vec![" ", "  ", " "],
            split_sep("– –  – ").collect::<Vec<&str>>()
        );
    }

    #[test]
    fn t_split_first() {
        assert_eq!(("ainoa", ""), split_first(" ainoa "));
        assert_eq!(("eka", "toka kolmas"), split_first("eka toka kolmas"));
        assert_eq!(("eka", "toka kolmas"), split_first(" eka  toka kolmas"));
        assert_eq!(("eka", "toka  kolmas "), split_first("eka  toka  kolmas "));
        assert_eq!(("€äö", "€äö  €äö "), split_first("€äö  €äö  €äö "));
    }
}
