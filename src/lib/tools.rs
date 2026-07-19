use crate::prelude::*;

pub fn parse_number_list(s: &str) -> Result<Vec<usize>> {
    if s.is_empty() {
        Err("Puuttuu tietueen numero(t).")?;
    }

    let errmsg = |v| format!("Sopimaton tietueen numero: ”{v}”.");
    let mut vec: Vec<usize> = Vec::with_capacity(25);

    for part in s.split(',').filter(|e| !e.is_empty()) {
        if part.is_all_digits() {
            let num = part.parse::<usize>()?;
            if num == 0 {
                return Err(errmsg(part).into());
            }
            vec.push(num);
            continue;
        }

        let (start, end) = match part.split_once('-') {
            None => return Err(errmsg(part).into()),
            Some((s, e)) => {
                if !s.is_all_digits() || !e.is_all_digits() {
                    return Err(format!("Sopimaton tietueiden sarja: ”{s}-{e}”.").into());
                }
                (s.parse::<usize>()?, e.parse::<usize>()?)
            }
        };

        if start == 0 || end == 0 {
            return Err(errmsg("0").into());
        }

        if start == end {
            vec.push(start);
            continue;
        }

        let inc = start < end;
        let mut i = start;
        loop {
            vec.push(i);
            if i == end {
                break;
            }
            if inc {
                i += 1;
            } else {
                i -= 1;
            }
        }
    }
    Ok(vec)
}

pub fn is_within_limits(limit: usize, list: &[usize]) -> bool {
    list.iter().all(|n| *n <= limit)
}

pub fn parse_number(s: &str) -> Option<f64> {
    use std::cmp::max;
    const MINUS_CHARS: &str = "-–−";

    if s.is_empty() {
        return None;
    }

    let mut sign = 1.0;
    let mut sign_set = false;
    let mut start = 0;
    let mut end = s.chars().count();
    let mut suffix = 0.0;
    let mut suffix_set = false;

    {
        let first = s.chars().next().unwrap();
        if MINUS_CHARS.contains(first) {
            sign = -1.0;
            sign_set = true;
            start = 1;
        } else if first == '+' {
            sign = 1.0;
            sign_set = true;
            start = 1;
        }
    }

    {
        let last = s.chars().next_back().unwrap();
        if last == '+' {
            suffix = 0.25;
            suffix_set = true;
            end = max(start, end - 1);
        } else if MINUS_CHARS.contains(last) {
            suffix = -0.25;
            suffix_set = true;
            end = max(start, end - 1);
        } else if last == '½' {
            suffix = 0.5;
            suffix_set = true;
            end = max(start, end - 1);
        }
    }

    let mut obj: String = s
        .chars()
        .skip(start)
        .take(end - start)
        .map(|c| if c == ',' { '.' } else { c })
        .collect();

    if obj.is_empty() {
        obj.push('0');
    }

    if suffix_set && !sign_set && obj.chars().all(|c| c.is_ascii_digit()) {
        Some(obj.parse::<f64>().unwrap() + suffix)
    } else if !suffix_set
        && obj.chars().all(|c| c.is_ascii_digit() || c == '.')
        && obj.chars().any(|c| c.is_ascii_digit())
        && obj.chars().filter(|c| *c == '.').count() <= 1
    {
        Some(sign * obj.parse::<f64>().unwrap())
    } else {
        None
    }
}

pub fn float_to_grade(float: f64) -> Option<String> {
    if float < 0.0 {
        return None;
    }

    let mut integer = float.trunc();
    let fractional = float.fract();

    let mut suffix = String::with_capacity(1);
    match fractional {
        0.25 => suffix.push('+'),
        0.5 => suffix.push('½'),
        0.75 => {
            integer += 1.0;
            suffix.push('-');
        }
        0.0 => (),
        _ => return None,
    }

    Some(format!("{integer:.0}{suffix}"))
}

pub fn split_sep(s: &str) -> impl Iterator<Item = &str> {
    let sep = s.chars().next().unwrap_or('/');
    s.split(sep).skip(1)
}

pub fn split_first(s: &str) -> (&str, &str) {
    let trimmed = s.trim_start();
    match trimmed.split_once(|c: char| c.is_whitespace()) {
        Some((first, rest)) => (first, rest.trim_start()),
        None => (trimmed, ""),
    }
}

pub trait StrExt {
    fn has_content(&self) -> bool;
    fn has_whitespace(&self) -> bool;
    fn is_all_digits(&self) -> bool;
    fn is_valid_group_name(&self) -> Result<()>;
}

impl StrExt for str {
    fn has_content(&self) -> bool {
        self.chars().any(|c| !c.is_whitespace())
    }

    fn has_whitespace(&self) -> bool {
        self.chars().any(|c| c.is_whitespace())
    }

    fn is_all_digits(&self) -> bool {
        !self.is_empty() && self.chars().all(|c| c.is_ascii_digit())
    }

    fn is_valid_group_name(&self) -> Result<()> {
        if self.has_whitespace() {
            Err("Ryhmätunnuksessa ei voi olla välilyöntejä.".into())
        } else if !self.has_content() {
            Err("Sopimaton ryhmätunnus.".into())
        } else {
            Ok(())
        }
    }
}

pub(crate) fn assert_group_names<'a, I>(names: I) -> Result<()>
where
    I: IntoIterator<Item = &'a String>,
{
    for group in names.into_iter() {
        group.is_valid_group_name()?;
    }
    Ok(())
}

pub(crate) trait Normalize {
    type Target;
    fn normalize(&self) -> Self::Target;
}

impl Normalize for str {
    type Target = Option<String>;
    fn normalize(&self) -> Self::Target {
        let mut new = String::with_capacity(self.len());
        for word in self.split_whitespace() {
            if !new.is_empty() {
                new.push(' ');
            }
            new.push_str(word);
        }

        if new.is_empty() { None } else { Some(new) }
    }
}

pub fn format_decimal(num: f64) -> String {
    const PRECISION: f64 = 100.0;
    format!("{:.2}", (num * PRECISION).round() / PRECISION).replace(".", ",")
}

pub fn umask() {
    unsafe {
        libc::umask(0o077);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_sep_fn() {
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
    fn split_first_fn() {
        assert_eq!(("ainoa", ""), split_first(" ainoa "));
        assert_eq!(("eka", "toka kolmas"), split_first("eka toka kolmas"));
        assert_eq!(("eka", "toka kolmas"), split_first(" eka  toka kolmas"));
        assert_eq!(("eka", "toka  kolmas "), split_first("eka  toka  kolmas "));
        assert_eq!(("€äö", "€äö  €äö "), split_first("€äö  €äö  €äö "));
    }

    #[test]
    fn parse_number_fn() {
        assert_eq!(None, parse_number(""));
        assert_eq!(None, parse_number("+"));
        assert_eq!(None, parse_number("-"));
        assert_eq!(None, parse_number("."));
        assert_eq!(None, parse_number("..3"));
        assert_eq!(None, parse_number(".3."));
        assert_eq!(None, parse_number("asdf"));
        assert_eq!(Some(4.0), parse_number("4"));
        assert_eq!(Some(4.3), parse_number("4.3"));
        assert_eq!(Some(4.3), parse_number("4,3"));
        assert_eq!(Some(4.0), parse_number("+4"));
        assert_eq!(Some(-4.0), parse_number("-4"));
        assert_eq!(Some(8.0), parse_number("8."));
        assert_eq!(Some(0.8), parse_number(".8"));
        assert_eq!(Some(7.75), parse_number("8-"));
        assert_eq!(Some(8.25), parse_number("8+"));
        assert_eq!(Some(8.5), parse_number("8½"));
        assert_eq!(Some(84.75), parse_number("85-"));
        assert_eq!(Some(85.25), parse_number("85+"));
        assert_eq!(Some(85.5), parse_number("85½"));
        assert_eq!(Some(0.5), parse_number("½"));
        assert_eq!(None, parse_number("+85-"));
        assert_eq!(None, parse_number("+85+"));
        assert_eq!(None, parse_number("+85½"));
        assert_eq!(None, parse_number("-85-"));
        assert_eq!(None, parse_number("-85+"));
        assert_eq!(None, parse_number("-85½"));
    }

    #[test]
    fn float_to_grade_fn() {
        assert_eq!(None, float_to_grade(-0.1));
        assert_eq!(None, float_to_grade(-5.0));
        assert_eq!(None, float_to_grade(5.13));
        assert_eq!(None, float_to_grade(8.99));
        assert_eq!(None, float_to_grade(8.26));
        assert_eq!(Some("8"), float_to_grade(8.0).as_deref());
        assert_eq!(Some("8+"), float_to_grade(8.25).as_deref());
        assert_eq!(Some("8½"), float_to_grade(8.5).as_deref());
        assert_eq!(Some("9-"), float_to_grade(8.75).as_deref());
    }

    #[test]
    fn parse_number_list_fn() {
        assert!(parse_number_list("").is_err());
        assert!(parse_number_list(" ").is_err());

        assert!(parse_number_list("0").is_err());
        assert!(parse_number_list(" 3").is_err());
        assert!(parse_number_list("1,2,0").is_err());

        assert_eq!(vec![1, 2, 3], parse_number_list("1,2,3").unwrap());
        assert_eq!(vec![1, 2, 3], parse_number_list("1,2,3-3").unwrap());
        assert_eq!(vec![1, 2, 3], parse_number_list(",1,,,2,3,").unwrap());

        assert!(parse_number_list("1,+2,3").is_err());
        assert!(parse_number_list("1,2,x").is_err());
        assert!(parse_number_list("1,2,a-b").is_err());
        assert!(parse_number_list("1,2,3-").is_err());
        assert!(parse_number_list("1,2,-3").is_err());

        assert_eq!(vec![1, 2, 3], parse_number_list("1-3").unwrap());
        assert_eq!(vec![1, 2, 3], parse_number_list("01-003").unwrap());
        assert_eq!(vec![3, 2, 1], parse_number_list("3-1").unwrap());
        assert_eq!(
            vec![1, 2, 3, 3, 2, 1],
            parse_number_list("1-3,3-1").unwrap()
        );

        assert!(parse_number_list("0-5").is_err());
        assert!(parse_number_list("000-5").is_err());
        assert!(parse_number_list("5-0").is_err());
        assert!(parse_number_list("2-5-6").is_err());

        assert_eq!(
            vec![3, 4, 5, 6, 7, 10, 15, 14, 13, 12],
            parse_number_list("3-7,10,15-12").unwrap()
        );
    }

    #[test]
    fn is_all_digits() {
        assert!("3".is_all_digits());
        assert!("364".is_all_digits());
        assert!("01234567890".is_all_digits());

        assert!(!"".is_all_digits());
        assert!(!" ".is_all_digits());
        assert!(!"x".is_all_digits());
        assert!(!"+6".is_all_digits());
        assert!(!"-6".is_all_digits());
        assert!(!".6".is_all_digits());
        assert!(!"6.0".is_all_digits());
    }

    #[test]
    fn is_within_limits_fn() {
        assert!(is_within_limits(10, &[3, 10, 4]));
        assert!(!is_within_limits(10, &[3, 11, 10, 4]));
        assert!(is_within_limits(11, &[3, 11, 10, 4]));
    }

    #[test]
    fn is_valid_group_name() {
        assert!("abc".is_valid_group_name().is_ok());
        assert!("abc€ø’".is_valid_group_name().is_ok());
        assert!(" abc".is_valid_group_name().is_err());
        assert!(" abc ".is_valid_group_name().is_err());
        assert!("abc ".is_valid_group_name().is_err());
        assert!("abc 123".is_valid_group_name().is_err());
        assert!("\t".is_valid_group_name().is_err());
        assert!(" ".is_valid_group_name().is_err());
    }

    #[test]
    fn normalize() {
        assert_eq!("abc 123", "  abc   123  ".normalize().unwrap());
        assert_eq!(Some("abc 123".to_string()), "  abc   123  ".normalize());
        assert_eq!("abc", "abc".normalize().unwrap());
        assert_eq!("abc", " abc ".normalize().unwrap());
        assert_eq!("€– –€ö", " €–   –€ö   ".normalize().unwrap());
        assert_eq!(None, "".normalize());
        assert_eq!(None, " \t  \t  ".normalize());
        assert_eq!("", "".normalize().unwrap_or_default());
    }

    #[test]
    fn has_content() {
        assert!("  abc   123  ".has_content());
        assert!("abc".has_content());
        assert!(" abc ".has_content());
        assert!(!" ".has_content());
        assert!(!"  \t  ".has_content());
        assert!(!"".has_content());
    }

    #[test]
    fn has_whitespace() {
        assert!("abc ".has_whitespace());
        assert!(" abc".has_whitespace());
        assert!("ab c".has_whitespace());
        assert!(" a b c ".has_whitespace());
        assert!("\tabc".has_whitespace());
        assert!("abc\t".has_whitespace());
        assert!("a\tbc".has_whitespace());
        assert!(!"€aböc".has_whitespace());
        assert!(!"".has_whitespace());
    }

    #[test]
    fn format_decimal_fn() {
        assert_eq!("5,00", format_decimal(5.0));
        assert_eq!("5,25", format_decimal(5.254));
        assert_eq!("5,26", format_decimal(5.255));
        assert_eq!("0,01", format_decimal(0.01));
        assert_eq!("0,00", format_decimal(0.0));
    }
}
