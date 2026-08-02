use crate::prelude::*;
use approx::relative_eq;

const MINUS_CHARS: &str = "-–−";
const MINUS_CHAR: char = '−';
const PLUS_CHAR: char = '+';
const HALF_CHAR: char = '½';

fn parse_number(s: &str) -> Option<f64> {
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
        } else if first == PLUS_CHAR {
            sign = 1.0;
            sign_set = true;
            start = 1;
        }
    }

    {
        let last = s.chars().next_back().unwrap();
        if last == PLUS_CHAR {
            suffix = 0.25;
            suffix_set = true;
            end = start.max(end - 1);
        } else if MINUS_CHARS.contains(last) {
            suffix = -0.25;
            suffix_set = true;
            end = start.max(end - 1);
        } else if last == HALF_CHAR {
            suffix = 0.5;
            suffix_set = true;
            end = start.max(end - 1);
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

pub fn parse_number_list(s: &str) -> Result<Vec<usize>> {
    if s.is_empty() {
        return Err(Error::from("Puuttuu tietueen numero(t)."));
    }

    let errmsg = |v| Error::from(format!("Sopimaton tietueen numero: ”{v}”."));
    let mut vec: Vec<usize> = Vec::with_capacity(25);

    for part in s.split(',').filter(|e| !e.is_empty()) {
        if part.is_all_digits() {
            let num = part.parse::<usize>()?;
            if num == 0 {
                return Err(errmsg(part));
            }
            vec.push(num);
            continue;
        }

        let (start, end) = match part.split_once('-') {
            None => return Err(errmsg(part)),
            Some((s, e)) => {
                if !s.is_all_digits() || !e.is_all_digits() {
                    return Err(Error::from(format!(
                        "Sopimaton tietueiden sarja: ”{s}-{e}”."
                    )));
                }
                (s.parse::<usize>()?, e.parse::<usize>()?)
            }
        };

        if start == 0 || end == 0 {
            return Err(errmsg("0"));
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
    fn float(&self) -> Option<f64>;
    fn grade_string(&self) -> Option<String>;
    fn normalize(&self) -> Option<String>;
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

    fn float(&self) -> Option<f64> {
        parse_number(self)
    }

    fn grade_string(&self) -> Option<String> {
        let float = self.float()?;

        if float < 0.0 {
            return None;
        }

        let mut integer = float.trunc() as i64;
        let fractional = float.fract();

        let mut suffix = String::with_capacity(2);
        if relative_eq!(0.25, fractional) {
            suffix.push(PLUS_CHAR);
        } else if relative_eq!(0.5, fractional) {
            suffix.push(HALF_CHAR);
        } else if relative_eq!(0.75, fractional) {
            integer += 1;
            suffix.push(MINUS_CHAR);
        } else if relative_eq!(0.0, fractional) {
            // No suffix.
        } else {
            return None;
        }

        Some(format!("{integer:.0}{suffix}"))
    }

    fn normalize(&self) -> Option<String> {
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

pub trait FloatExt {
    fn format_decimal(self) -> String;
}

impl FloatExt for f64 {
    fn format_decimal(self) -> String {
        const PRECISION: f64 = 100.0;
        let string = format!("{:.2}", (self * PRECISION).round() / PRECISION);
        let mut new = String::with_capacity(string.len());
        for character in string.chars() {
            new.push(match character {
                '.' => ',',
                '-' => MINUS_CHAR,
                c => c,
            });
        }
        new
    }
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
        fn test(s: &str) -> Vec<&str> {
            split_sep(s).collect()
        }

        assert_eq!(None, split_sep("").next());
        let mut parts = split_sep("/eka/toka");
        assert_eq!(Some("eka"), parts.next());
        assert_eq!(Some("toka"), parts.next());
        assert_eq!(None, parts.next());

        assert_eq!(vec!["eka", "toka"], test("/eka/toka"));
        assert_eq!(vec!["äiti", "öljy", ""], test("/äiti/öljy/"));
        assert_eq!(vec!["äiti", "", "öljy"], test("/äiti//öljy"));
        assert_eq!(vec!["äiti", "", "öljy"], test("–äiti––öljy"));
        assert_eq!(vec![""], test("/"));
        assert_eq!(vec![""], test("–"));
        assert_eq!(vec!["", "", ""], test("///"));
        assert_eq!(vec!["", "", ""], test("–––"));
        assert_eq!(vec![" ", "  ", " "], test("/ /  / "));
        assert_eq!(vec![" ", "  ", " "], test("– –  – "));
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
    fn float() {
        assert_eq!(None, "".float());
        assert_eq!(None, "+".float());
        assert_eq!(None, "-".float());
        assert_eq!(None, ".".float());
        assert_eq!(None, "..3".float());
        assert_eq!(None, ".3.".float());
        assert_eq!(None, "asdf".float());
        assert_eq!(Some(4.0), "4".float());
        assert_eq!(Some(4.3), "4.3".float());
        assert_eq!(Some(4.3), "4,3".float());
        assert_eq!(Some(4.0), "+4".float());
        assert_eq!(Some(-4.0), "-4".float());
        assert_eq!(Some(-4.0), "–4".float());
        assert_eq!(Some(-4.0), "−4".float());
        assert_eq!(Some(8.0), "8.".float());
        assert_eq!(Some(0.8), ".8".float());
        assert_eq!(Some(7.75), "8-".float());
        assert_eq!(Some(7.75), "8–".float());
        assert_eq!(Some(7.75), "8−".float());
        assert_eq!(Some(8.25), "8+".float());
        assert_eq!(Some(8.5), "8½".float());
        assert_eq!(Some(84.75), "85-".float());
        assert_eq!(Some(85.25), "85+".float());
        assert_eq!(Some(85.5), "85½".float());
        assert_eq!(Some(0.5), "½".float());
        assert_eq!(None, "+85-".float());
        assert_eq!(None, "+85+".float());
        assert_eq!(None, "+85½".float());
        assert_eq!(None, "-85-".float());
        assert_eq!(None, "-85+".float());
        assert_eq!(None, "-85½".float());
    }

    #[test]
    fn grade_string() {
        assert_eq!(None, "-0.1".grade_string());
        assert_eq!(None, "-5.0".grade_string());
        assert_eq!(None, "5.13".grade_string());
        assert_eq!(None, "8.9999".grade_string());
        assert_eq!(None, "8.0001".grade_string());
        assert_eq!(None, "8.4999".grade_string());
        assert_eq!(None, "8.2499".grade_string());
        assert_eq!(Some("8"), "8.0".grade_string().as_deref());
        assert_eq!(Some("8+"), "8.25".grade_string().as_deref());
        assert_eq!(Some("8½"), "8.5".grade_string().as_deref());
        assert_eq!(Some("9−"), "8.75".grade_string().as_deref());
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
    fn normalize() {
        assert_eq!(Some("abc 123"), "  abc   123  ".normalize().as_deref());
        assert_eq!(Some("abc 123".to_string()), "  abc   123  ".normalize());
        assert_eq!(Some("abc"), "abc".normalize().as_deref());
        assert_eq!(Some("abc"), " abc ".normalize().as_deref());
        assert_eq!(Some("€– –€ö"), " €–   –€ö   ".normalize().as_deref());
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
    fn format_decimal() {
        assert_eq!("5,00", 5.0.format_decimal());
        assert_eq!("5,25", 5.254.format_decimal());
        assert_eq!("−5,25", (-5.254).format_decimal());
        assert_eq!("5,26", 5.255.format_decimal());
        assert_eq!("−0,01", (-0.01).format_decimal());
        assert_eq!("0,00", 0.0.format_decimal());
    }
}
