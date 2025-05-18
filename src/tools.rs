use std::error::Error;

pub fn parse_number_list(s: &str) -> Result<Vec<usize>, Box<dyn Error>> {
    let mut vec: Vec<usize> = Vec::with_capacity(25);
    let errmsg = |v| format!("Sopimaton tietueen numero: ”{v}”.");

    for part in s.split(',').filter(|e| !e.is_empty()) {
        if is_all_digits(part) {
            let num = part.parse::<usize>()?;
            if num == 0 {
                Err(errmsg(part))?;
            }
            vec.push(num);
            continue;
        }

        let (start, end) = match part.split_once('-') {
            None => Err(errmsg(part))?,
            Some((s, e)) => {
                if !is_all_digits(s) || !is_all_digits(e) {
                    Err(format!("Sopimaton tietueiden sarja: ”{s}-{e}”."))?;
                }
                (s.parse::<usize>()?, e.parse::<usize>()?)
            }
        };

        if start == 0 || end == 0 {
            Err(errmsg("0"))?;
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

fn is_all_digits(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
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

pub fn has_content(s: &str) -> bool {
    s.chars().any(|c| !c.is_whitespace())
}

pub fn normalize_str(s: &str) -> String {
    let mut new = String::with_capacity(s.len());
    for (n, word) in s.split_whitespace().enumerate() {
        if n > 0 {
            new.push(' ');
        }
        new.push_str(word);
    }
    new
}

pub fn format_decimal(num: f64) -> String {
    const PRECISION: f64 = 100.0;
    format!("{:.2}", (num * PRECISION).round() / PRECISION).replace(".", ",")
}

pub fn format_decimal_n(num: f64, decimals: usize) -> String {
    let precision: f64 = 10.0_f64.powf(decimals as f64);
    format!(
        "{n:.d$}",
        d = decimals,
        n = (num * precision).round() / precision
    )
    .replace(".", ",")
}

pub fn umask(mask: u32) -> u32 {
    unsafe { libc::umask(mask) }
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

    #[test]
    fn t_parse_number() {
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
    fn t_float_to_grade() {
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
    fn t_parse_number_list() {
        assert_eq!(false, parse_number_list("0").is_ok());
        assert_eq!(false, parse_number_list(" 3").is_ok());
        assert_eq!(false, parse_number_list("1,2,0").is_ok());
        assert_eq!(vec![1, 2, 3], parse_number_list("1,2,3").unwrap());
        assert_eq!(vec![1, 2, 3], parse_number_list(",1,,,2,3,").unwrap());
        assert_eq!(false, parse_number_list("1,+2,3").is_ok());
        assert_eq!(false, parse_number_list("1,2,x").is_ok());
        assert_eq!(false, parse_number_list("1,2,a-b").is_ok());
        assert_eq!(false, parse_number_list("1,2,3-").is_ok());
        assert_eq!(false, parse_number_list("1,2,-3").is_ok());
        assert_eq!(vec![1, 2, 3], parse_number_list("1-3").unwrap());
        assert_eq!(vec![1, 2, 3], parse_number_list("01-003").unwrap());
        assert_eq!(vec![3, 2, 1], parse_number_list("3-1").unwrap());
        assert_eq!(
            vec![1, 2, 3, 3, 2, 1],
            parse_number_list("1-3,3-1").unwrap()
        );
        assert_eq!(false, parse_number_list("0-5").is_ok());
        assert_eq!(false, parse_number_list("000-5").is_ok());
        assert_eq!(false, parse_number_list("5-0").is_ok());
        assert_eq!(false, parse_number_list("2-5-6").is_ok());
        assert_eq!(
            vec![3, 4, 5, 6, 7, 10, 15, 14, 13, 12],
            parse_number_list("3-7,10,15-12").unwrap()
        );
    }

    #[test]
    fn t_is_all_digits() {
        assert_eq!(true, is_all_digits("3"));
        assert_eq!(true, is_all_digits("364"));
        assert_eq!(true, is_all_digits("01234567890"));
        assert_eq!(false, is_all_digits(""));
        assert_eq!(false, is_all_digits(" "));
        assert_eq!(false, is_all_digits("x"));
        assert_eq!(false, is_all_digits("+6"));
        assert_eq!(false, is_all_digits("-6"));
        assert_eq!(false, is_all_digits(".6"));
        assert_eq!(false, is_all_digits("6.0"));
    }

    #[test]
    fn t_is_within_limits() {
        assert_eq!(true, is_within_limits(10, &[3, 10, 4]));
        assert_eq!(false, is_within_limits(10, &[3, 11, 10, 4]));
        assert_eq!(true, is_within_limits(11, &[3, 11, 10, 4]));
    }

    #[test]
    fn t_normalize_str() {
        assert_eq!("abc 123", normalize_str("  abc   123  "));
        assert_eq!("abc", normalize_str("abc"));
        assert_eq!("abc", normalize_str(" abc "));
        assert_eq!("€– –€ö", normalize_str(" €–   –€ö   "));
    }

    #[test]
    fn t_has_content() {
        assert_eq!(true, has_content("  abc   123  "));
        assert_eq!(true, has_content("abc"));
        assert_eq!(true, has_content(" abc "));
        assert_eq!(false, has_content(" "));
        assert_eq!(false, has_content("  \t  "));
        assert_eq!(false, has_content(""));
    }

    #[test]
    fn t_format_decimal() {
        assert_eq!("5,00", format_decimal(5.0));
        assert_eq!("5,25", format_decimal(5.254));
        assert_eq!("5,26", format_decimal(5.255));
        assert_eq!("0,01", format_decimal(0.01));
        assert_eq!("0,00", format_decimal(0.0));
    }

    #[test]
    fn t_format_decimal_n() {
        assert_eq!("5", format_decimal_n(5.0, 0));
        assert_eq!("5,000", format_decimal_n(5.0, 3));
        assert_eq!("5,5", format_decimal_n(5.456, 1));
        assert_eq!("5,6", format_decimal_n(5.556, 1));
        assert_eq!("5,46", format_decimal_n(5.456, 2));
        assert_eq!("5,456", format_decimal_n(5.456, 3));
        assert_eq!("5,4560", format_decimal_n(5.456, 4));
    }
}
