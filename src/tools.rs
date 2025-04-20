pub fn parse_float(s: &str) -> Option<f64> {
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

    #[test]
    fn t_parse_float() {
        assert_eq!(None, parse_float(""));
        assert_eq!(None, parse_float("+"));
        assert_eq!(None, parse_float("-"));
        assert_eq!(None, parse_float("."));
        assert_eq!(None, parse_float("..3"));
        assert_eq!(None, parse_float(".3."));
        assert_eq!(None, parse_float("asdf"));
        assert_eq!(Some(4.0), parse_float("4"));
        assert_eq!(Some(4.3), parse_float("4.3"));
        assert_eq!(Some(4.3), parse_float("4,3"));
        assert_eq!(Some(4.0), parse_float("+4"));
        assert_eq!(Some(-4.0), parse_float("-4"));
        assert_eq!(Some(8.0), parse_float("8."));
        assert_eq!(Some(0.8), parse_float(".8"));
        assert_eq!(Some(7.75), parse_float("8-"));
        assert_eq!(Some(8.25), parse_float("8+"));
        assert_eq!(Some(8.5), parse_float("8½"));
        assert_eq!(Some(84.75), parse_float("85-"));
        assert_eq!(Some(85.25), parse_float("85+"));
        assert_eq!(Some(85.5), parse_float("85½"));
        assert_eq!(Some(0.5), parse_float("½"));
        assert_eq!(None, parse_float("+85-"));
        assert_eq!(None, parse_float("+85+"));
        assert_eq!(None, parse_float("+85½"));
        assert_eq!(None, parse_float("-85-"));
        assert_eq!(None, parse_float("-85+"));
        assert_eq!(None, parse_float("-85½"));
    }
}

pub fn umask(mask: u32) -> u32 {
    unsafe { libc::umask(mask) }
}
