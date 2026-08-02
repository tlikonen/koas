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

        let grade = if relative_eq!(0.25, fractional) {
            format!("{integer}{PLUS_CHAR}")
        } else if relative_eq!(0.5, fractional) {
            format!("{integer}{HALF_CHAR}")
        } else if relative_eq!(0.75, fractional) {
            integer += 1;
            format!("{integer}{MINUS_CHAR}")
        } else if relative_eq!(0.0, fractional) {
            format!("{integer}")
        } else {
            return None;
        };

        Some(grade)
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

/// Set `umask 0077`.
pub fn umask() {
    unsafe {
        libc::umask(0o077);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
