pub fn split_sep(s: &str) -> Vec<String> {
    let mut chars = s.chars();
    let sep = chars.nth(0).expect("Tyhjä merkkijono");
    chars
        .collect::<String>()
        .split(sep)
        .map(|i| i.to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_split_sep() {
        assert_eq!(vec!["eka", "toka"], split_sep("/eka/toka"));
        assert_eq!(vec![""], split_sep("/"));
    }
}
