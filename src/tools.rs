pub fn split_sep(s: &str) -> impl Iterator<Item = &str> {
    let sep = s.chars().next().unwrap_or('/');
    s.split(sep).skip(1)
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
}
