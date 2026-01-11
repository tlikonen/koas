use crate::prelude::*;

#[derive(Clone)]
pub enum Mode {
    Interactive,
    Single(String),
    Stdin,
}

#[non_exhaustive]
#[derive(Default, Clone)]
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
}

impl Output {
    pub fn select(value: &str) -> ResultDE<Self> {
        let out = match value.to_lowercase().as_str() {
            "unicode" | "u" => Output::Unicode,
            "unicode-avoin" | "ua" => Output::UnicodeOpen,
            "ascii" | "a" => Output::Ascii,
            "ascii-avoin" | "aa" => Output::AsciiOpen,
            "emacs" | "e" => Output::Orgmode,
            "tab" | "t" => Output::Tab,
            "csv" | "c" => Output::Csv,
            "latex" | "l" => Output::Latex,
            _ => return Err(value.into()),
        };
        Ok(out)
    }
}

#[derive(Default, Clone)]
pub struct Modes {
    mode: Option<Mode>,
    output: Option<Output>,
    // upgrade: bool,
}

impl Modes {
    pub fn output(&self) -> &Output {
        self.output.as_ref().expect("Uninitialized Modes::output.")
    }

    pub fn set_output(&mut self, v: Output) {
        self.output = Some(v);
    }

    pub fn mode(&self) -> &Mode {
        self.mode.as_ref().expect("Uninitialized Modes::mode.")
    }

    pub fn set_mode(&mut self, v: Mode) {
        self.mode = Some(v);
    }

    pub fn is_interactive(&self) -> bool {
        matches!(self.mode(), Mode::Interactive)
    }

    // pub fn upgrade(&self) -> bool {
    //     self.upgrade
    // }

    // pub fn set_upgrade(&mut self) {
    //     self.upgrade = true;
    // }
}
