pub enum Mode {
    Interactive,
    Single(String),
    Stdin,
}

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
}

#[derive(Default)]
pub struct Modes {
    mode: Option<Mode>,
    output: Option<Output>,
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
}
