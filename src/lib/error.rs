use rustyline::error::ReadlineError;
use std::num::ParseIntError;
use std::{fmt, io};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Exit,
    Generic(String),
    Io {
        kind: io::ErrorKind,
        error: io::Error,
    },
    Db(sqlx::Error),
    UnknownCmd(String),
    UnknownTbl(String),
    GroupName(String),
}

impl Error {
    pub fn unknown_cmd(cmd: impl ToString) -> Self {
        Self::UnknownCmd(cmd.to_string())
    }

    pub(crate) fn unknown_tbl(tbl: impl ToString) -> Self {
        Self::UnknownTbl(tbl.to_string())
    }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}"),
            Self::Io { error, .. } => write!(f, "Tiedonsiirtovirhe: {error}"),
            Self::Db(v) => write!(f, "Tietokantavirhe: {v}"),
            Self::UnknownCmd(v) => write!(f, "Tuntematon komento ”{v}”."),
            Self::UnknownTbl(v) => write!(f, "Tuntematon taulukkotyyppi ”{v}”."),
            Self::GroupName(v) => write!(f, "Sopimaton ryhmän nimi ”{v}”."),
            _ => Ok(()),
        }
    }
}

impl From<&str> for Error {
    fn from(err: &str) -> Self {
        Self::Generic(err.to_string())
    }
}

impl From<String> for Error {
    fn from(err: String) -> Self {
        Self::Generic(err)
    }
}

impl From<sqlx::Error> for Error {
    fn from(err: sqlx::Error) -> Self {
        Self::Db(err)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io {
            kind: err.kind(),
            error: err,
        }
    }
}

impl From<ParseIntError> for Error {
    fn from(err: ParseIntError) -> Self {
        Self::Generic(format!("{err}"))
    }
}

impl From<ReadlineError> for Error {
    fn from(err: ReadlineError) -> Self {
        Self::Generic(format!("{err}"))
    }
}
