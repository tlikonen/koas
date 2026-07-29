use crate::database::OldDb;
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
    OldDatabase(OldDb),
    OldProgram,
    InvalidLastname(String),
    InvalidFirstname(String),
    InvalidGroupname(String),
    InvalidDescription(String),
    InvalidAssignmentName(String),
    InvalidAssignmentShort(String),
    InvalidAssignmentWeight(String),
    InvalidAssignmentPosition(String),
    InvalidGrade(String),
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
            Self::Exit => Ok(()),
            Self::Generic(v) => write!(f, "{v}"),
            Self::Io { error, .. } => write!(f, "Tiedonsiirtovirhe: {error}"),
            Self::Db(v) => write!(f, "Tietokantavirhe: {v}"),
            Self::UnknownCmd(v) => write!(f, "Tuntematon komento ”{v}”."),
            Self::UnknownTbl(v) => write!(f, "Tuntematon taulukkotyyppi ”{v}”."),
            Self::OldDatabase(_) => write!(f, "Arvosanatietokannan versio on vanhentunut."),
            Self::OldProgram => write!(
                f,
                "Ohjelman versio on vanhentunut, ja tietokanta vaatii uudemman."
            ),
            Self::InvalidLastname(s) => write!(f, "Sopimaton sukunimi ”{s}”."),
            Self::InvalidFirstname(s) => write!(f, "Sopimaton etunimi ”{s}”."),
            Self::InvalidGroupname(s) => write!(f, "Sopimaton ryhmätunnus ”{s}”."),
            Self::InvalidDescription(s) => write!(f, "Sopimaton kuvaus ”{s}”."),
            Self::InvalidAssignmentName(s) => write!(f, "Sopimaton suorituksen nimi ”{s}”."),
            Self::InvalidAssignmentShort(s) => {
                write!(f, "Sopimaton suorituksen lyhenne (1–5 merkkiä) ”{s}”.")
            }
            Self::InvalidAssignmentWeight(s) => write!(f, "Sopimaton painokerroin ”{s}”."),
            Self::InvalidAssignmentPosition(s) => {
                write!(f, "Sopimaton suorituksen järjestysluku ”{s}”.")
            }
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
        match err {
            ReadlineError::Eof => Self::Exit,
            _ => Self::Generic(format!("{err}")),
        }
    }
}
