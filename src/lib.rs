pub mod commands;
mod config;
mod database;
mod error;
mod objects;
mod prelude;
mod print;
pub mod tools;

pub use {
    crate::{
        config::Config,
        database::connect,
        error::{Error, Result},
        objects::{
            Assignment, AssignmentsForGroup, AssignmentsForGroups, CopyToEditable, Editable, Grade,
            GradesForAssignment, GradesForAssignments, GradesForGroup, GradesForGroups,
            GradesForStudent, GradesForStudents, Group, Groups, HasData, SimpleGrade,
            SimpleStudent, Student, Students,
        },
        print::{Cell, MakeTable, Output, PrintQuery, PrintQueryList, PrintQueryNum, Row, Table},
    },
    sqlx::{Connection, PgConnection as DBase},
};

pub const PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub const PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub const PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");
