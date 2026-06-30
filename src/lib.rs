pub mod commands;
mod config;
pub mod database;
mod error;
mod modes;
mod objects;
mod prelude;
mod print;
pub mod tools;

pub use {
    crate::{
        config::Config,
        error::{Error, Result},
        modes::{Mode, Modes, Output},
        objects::Editable,
    },
    sqlx::{Connection as _, PgConnection as DBase},
};

pub static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");
