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
        modes::{Mode, Modes},
        objects::Editable,
        print::Output,
    },
    sqlx::{Connection as _, PgConnection as DBase},
};

pub const PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub const PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub const PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");
