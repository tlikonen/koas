pub mod commands;
mod config;
pub mod database;
mod error;
mod objects;
pub mod output;
mod prelude;
pub mod tools;

pub use {
    crate::{
        config::Config,
        error::{Error, Result},
    },
    sqlx::{Connection, PgConnection},
};

pub const PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub const PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub const PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");
