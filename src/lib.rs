mod commands;
mod config;
mod database;
mod modes;
mod objects;
mod prelude;
mod print;
mod tools;

pub use crate::prelude::*;

pub static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

pub async fn command_stage(mut modes: Modes, config: Config) -> Result<(), Box<dyn Error>> {
    let mut db = database::connect(&config, &modes).await?;
    let mut editable: Editable = Default::default();

    match modes.clone().mode() {
        Mode::Interactive => {
            println!(
                "{prg} v{ver} (postgres://{user}@{host}:{port}/{db})",
                prg = PROGRAM_NAME,
                ver = PROGRAM_VERSION,
                user = config.user,
                host = config.host,
                port = config.port,
                db = config.database,
            );

            let prompt = format!("{PROGRAM_NAME}> ");
            let mut rl = rustyline::DefaultEditor::new()?;

            loop {
                let line = rl.readline(&prompt)?;
                if line.is_empty() {
                    break;
                }
                rl.add_history_entry(&line)?;

                let (cmd, args) = tools::split_first(&line);
                // if modes.upgrade() && cmd == UPGRADE_COMMAND {}

                match commands(&mut modes, &mut db, &mut editable, cmd, args).await {
                    Ok(Ok(())) => (),
                    Ok(Err(e)) => eprintln!("{e} Apua saa ?:llä."),
                    Err(e) => eprintln!("{e}"),
                }
            }
        }

        Mode::Single(line) => {
            let (cmd, args) = tools::split_first(line);
            if let Err(e) = commands(&mut modes, &mut db, &mut editable, cmd, args).await? {
                Err(format!("{e} Apua saa valitsimella ”--ohje”."))?;
            }
        }

        Mode::Stdin => {
            let mut ta = db.begin().await?;
            for item in io::stdin().lines() {
                let line = item?;
                if !line.is_empty() {
                    let (cmd, args) = tools::split_first(&line);
                    if let Err(e) = commands(&mut modes, &mut ta, &mut editable, cmd, args).await? {
                        Err(format!("{e} Apua saa valitsimella ”--ohje”."))?;
                    }
                }
            }
            ta.commit().await?;
        }
    }
    Ok(())
}

async fn commands(
    modes: &mut Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<Result<(), String>, Box<dyn Error>> {
    match (cmd, modes.mode()) {
        ("ho", _) => commands::students(modes, db, editable, args).await?,
        ("hr", _) => commands::groups(modes, db, editable, args).await?,
        ("hs", _) => commands::assignments(modes, db, editable, args).await?,
        ("has", _) => commands::grades_for_assignments(modes, db, editable, args).await?,
        ("hao", _) => commands::grades_for_students(modes, db, editable, args).await?,
        ("hak", _) => commands::grades_for_group(modes, db, editable, args).await?,

        ("tp", _) => commands::student_ranking(modes, db, editable, args, false).await?,
        ("tpk", _) => commands::student_ranking(modes, db, editable, args, true).await?,
        ("tj", _) => commands::grade_distribution(modes, db, editable, args, false).await?,
        ("tjk", _) => commands::grade_distribution(modes, db, editable, args, true).await?,

        ("lo", _) => commands::insert_student(db, editable, args).await?,
        ("ls", _) => commands::insert_assignment(db, editable, args).await?,

        ("m", Mode::Interactive) => commands::edit(db, editable, args).await?,
        ("ms", Mode::Interactive) => commands::edit_series(db, editable, args).await?,
        ("ma", Mode::Interactive) => commands::convert_to_grade(db, editable, args).await?,
        ("md", Mode::Interactive) => commands::convert_to_decimal(db, editable, args).await?,
        ("poista", Mode::Interactive) => commands::delete(db, editable, args).await?,

        ("tlk", _) => commands::table_format(modes, args)?,
        ("tk", _) => commands::stats(modes, db, editable).await?,

        ("?", _) => {
            editable.clear();
            commands::help(args)?;
        }

        (c, _) => return Ok(Err(format!("Tuntematon komento ”{c}”."))),
    }
    Ok(Ok(()))
}
