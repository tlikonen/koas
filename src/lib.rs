mod commands;
mod config;
mod database;
mod modes;
mod prelude;
mod print;
mod tools;

pub use crate::prelude::*;

pub static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

pub async fn command_stage(mut modes: Modes, config: Config) -> Result<(), Box<dyn Error>> {
    let mut db = database::connect(&config).await?;
    database::init(&mut db, &modes).await?;

    let mut editable: Editable = Default::default();

    match modes.mode() {
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

                match interactive_commands(&mut modes, &mut db, &mut editable, cmd, args).await {
                    Ok(true) => (),
                    Ok(false) => eprintln!("Tuntematon komento ”{cmd}”. Apua saa ?:llä."),
                    Err(e) => eprintln!("{e}"),
                }
            }
        }

        Mode::Single(line) => {
            let (cmd, args) = tools::split_first(line);
            if !non_interactive_commands(&modes, &mut db, &mut editable, cmd, args).await? {
                Err(format!(
                    "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                ))?;
            }
        }

        Mode::Stdin => {
            let mut ta = db.begin().await?;
            for item in io::stdin().lines() {
                let line = item?;
                if !line.is_empty() {
                    let (cmd, args) = tools::split_first(&line);
                    if !non_interactive_commands(&modes, &mut ta, &mut editable, cmd, args).await? {
                        Err(format!(
                            "Tuntematon komento ”{cmd}”. Apua saa valitsimella ”--ohje”."
                        ))?;
                    }
                }
            }
            ta.commit().await?;
        }
    }
    Ok(())
}

async fn interactive_commands(
    modes: &mut Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    match cmd {
        "ho" => commands::students(modes, db, editable, args).await?,
        "hr" => commands::groups(modes, db, editable, args).await?,
        "hs" => commands::assignments(modes, db, editable, args).await?,
        "has" => commands::grades_for_assignments(modes, db, editable, args).await?,
        "hao" => commands::grades_for_students(modes, db, editable, args).await?,
        "hak" => commands::grades_for_group(modes, db, editable, args).await?,

        "tp" => commands::student_ranking(modes, db, editable, args, false).await?,
        "tpk" => commands::student_ranking(modes, db, editable, args, true).await?,
        "tj" => commands::grade_distribution(modes, db, editable, args, false).await?,
        "tjk" => commands::grade_distribution(modes, db, editable, args, true).await?,

        "lo" => commands::insert_student(db, editable, args).await?,
        "ls" => commands::insert_assignment(db, editable, args).await?,

        "m" => commands::edit(db, editable, args).await?,
        "ms" => commands::edit_series(db, editable, args).await?,
        "ma" => commands::convert_to_grade(db, editable, args).await?,
        "md" => commands::convert_to_decimal(db, editable, args).await?,
        "poista" => commands::delete(db, editable, args).await?,

        "tlk" => commands::table_format(modes, args)?,
        "tk" => commands::stats(modes, db, editable).await?,

        "?" => {
            editable.clear();
            commands::help(args)?;
        }

        _ => return Ok(false),
    }
    Ok(true)
}

async fn non_interactive_commands(
    modes: &Modes,
    db: &mut PgConnection,
    editable: &mut Editable,
    cmd: &str,
    args: &str,
) -> Result<bool, Box<dyn Error>> {
    match cmd {
        "ho" => commands::students(modes, db, editable, args).await?,
        "hr" => commands::groups(modes, db, editable, args).await?,
        "hs" => commands::assignments(modes, db, editable, args).await?,
        "has" => commands::grades_for_assignments(modes, db, editable, args).await?,
        "hao" => commands::grades_for_students(modes, db, editable, args).await?,
        "hak" => commands::grades_for_group(modes, db, editable, args).await?,

        "tp" => commands::student_ranking(modes, db, editable, args, false).await?,
        "tpk" => commands::student_ranking(modes, db, editable, args, true).await?,
        "tj" => commands::grade_distribution(modes, db, editable, args, false).await?,
        "tjk" => commands::grade_distribution(modes, db, editable, args, true).await?,

        "lo" => commands::insert_student(db, editable, args).await?,
        "ls" => commands::insert_assignment(db, editable, args).await?,

        "tk" => commands::stats(modes, db, editable).await?,

        "?" => {
            editable.clear();
            commands::help(args)?;
        }

        _ => return Ok(false),
    }
    Ok(true)
}
