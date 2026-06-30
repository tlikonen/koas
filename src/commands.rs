mod assignments;
mod edit;
mod grades;
mod groups;
mod students;

pub use assignments::*;
pub use edit::*;
pub use grades::*;
pub use groups::*;
pub use students::*;

use crate::prelude::*;

pub async fn stats(modes: &Modes, db: &mut DBase, editable: &mut Editable) -> Result<()> {
    editable.clear();

    Stats::query(db).await?.print(modes.output())?;
    Ok(())
}

pub fn table_format(modes: &mut Modes, args: &str) -> Result<()> {
    let (first, _) = tools::split_first(args);
    if first.is_empty() {
        return Err("Anna argumentiksi taulukkotyyli. Apua saa ?:llä.".into());
    }

    let new = Output::select(first)?;
    modes.set_output(new);
    Ok(())
}

pub fn help(topic: &str) -> Result<()> {
    static HO: &str = include_str!("../help/command-ho.txt");
    static HR: &str = include_str!("../help/command-hr.txt");
    static HS: &str = include_str!("../help/command-hs.txt");
    static HAS: &str = include_str!("../help/command-has.txt");
    static HAO: &str = include_str!("../help/command-hao.txt");
    static HAK: &str = include_str!("../help/command-hak.txt");

    static M: &str = include_str!("../help/command-m.txt");
    static MS: &str = include_str!("../help/command-ms.txt");
    static MA: &str = include_str!("../help/command-ma.txt");
    static MD: &str = include_str!("../help/command-md.txt");
    static POISTA: &str = include_str!("../help/command-poista.txt");

    static LO: &str = include_str!("../help/command-lo.txt");
    static LS: &str = include_str!("../help/command-ls.txt");

    static TP: &str = include_str!("../help/command-tp.txt");
    static TJ: &str = include_str!("../help/command-tj.txt");
    static TK: &str = include_str!("../help/command-tk.txt");
    static TLK: &str = include_str!("../help/command-tlk.txt");

    static QM: &str = include_str!("../help/command-qm.txt");
    static QUICK: &str = include_str!("../help/quick.txt");

    let mut stdout = io::stdout();

    match topic {
        "" => writeln!(stdout, "\n{QUICK}")?,

        "ho" => writeln!(stdout, "\n{HO}")?,
        "hr" => writeln!(stdout, "\n{HR}")?,
        "hs" => writeln!(stdout, "\n{HS}")?,
        "has" => writeln!(stdout, "\n{HAS}")?,
        "hao" => writeln!(stdout, "\n{HAO}")?,
        "hak" => writeln!(stdout, "\n{HAK}")?,

        "m" => writeln!(stdout, "\n{M}")?,
        "ms" => writeln!(stdout, "\n{MS}")?,
        "ma" => writeln!(stdout, "\n{MA}")?,
        "md" => writeln!(stdout, "\n{MD}")?,
        "poista" => writeln!(stdout, "\n{POISTA}")?,
        "lo" => writeln!(stdout, "\n{LO}")?,
        "ls" => writeln!(stdout, "\n{LS}")?,

        "tp" | "tpk" => writeln!(stdout, "\n{TP}")?,
        "tj" | "tjk" => writeln!(stdout, "\n{TJ}")?,
        "tk" => writeln!(stdout, "\n{TK}")?,

        "tlk" => writeln!(stdout, "\n{TLK}")?,

        "?" => writeln!(stdout, "\n{QM}")?,
        "komennot" => writeln!(
            stdout,
            "\n{QUICK}\n{info}",
            info = include_str!("../help/command.txt")
        )?,
        "tietokanta" => writeln!(stdout, "\n{}", include_str!("../help/database.txt"))?,
        "asetukset" => writeln!(stdout, "\n{}", include_str!("../help/settings.txt"))?,

        u => return Err(format!("Tuntematon ohjeiden aihe: ”{u}”.").into()),
    }
    Ok(())
}
