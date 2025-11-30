use crate::prelude::*;

pub const PROGRAM_DB_VERSION: i32 = 10;
const UPGRADE_COMMAND: &str = "päivitä";

pub async fn init(db: &mut PgConnection, modes: &Modes) -> Result<(), Box<dyn Error>> {
    let db_exists = sqlx::query("SELECT 1 FROM pg_tables WHERE tablename = 'hallinto'")
        .fetch_optional(&mut *db)
        .await?
        .is_some();

    if db_exists {
        let db_version: i32 = sqlx::query("SELECT arvo FROM hallinto WHERE avain = 'versio'")
            .fetch_one(&mut *db)
            .await?
            .try_get("arvo")?;

        match db_version.cmp(&PROGRAM_DB_VERSION) {
            Ordering::Equal => (),

            Ordering::Greater => {
                let err_msg = || {
                    format!(
                        "Nykyinen arvosanatietokannan versio on {db_version}, \
                         mutta tämä ohjelma tukee vain versiota {PROGRAM_DB_VERSION}.\n\
                         Päivitä ohjelma, koska se ei välttämättä toimi oikein."
                    )
                };

                if modes.is_interactive() {
                    eprintln!("{}", err_msg());
                } else {
                    Err(err_msg())?;
                }
            }

            Ordering::Less => {
                let err_msg = || {
                    format!(
                        "Nykyinen arvosanatietokannan versio {db_version} on vanhentunut, \
                         sillä tämä ohjelma on tehty versiolle {PROGRAM_DB_VERSION}.\n\
                         Päivitä tietokanta vuorovaikutteisessa tilassa komennolla \
                         ”{UPGRADE_COMMAND}”."
                    )
                };

                if modes.is_interactive() {
                    eprintln!("{}", err_msg());
                    // modes.set_upgrade();
                } else {
                    Err(err_msg())?;
                }
            }
        }
    } else {
        // Database objects don't exist. Create all.
        eprintln!("Valmistellaan arvosanatietokanta.");

        let mut ta = db.begin().await?;

        sqlx::query("CREATE TABLE hallinto (avain TEXT PRIMARY KEY, arvo INTEGER, teksti TEXT)")
            .execute(&mut *ta)
            .await?;

        sqlx::query("INSERT INTO hallinto (avain, arvo) VALUES ('versio', $1)")
            .bind(PROGRAM_DB_VERSION)
            .execute(&mut *ta)
            .await?;

        // Seuraavassa versiossa ehkä vaatimuksia: NOT NULL.
        sqlx::query(
            "CREATE TABLE oppilaat \
             (oid SERIAL PRIMARY KEY, \
             sukunimi TEXT, \
             etunimi TEXT, \
             lisatiedot TEXT DEFAULT '')",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query("CREATE INDEX idx_oppilaat_sukunimi_etunimi ON oppilaat (sukunimi, etunimi)")
            .execute(&mut *ta)
            .await?;

        // Seuraavassa versiossa ehkä vaatimuksia: NOT NULL.
        sqlx::query(
            "CREATE TABLE ryhmat \
             (rid SERIAL PRIMARY KEY, \
             nimi TEXT UNIQUE NOT NULL, \
             lisatiedot TEXT DEFAULT '')",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query(
            "CREATE TABLE oppilaat_ryhmat \
             (oid INTEGER NOT NULL REFERENCES oppilaat(oid) ON DELETE CASCADE, \
             rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE, \
             PRIMARY KEY (oid, rid))",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query("CREATE INDEX idx_oppilaat_ryhmat_rid ON oppilaat_ryhmat (rid)")
            .execute(&mut *ta)
            .await?;

        // Seuraavassa versiossa ehkä vaatimuksia: NOT NULL.
        // Painokertoimeen ehkä vaatimus x IS NULL OR x >= 1.
        sqlx::query(
            "CREATE TABLE suoritukset \
             (sid SERIAL PRIMARY KEY, \
             rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE, \
             sija INTEGER, \
             nimi TEXT DEFAULT '', \
             lyhenne TEXT DEFAULT '', \
             painokerroin INTEGER)",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query("CREATE INDEX idx_suoritukset_rid ON suoritukset (rid)")
            .execute(&mut *ta)
            .await?;

        // Seuraavassa tietokannan päivityksessä:
        // UPDATE arvosanat SET arvosana = NULL WHERE arvosana = '';
        // UPDATE arvosanat SET lisatiedot = NULL WHERE lisatiedot = '';
        // DELETE FROM arvosanat WHERE arvosana IS NULL AND lisatiedot IS NULL;
        sqlx::query(
            "CREATE TABLE arvosanat \
             (sid INTEGER NOT NULL REFERENCES suoritukset(sid) ON DELETE CASCADE, \
             oid INTEGER NOT NULL REFERENCES oppilaat(oid) ON DELETE CASCADE, \
             arvosana TEXT, \
             lisatiedot TEXT, \
             PRIMARY KEY (sid, oid))",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query("CREATE INDEX idx_arvosanat_oid ON arvosanat (oid)")
            .execute(&mut *ta)
            .await?;

        sqlx::query(
            "CREATE VIEW view_oppilaat AS \
             SELECT o.oid, o.sukunimi, o.etunimi, r.rid, r.nimi AS ryhma, o.lisatiedot AS olt \
             FROM oppilaat AS o \
             LEFT JOIN oppilaat_ryhmat AS j ON j.oid = o.oid \
             LEFT JOIN ryhmat AS r ON r.rid = j.rid",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query(
            "CREATE VIEW view_suoritukset AS \
             SELECT r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, \
             s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin \
             FROM suoritukset AS s \
             JOIN ryhmat AS r ON r.rid = s.rid",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query(
            "CREATE VIEW view_arvosanat AS \
             SELECT o.oid, o.sukunimi, o.etunimi, o.lisatiedot AS olt, \
             r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, \
             s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin, \
             a.arvosana, a.lisatiedot AS alt \
             FROM oppilaat_ryhmat AS j \
             JOIN oppilaat AS o ON o.oid = j.oid \
             JOIN ryhmat AS r ON r.rid = j.rid \
             LEFT JOIN suoritukset AS s ON r.rid = s.rid \
             LEFT JOIN arvosanat AS a ON o.oid = a.oid AND s.sid = a.sid",
        )
        .execute(&mut *ta)
        .await?;

        ta.commit().await?;
    }

    Ok(())
}
