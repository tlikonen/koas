use super::*;
use std::cmp::Ordering;

const PROGRAM_DB_VERSION: i32 = 11;

pub async fn connect(config: &Config) -> Result<DBase> {
    let connect_string = format!(
        "postgres://{user}:{password}@{host}:{port}/{db}",
        user = config.user,
        password = config.password,
        host = config.host,
        port = config.port,
        db = config.database,
    );

    let db = DBase::connect(&connect_string).await?;
    init::initialize(db).await
}

pub(super) async fn initialize(mut db: DBase) -> Result<DBase> {
    let mut stderr = io::stderr();

    if db_exists(&mut db).await? {
        let db_version = get_db_version(&mut db).await?;
        match db_version.cmp(&PROGRAM_DB_VERSION) {
            Ordering::Equal => (),
            Ordering::Greater => return Err(Error::OldProgram),
            Ordering::Less => return Err(Error::OldDatabase(OldDb { db })),
        }
    } else {
        // Database objects don't exist. Create all.
        let _ = writeln!(stderr, "Valmistellaan arvosanatietokanta.");

        let mut ta = db.begin().await?;

        let commands = [
            // hallinto
            "CREATE TABLE hallinto (avain TEXT PRIMARY KEY, arvo INTEGER, teksti TEXT)",
            // oppilaat
            "CREATE TABLE oppilaat \
             (oid SERIAL PRIMARY KEY, \
             sukunimi TEXT NOT NULL, \
             etunimi TEXT NOT NULL, \
             lisatiedot TEXT NOT NULL DEFAULT '')",
            "CREATE INDEX idx_oppilaat_sukunimi_etunimi ON oppilaat (sukunimi, etunimi)",
            // ryhmat
            "CREATE TABLE ryhmat \
             (rid SERIAL PRIMARY KEY, \
             nimi TEXT UNIQUE NOT NULL, \
             lisatiedot TEXT NOT NULL DEFAULT '')",
            // oppilaat_ryhmat
            "CREATE TABLE oppilaat_ryhmat \
             (oid INTEGER NOT NULL REFERENCES oppilaat(oid) ON DELETE CASCADE ON UPDATE CASCADE, \
             rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE ON UPDATE CASCADE, \
             PRIMARY KEY (oid, rid))",
            "CREATE INDEX idx_oppilaat_ryhmat_rid ON oppilaat_ryhmat (rid)",
            // suoritukset
            "CREATE TABLE suoritukset \
             (sid SERIAL PRIMARY KEY, \
             rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE ON UPDATE CASCADE, \
             sija INTEGER NOT NULL CHECK (sija >= 1), \
             nimi TEXT NOT NULL, \
             lyhenne TEXT NOT NULL, \
             painokerroin INTEGER CHECK (painokerroin >= 1))",
            "CREATE INDEX idx_suoritukset_rid ON suoritukset (rid)",
            // arvosanat
            "CREATE TABLE arvosanat \
             (sid INTEGER NOT NULL REFERENCES suoritukset(sid) ON DELETE CASCADE ON UPDATE CASCADE, \
             oid INTEGER NOT NULL REFERENCES oppilaat(oid) ON DELETE CASCADE ON UPDATE CASCADE, \
             arvosana TEXT, \
             lisatiedot TEXT, \
             PRIMARY KEY (sid, oid))",
            "CREATE INDEX idx_arvosanat_oid ON arvosanat (oid)",
            // CREATE VIEW
            "CREATE VIEW view_oppilaat AS \
             SELECT o.oid, o.sukunimi, o.etunimi, r.rid, r.nimi AS ryhma, o.lisatiedot AS olt \
             FROM oppilaat AS o \
             LEFT JOIN oppilaat_ryhmat AS j ON j.oid = o.oid \
             LEFT JOIN ryhmat AS r ON r.rid = j.rid",
            "CREATE VIEW view_suoritukset AS \
             SELECT r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, \
             s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin \
             FROM suoritukset AS s \
             JOIN ryhmat AS r ON r.rid = s.rid",
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
        ];

        for command in commands {
            sqlx::query(command).execute(&mut *ta).await?;
        }

        sqlx::query("INSERT INTO hallinto (avain, arvo) VALUES ('versio', $1)")
            .bind(PROGRAM_DB_VERSION)
            .execute(&mut *ta)
            .await?;

        ta.commit().await?;
    }

    Ok(db)
}

async fn db_exists(db: &mut DBase) -> Result<bool> {
    let exists = sqlx::query("SELECT 1 FROM pg_tables WHERE tablename = 'hallinto'")
        .fetch_optional(&mut *db)
        .await?
        .is_some();
    Ok(exists)
}

async fn get_db_version(db: &mut DBase) -> Result<i32> {
    let version: i32 = sqlx::query("SELECT arvo FROM hallinto WHERE avain = 'versio'")
        .fetch_one(&mut *db)
        .await?
        .try_get("arvo")?;
    Ok(version)
}

/// Old database version that needs upgrading.
#[derive(Debug)]
pub struct OldDb {
    db: DBase,
}

impl OldDb {
    /// Upgrade database and return connection.
    pub async fn upgrade(self) -> Result<DBase> {
        let mut db = self.db;

        let db_version = get_db_version(&mut db).await?;
        if db_version >= PROGRAM_DB_VERSION {
            return Err(Error::from("Tietokantaa ei päivitetty."));
        }

        for version in (db_version + 1)..=PROGRAM_DB_VERSION {
            let mut ta = db.begin().await?;
            match version {
                11 => upgrade_to_version_11(&mut ta).await?,
                ver => {
                    return Err(Error::from(format!(
                        "Päivittäminen versioon {ver} ei ole mahdollista."
                    )));
                }
            }
            ta.commit().await?;
        }
        Ok(db)
    }
}

async fn upgrade_to_version_11(db: &mut DBase) -> Result<()> {
    // Tarkempia vaatimuksia: NOT NULL ja CHECK. Viiteavaimiin lisäksi
    // ON UPDATE CASCADE.
    const VERSION: i32 = 11;
    let commands = [
        // oppilaat
        "UPDATE oppilaat SET lisatiedot = '' WHERE lisatiedot IS NULL",
        "ALTER TABLE oppilaat ALTER COLUMN sukunimi SET NOT NULL",
        "ALTER TABLE oppilaat ALTER COLUMN etunimi SET NOT NULL",
        "ALTER TABLE oppilaat ALTER COLUMN lisatiedot SET NOT NULL",
        // ryhmat
        "UPDATE ryhmat SET lisatiedot = '' WHERE lisatiedot IS NULL",
        "ALTER TABLE ryhmat ALTER COLUMN lisatiedot SET NOT NULL",
        // oppilaat_ryhmat
        "ALTER TABLE oppilaat_ryhmat DROP CONSTRAINT oppilaat_ryhmat_oid_fkey",
        "ALTER TABLE oppilaat_ryhmat DROP CONSTRAINT oppilaat_ryhmat_rid_fkey",
        "ALTER TABLE oppilaat_ryhmat ADD FOREIGN KEY (oid) REFERENCES oppilaat(oid) \
         ON DELETE CASCADE ON UPDATE CASCADE",
        "ALTER TABLE oppilaat_ryhmat ADD FOREIGN KEY (rid) REFERENCES ryhmat(rid) \
         ON DELETE CASCADE ON UPDATE CASCADE",
        // suoritukset
        "UPDATE suoritukset SET sija = 999999 WHERE sija IS NULL OR sija < 1",
        "UPDATE suoritukset SET painokerroin = NULL WHERE painokerroin < 1",
        "ALTER TABLE suoritukset ALTER COLUMN sija SET NOT NULL",
        "ALTER TABLE suoritukset ALTER COLUMN nimi SET NOT NULL",
        "ALTER TABLE suoritukset ALTER COLUMN lyhenne SET NOT NULL",
        "ALTER TABLE suoritukset ALTER COLUMN nimi DROP DEFAULT",
        "ALTER TABLE suoritukset ALTER COLUMN lyhenne DROP DEFAULT",
        "ALTER TABLE suoritukset ADD CHECK (sija >= 1)",
        "ALTER TABLE suoritukset ADD CHECK (painokerroin >= 1)",
        "ALTER TABLE suoritukset DROP CONSTRAINT suoritukset_rid_fkey",
        "ALTER TABLE suoritukset ADD FOREIGN KEY (rid) REFERENCES ryhmat(rid) \
         ON DELETE CASCADE ON UPDATE CASCADE",
        // arvosanat
        "UPDATE arvosanat SET arvosana = NULL WHERE arvosana = ''",
        "UPDATE arvosanat SET lisatiedot = NULL WHERE lisatiedot = ''",
        "DELETE FROM arvosanat WHERE arvosana IS NULL AND lisatiedot IS NULL",
        "ALTER TABLE arvosanat DROP CONSTRAINT arvosanat_oid_fkey",
        "ALTER TABLE arvosanat DROP CONSTRAINT arvosanat_sid_fkey",
        "ALTER TABLE arvosanat ADD FOREIGN KEY (oid) REFERENCES oppilaat(oid) \
         ON DELETE CASCADE ON UPDATE CASCADE",
        "ALTER TABLE arvosanat ADD FOREIGN KEY (sid) REFERENCES suoritukset(sid) \
         ON DELETE CASCADE ON UPDATE CASCADE",
    ];

    for command in commands {
        sqlx::query(command).execute(&mut *db).await?;
    }

    sqlx::query("UPDATE hallinto SET arvo = $1 WHERE avain = 'versio'")
        .bind(VERSION)
        .execute(&mut *db)
        .await?;

    Ok(())
}
