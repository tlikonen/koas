use super::*;
use std::cmp::Ordering;

const PROGRAM_DB_VERSION: i32 = 10;

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

        sqlx::query("CREATE TABLE hallinto (avain TEXT PRIMARY KEY, arvo INTEGER, teksti TEXT)")
            .execute(&mut *ta)
            .await?;

        sqlx::query("INSERT INTO hallinto (avain, arvo) VALUES ('versio', $1)")
            .bind(PROGRAM_DB_VERSION)
            .execute(&mut *ta)
            .await?;

        // UPDATE oppilaat SET lisatiedot = '' WHERE lisatiedot IS NULL
        // ALTER TABLE oppilaat ALTER COLUMN sukunimi SET NOT NULL
        // ALTER TABLE oppilaat ALTER COLUMN etunimi SET NOT NULL
        // ALTER TABLE oppilaat ALTER COLUMN lisatiedot SET NOT NULL
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

        // UPDATE ryhmat SET lisatiedot = '' WHERE lisatiedot IS NULL
        // ALTER TABLE ryhmat ALTER COLUMN lisatiedot SET NOT NULL
        sqlx::query(
            "CREATE TABLE ryhmat \
             (rid SERIAL PRIMARY KEY, \
             nimi TEXT UNIQUE NOT NULL, \
             lisatiedot TEXT DEFAULT '')",
        )
        .execute(&mut *ta)
        .await?;

        // ALTER TABLE oppilaat_ryhmat DROP CONSTRAINT oppilaat_ryhmat_oid_fkey
        // ALTER TABLE oppilaat_ryhmat DROP CONSTRAINT oppilaat_ryhmat_rid_fkey
        //
        // ALTER TABLE oppilaat_ryhmat ADD FOREIGN KEY (oid) REFERENCES oppilaat(oid) ON DELETE CASCADE ON UPDATE CASCADE
        //
        // ALTER TABLE oppilaat_ryhmat ADD FOREIGN KEY (rid) REFERENCES ryhmat(rid) ON DELETE CASCADE ON UPDATE CASCADE
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


        // UPDATE suoritukset SET sija = i32::MAX WHERE sija IS NULL OR sija < 1
        // UPDATE suoritukset SET painokerroin = NULL WHERE painokerroin < 1
        // ALTER TABLE suoritukset ALTER COLUMN sija SET NOT NULL
        // ALTER TABLE suoritukset ALTER COLUMN nimi SET NOT NULL
        // ALTER TABLE suoritukset ALTER COLUMN lyhenne SET NOT NULL
        // ALTER TABLE suoritukset ALTER COLUMN nimi DROP DEFAULT
        // ALTER TABLE suoritukset ALTER COLUMN lyhenne DROP DEFAULT
        // ALTER TABLE suoritukset ADD CHECK (sija >= 1)
        // ALTER TABLE suoritukset ADD CHECK (painokerroin IS NULL or painokerroin >= 1)
        //
        // ALTER TABLE suoritukset DROP CONSTRAINT suoritukset_rid_fkey
        // ALTER TABLE suoritukset ADD FOREIGN KEY (rid) REFERENCES ryhmat(rid) ON DELETE CASCADE ON UPDATE CASCADE
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

        // UPDATE arvosanat SET arvosana = NULL WHERE arvosana = ''
        // UPDATE arvosanat SET lisatiedot = NULL WHERE lisatiedot = ''
        // DELETE FROM arvosanat WHERE arvosana IS NULL AND lisatiedot IS NULL
        //
        // ALTER TABLE arvosanat DROP CONSTRAINT arvosanat_oid_fkey
        // ALTER TABLE arvosanat DROP CONSTRAINT arvosanat_sid_fkey
        //
        // ALTER TABLE arvosanat ADD FOREIGN KEY (oid) REFERENCES oppilaat(oid) ON DELETE CASCADE ON UPDATE CASCADE
        //
        // ALTER TABLE arvosanat ADD FOREIGN KEY (sid) REFERENCES suoritukset(sid) ON DELETE CASCADE ON UPDATE CASCADE
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

async fn upgrade_to_version_11(_db: &mut DBase) -> Result<()> {
    Ok(())
}
