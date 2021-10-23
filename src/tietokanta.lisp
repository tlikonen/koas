;;;; Koas - tietokantaohjelma kouluarvosanoille


;;; Copyright (C) 2013-2021 Teemu Likonen <tlikonen@iki.fi>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; The license text: <http://www.gnu.org/licenses/gpl-3.0.html>


(defpackage #:tietokanta
  (:use #:cl #:yhteinen)
  (:export
   #:alusta-sqlite-tiedostopolku #:*sqlite-tiedosto*
   #:*sqlite-nimi* #:*psql-nimi*
   #:query-last-insert-rowid
   #:lisää-muokkauslaskuriin
   #:tietokanta-käytössä #:sqlite-käytössä
   #:molemmat-tietokannat-käytössä
   #:query #:query-1 #:query-nconc
   #:sql-mj #:sql-like-suoja
   #:with-transaction
   #:eheytys
   #:query-returning
   #:substitute-nulls
   #:kopioi-sqlite-psql
   #:kopioi-psql-sqlite
   #:ohjelman-alkuilmoitus
   ))

(in-package #:tietokanta)


(defvar *sqlite-tiedosto* nil)
(defvar *tietokanta* nil)
(defvar *muokkaukset-kunnes-eheytys* 5000)
(defparameter *psql-last-insert-id* 0)
(defparameter *psql-nimi* "psql")
(defparameter *sqlite-nimi* "sqlite")
(defparameter *ohjelman-tietokantaversio* 10)


(defclass tietokanta-asetukset ()
  ((user :accessor user :initarg :user :type string)
   (password :accessor password :initarg :password :type string)
   (database :accessor database :initarg :database :type string )
   (host :accessor host :initarg :host :type string)
   (port :accessor port :initarg :port :type integer)))


(defvar *psql-asetukset*
  (make-instance 'tietokanta-asetukset
                 :user "" :password ""
                 :database "" :host "" :port 5432))


(defun sqlite-yhteys-p ()
  (typep *tietokanta* 'sqlite:sqlite-handle))

(defun psql-yhteys-p ()
  (typep *tietokanta* 'postmodern:database-connection))


(defun ohjelman-alkuilmoitus ()
  (cond ((sqlite-yhteys-p)
         (viesti "KOAS - SQLite (~A)~%" *sqlite-tiedosto*))
        ((psql-yhteys-p)
         (viesti "KOAS - PostgreSQL (postgresql://~A@~A:~A/~A)~%"
                 (user *psql-asetukset*)
                 (host *psql-asetukset*)
                 (port *psql-asetukset*)
                 (database *psql-asetukset*)))))


(defun alusta-sqlite-tiedostopolku ()
  (unless *sqlite-tiedosto*
    (setf *sqlite-tiedosto*
          (merge-pathnames (make-pathname :directory '(:relative ".config")
                                          :name "koas-kehitys" :type "db")
                           (user-homedir-pathname))))
  (ensure-directories-exist *sqlite-tiedosto*))


(defun query (format-string &rest parameters)
  (cond ((sqlite-yhteys-p)
         (sqlite:execute-to-list *tietokanta*
                                 (apply #'format nil format-string
                                        parameters)))
        ((psql-yhteys-p)
         (pomo:query (apply #'format nil format-string parameters)))
        (t (virhe "Ei yhteyttä tietokantaan."))))


(defun query-returning (ret format-string &rest parameters)
  (cond ((sqlite-yhteys-p)
         (sqlite:execute-to-list *tietokanta*
                                 (apply #'format nil format-string
                                        parameters)))
        ((psql-yhteys-p)
         (setf *psql-last-insert-id*
               (caar (pomo:query
                      (apply #'format nil
                             (concatenate 'string format-string
                                          " RETURNING " ret)
                             parameters)))))
        (t (virhe "Ei yhteyttä tietokantaan."))))


(defun query-nconc (format-string &rest parameters)
  (reduce #'nconc (apply #'query format-string parameters)))


(defun query-1 (format-string &rest parameters)
  (caar (apply #'query format-string parameters)))


(defmacro with-transaction (&body body)
  `(cond ((sqlite-yhteys-p)
          (sqlite:with-transaction *tietokanta* ,@body))
         ((psql-yhteys-p)
          (pomo:with-transaction () ,@body))))


(defun query-last-insert-rowid ()
  ;; Sqlite 3.35.0 (2021-03-12) tukee INSERT ... RETURNING -lausetta.
  ;; Sitten kun se otetaan käyttöön, voidaan poistaa tämä funktio.
  (cond ((sqlite-yhteys-p)
         (sqlite:last-insert-rowid *tietokanta*))
        ((psql-yhteys-p)
         *psql-last-insert-id*)
        (t (virhe "Ei yhteyttä tietokantaan."))))


(defun substitute-nulls (seq)
  (substitute nil :null seq))


(defun sql-mj (asia)
  (string-io:sql-string (typecase asia
                          (string asia)
                          (character (string asia))
                          (integer (princ-to-string asia))
                          (t ""))))


(defun sql-like-suoja (asia &optional jokerit)
  (string-io:sql-escape-like (typecase asia
                               (string asia)
                               (character (string asia))
                               (integer (princ-to-string asia))
                               (t ""))
                             :wild-before jokerit
                             :wild-after jokerit))


(defun aseta-muokkauslaskuri (arvo)
  (when (sqlite-yhteys-p)
    (query "UPDATE hallinto SET arvo = ~A WHERE avain = 'muokkauslaskuri'"
           arvo)
    arvo))


(defun hae-muokkauslaskuri ()
  (when (sqlite-yhteys-p)
    (query-1 "SELECT arvo FROM hallinto WHERE avain = 'muokkauslaskuri'")))


(defun lisää-muokkauslaskuriin (muokkaukset)
  (when (sqlite-yhteys-p)
    (query "UPDATE hallinto SET arvo = arvo + ~A ~
                WHERE avain = 'muokkauslaskuri'"
           muokkaukset))
  muokkaukset)


(defun eheytys (&optional nyt)
  (when (sqlite-yhteys-p)
    (let ((laskuri (or (hae-muokkauslaskuri) 0)))
      (when (or nyt (>= laskuri *muokkaukset-kunnes-eheytys*))
        (ignore-errors
         (query "VACUUM")
         (aseta-muokkauslaskuri 0)
         t)))))


(defgeneric päivitä-tietokanta (tyyppi versio))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 2)))
  ;; Kaikki arvosanat yhteen taulukkoon.
  (with-transaction
    (query "CREATE TABLE arvosanat ~
                (sid INTEGER, oid INTEGER, arvosana TEXT, lisatiedot TEXT)")
    (loop :for (sid . nil) :in (query "SELECT sid FROM suoritukset")
          :do
          (loop :for (oid arv lis)
                :in (query "SELECT * FROM suoritus_~A" sid)
                :do (query "INSERT INTO arvosanat ~
                                (sid, oid, arvosana, lisatiedot)
                                VALUES (~A, ~A, ~A, ~A)"
                           sid oid (sql-mj arv) (sql-mj lis)))
          (ignore-errors (query "DROP TABLE suoritus_~A" sid)))
    (query "INSERT INTO hallinto (avain, arvo) VALUES ('versio', 2)")))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 3)))
  ;; Oppilaiden ryhmät määritellään uudessa taulukossa oppilaat_ryhmat.
  ;; Myös ryhmän suoritukset määritellään järkevämmin relaatioilla eikä
  ;; merkkijonolistan avulla.
  (with-transaction
    (unless (hae-muokkauslaskuri)
      (query "INSERT INTO hallinto (avain, arvo) ~
                VALUES ('muokkauslaskuri', 0)"))

    (query "CREATE TABLE oppilaat_v3 ~
                (oid INTEGER PRIMARY KEY, ~
                sukunimi TEXT, etunimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")
    (query "CREATE TABLE ryhmat_v3 ~
                (rid INTEGER PRIMARY KEY, nimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")
    (query "CREATE TABLE suoritukset_v3 ~
                (sid INTEGER PRIMARY KEY, ~
                rid INTEGER, ~
                sija INTEGER, ~
                nimi TEXT DEFAULT '', ~
                lyhenne TEXT DEFAULT '', ~
                painokerroin INTEGER)")
    (query "CREATE TABLE oppilaat_ryhmat ~
                (oid INTEGER, rid INTEGER)")

    (loop :for (ryhmä . nil) :in (query "SELECT ryhma FROM ryhmat")
          :do (query "INSERT INTO ryhmat_v3 ~
                (nimi, lisatiedot) VALUES (~A, '')"
                     (sql-mj ryhmä)))

    (loop :for (oid sukunimi etunimi ryhmä-mj lisätiedot)
          :in (query "SELECT * FROM oppilaat")
          :for ryhmät := (or (mj-lista-listaksi ryhmä-mj) (list "ryhmätön"))
          :do
          (query "INSERT INTO oppilaat_v3 ~
                (oid, sukunimi, etunimi, lisatiedot)
                VALUES (~A, ~A, ~A, ~A)"
                 oid (sql-mj sukunimi) (sql-mj etunimi) (sql-mj lisätiedot))
          (loop :for ryhmä :in ryhmät
                :do
                (let ((rid (query-1 "SELECT rid FROM ryhmat_v3 ~
                        WHERE nimi = ~A" (sql-mj ryhmä))))
                  (unless rid
                    (query "INSERT INTO ryhmat_v3 ~
                        (nimi, lisatiedot) VALUES (~A, '')"
                           (sql-mj ryhmä))
                    (setf rid (query-last-insert-rowid)))
                  (query "INSERT INTO oppilaat_ryhmat ~
                        (oid, rid) VALUES (~A, ~A)" oid rid))))

    (loop :for (ryhmä suoritukset-mj) :in (query "SELECT * FROM ryhmat")
          :for suoritukset := (mapcar #'lue-numero
                                      (mj-lista-listaksi suoritukset-mj))
          :do
          (loop :for sid :in suoritukset
                :for sija :upfrom 1
                :for rid := (query-1 "SELECT rid FROM ryhmat_v3 ~
                                WHERE nimi = ~A" (sql-mj ryhmä))
                :for (nimi lyhenne painokerroin)
                := (first (query "SELECT nimi, lyhenne, painokerroin ~
                                FROM suoritukset WHERE sid = ~A" sid))
                :do
                (query "INSERT INTO suoritukset_v3 ~
                        (sid, rid, sija, nimi, lyhenne, painokerroin) ~
                        VALUES (~A, ~A, ~A, ~A, ~A, ~A)"
                       sid rid sija (sql-mj nimi) (sql-mj lyhenne)
                       (or painokerroin "NULL"))))

    (query "DROP TABLE oppilaat")
    (query "DROP TABLE ryhmat")
    (query "DROP TABLE suoritukset")

    (query "ALTER TABLE oppilaat_v3 RENAME TO oppilaat")
    (query "ALTER TABLE ryhmat_v3 RENAME TO ryhmat")
    (query "ALTER TABLE suoritukset_v3 RENAME TO suoritukset")

    (query "UPDATE hallinto SET arvo = 3 WHERE avain = 'versio'")))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 4)))
  ;; Valmiita kyselyjä perustoimintoja varten. Hallinto-taulukon
  ;; arvo-kentän tietotyypiksi integer.
  (with-transaction
    (query "CREATE VIEW view_oppilaat AS ~
                SELECT o.oid, o.sukunimi, o.etunimi, ~
                r.rid, r.nimi AS ryhma, o.lisatiedot AS olt ~
                FROM oppilaat AS o ~
                LEFT JOIN oppilaat_ryhmat AS j ~
                ON j.oid = o.oid AND j.rid = r.rid ~
                LEFT JOIN ryhmat AS r ON r.rid = j.rid")

    (query "CREATE VIEW view_suoritukset AS ~
                SELECT r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, ~
                s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin ~
                FROM suoritukset AS s ~
                JOIN ryhmat AS r ON r.rid = s.rid")

    (query "CREATE VIEW view_arvosanat AS ~
                SELECT o.oid, o.sukunimi, o.etunimi, o.lisatiedot AS olt, ~
                r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, ~
                s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin, ~
                a.arvosana, a.lisatiedot AS alt ~
                FROM oppilaat_ryhmat AS j ~
                JOIN oppilaat AS o ON o.oid = j.oid ~
                JOIN ryhmat AS r ON r.rid = j.rid ~
                LEFT JOIN suoritukset AS s ON r.rid = s.rid ~
                LEFT JOIN arvosanat AS a ON o.oid = a.oid AND s.sid = a.sid")

    (query "CREATE TABLE hallinto_v4 (avain TEXT UNIQUE, arvo INTEGER)")
    (query "INSERT INTO hallinto_v4 SELECT avain, arvo FROM hallinto")
    (query "DROP TABLE hallinto")
    (query "ALTER TABLE hallinto_v4 RENAME TO hallinto")

    (query "UPDATE hallinto SET arvo = 4 WHERE avain = 'versio'")))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 5)))
  ;; Foreign key sekä composite primary key käyttöön.
  (query "PRAGMA foreign_keys = OFF")

  (with-transaction
    (query "ALTER TABLE oppilaat_ryhmat RENAME TO oppilaat_ryhmat_vanha")
    (query "ALTER TABLE suoritukset RENAME TO suoritukset_vanha")
    (query "ALTER TABLE arvosanat RENAME TO arvosanat_vanha")

    (query "CREATE TABLE oppilaat_ryhmat ~
        (oid INTEGER NOT NULL REFERENCES oppilaat(oid) ON DELETE CASCADE, ~
        rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE, ~
        PRIMARY KEY (oid, rid))")

    (query "CREATE TABLE suoritukset ~
        (sid INTEGER PRIMARY KEY, ~
        rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE, ~
        sija INTEGER, ~
        nimi TEXT DEFAULT '', ~
        lyhenne TEXT DEFAULT '', ~
        painokerroin INTEGER)")

    (query "CREATE TABLE arvosanat ~
        (sid INTEGER NOT NULL REFERENCES suoritukset(sid) ON DELETE CASCADE, ~
        oid INTEGER NOT NULL REFERENCES oppilaat(oid) ON DELETE CASCADE, ~
        arvosana TEXT, ~
        lisatiedot TEXT, ~
        PRIMARY KEY (sid, oid))")

    (query "INSERT INTO oppilaat_ryhmat ~
        SELECT oid, rid FROM oppilaat_ryhmat_vanha")
    (query "INSERT INTO suoritukset ~
        SELECT sid, rid, sija, nimi, lyhenne, painokerroin ~
        FROM suoritukset_vanha")
    (query "INSERT INTO arvosanat ~
        SELECT sid, oid, arvosana, lisatiedot FROM arvosanat_vanha")

    (query "DROP TABLE oppilaat_ryhmat_vanha")
    (query "DROP TABLE suoritukset_vanha")
    (query "DROP TABLE arvosanat_vanha")

    (query "UPDATE hallinto SET arvo = 5 WHERE avain = 'versio'"))

  (query "PRAGMA foreign_keys = ON"))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 6)))
  ;; UNIQUE NOT NULL -vaatimus ryhmän nimelle.
  (query "PRAGMA foreign_keys = OFF")

  (with-transaction
    (query "ALTER TABLE ryhmat RENAME TO ryhmat_vanha")

    (query "CREATE TABLE ryhmat ~
        (rid INTEGER PRIMARY KEY, ~
        nimi TEXT UNIQUE NOT NULL, ~
        lisatiedot TEXT DEFAULT '')")
    (query "INSERT INTO ryhmat ~
        SELECT rid, nimi, lisatiedot FROM ryhmat_vanha")
    (query "DROP TABLE ryhmat_vanha")

    (query "UPDATE hallinto SET arvo = 6 WHERE avain = 'versio'"))

  (query "PRAGMA foreign_keys = ON"))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 7)))
  (with-transaction
    (query "UPDATE hallinto SET arvo = 7 WHERE avain = 'versio'")
    (query "PRAGMA auto_vacuum = FULL")))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 8)))
  ;; Korjataan view_oppilaat: ON-lause ei voi viitata seuraavaan
  ;; JOINiin.
  (with-transaction
    (query "DROP VIEW IF EXISTS view_oppilaat")
    (query "CREATE VIEW view_oppilaat AS ~
                SELECT o.oid, o.sukunimi, o.etunimi, ~
                r.rid, r.nimi AS ryhma, o.lisatiedot AS olt ~
                FROM oppilaat AS o ~
                LEFT JOIN oppilaat_ryhmat AS j ON j.oid = o.oid ~
                LEFT JOIN ryhmat AS r ON r.rid = j.rid")
    (query "UPDATE hallinto SET arvo = 8 WHERE avain = 'versio'")))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 9)))
  (with-transaction
    (query "CREATE INDEX idx_oppilaat_ryhmat_rid ON oppilaat_ryhmat (rid)")
    (query "CREATE INDEX idx_suoritukset_rid ON suoritukset (rid)")
    (query "CREATE INDEX idx_arvosanat_oid ON arvosanat (oid)")
    (query "UPDATE hallinto SET arvo = 9 WHERE avain = 'versio'")))


(defmethod päivitä-tietokanta ((tyyppi sqlite:sqlite-handle)
                               (versio (eql 10)))
  ;; Lisätään hallinto-taulukkoon sarake teksti TEXT ja
  ;; oppilaat-taulukkoon indeksi.
  (with-transaction
    (query "ALTER TABLE hallinto RENAME TO hallinto_vanha")
    (query "CREATE TABLE hallinto ~
                (avain TEXT PRIMARY KEY NOT NULL, ~
                arvo INTEGER, ~
                teksti TEXT)")
    (query "INSERT INTO hallinto (avain, arvo) ~
                SELECT avain, arvo FROM hallinto_vanha")

    (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('tietokanta tyyppi', ~A)" (sql-mj *sqlite-nimi*))
    (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-host', '')")
    (query "INSERT INTO hallinto (avain, arvo) ~
                VALUES ('psql-port', 5432)")
    (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-database', '')")
    (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-user', '')")
    (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-password', '')")

    (query "DROP TABLE hallinto_vanha")

    (query "CREATE INDEX idx_oppilaat_sukunimi_etunimi
                ON oppilaat (sukunimi, etunimi)")

    (query "UPDATE hallinto SET arvo = 10 WHERE avain = 'versio'")))


(defun tietokannan-versio ()
  ;; Täällä tarvitaan LUE-NUMERO-funktiota, koska aiemmissa versioissa
  ;; arvo-kenttä oli merkkijonotyyppiä.
  (let ((kysely (query-1 "SELECT arvo FROM hallinto WHERE avain = 'versio'")))
    (if kysely (lue-numero kysely) 1)))


(defun tietokannan-versiotarkistus (tyyppi)
  (let ((versio (tietokannan-versio)))
    (cond ((< versio *ohjelman-tietokantaversio*)
           (viesti "Päivitetään ~A-tietokanta versioon ~D.~%"
                   (cond ((typep tyyppi 'sqlite:sqlite-handle)
                          "SQLite")
                         ((typep tyyppi 'pomo:database-connection)
                          "PostgreSQL"))
                   *ohjelman-tietokantaversio*)
           (loop :for kohde :from (1+ versio)
                 :upto *ohjelman-tietokantaversio*
                 :do (päivitä-tietokanta tyyppi kohde))
           (eheytys t))
          ((> versio *ohjelman-tietokantaversio*)
           (virhe "ONGELMA! Tietokannan versio on ~A mutta ohjelma ~
                osaa vain version ~A. Päivitä ohjelma!"
                  versio *ohjelman-tietokantaversio*)))))


(defgeneric alusta-tietokanta (tyyppi))


(defmethod alusta-tietokanta ((tyyppi sqlite:sqlite-handle))
  (if (query-1 "SELECT 1 FROM sqlite_master ~
                WHERE type = 'table' AND name = 'hallinto'")
      (tietokannan-versiotarkistus tyyppi)

      ;; Tietokanta puuttuu
      (with-transaction
        (viesti "~&Valmistellaan tietokanta \"~A\".~%"
                (pathconv:namestring *sqlite-tiedosto*))

        (query "PRAGMA auto_vacuum = FULL")

        ;; SQlitessa PRIMARY KEY ei sisällä NOT NULLia, vaikka
        ;; SQL-standardissa pitäisi.
        (query "CREATE TABLE IF NOT EXISTS hallinto ~
                (avain TEXT PRIMARY KEY NOT NULL, ~
                arvo INTEGER, ~
                teksti TEXT)")

        (query "INSERT INTO hallinto (avain, arvo) VALUES ('versio', ~A)"
               *ohjelman-tietokantaversio*)
        (query "INSERT INTO hallinto (avain, arvo) ~
                VALUES ('muokkauslaskuri', 0)")
        (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('tietokanta tyyppi', ~A)" (sql-mj *sqlite-nimi*))
        (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-host', '')")
        (query "INSERT INTO hallinto (avain, arvo) ~
                VALUES ('psql-port', 5432)")
        (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-database', '')")
        (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-user', '')")
        (query "INSERT INTO hallinto (avain, teksti) ~
                VALUES ('psql-password', '')")

        (query "CREATE TABLE oppilaat ~
                (oid INTEGER PRIMARY KEY, ~
                sukunimi TEXT, etunimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")

        (query "CREATE INDEX idx_oppilaat_sukunimi_etunimi
                ON oppilaat (sukunimi, etunimi)")

        (query "CREATE TABLE ryhmat ~
                (rid INTEGER PRIMARY KEY, ~
                nimi TEXT UNIQUE NOT NULL, ~
                lisatiedot TEXT DEFAULT '')")

        (query "CREATE TABLE oppilaat_ryhmat ~
                (oid INTEGER NOT NULL ~
                        REFERENCES oppilaat(oid) ON DELETE CASCADE, ~
                rid INTEGER NOT NULL ~
                        REFERENCES ryhmat(rid) ON DELETE CASCADE, ~
                PRIMARY KEY (oid, rid))")

        (query "CREATE INDEX idx_oppilaat_ryhmat_rid ~
                ON oppilaat_ryhmat (rid)")

        (query "CREATE TABLE suoritukset ~
                (sid INTEGER PRIMARY KEY, ~
                rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE, ~
                sija INTEGER, ~
                nimi TEXT DEFAULT '', ~
                lyhenne TEXT DEFAULT '', ~
                painokerroin INTEGER)")

        (query "CREATE INDEX idx_suoritukset_rid ~
                ON suoritukset (rid)")

        (query "CREATE TABLE arvosanat ~
                (sid INTEGER NOT NULL ~
                        REFERENCES suoritukset(sid) ON DELETE CASCADE, ~
                oid INTEGER NOT NULL ~
                        REFERENCES oppilaat(oid) ON DELETE CASCADE, ~
                arvosana TEXT, ~
                lisatiedot TEXT, ~
                PRIMARY KEY (sid, oid))")

        (query "CREATE INDEX idx_arvosanat_oid ~
                ON arvosanat (oid)")

        (query "CREATE VIEW view_oppilaat AS ~
                SELECT o.oid, o.sukunimi, o.etunimi, ~
                r.rid, r.nimi AS ryhma, o.lisatiedot AS olt ~
                FROM oppilaat AS o ~
                LEFT JOIN oppilaat_ryhmat AS j ON j.oid = o.oid ~
                LEFT JOIN ryhmat AS r ON r.rid = j.rid")

        (query "CREATE VIEW view_suoritukset AS ~
                SELECT r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, ~
                s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin ~
                FROM suoritukset AS s ~
                JOIN ryhmat AS r ON r.rid = s.rid")

        (query "CREATE VIEW view_arvosanat AS ~
                SELECT o.oid, o.sukunimi, o.etunimi, o.lisatiedot AS olt, ~
                r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, ~
                s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin, ~
                a.arvosana, a.lisatiedot AS alt ~
                FROM oppilaat_ryhmat AS j ~
                JOIN oppilaat AS o ON o.oid = j.oid ~
                JOIN ryhmat AS r ON r.rid = j.rid ~
                LEFT JOIN suoritukset AS s ON r.rid = s.rid ~
                LEFT JOIN arvosanat AS a ON o.oid = a.oid AND s.sid = a.sid")))

  (query "PRAGMA foreign_keys = ON")
  (query "PRAGMA case_sensitive_like = ON"))


(defmethod alusta-tietokanta ((tyyppi pomo:database-connection))
  (if (query-1 "SELECT 1 FROM pg_catalog.pg_tables ~
        WHERE schemaname = 'public' AND tablename = 'hallinto'")
      (tietokannan-versiotarkistus tyyppi)

      ;; Tietokanta puuttuu
      (with-transaction
        (viesti "~&Valmistellaan PostgreSQL-tietokanta.~%")

        (query "CREATE TABLE hallinto ~
                (avain TEXT PRIMARY KEY, ~
                arvo INTEGER, ~
                teksti TEXT)")

        (query "INSERT INTO hallinto (avain, arvo) VALUES ('versio', ~A)"
               *ohjelman-tietokantaversio*)

        (query "CREATE TABLE oppilaat ~
                (oid SERIAL PRIMARY KEY, ~
                sukunimi TEXT, etunimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")

        (query "CREATE INDEX idx_oppilaat_sukunimi_etunimi
                ON oppilaat (sukunimi, etunimi)")

        (query "CREATE TABLE ryhmat ~
                (rid SERIAL PRIMARY KEY, ~
                nimi TEXT UNIQUE NOT NULL, ~
                lisatiedot TEXT DEFAULT '')")

        (query "CREATE TABLE oppilaat_ryhmat ~
                (oid INTEGER NOT NULL ~
                        REFERENCES oppilaat(oid) ON DELETE CASCADE, ~
                rid INTEGER NOT NULL ~
                        REFERENCES ryhmat(rid) ON DELETE CASCADE, ~
                PRIMARY KEY (oid, rid))")

        (query "CREATE INDEX idx_oppilaat_ryhmat_rid ~
                ON oppilaat_ryhmat (rid)")

        (query "CREATE TABLE suoritukset ~
                (sid SERIAL PRIMARY KEY, ~
                rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE, ~
                sija INTEGER, ~
                nimi TEXT DEFAULT '', ~
                lyhenne TEXT DEFAULT '', ~
                painokerroin INTEGER)")

        (query "CREATE INDEX idx_suoritukset_rid ~
                ON suoritukset (rid)")

        (query "CREATE TABLE arvosanat ~
                (sid INTEGER NOT NULL ~
                        REFERENCES suoritukset(sid) ON DELETE CASCADE, ~
                oid INTEGER NOT NULL ~
                        REFERENCES oppilaat(oid) ON DELETE CASCADE, ~
                arvosana TEXT, ~
                lisatiedot TEXT, ~
                PRIMARY KEY (sid, oid))")

        (query "CREATE INDEX idx_arvosanat_oid ~
                ON arvosanat (oid)")

        (query "CREATE VIEW view_oppilaat AS ~
                SELECT o.oid, o.sukunimi, o.etunimi, ~
                r.rid, r.nimi AS ryhma, o.lisatiedot AS olt ~
                FROM oppilaat AS o ~
                LEFT JOIN oppilaat_ryhmat AS j ON j.oid = o.oid ~
                LEFT JOIN ryhmat AS r ON r.rid = j.rid")

        (query "CREATE VIEW view_suoritukset AS ~
                SELECT r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, ~
                s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin ~
                FROM suoritukset AS s ~
                JOIN ryhmat AS r ON r.rid = s.rid")

        (query "CREATE VIEW view_arvosanat AS ~
                SELECT o.oid, o.sukunimi, o.etunimi, o.lisatiedot AS olt, ~
                r.rid, r.nimi AS ryhma, r.lisatiedot AS rlt, ~
                s.sid, s.nimi AS suoritus, s.lyhenne, s.sija, s.painokerroin, ~
                a.arvosana, a.lisatiedot AS alt ~
                FROM oppilaat_ryhmat AS j ~
                JOIN oppilaat AS o ON o.oid = j.oid ~
                JOIN ryhmat AS r ON r.rid = j.rid ~
                LEFT JOIN suoritukset AS s ON r.rid = s.rid ~
                LEFT JOIN arvosanat AS a ON o.oid = a.oid AND s.sid = a.sid"))))


(defun connect-sqlite ()
  (unless (sqlite-yhteys-p)
    (sb-posix:umask #o0077)
    (alusta-sqlite-tiedostopolku)
    (setf *tietokanta* (sqlite:connect (pathconv:namestring
                                        *sqlite-tiedosto*)))
    (alusta-tietokanta *tietokanta*)
    *tietokanta*))


(defun disconnect-sqlite ()
  (when (sqlite-yhteys-p)
    (prog1 (sqlite:disconnect *tietokanta*)
      (setf *tietokanta* nil))))


(defun connect-psql (&key (user (user *psql-asetukset*))
                          (password (password *psql-asetukset*))
                          (database (database *psql-asetukset*))
                          (host (host *psql-asetukset*))
                          (port (port *psql-asetukset*)))

  (unless (psql-yhteys-p)
    (setf *tietokanta* (pomo:connect database user password host
                                     :port (or port 5432))
          pomo:*database* *tietokanta*)
    (alusta-tietokanta *tietokanta*)
    *tietokanta*))


(defun disconnect-psql ()
  (when (psql-yhteys-p)
    (pomo:disconnect *tietokanta*)
    (setf *tietokanta* nil
          pomo:*database* nil)))


(defun lue-psql-asetukset ()
  (flet ((lue (avain)
           (query-1 "SELECT teksti FROM hallinto ~
                        WHERE avain = ~A" (sql-mj avain))))

    (setf (user *psql-asetukset*) (lue "psql-user"))
    (setf (password *psql-asetukset*) (lue "psql-password"))
    (setf (database *psql-asetukset*) (lue "psql-database"))
    (setf (host *psql-asetukset*) (lue "psql-host"))
    (setf (port *psql-asetukset*) (query-1 "SELECT arvo FROM hallinto ~
                        WHERE avain = 'psql-port'"))

    (unless (and (integerp (port *psql-asetukset*))
                 (<= 1 (port *psql-asetukset*) 65535)
                 (every (lambda (x)
                          (plusp (length x)))
                        (list (user *psql-asetukset*)
                              (password *psql-asetukset*)
                              (database *psql-asetukset*)
                              (host *psql-asetukset*))))
      (virhe "Virheelliset PostgreSQL-asetukset.")))
  *tietokanta*)


(defmacro tietokanta-käytössä (&body body)
  `(let ((*tietokanta* nil)
         (pomo:*database* nil))
     (unwind-protect
          (progn
            (connect-sqlite)
            (when (equal *psql-nimi* (query-1 "SELECT teksti FROM hallinto ~
                        WHERE avain = 'tietokanta tyyppi'"))
              (lue-psql-asetukset)
              (disconnect-sqlite)
              (connect-psql))
            ,@body)

       (disconnect-psql)
       (disconnect-sqlite))))


(defmacro sqlite-käytössä (&body body)
  `(let ((*tietokanta* nil))
     (unwind-protect (progn (connect-sqlite) ,@body)
       (disconnect-sqlite))))


(defmacro molemmat-tietokannat-käytössä ((&key sqlite-yhteys psql-yhteys)
                                         &body body)
  (let ((sqlite (gensym "SQLITE"))
        (psql (gensym "PSQL")))
    `(let ((*tietokanta* nil)
           (pomo:*database* nil)
           (,sqlite-yhteys nil)
           (,psql-yhteys nil)
           (,sqlite nil)
           (,psql nil))
       (declare (ignorable ,sqlite-yhteys ,psql-yhteys))
       (unwind-protect
            (progn
              (setf ,sqlite (connect-sqlite))
              (lue-psql-asetukset)
              (setf ,psql (connect-psql))
              (setf ,sqlite-yhteys ,sqlite)
              (setf ,psql-yhteys ,psql)
              (setf pomo:*database* ,psql-yhteys)
              (setf *tietokanta* nil)
              ,@body)

         (let ((*tietokanta* ,psql))
           (disconnect-psql))
         (let ((*tietokanta* ,sqlite))
           (disconnect-sqlite))))))


(defun kopioi-sqlite-psql ()
  (molemmat-tietokannat-käytössä
      (:sqlite-yhteys sqlite :psql-yhteys psql)
    (flet ((qluku (fmt &rest args)
             (sqlite:execute-to-list sqlite (apply #'format nil fmt args)))
           (qkirj (fmt &rest args)
             (pomo:query (apply #'format nil fmt args))))

      (pomo:with-transaction ()
        (qkirj "DELETE FROM oppilaat")
        (qkirj "DELETE FROM ryhmat")
        (qkirj "DELETE FROM oppilaat_ryhmat")
        (qkirj "DELETE FROM suoritukset")
        (qkirj "DELETE FROM arvosanat")

        ;; oppilaat
        (loop :for (oid sukunimi etunimi lisätiedot)
              :in (qluku "SELECT oid, sukunimi, etunimi, lisatiedot ~
                                FROM oppilaat")
              :do (qkirj "INSERT INTO oppilaat ~
                        (oid, sukunimi, etunimi, lisatiedot)
                        VALUES (~A, ~A, ~A, ~A)"
                         oid (sql-mj sukunimi) (sql-mj etunimi)
                         (sql-mj lisätiedot)))
        (qkirj "SELECT setval('oppilaat_oid_seq',
                                (SELECT max(oid) FROM oppilaat))")

        ;; ryhmät
        (loop :for (rid nimi lisätiedot)
              :in (qluku "SELECT rid, nimi, lisatiedot FROM ryhmat")
              :do (qkirj "INSERT INTO ryhmat ~
                        (rid, nimi, lisatiedot)
                        VALUES (~A, ~A, ~A)"
                         rid (sql-mj nimi) (sql-mj lisätiedot)))
        (qkirj "SELECT setval('ryhmat_rid_seq',
                                (SELECT max(rid) FROM ryhmat))")

        ;; oppilaat_ryhmät
        (loop :for (oid rid)
              :in (qluku "SELECT oid, rid FROM oppilaat_ryhmat")
              :do (qkirj "INSERT INTO oppilaat_ryhmat ~
                        (oid, rid) VALUES (~A, ~A)"
                         oid rid))

        ;; suoritukset
        (loop :for (sid rid sija nimi lyhenne painokerroin)
              :in (qluku "SELECT sid, rid, sija, ~
                                nimi, lyhenne, painokerroin ~
                                FROM suoritukset")
              :do (qkirj "INSERT INTO suoritukset ~
                        (sid, rid, sija, nimi, lyhenne, painokerroin)
                        VALUES (~A, ~A, ~A, ~A, ~A, ~A)"
                         sid rid sija
                         (sql-mj nimi) (sql-mj lyhenne)
                         (or painokerroin "NULL")))
        (qkirj "SELECT setval('suoritukset_sid_seq',
                                (SELECT max(sid) FROM suoritukset))")

        ;; arvosanat
        (loop :for (sid oid arvosana lisatiedot)
              :in (qluku "SELECT sid, oid, arvosana, lisatiedot ~
                                FROM arvosanat")
              :do (qkirj "INSERT INTO arvosanat ~
                        (sid, oid, arvosana, lisatiedot)
                        VALUES (~A, ~A, ~A, ~A)"
                         sid oid (sql-mj (or arvosana ""))
                         (sql-mj lisatiedot))))))
  *psql-nimi*)


(defun kopioi-psql-sqlite ()
  (molemmat-tietokannat-käytössä
      (:sqlite-yhteys sqlite :psql-yhteys psql)
    (flet ((qluku (fmt &rest args)
             (pomo:query (apply #'format nil fmt args)))
           (qkirj (fmt &rest args)
             (sqlite:execute-to-list sqlite (apply #'format nil fmt args))))

      (sqlite:with-transaction sqlite
        (qkirj "DELETE FROM oppilaat")
        (qkirj "DELETE FROM ryhmat")
        (qkirj "DELETE FROM oppilaat_ryhmat")
        (qkirj "DELETE FROM suoritukset")
        (qkirj "DELETE FROM arvosanat")

        ;; oppilaat
        (loop :for rivi
              :in (qluku "SELECT oid, sukunimi, etunimi, lisatiedot ~
                                FROM oppilaat")
              :for (oid sukunimi etunimi lisätiedot)
                 := (substitute-nulls rivi)
              :do (qkirj "INSERT INTO oppilaat ~
                        (oid, sukunimi, etunimi, lisatiedot)
                        VALUES (~A, ~A, ~A, ~A)"
                         oid (sql-mj sukunimi) (sql-mj etunimi)
                         (sql-mj lisätiedot)))

        ;; ryhmät
        (loop :for rivi
              :in (qluku "SELECT rid, nimi, lisatiedot FROM ryhmat")
              :for (rid nimi lisätiedot) := (substitute-nulls rivi)
              :do (qkirj "INSERT INTO ryhmat ~
                        (rid, nimi, lisatiedot)
                        VALUES (~A, ~A, ~A)"
                         rid (sql-mj nimi) (sql-mj lisätiedot)))

        ;; oppilaat_ryhmät
        (loop :for (oid rid)
              :in (qluku "SELECT oid, rid FROM oppilaat_ryhmat")
              :do (qkirj "INSERT INTO oppilaat_ryhmat ~
                        (oid, rid) VALUES (~A, ~A)"
                         oid rid))

        ;; suoritukset
        (loop :for rivi
              :in (qluku "SELECT sid, rid, sija, ~
                                nimi, lyhenne, painokerroin ~
                                FROM suoritukset")
              :for (sid rid sija nimi lyhenne painokerroin)
                 := (substitute-nulls rivi)
              :do (qkirj "INSERT INTO suoritukset ~
                        (sid, rid, sija, nimi, lyhenne, painokerroin)
                        VALUES (~A, ~A, ~A, ~A, ~A, ~A)"
                         sid rid sija
                         (sql-mj nimi) (sql-mj lyhenne)
                         (or painokerroin "NULL")))

        ;; arvosanat
        (loop :for rivi
              :in (qluku "SELECT sid, oid, arvosana, lisatiedot ~
                                FROM arvosanat")
              :for (sid oid arvosana lisatiedot)
                 := (substitute-nulls rivi)
              :do (qkirj "INSERT INTO arvosanat ~
                        (sid, oid, arvosana, lisatiedot)
                        VALUES (~A, ~A, ~A, ~A)"
                         sid oid (sql-mj (or arvosana ""))
                         (sql-mj lisatiedot)))))

    (let ((*tietokanta* sqlite))
      (eheytys t)))
  *sqlite-nimi*)
