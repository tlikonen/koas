;;;; Koas - tietokantaohjelma kouluarvosanoille


;;; Copyright (C) 2013-2019 Teemu Likonen <tlikonen@iki.fi>
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
   #:query-last-insert-rowid
   #:lisää-muokkauslaskuriin
   #:tietokanta-käytössä
   #:query #:query-1 #:query-nconc
   #:sql-mj #:sql-like-suoja
   #:with-transaction
   ))

(in-package #:tietokanta)


(defvar *tiedosto* nil)
(defvar *tietokanta* nil)
(defvar *muokkaukset-kunnes-eheytys* 5000)
(defparameter *ohjelman-tietokantaversio* 8)


(defun alusta-tiedostopolku ()
  (unless *tiedosto*
    (setf *tiedosto*
          (merge-pathnames (make-pathname :directory '(:relative ".config")
                                          :name "koas" :type "db")
                           (user-homedir-pathname))))
  (ensure-directories-exist *tiedosto*))


(defun query (format-string &rest parameters)
  (if (typep *tietokanta* 'sqlite:sqlite-handle)
      (sqlite:execute-to-list *tietokanta*
                              (apply #'format nil format-string parameters))
      (virhe "Ei yhteyttä tietokantaan.")))


(defun query-nconc (format-string &rest parameters)
  (reduce #'nconc (apply #'query format-string parameters)))


(defun query-1 (format-string &rest parameters)
  (caar (apply #'query format-string parameters)))


(defmacro with-transaction (&body body)
  `(sqlite:with-transaction *tietokanta* ,@body))


(defun query-last-insert-rowid ()
  (sqlite:last-insert-rowid *tietokanta*))


(defun sql-mj (asia)
  (with-output-to-string (ulos)
    (princ #\' ulos)
    (loop :for merkki :across (typecase asia
                                (string asia)
                                (character (string asia))
                                (integer (princ-to-string asia))
                                (t ""))
          :do (princ (if (char= merkki #\') "''" merkki) ulos))
    (princ #\' ulos)))


(defun sql-like-suoja (mj &optional jokerit)
  (with-output-to-string (ulos)
    (format ulos "'~A" (if jokerit "%" ""))
    (loop :for merkki :across (typecase mj
                                (string mj)
                                (character (string mj))
                                (integer (princ-to-string mj))
                                (t ""))
          :do (princ (cond ((char= merkki #\') "''")
                           ((find merkki "_%\\") (format nil "\\~A" merkki))
                           (t merkki))
                     ulos))
    (format ulos "~A' ESCAPE '\\'" (if jokerit "%" ""))))


(defun aseta-muokkauslaskuri (arvo)
  (query "UPDATE hallinto SET arvo = ~A WHERE avain = 'muokkauslaskuri'"
         arvo)
  arvo)


(defun hae-muokkauslaskuri ()
  (query-1 "SELECT arvo FROM hallinto WHERE avain = 'muokkauslaskuri'"))


(defun lisää-muokkauslaskuriin (muokkaukset)
  (query "UPDATE hallinto SET arvo = arvo + ~A WHERE avain = 'muokkauslaskuri'"
         muokkaukset)
  muokkaukset)


(defun eheytys (&optional nyt)
  (let ((laskuri (or (hae-muokkauslaskuri) 0)))
    (when (or nyt (>= laskuri *muokkaukset-kunnes-eheytys*))
      (ignore-errors
        (query "VACUUM")
        (aseta-muokkauslaskuri 0)
        t))))


(defgeneric päivitä-tietokanta (versio))


(defmethod päivitä-tietokanta ((versio (eql 2)))
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


(defmethod päivitä-tietokanta ((versio (eql 3)))
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


(defmethod päivitä-tietokanta ((versio (eql 4)))
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


(defmethod päivitä-tietokanta ((versio (eql 5)))
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


(defmethod päivitä-tietokanta ((versio (eql 6)))
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


(defmethod päivitä-tietokanta ((versio (eql 7)))
  (with-transaction
    (query "UPDATE hallinto SET arvo = 7 WHERE avain = 'versio'")
    (query "PRAGMA auto_vacuum = FULL")))


(defmethod päivitä-tietokanta ((versio (eql 8)))
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


(defun tietokannan-versio ()
  ;; Täällä tarvitaan LUE-NUMERO-funktiota, koska aiemmissa versioissa
  ;; arvo-kenttä oli merkkijonotyyppiä.
  (let ((kysely (query-1 "SELECT arvo FROM hallinto WHERE avain = 'versio'")))
    (if kysely (lue-numero kysely) 1)))


(defun alusta-tietokanta ()
  (if (query-1 "SELECT 1 FROM sqlite_master ~
                WHERE type = 'table' AND name = 'hallinto'")
      (let ((versio (tietokannan-versio)))
        (cond ((< versio *ohjelman-tietokantaversio*)
               (viesti "Päivitetään tietokanta: v~D -> v~D.~%"
                       versio *ohjelman-tietokantaversio*)
               (loop :for kohde :from (1+ versio)
                     :upto *ohjelman-tietokantaversio*
                     :do (päivitä-tietokanta kohde))
               (eheytys t))
              ((> versio *ohjelman-tietokantaversio*)
               (virhe "ONGELMA! Tietokannan versio on ~A mutta ohjelma ~
                osaa vain version ~A. Päivitä ohjelma!"
                      versio *ohjelman-tietokantaversio*))))

      ;; Tietokanta puuttuu
      (with-transaction
        (viesti "~&Valmistellaan tietokanta (~A).~%~
                Ota tietokantatiedostosta varmuuskopio riittävän usein.~%"
                (pathconv:namestring *tiedosto*))

        (query "PRAGMA auto_vacuum = FULL")

        (query "CREATE TABLE IF NOT EXISTS hallinto ~
                (avain TEXT UNIQUE, arvo INTEGER)")

        (query "INSERT INTO hallinto (avain, arvo) VALUES ('versio', ~A)"
               *ohjelman-tietokantaversio*)

        (query "INSERT INTO hallinto (avain, arvo) ~
                VALUES ('muokkauslaskuri', 0)")

        (query "CREATE TABLE oppilaat ~
                (oid INTEGER PRIMARY KEY, ~
                sukunimi TEXT, etunimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")

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

        (query "CREATE TABLE suoritukset ~
                (sid INTEGER PRIMARY KEY, ~
                rid INTEGER NOT NULL REFERENCES ryhmat(rid) ON DELETE CASCADE, ~
                sija INTEGER, ~
                nimi TEXT DEFAULT '', ~
                lyhenne TEXT DEFAULT '', ~
                painokerroin INTEGER)")

        (query "CREATE TABLE arvosanat ~
                (sid INTEGER NOT NULL ~
                        REFERENCES suoritukset(sid) ON DELETE CASCADE, ~
                oid INTEGER NOT NULL ~
                        REFERENCES oppilaat(oid) ON DELETE CASCADE, ~
                arvosana TEXT, ~
                lisatiedot TEXT, ~
                PRIMARY KEY (sid, oid))")

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


(defun connect ()
  (unless (typep *tietokanta* 'sqlite:sqlite-handle)
    (alusta-tiedostopolku)
    (setf *tietokanta* (sqlite:connect (pathconv:namestring *tiedosto*)))
    (alusta-tietokanta)
    *tietokanta*))


(defun disconnect ()
  (when (typep *tietokanta* 'sqlite:sqlite-handle)
    (prog1 (sqlite:disconnect *tietokanta*)
      (setf *tietokanta* nil))))


(defmacro tietokanta-käytössä (&body body)
  `(let ((*tietokanta* nil))
     (unwind-protect (progn (connect) ,@body)
       (disconnect))))
