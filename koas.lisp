;;;; Koas - tietokantaohjelma kouluarvosanoille


;;; Copyright (C) 2013 Teemu Likonen <tlikonen@iki.fi>
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


(defpackage #:koas
  (:use #:cl)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:main))

(in-package #:koas)


(defvar *tiedosto* nil)
(defvar *readline* nil)
(defvar *tietokanta* nil)
(defvar *muokattavat* nil)
(defvar *vuorovaikutteinen* t)
(defvar *tulostusmuoto* nil)
(defvar *suppea* nil)
(defvar *poistoraja* 10)
(defvar *muokkaukset-kunnes-eheytys* 5000)
(defparameter *tietokannan-versio* 3)


(defun alusta-tiedostopolku ()
  (unless *tiedosto*
    (setf *tiedosto*
          (make-pathname
           :directory (append (pathname-directory (user-homedir-pathname))
                              '(".config"))
           :name "koas" :type "db")))
  (ensure-directories-exist *tiedosto*))


(define-condition poistu-ohjelmasta () nil)

(define-condition virhe ()
  ((teksti :reader teksti :initarg :teksti))
  (:report (lambda (tila virta)
             (format virta "~A" (teksti tila)))))

(defun virhe (fmt &rest args)
  (error 'virhe :teksti (apply #'format nil fmt args)))


(defun viesti (fmt &rest args)
  (apply #'format t fmt args))


(defun query (format-string &rest parameters)
  (if (typep *tietokanta* 'sqlite:sqlite-handle)
      (sqlite:execute-to-list *tietokanta*
                              (apply #'format nil format-string parameters))
      (virhe "Ei yhteyttä tietokantaan.")))
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


(defun sql-like-suoja (mj &optional (alku "") (loppu ""))
  (with-output-to-string (ulos)
    (format ulos "'~A" alku)
    (loop :for merkki :across (typecase mj
                                (string mj)
                                (character (string mj))
                                (integer (princ-to-string mj))
                                (t ""))
          :do (princ (cond ((char= merkki #\') "''")
                           ((find merkki "_%\\") (format nil "\\~A" merkki))
                           (t merkki))
                     ulos))
    (format ulos "~A' escape '\\'" loppu)))


(defun mj-lista-listaksi (mj-lista)
  (split-sequence #\space mj-lista :remove-empty-subseqs t))


(defun lista-mj-listaksi (lista)
  (format nil "~{~A~^ ~}" lista))


(defun lue-numero (objekti)
  (cond
    ((numberp objekti) objekti)
    ((plusp (length objekti))
     (let* ((merkki)
            (lisa)
            (alku 0)
            (loppu (length objekti)))

       (let ((eka (elt objekti 0)))
         (cond ((find eka "-–") (setf merkki -1 alku 1))
               ((char= #\+ eka) (setf merkki 1 alku 1))))

       (let ((vika (elt objekti (1- loppu))))
         (cond ((char= #\+ vika) (setf lisa 1/4 loppu (max alku (1- loppu))))
               ((find vika "-–") (setf lisa -1/4 loppu (max alku (1- loppu))))
               ((char= #\½ vika) (setf lisa 1/2 loppu (max alku (1- loppu))))))

       (setf objekti (subseq objekti alku loppu))
       (setf objekti (substitute #\. #\, objekti))
       (if (string= objekti "") (setf objekti "0"))

       (cond
         ((and lisa (not merkki) (every #'digit-char-p objekti))
          (+ (parse-integer objekti) lisa))
         ((and (not lisa)
               (every (lambda (c)
                        (or (digit-char-p c) (char= #\. c)))
                      objekti)
               (some #'digit-char-p objekti)
               (<= 0 (count #\. objekti) 1))
          (* (or merkki 1) (decimals:parse-decimal-number objekti))))))))


(defun poista-tyhjät-ryhmät (&optional rid-lista)
  (unless rid-lista
    (setf rid-lista (mapcar #'first (query "SELECT rid FROM ryhmat"))))
  (loop :for rid :in rid-lista
        :if (and (not (query "SELECT rid FROM oppilaat_ryhmat WHERE rid=~A"
                             rid))
                 (not (query "SELECT rid FROM suoritukset WHERE rid=~A" rid)))
        :do (query "DELETE FROM ryhmat WHERE rid=~A" rid)
        :and :collect rid))


(defun aseta-muokkauslaskuri (arvo)
  (query "UPDATE hallinto SET arvo=~A WHERE avain='muokkauslaskuri'"
         (sql-mj arvo))
  arvo)


(defun hae-muokkauslaskuri ()
  (lue-numero (caar (query "SELECT arvo FROM hallinto ~
                                WHERE avain='muokkauslaskuri'"))))


(defun lisää-muokkauslaskuriin (muokkaukset)
  (aseta-muokkauslaskuri (+ (or (hae-muokkauslaskuri) 0) muokkaukset)))


(defun eheytys (&optional nyt)
  (let ((laskuri (or (hae-muokkauslaskuri) 0)))
    (if (or nyt (>= laskuri *muokkaukset-kunnes-eheytys*))
        (ignore-errors
          (with-transaction
            (poista-tyhjät-ryhmät)
            (query "DELETE FROM arvosanat ~
                        WHERE (arvosana='' OR arvosana IS NULL) ~
                        AND (lisatiedot='' OR lisatiedot IS NULL)"))
          (query "VACUUM")
          (aseta-muokkauslaskuri 0)
          t)
        nil)))


(defun päivitä-versiosta-1-versioon-2 ()
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
    (query "INSERT INTO hallinto (avain, arvo) VALUES ('versio', '2')")))


(defun päivitä-versiosta-2-versioon-3 ()
  (with-transaction
    (unless (hae-muokkauslaskuri)
      (query "INSERT INTO hallinto (avain, arvo) ~
                VALUES ('muokkauslaskuri', '0')"))

    (query "CREATE TABLE oppilaat_v3 ~
                (oid INTEGER PRIMARY KEY, ~
                sukunimi TEXT, etunimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")
    (query "CREATE TABLE ryhmat_v3 ~
                (rid INTEGER PRIMARY KEY, nimi TEXT,
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
                (nimi, lisatiedot) VALUES (~A,'')"
                     (sql-mj ryhmä)))

    (loop :for (oid sukunimi etunimi ryhmä-mj lisätiedot)
          :in (query "SELECT * FROM oppilaat")
          :for ryhmät := (or (mj-lista-listaksi ryhmä-mj) (list "ryhmätön"))
          :do
          (query "INSERT INTO oppilaat_v3 ~
                (oid, sukunimi, etunimi, lisatiedot)
                VALUES (~A,~A,~A,~A)"
                 oid (sql-mj sukunimi) (sql-mj etunimi) (sql-mj lisätiedot))
          (loop :for ryhmä :in ryhmät
                :do
                (let ((rid (caar (query "SELECT rid FROM ryhmat_v3 ~
                        WHERE nimi=~A" (sql-mj ryhmä)))))
                  (unless rid
                    (query "INSERT INTO ryhmat_v3 ~
                        (nimi, lisatiedot) VALUES (~A,'')"
                           (sql-mj ryhmä))
                    (setf rid (query-last-insert-rowid)))
                  (query "INSERT INTO oppilaat_ryhmat ~
                        (oid, rid) VALUES (~A,~A)" oid rid))))

    (loop :for (ryhmä suoritukset-mj) :in (query "SELECT * FROM ryhmat")
          :for suoritukset := (mapcar #'lue-numero
                                      (mj-lista-listaksi suoritukset-mj))
          :do
          (loop :for sid :in suoritukset
                :for sija :upfrom 1
                :for rid := (caar (query "SELECT rid FROM ryhmat_v3 ~
                                WHERE nimi=~A" (sql-mj ryhmä)))
                :for (nimi lyhenne painokerroin)
                := (first (query "SELECT nimi,lyhenne,painokerroin ~
                                FROM suoritukset WHERE sid=~A" sid))
                :do
                (query "INSERT INTO suoritukset_v3 ~
                        (sid, rid, sija, nimi, lyhenne, painokerroin) ~
                        VALUES (~A,~A,~A,~A,~A,~A)"
                       sid rid sija (sql-mj nimi) (sql-mj lyhenne)
                       (or painokerroin "NULL"))))

    (query "DROP TABLE oppilaat")
    (query "DROP TABLE ryhmat")
    (query "DROP TABLE suoritukset")

    (query "CREATE TABLE oppilaat ~
                (oid INTEGER PRIMARY KEY, ~
                sukunimi TEXT, etunimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")
    (query "CREATE TABLE ryhmat ~
                (rid INTEGER PRIMARY KEY, nimi TEXT,
                lisatiedot TEXT DEFAULT '')")
    (query "CREATE TABLE suoritukset ~
                (sid INTEGER PRIMARY KEY, ~
                rid INTEGER, ~
                sija INTEGER, ~
                nimi TEXT DEFAULT '', ~
                lyhenne TEXT DEFAULT '', ~
                painokerroin INTEGER)")

    (loop :for (oid sukunimi etunimi lisätiedot)
          :in (query "SELECT * FROM oppilaat_v3")
          :do (query "INSERT INTO oppilaat ~
                (oid,sukunimi,etunimi,lisatiedot) ~
                VALUES (~A,~A,~A,~A)"
                     oid (sql-mj sukunimi) (sql-mj etunimi)
                     (sql-mj lisätiedot)))

    (loop :for (rid nimi lisätiedot) :in (query "SELECT * FROM ryhmat_v3")
          :do (query "INSERT INTO ryhmat ~
                (rid, nimi, lisatiedot) ~
                VALUES (~A,~A,~A)" rid (sql-mj nimi) (sql-mj lisätiedot)))

    (loop :for (sid rid sija nimi lyhenne painokerroin)
          :in (query "SELECT * FROM suoritukset_v3")
          :do (query "INSERT INTO suoritukset ~
                (sid, rid, sija, nimi, lyhenne, painokerroin) ~
                VALUES (~A,~A,~A,~A,~A,~A)"
                     sid rid sija (sql-mj nimi)
                     (sql-mj lyhenne) (or painokerroin "NULL")))

    (query "DROP TABLE oppilaat_v3")
    (query "DROP TABLE ryhmat_v3")
    (query "DROP TABLE suoritukset_v3")

    (query "UPDATE hallinto SET arvo='3' WHERE avain='versio'")))


(defun tietokannan-versio ()
  (let ((kysely (caar (query "SELECT arvo FROM hallinto ~
                                WHERE avain='versio'"))))
    (if kysely (lue-numero kysely) 1)))


(defun alusta-tietokanta ()
  (let ((kaikki (mapcar #'first (query "SELECT name FROM sqlite_master ~
                                        WHERE type='table'"))))
    (flet ((löytyy (asia)
             (member asia kaikki :test #'string-equal)))

      (if (löytyy "hallinto")
          (let ((versio (tietokannan-versio)))
            (cond ((= versio 1)
                   (viesti "Päivitetään tietokanta: v1 > v3.~%")
                   (päivitä-versiosta-1-versioon-2)
                   (päivitä-versiosta-2-versioon-3)
                   (eheytys t))
                  ((= versio 2)
                   (viesti "Päivitetään tietokanta: v2 > v3.~%")
                   (päivitä-versiosta-2-versioon-3)
                   (eheytys t))
                  ((> versio *tietokannan-versio*)
                   (viesti "VAROITUS! Tietokannan versio on ~A mutta ohjelma ~
                osaa vain version ~A.~%Päivitä ohjelma!~%"
                           versio *tietokannan-versio*)
                   (error 'poistu-ohjelmasta))))

          ;; Tietokanta puuttuu
          (with-transaction
            (viesti "~&Valmistellaan tietokanta (~A).~%~
                Ota tietokantatiedostosta varmuuskopio riittävän usein.~%"
                    (sb-ext:native-pathname *tiedosto*))

            (query "CREATE TABLE hallinto (avain TEXT UNIQUE, arvo TEXT)")
            (query "INSERT INTO hallinto (avain, arvo) VALUES ('versio',~A)"
                   (sql-mj *tietokannan-versio*))
            (query "INSERT INTO hallinto (avain, arvo) ~
                VALUES ('muokkauslaskuri', '0')")

            (query "CREATE TABLE oppilaat ~
                (oid INTEGER PRIMARY KEY, ~
                sukunimi TEXT, etunimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")
            (query "CREATE TABLE ryhmat ~
                (rid INTEGER PRIMARY KEY, nimi TEXT, ~
                lisatiedot TEXT DEFAULT '')")
            (query "CREATE TABLE oppilaat_ryhmat ~
                (oid INTEGER, rid INTEGER)")
            (query "CREATE TABLE suoritukset ~
                (sid INTEGER PRIMARY KEY, ~
                rid INTEGER, ~
                sija INTEGER, ~
                nimi TEXT DEFAULT '', ~
                lyhenne TEXT DEFAULT '', ~
                painokerroin INTEGER)")
            (query "CREATE TABLE arvosanat ~
                (sid INTEGER, oid INTEGER, arvosana TEXT, lisatiedot TEXT)")))

      (query "PRAGMA case_sensitive_like=0"))))


(defun connect ()
  (unless (typep *tietokanta* 'sqlite:sqlite-handle)
    (alusta-tiedostopolku)
    (setf *tietokanta* (sqlite:connect *tiedosto*))
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


(defun arvottu-järjestys (lista)
  (flet ((poista-osa (n sequence)
           (delete-if (constantly t) sequence :start n :count 1)))
    (loop :with rs := (make-random-state t)
          :with lista := (copy-seq lista)
          :for i :from (length lista) :downto 1
          :for satunnainen := (random i rs)
          :collect (prog1 (elt lista satunnainen)
                     (setf lista (poista-osa satunnainen lista)))
          :into uusi-lista
          :finally (return (nconc lista uusi-lista)))))


(defun lue-rivi (kehote &optional muistiin)
  (if *readline*
      (script:readline kehote muistiin)
      (progn
        (format *query-io* "~A" kehote)
        (force-output *query-io*)
        (read-line *query-io*))))


(defun normalisoi-mj (mj)
  (format nil "~{~A~^ ~}" (split-sequence #\space mj :remove-empty-subseqs t)))


(defun normalisoi-ryhmät (asia)
  (assert (or (stringp asia) (listp asia)))
  (when (stringp asia)
    (setf asia (mj-lista-listaksi asia)))
  (setf asia (mapcar #'normalisoi-mj asia))
  (setf asia (remove-duplicates asia :test #'equalp))
  (sort asia #'string-lessp))


(defun normalisoi-painokerroin (asia)
  (cond
    ((integerp asia) asia)
    ((stringp asia)
     (let ((luku (lue-numero asia)))
       (if (integerp luku) luku)))))


(defun otsikko (mj)
  (setf mj (or mj ""))
  (case *tulostusmuoto*
    (:wilma (format nil "**~A**" (if (equal "" mj) "" mj)))
    (:org (if (equal "" mj) "" (format nil "*~A*" mj)))
    (t mj)))


(defun muoto (&rest tyypit)
  (member *tulostusmuoto* tyypit :test #'eql))


(defun olion-mj-pituus (olio)
  (length (prin1-to-string olio)))


(defun numeroi (taulu)
  (let ((suurin-leveys (olion-mj-pituus (length taulu))))
    (loop :for i :upfrom 1
          :for rivi :in taulu
          :collect (cons (format nil "~V@A" suurin-leveys i) rivi))))


(defun tulosta-taulu (taulu &key (virta *standard-output*))
  (when taulu
    (flet ((viivap (ob)
             (find ob '(:viiva :viiva-alku :viiva-loppu :viiva-otsikko))))
      (let* ((sarakkeiden-lkm (reduce #'max taulu :key (lambda (osa)
                                                         (if (viivap osa)
                                                             0
                                                             (length osa)))))
             (leveimmät-sarakkeet (make-list sarakkeiden-lkm
                                             :initial-element 0)))

        (setf taulu
              (loop :for rivi :in taulu
                    :collect
                    (if (viivap rivi)
                        rivi
                        (let ((lisättävät (- sarakkeiden-lkm (length rivi)))
                              (rivi-mj
                               (mapcar (lambda (osa)
                                         (cond ((null osa) "")
                                               ((stringp osa) osa)
                                               (t (princ-to-string osa))))
                                       rivi)))

                          (when (plusp lisättävät)
                            (setf rivi-mj
                                  (nconc rivi-mj
                                         (make-list lisättävät
                                                    :initial-element ""))))
                          (setf leveimmät-sarakkeet
                                (mapcar #'max (mapcar #'length rivi-mj)
                                        leveimmät-sarakkeet))

                          rivi-mj))))

        (format virta "~&")
        (loop :for rivi :in taulu
              :for uusi := (if (viivap rivi)
                               (make-list sarakkeiden-lkm
                                          :initial-element :viiva)
                               rivi)

              :if (and (muoto :latex)
                       (find rivi '(:viiva-otsikko :viiva)))
              :do (format virta "~%")

              :else :if (or (muoto nil :org)
                            (and (muoto :wilma)
                                 (not (member rivi '(:viiva-alku :viiva-otsikko
                                                     :viiva-loppu))))
                            (and (muoto :latex)
                                 (not (viivap rivi))))
              :do
              (if (muoto :latex)
                  (format virta "\\rivi")
                  (format virta "~:[|~;+~]" (and (viivap rivi) (muoto nil))))
              (loop :for (osa . loput) :on uusi
                    :for leveys :in leveimmät-sarakkeet
                    :do
                    (cond
                      ((and (muoto :org nil) (viivap osa))
                       (format virta "--~V,,,'-<~>~:[|~;+~]" leveys
                               (or loput (and (not loput) (muoto nil)))))
                      ((and (muoto :wilma) (viivap osa))
                       (format virta " ~V<~> |" leveys))
                      ((and (muoto :latex) (not (viivap osa)))
                       (format virta "{~A}"
                               (with-output-to-string (s)
                                 (loop :for m :across (string-trim " " osa)
                                       :if (find m "%&{}")
                                       :do (princ #\\ s)
                                       :do (princ m s)))))
                      (t (format virta " ~VA |" leveys osa))))
              (format virta "~%"))))))


(defclass oppilas ()
  ((oid :accessor oid :initarg :oid)
   (sukunimi :accessor sukunimi :initarg :sukunimi)
   (etunimi :accessor etunimi :initarg :etunimi)
   (ryhmälista :accessor ryhmälista :initarg :ryhmälista)
   (lisätiedot :accessor lisätiedot :initarg :lisätiedot)))

(defclass oppilaat ()
  ((oppilaslista :accessor oppilaslista :initarg :oppilaslista)))

(defclass suoritus ()
  ((ryhmä :reader ryhmä :initarg :ryhmä)
   (rid :accessor rid :initarg :rid)
   (sid :accessor sid :initarg :sid)
   (nimi :accessor nimi :initarg :nimi)
   (lyhenne :accessor lyhenne :initarg :lyhenne)
   (painokerroin :accessor painokerroin :initarg :painokerroin :initform nil)))

(defclass suoritukset ()
  ((ryhmä :reader ryhmä :initarg :ryhmä)
   (suorituslista :reader suorituslista :initarg :suorituslista)))

(defclass ryhmä ()
  ((rid :accessor rid :initarg :rid)
   (nimi :accessor nimi :initarg :nimi)
   (lisätiedot :accessor lisätiedot :initarg :lisätiedot :initform nil)))

(defclass ryhmät ()
  ((ryhmälista :reader ryhmälista :initarg :ryhmälista)))

(defclass arvosana ()
  ((oid :reader oid :initarg :oid)
   (sukunimi :accessor sukunimi :initarg :sukunimi)
   (etunimi :accessor etunimi :initarg :etunimi)
   (sid :reader sid :initarg :sid)
   (nimi :accessor nimi :initarg :nimi); suorituksen nimi
   (lyhenne :accessor lyhenne :initarg :lyhenne)
   (painokerroin :accessor painokerroin :initarg :painokerroin :initform nil)
   (arvosana :accessor arvosana :initarg :arvosana :initform "")
   (lisätiedot :accessor lisätiedot :initarg :lisätiedot :initform nil)))

(defclass arvosanat-suorituksesta ()
  ((nimi :accessor nimi :initarg :nimi)
   (ryhmä :reader ryhmä :initarg :ryhmä)
   (arvosanalista :reader arvosanalista :initarg :arvosanalista)))

(defclass arvosanat-suorituksista ()
  ((lista :reader lista :initarg :lista)))

(defclass arvosanat-oppilaalta ()
  ((oid :reader oid :initarg :oid)
   (sukunimi :accessor sukunimi :initarg :sukunimi)
   (etunimi :accessor etunimi :initarg :etunimi)
   (lisätiedot :reader lisätiedot :initarg :lisätiedot)
   (rid :accessor rid :initarg :rid)
   (ryhmä :reader ryhmä :initarg :ryhmä)
   (arvosanalista :reader arvosanalista :initarg :arvosanalista)))

(defclass arvosanat-oppilailta ()
  ((lista :reader lista :initarg :lista)))

(defclass arvosanat-koonti ()
  ((ryhmä :reader ryhmä :initarg :ryhmä)
   (oppilaslista :accessor oppilaslista :initarg :oppilaslista)
   (suorituslista :reader suorituslista :initarg :suorituslista)
   (taulukko :reader taulukko :initarg :taulukko)))

(defclass tilasto-jakauma ()
  ((hajautustaulu :reader hajautustaulu :initarg :hajautustaulu)))

(defclass tilasto-koonti ()
  ((oppilaita :reader oppilaita :initarg :oppilaita)
   (ryhmiä :reader ryhmiä :initarg :ryhmiä)
   (suorituksia :reader suorituksia :initarg :suorituksia)
   (arvosanoja :reader arvosanoja :initarg :arvosanoja)))


(defun muuta-arvosanaksi (luku)
  (let ((neliporras (* 1/4 (decimals:round-half-away-from-zero
                            (abs luku) 1/4)))
        (merkki (if (minusp luku) "-" "")))
    (multiple-value-bind (koko murto)
        (truncate neliporras)
      (let ((lisä (cond ((= murto 1/4) "+")
                        ((= murto 1/2) "½")
                        ((= murto 3/4) (incf koko) "-")
                        (t ""))))
        (format nil "~A~A~A" merkki koko lisä)))))


(defun tulosta-luku (luku &optional desimaalit)
  (if desimaalit
      (decimals:format-decimal-number luku
                                      :round-magnitude (- (abs desimaalit))
                                      :decimal-separator #\,
                                      :show-trailing-zeros t)
      (muuta-arvosanaksi luku)))


(defun keskiarvo (luvut &optional painotukset desimaalit)
  (unless painotukset
    (setf painotukset (make-list (length luvut) :initial-element 1)))
  (if (/= (length luvut) (length painotukset))
      "?"
      (let ((luvut (mapcar #'lue-numero luvut))
            (painotukset (mapcar #'lue-numero painotukset))
            (summa 0)
            (painotussumma 0))
        (loop :while (and luvut painotukset) :do
              (when (and (numberp (first painotukset))
                         (numberp (first luvut)))
                (setf summa (+ summa (* (first  painotukset) (first luvut)))
                      painotussumma (+ painotussumma (first painotukset))))
              (setf luvut (rest luvut)
                    painotukset (rest painotukset)))
        (if (= painotussumma 0)
            ""
            (tulosta-luku (/ summa painotussumma) desimaalit)))))


(defun hae-oppilaat (sukunimi &optional (etunimi "") (ryhmä "")
                                (lisätiedot ""))
  (let ((kysely ;Ei karsita vielä ryhmän perusteella.
         (query "SELECT o.oid,o.sukunimi,o.etunimi,r.nimi,o.lisatiedot ~
                FROM oppilaat AS o ~
                LEFT JOIN oppilaat_ryhmat AS j ON j.oid=o.oid AND j.rid=r.rid ~
                LEFT JOIN ryhmat AS r ON r.rid=j.rid ~
                WHERE o.sukunimi LIKE ~A ~
                AND o.etunimi LIKE ~A ~
                AND o.lisatiedot LIKE ~A ~
                ORDER BY o.sukunimi,o.etunimi,o.oid,r.nimi DESC"
                (sql-like-suoja sukunimi "%" "%")
                (sql-like-suoja etunimi "%" "%")
                (sql-like-suoja lisätiedot "%" "%"))))

    (loop :with oppilaat := nil
          :with ryhmät := nil
          :for (rivi . loput) :on kysely
          :for (oid sukunimi etunimi r-nimi lisätiedot) := rivi
          :for seuraava-oid := (caar loput)
          :do

          (push (or r-nimi "") ryhmät)
          (unless (eql oid seuraava-oid)
            (when (some (lambda (r) ;karsinta ryhmän perustella
                          (search ryhmä r :test #'equalp))
                        ryhmät)
              (push (make-instance 'oppilas
                                   :oid oid
                                   :sukunimi sukunimi
                                   :etunimi etunimi
                                   :ryhmälista
                                   (delete-if #'zerop ryhmät :key #'length)
                                   :lisätiedot lisätiedot)
                    oppilaat))
            (setf ryhmät nil))

          :finally
          (return (when oppilaat
                    (make-instance 'oppilaat :oppilaslista
                                   (nreverse oppilaat)))))))


(defun hae-suoritukset (ryhmä)
  (let ((suoritukset
         (query "SELECT s.sid,r.rid,s.nimi,s.lyhenne,s.painokerroin ~
                FROM suoritukset AS s ~
                JOIN ryhmat as r ON r.rid=s.rid ~
                WHERE r.nimi LIKE ~A ~
                ORDER BY s.sija,s.sid"
                (sql-like-suoja ryhmä))))

    (when suoritukset
      (make-instance
       'suoritukset
       :ryhmä ryhmä
       :suorituslista
       (loop :for (sid rid nimi lyhenne painokerroin) :in suoritukset
             :collect (make-instance 'suoritus
                                     :ryhmä ryhmä
                                     :rid rid
                                     :sid sid
                                     :nimi nimi
                                     :lyhenne lyhenne
                                     :painokerroin painokerroin))))))


(defun hae-ryhmät (&optional (ryhmä "") (lisätiedot ""))
  (let ((ryhmät (query "SELECT rid,nimi,lisatiedot FROM ryhmat ~
                WHERE nimi LIKE ~A AND lisatiedot LIKE ~A ~
                ORDER BY nimi,rid"
                       (sql-like-suoja ryhmä "%" "%")
                       (sql-like-suoja lisätiedot "%" "%"))))
    (when ryhmät
      (make-instance
       'ryhmät
       :ryhmälista (loop :for (rid nimi lisätiedot) :in ryhmät
                         :collect
                         (make-instance 'ryhmä
                                        :rid rid
                                        :nimi nimi
                                        :lisätiedot lisätiedot))))))


(defun hae-arvosanat-suorituksista (ryhmä &optional (nimi "") (lyhenne ""))
  (let ((kysely
         (query "SELECT r.nimi,~
                s.sid,s.nimi,s.lyhenne,s.painokerroin,~
                o.oid,o.sukunimi,o.etunimi,a.arvosana,a.lisatiedot ~
                FROM suoritukset AS s ~
                LEFT JOIN ryhmat AS r ON s.rid=r.rid ~
                LEFT JOIN oppilaat_ryhmat AS j ON s.rid=j.rid ~
                LEFT JOIN oppilaat AS o ON o.oid=j.oid ~
                LEFT JOIN arvosanat AS a ON a.oid=o.oid AND a.sid=s.sid ~
                WHERE r.nimi LIKE ~A ~
                AND s.nimi LIKE ~A ~
                AND s.lyhenne LIKE ~A ~
                ORDER BY r.nimi,r.rid,s.sija,s.sid,o.sukunimi,o.etunimi,o.oid"
                (sql-like-suoja ryhmä)
                (sql-like-suoja nimi "%" "%")
                (sql-like-suoja lyhenne "%" "%"))))

    (loop :with suoritukset := nil
          :with arvosanat := nil
          :for (rivi . loput) :on kysely
          :for (r-nimi sid s-nimi lyhenne painokerroin
                       oid sukunimi etunimi arvosana a-lisätiedot) := rivi
          :for seuraava-sid := (nth 1 (first loput))
          :do

          (push (make-instance 'arvosana
                               :oid oid
                               :sukunimi sukunimi
                               :etunimi etunimi
                               :sid sid
                               :nimi s-nimi
                               :lyhenne lyhenne
                               :painokerroin painokerroin
                               :arvosana arvosana
                               :lisätiedot a-lisätiedot)
                arvosanat)

          (unless (eql sid seuraava-sid)
            (push (make-instance 'arvosanat-suorituksesta
                                 :nimi s-nimi
                                 :ryhmä r-nimi
                                 :arvosanalista (nreverse arvosanat))
                  suoritukset)
            (setf arvosanat nil))

          :finally
          (return (when suoritukset
                    (make-instance 'arvosanat-suorituksista
                                   :lista (nreverse suoritukset)))))))


(defun hae-arvosanat-oppilailta (sukunimi &optional (etunimi "") (ryhmä "")
                                            (lisätiedot ""))
  (let ((kysely
         (query "SELECT o.oid,o.sukunimi,o.etunimi,o.lisatiedot,~
                r.rid,r.nimi,~
                s.sid,s.nimi,s.lyhenne,s.painokerroin,~
                a.arvosana,a.lisatiedot ~
                FROM oppilaat_ryhmat AS j ~
                JOIN oppilaat AS o ON o.oid=j.oid ~
                JOIN ryhmat AS r ON r.rid=j.rid ~
                LEFT JOIN suoritukset AS s ON r.rid=s.rid ~
                LEFT JOIN arvosanat AS a ON o.oid=a.oid AND s.sid=a.sid ~
                WHERE o.sukunimi LIKE ~A ~
                AND o.etunimi LIKE ~A ~
                AND r.nimi LIKE ~A ~
                AND o.lisatiedot LIKE ~A ~
                ORDER BY o.sukunimi,o.etunimi,o.oid,r.nimi,r.rid,s.sija,s.sid"
                (sql-like-suoja sukunimi "%" "%")
                (sql-like-suoja etunimi "%" "%")
                (sql-like-suoja ryhmä "%" "%")
                (sql-like-suoja lisätiedot "%" "%"))))

    (loop :with oppilas-ryhmät := nil
          :with arvosanat := nil
          :for (rivi . loput) :on kysely
          :for (oid sukunimi etunimi o-lisätiedot rid r-nimi
                    sid s-nimi lyhenne painokerroin arvosana a-lisätiedot)
          := rivi
          :for seuraava-oid := (caar loput)
          :for seuraava-rid := (nth 4 (first loput))
          :do

          (push (make-instance 'arvosana
                               :oid oid
                               :sukunimi sukunimi
                               :etunimi etunimi
                               :sid sid
                               :nimi s-nimi
                               :lyhenne lyhenne
                               :painokerroin painokerroin
                               :arvosana arvosana
                               :lisätiedot a-lisätiedot)
                arvosanat)

          (when (or (not (eql oid seuraava-oid))
                    (not (eql rid seuraava-rid)))
            (when (some (lambda (as)
                          (or (arvosana as)
                              (lisätiedot as)))
                        arvosanat)
              (push (make-instance 'arvosanat-oppilaalta
                                   :oid oid
                                   :sukunimi sukunimi
                                   :etunimi etunimi
                                   :lisätiedot o-lisätiedot
                                   :rid rid
                                   :ryhmä r-nimi
                                   :arvosanalista (nreverse arvosanat))
                    oppilas-ryhmät))
            (setf arvosanat nil))

          :finally
          (return (when oppilas-ryhmät
                    (make-instance 'arvosanat-oppilailta
                                   :lista (nreverse oppilas-ryhmät)))))))


(defun hae-arvosanat-koonti (ryhmä)
  (let* ((suorituslista
          (query "SELECT r.nimi,s.sid,s.nimi,s.lyhenne,s.painokerroin ~
                FROM ryhmat AS r ~
                JOIN suoritukset AS s ON r.rid=s.rid ~
                WHERE r.nimi LIKE ~A ~
                ORDER BY r.nimi,r.rid,s.sija,s.sid"
                 (sql-like-suoja ryhmä))))

    (when suorituslista
      (let* ((kysely
              (query "SELECT o.sukunimi,o.etunimi,~
                        ~{~A.arvosana~*~^,~}~:* ~
                        FROM oppilaat_ryhmat AS j ~
                        LEFT JOIN oppilaat AS o ON j.oid=o.oid ~
                        LEFT JOIN ryhmat AS r on j.rid=r.rid ~
                        ~{LEFT JOIN arvosanat AS ~A~:* ~
                        ON o.oid=~A.oid~:* AND ~A.sid=~A ~} ~
                        WHERE r.nimi LIKE ~A ~
                        ORDER BY o.sukunimi,o.etunimi,o.oid"
                     (loop :for suoritus :in suorituslista
                           ;; SQLiten suurin taulukkomäärä joinissa 64.
                           :for i :from 1 :below 64
                           :collect (format nil "a~A" i)
                           :collect (nth 1 suoritus))
                     (sql-like-suoja ryhmä))))

        (when kysely
          (let ((taulukko (make-array (list (length kysely)
                                            (length suorituslista))))
                (oppilaslista nil))

            (loop :for (sukunimi etunimi . arvosanat) :in kysely
                  :for opp :upfrom 0
                  :do
                  (push (format nil "~A, ~A" sukunimi etunimi) oppilaslista)
                  (loop :for arvosana :in arvosanat
                        :for arv :upfrom 0
                        :do (setf (aref taulukko opp arv) arvosana)))

            (make-instance 'arvosanat-koonti
                           :ryhmä (caar suorituslista)
                           :oppilaslista (nreverse oppilaslista)
                           :suorituslista suorituslista
                           :taulukko taulukko)))))))


(defun tilasto-jakauma-1 (hajautustaulu &optional (sukunimi "") (etunimi "")
                                          (ryhmä "") (lisätiedot "")
                                          (suoritus "") (lyhenne "")
                                          painokerroin)
  (let ((kysely
         (mapcar #'first (query "SELECT a.arvosana ~
                FROM oppilaat_ryhmat AS j ~
                JOIN oppilaat AS o ON o.oid=j.oid ~
                JOIN ryhmat AS r ON r.rid=j.rid ~
                JOIN suoritukset AS s ON r.rid=s.rid ~
                JOIN arvosanat AS a ON a.oid=o.oid AND s.sid=a.sid ~
                WHERE o.sukunimi LIKE ~A ~
                AND o.etunimi LIKE ~A ~
                AND r.nimi LIKE ~A ~
                AND o.lisatiedot LIKE ~A ~
                AND s.nimi LIKE ~A ~
                AND s.lyhenne LIKE ~A ~A"
                                (sql-like-suoja sukunimi "%" "%")
                                (sql-like-suoja etunimi "%" "%")
                                (sql-like-suoja ryhmä "%" "%")
                                (sql-like-suoja lisätiedot "%" "%")
                                (sql-like-suoja suoritus "%" "%")
                                (sql-like-suoja lyhenne "%" "%")
                                (if painokerroin
                                    "AND s.painokerroin>=1"
                                    "")))))
    (loop :for mj :in kysely
          :for as := (lue-numero mj)
          :if (numberp as)
          :do (incf (gethash (tulosta-luku as) hajautustaulu 0)))))


(defun tilasto-jakauma (hakulista &optional painokerroin)
  (loop :with taulu := (make-hash-table :test #'equalp)
        :for (sukunimi etunimi ryhmä lisätiedot suoritus lyhenne)
        :in hakulista
        :do (tilasto-jakauma-1 taulu sukunimi etunimi ryhmä lisätiedot
                               suoritus lyhenne painokerroin)
        :finally
        (when (loop :for n :being :each :hash-value :in taulu
                    :thereis (plusp n))
          (return (make-instance 'tilasto-jakauma :hajautustaulu taulu)))))


(defun tilasto-koonti ()
  (let ((oppilaita (caar (query "SELECT count(*) FROM oppilaat")))
        (ryhmiä (caar (query "SELECT count(*) FROM ryhmat")))
        (suorituksia (caar (query "SELECT count(*) FROM suoritukset")))
        (arvosanoja (caar (query "SELECT count(*) FROM arvosanat"))))
    (make-instance 'tilasto-koonti
                   :oppilaita oppilaita
                   :ryhmiä ryhmiä
                   :suorituksia suorituksia
                   :arvosanoja arvosanoja)))


(defun tulosta-muokattavat (&rest kentät)
  (when *muokattavat*
    (viesti "~&~[~;Tietue: 1~:;Tietueet: 1-~:*~A~]. ~
                        ~:[Kenttä: ~;Kentät: /~]~{~A~^/~}~%"
            (length *muokattavat*)
            (> (length kentät) 1)
            kentät)))


(defgeneric tulosta (object))


(defmethod tulosta ((opp oppilaat))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*))
                        (coerce (oppilaslista opp) 'vector)))
  (let ((taulu (loop :for oppilas :in (oppilaslista opp)
                     :collect (append
                               (list (sukunimi oppilas))
                               (list (etunimi oppilas))
                               (list (lista-mj-listaksi
                                      (ryhmälista oppilas)))
                               (unless *suppea*
                                 (list (lisätiedot oppilas)))))))

    (tulosta-taulu
     (append (list :viiva-alku)
             (list (append (if *muokattavat* (list nil))
                           (list (otsikko "Sukunimi"))
                           (list (otsikko "Etunimi"))
                           (list (otsikko "Ryhmät"))
                           (unless *suppea*
                             (list (otsikko "Lisätiedot")))))
             (list :viiva-otsikko)
             (if *muokattavat* (numeroi taulu) taulu)
             (list :viiva-loppu)))

    (tulosta-muokattavat "sukunimi" "etunimi" "ryhmät" "lisätiedot")))


(defmethod tulosta ((suo suoritukset))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*))
                        (coerce (suorituslista suo) 'vector)))

  (let ((taulu (loop :for suoritus :in (suorituslista suo)
                     :collect (list (nimi suoritus)
                                    (lyhenne suoritus)
                                    (painokerroin suoritus)))))

    (tulosta-taulu
     (append (list :viiva-alku)
             (list (list (otsikko "Ryhmä:") (ryhmä suo)))
             (list :viiva-loppu)))
    (viesti "~%")

    (tulosta-taulu
     (append (list :viiva-alku)
             (list (append (if *muokattavat* (list nil))
                           (list (otsikko "Suoritus"))
                           (list (otsikko "Lyh"))
                           (list (otsikko "K"))))
             (list :viiva-otsikko)
             (if *muokattavat* (numeroi taulu) taulu)
             (list :viiva-loppu)))

    (tulosta-muokattavat "suoritus" "lyhenne" "painokerroin"
                         (format nil "sija(1~[~;~:;-~:*~A~])"
                                 (length *muokattavat*)))

    (unless (muoto nil :latex)
      (viesti "~%")
      (tulosta-taulu (list (list (otsikko "K") "= painokerroin"))))))


(defmethod tulosta ((lista ryhmät))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*))
                        (coerce (ryhmälista lista) 'vector)))



  (let ((taulu (loop :for ryhmä :in (ryhmälista lista)
                     :collect (list (nimi ryhmä)
                                    (lisätiedot ryhmä)))))
    (tulosta-taulu
     (append (list :viiva-alku)
             (list (append (if *muokattavat* (list nil))
                           (list (otsikko "Nimi"))
                           (list (otsikko "Lisätiedot"))))
             (list :viiva-otsikko)
             (if *muokattavat* (numeroi taulu) taulu)
             (list :viiva-loppu))))
  (tulosta-muokattavat "nimi" "lisätiedot"))


(defmethod tulosta ((arv arvosanat-suorituksista))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*)
                                 (= (length (lista arv)) 1))
                        (coerce (arvosanalista (first (lista arv))) 'vector)))

  (loop :for (arv-suo . lisää) :on (lista arv)
        :do
        (let* ((luvut)
               (taulu (loop :for arvosana :in (arvosanalista arv-suo)
                            :for suku := (sukunimi arvosana)
                            :for etu := (etunimi arvosana)
                            :collect (append (list (format nil "~A, ~A"
                                                           suku etu))
                                             (list (arvosana arvosana))
                                             (unless *suppea*
                                               (list (lisätiedot arvosana))))
                            :do (push (arvosana arvosana) luvut))))

          (tulosta-taulu
           (append (list :viiva-alku)
                   (list (list (otsikko "Ryhmä:") (ryhmä arv-suo)))
                   (list (list (otsikko "Suoritus:") (nimi arv-suo)))
                   (list :viiva-loppu)))
          (viesti "~%")

          (tulosta-taulu
           (append (list :viiva-alku)
                   (list (append (if *muokattavat* (list nil))
                                 (list (otsikko "Oppilas"))
                                 (list (otsikko "As"))
                                 (unless *suppea*
                                   (list (otsikko "Lisätiedot")))))
                   (list :viiva-otsikko)
                   (if *muokattavat* (numeroi taulu) taulu)
                   (list :viiva)
                   (list (append (if *muokattavat* (list nil))
                                 (list "Keskiarvo" (keskiarvo luvut))))
                   (list :viiva-loppu)))

          (unless (muoto nil :latex)
            (viesti "~%")
            (tulosta-taulu (list (list (otsikko "As") "= arvosana")))))

        :if lisää :do (viesti "~%~%"))

  (tulosta-muokattavat "arvosana" "lisätiedot"))


(defmethod tulosta ((arv arvosanat-oppilailta))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*)
                                 (= (length (lista arv)) 1))
                        (coerce (arvosanalista (first (lista arv))) 'vector)))

  (loop :for (arv-opp . lisää) :on (lista arv)
        :do
        (let* ((arvot)
               (kertoimet)
               (taulu (loop :for arvosana :in (arvosanalista arv-opp)
                            :collect (append (list (nimi arvosana))
                                             (list (arvosana arvosana))
                                             (list (painokerroin arvosana))
                                             (unless *suppea*
                                               (list (lisätiedot arvosana))))
                            :do
                            (push (arvosana arvosana) arvot)
                            (push (painokerroin arvosana) kertoimet))))

          (tulosta-taulu
           (append (list :viiva-alku)
                   (list (list (otsikko "Oppilas:")
                               (format nil "~A, ~A"
                                       (sukunimi arv-opp)
                                       (etunimi arv-opp))))
                   (list (list (otsikko "Ryhmä:") (ryhmä arv-opp)))
                   (let ((lis (lisätiedot arv-opp)))
                     (if (or (not lis) (equal lis "") *suppea*)
                         nil
                         (list (list (otsikko "Lisätiedot:") lis))))
                   (list :viiva-loppu)))
          (viesti "~%")

          (tulosta-taulu
           (append (list :viiva-alku)
                   (list (append (if *muokattavat* (list nil))
                                 (list (otsikko "Suoritus"))
                                 (list (otsikko "As"))
                                 (list (otsikko "K"))
                                 (unless *suppea*
                                   (list (otsikko "Lisätiedot")))))
                   (list :viiva-otsikko)
                   (if *muokattavat* (numeroi taulu) taulu)
                   (list :viiva)
                   (list (append (if *muokattavat* (list nil))
                                 (list "Keskiarvo"
                                       (keskiarvo arvot kertoimet 2))))
                   (list :viiva-loppu)))

          (unless (muoto nil :latex)
            (viesti "~%")
            (tulosta-taulu
             (list (list (otsikko "As") "= arvosana"
                         (otsikko "K") "= painokerroin")))))

        :if lisää :do (viesti "~%~%"))

  (tulosta-muokattavat "arvosana" "lisätiedot"))


(defmethod tulosta ((koonti arvosanat-koonti))
  (setf *muokattavat* nil)
  (let ((kertoimet)
        (lyhenteet)
        (ka-oppilas (make-array (array-dimension (taulukko koonti) 0)
                                :initial-element nil))
        (ka-suoritus (make-array (array-dimension (taulukko koonti) 1)
                                 :initial-element nil)))

    (loop :for (nil nil nil lyhenne painokerroin) :in (suorituslista koonti)
          :collect lyhenne :into lyh
          :collect painokerroin :into ker
          :finally (setf lyhenteet lyh kertoimet ker))

    (loop :for opp :from 0
          :below (array-dimension (taulukko koonti) 0)
          :do (loop :for suo :from 0
                    :below (array-dimension (taulukko koonti) 1)
                    :collect (aref (taulukko koonti) opp suo) :into luvut
                    :finally (setf (aref ka-oppilas opp)
                                   (keskiarvo luvut kertoimet 2))))

    (loop :for suo :from 0
          :below (array-dimension (taulukko koonti) 1)
          :do (loop :for opp :from 0
                    :below (array-dimension (taulukko koonti) 0)
                    :collect (aref (taulukko koonti) opp suo) :into luvut
                    :finally (setf (aref ka-suoritus suo) (keskiarvo luvut))))

    (tulosta-taulu
     (append (list :viiva-alku)
             (list (append (list (otsikko "Ryhmä:")) (list (ryhmä koonti))))
             (list :viiva-loppu)))
    (viesti "~%")

    (tulosta-taulu
     (append
      (list :viiva-alku)
      (list (append (list (otsikko "Suoritus"))
                    (mapcar #'otsikko lyhenteet)
                    (list (otsikko "ka"))))
      (list (append (list (otsikko "Painokerroin"))
                    (mapcar #'otsikko kertoimet)
                    (list (otsikko ""))))
      (list :viiva-otsikko)
      (loop :for nimi :in (oppilaslista koonti)
            :for oppilas :from 0 :below (array-dimension (taulukko koonti) 0)
            :collect (loop :for suoritus :from 0
                           :below (array-dimension (taulukko koonti) 1)
                           :collect (aref (taulukko koonti) oppilas suoritus)
                           :into rivi
                           :finally (return (append (list nimi)
                                                    rivi
                                                    (list (aref ka-oppilas
                                                                oppilas))))))
      (list :viiva)
      (list (append (list "Keskiarvo")
                    (coerce ka-suoritus 'list)
                    (list (keskiarvo (coerce ka-oppilas 'list)))))
      (list :viiva-loppu)))

    (unless *suppea*
      (viesti "~%")
      (tulosta-taulu
       (append (list :viiva-alku)
               (list (list (otsikko "Lyh") (otsikko "Suoritus")))
               (list :viiva-otsikko)
               (loop :for (nil nil nimi lyhenne nil) :in (suorituslista koonti)
                     :collect (list lyhenne nimi))
               '(("ka" "Keskiarvo"))
               (list :viiva-loppu))))))


(defmethod tulosta ((jakauma tilasto-jakauma))
  (setf *muokattavat* nil)
  (let ((as-pienin)
        (as-suurin)
        (suurin-arvo 1)
        (leveys 40))
    (maphash (lambda (k v)
               (if (not as-pienin)
                   (setf as-pienin (lue-numero k))
                   (setf as-pienin (min as-pienin (lue-numero k))))
               (if (not as-suurin)
                   (setf as-suurin (lue-numero k))
                   (setf as-suurin (max as-suurin (lue-numero k))))
               (setf suurin-arvo (max suurin-arvo v)))
             (hajautustaulu jakauma))
    (loop :with lkm-leveys := (max 3 (olion-mj-pituus suurin-arvo))
          :for i :from (floor as-pienin) :upto (ceiling as-suurin) :by 1/4
          :for as := (tulosta-luku i)
          :for määrä := (gethash as (hajautustaulu jakauma) 0)
          :collect (list as (format nil "~V@A" lkm-leveys määrä)
                         (let* ((suhde (/ määrä suurin-arvo))
                                (pituus (decimals:round-half-away-from-zero
                                         (* suhde leveys))))
                           (make-string pituus :initial-element #\#)))
          :into taulu
          :finally
          (tulosta-taulu
           (append (list :viiva-alku)
                   (list (list (otsikko "As") (otsikko "Lkm") (otsikko "")))
                   (list :viiva-otsikko)
                   taulu
                   (list :viiva-loppu)))

          (unless (muoto nil :latex)
            (viesti "~%")
            (tulosta-taulu
             (list (list (otsikko "As") "= arvosana"
                         (otsikko "Lkm") "= lukumäärä")))))))


(defmethod tulosta ((koonti tilasto-koonti))
  (setf *muokattavat* nil)
  (let ((suurin (reduce #'max (list (oppilaita koonti)
                                    (ryhmiä koonti)
                                    (suorituksia koonti)
                                    (arvosanoja koonti))
                        :key #'olion-mj-pituus)))
    (flet ((rivi (otsikko olio)
             (list (otsikko otsikko) (format nil "~V@A" suurin olio))))
      (tulosta-taulu
       (append (list :viiva-alku)
               (list (rivi "Oppilaita:" (oppilaita koonti)))
               (list (rivi "Ryhmiä:" (ryhmiä koonti)))
               (list (rivi "Suorituksia:" (suorituksia koonti)))
               (list (rivi "Arvosanoja:" (arvosanoja koonti)))
               (list :viiva-loppu))))))


(defmethod tulosta ((object t))
  (setf *muokattavat* nil)
  (format *error-output* "~&Ei löytynyt.~%"))


(defgeneric lisää (asia &key &allow-other-keys))


(defmethod lisää ((oppilas oppilas) &key)
  (query "INSERT INTO oppilaat (sukunimi,etunimi,lisatiedot) ~
        VALUES (~A,~A,~A)"
         (sql-mj (sukunimi oppilas))
         (sql-mj (etunimi oppilas))
         (sql-mj (lisätiedot oppilas)))
  (let ((oid (query-last-insert-rowid)))
    (loop :for ryhmä :in (ryhmälista oppilas)
          :for rid := (caar (query "SELECT rid FROM ryhmat ~
                                        WHERE nimi LIKE ~A"
                                   (sql-like-suoja ryhmä)))

          :unless rid :do
          (query "INSERT INTO ryhmat (nimi,lisatiedot) VALUES (~A,'')"
                 (sql-mj ryhmä))
          (setf rid (query-last-insert-rowid))

          :do
          (query "INSERT INTO oppilaat_ryhmat (oid, rid) VALUES (~A,~A)"
                 oid rid))
    oid))


(defmethod lisää ((suoritus suoritus) &key sija)
  (let ((rid (caar (query "SELECT rid FROM ryhmat WHERE nimi LIKE ~A"
                          (sql-like-suoja (ryhmä suoritus))))))
    (unless rid
      (query "INSERT INTO ryhmat (nimi) VALUES (~A)" (sql-mj (ryhmä suoritus)))
      (setf rid (query-last-insert-rowid)))

    (setf (rid suoritus) rid)

    (query "INSERT INTO suoritukset (rid,nimi,lyhenne,painokerroin) ~
                VALUES (~A,~A,~A,~A)"
           rid (sql-mj (nimi suoritus)) (sql-mj (lyhenne suoritus))
           (or (painokerroin suoritus) "NULL"))

    (let* ((uusi-sid (query-last-insert-rowid))
           (sid-lista
            (mapcar #'first (query "SELECT sid FROM suoritukset ~
                                        WHERE rid=~A ~
                                        AND NOT sid=~A ~
                                        ORDER BY sija,sid"
                                   rid uusi-sid))))
      (setf (sid suoritus) uusi-sid)
      (cond ((not sija) (setf sija (1+ (length sid-lista))))
            ((< sija 1) (setf sija 1)))
      (query "UPDATE suoritukset SET sija=~A WHERE sid=~A" sija uusi-sid)
      (loop :with i := 0
            :for sid :in sid-lista
            :do (incf i)
            :if (= i sija) :do (incf i)
            :do (query "UPDATE suoritukset SET sija=~A WHERE sid=~A" i sid)))))


(defgeneric muokkaa (asia &key &allow-other-keys))


(defmethod muokkaa ((oppilas oppilas) &key)
  (let ((vanha-rid-lista
         (mapcar #'first (query "select rid from oppilaat_ryhmat where oid=~A"
                                (oid oppilas))))
        (uusi-rid-lista nil))

    (query "UPDATE oppilaat SET sukunimi=~A,etunimi=~A,lisatiedot=~A ~
                WHERE oid=~A"
           (sql-mj (sukunimi oppilas))
           (sql-mj (etunimi oppilas))
           (sql-mj (lisätiedot oppilas))
           (oid oppilas))

    (loop :for ryhmä :in (ryhmälista oppilas)
          :for rid := (caar (query "SELECT rid FROM ryhmat ~
                                        WHERE nimi LIKE ~A"
                                   (sql-like-suoja ryhmä)))

          :unless rid :do
          (query "INSERT INTO ryhmat (nimi) VALUES (~A)" (sql-mj ryhmä))
          (setf rid (query-last-insert-rowid))

          :unless (member rid vanha-rid-lista) :do
          (query "INSERT INTO oppilaat_ryhmat (oid, rid) VALUES (~A,~A)"
                 (oid oppilas) rid)

          :collect rid :into rid-lista
          :finally (setf uusi-rid-lista rid-lista))

    (let ((ero (set-difference vanha-rid-lista uusi-rid-lista)))
      (when ero
        (query "DELETE FROM oppilaat_ryhmat ~
                WHERE oid=~A AND (~{rid=~A~^ OR ~})"
               (oid oppilas) ero)
        (poista-tyhjät-ryhmät ero)))

    oppilas))


(defmethod muokkaa ((suoritus suoritus) &key sija)
  (query "UPDATE suoritukset SET nimi=~A,lyhenne=~A,painokerroin=~A ~
                WHERE sid=~A"
         (sql-mj (nimi suoritus)) (sql-mj (lyhenne suoritus))
         (or (painokerroin suoritus) "NULL")
         (sid suoritus))

  (when sija
    (let ((sid-lista
           (mapcar #'first (query "SELECT sid FROM suoritukset ~
                                        WHERE rid=~A ~
                                        AND NOT sid=~A ~
                                        ORDER BY sija,sid"
                                  (rid suoritus) (sid suoritus)))))

      (query "UPDATE suoritukset SET sija=~A WHERE sid=~A" sija (sid suoritus))
      (loop :with i := 0
            :for sid :in sid-lista
            :do (incf i)
            :if (= i sija) :do (incf i)
            :do (query "UPDATE suoritukset SET sija=~A WHERE sid=~A" i sid)))))


(defmethod muokkaa ((ryhmä ryhmä) &key)
  (query "UPDATE ryhmat SET nimi=~A,lisatiedot=~A WHERE rid=~A"
         (sql-mj (nimi ryhmä))
         (sql-mj (lisätiedot ryhmä))
         (rid ryhmä)))


(defmethod muokkaa ((arvosana arvosana) &key)
  (let ((lisätiedot (let ((lisä (lisätiedot arvosana)))
                      (if (or (not lisä) (equal lisä ""))
                          "NULL"
                          (sql-mj lisä)))))
    (if (query "SELECT oid FROM arvosanat WHERE sid=~A AND oid=~A"
               (sid arvosana) (oid arvosana))
        (query "UPDATE arvosanat SET arvosana=~A,lisatiedot=~A ~
                WHERE sid=~A AND oid=~A"
               (sql-mj (arvosana arvosana)) lisätiedot (sid arvosana)
               (oid arvosana))
        (query "INSERT INTO arvosanat (sid,oid,arvosana,lisatiedot) ~
                VALUES (~A,~A,~A,~A)"
               (sid arvosana) (oid arvosana) (sql-mj (arvosana arvosana))
               lisätiedot))))


(defgeneric poista (asia))


(defmethod poista ((oppilas oppilas))
  (let ((rid-lista
         (mapcar #'first (query "SELECT rid FROM oppilaat_ryhmat WHERE oid=~A"
                                (oid oppilas)))))
    (query "DELETE FROM oppilaat_ryhmat WHERE oid=~A" (oid oppilas))
    (query "DELETE FROM arvosanat WHERE oid=~A" (oid oppilas))
    (query "DELETE FROM oppilaat WHERE oid=~A" (oid oppilas))
    (when rid-lista
      (poista-tyhjät-ryhmät rid-lista))))


(defmethod poista ((suoritus suoritus))
  (query "DELETE FROM suoritukset WHERE sid=~A" (sid suoritus))
  (query "DELETE FROM arvosanat WHERE sid=~A" (sid suoritus))
  (poista-tyhjät-ryhmät (list (rid suoritus)))
  (let ((sid-lista
         (mapcar #'first (query "SELECT sid FROM suoritukset ~
                                WHERE rid=~A ORDER BY sija,sid"
                                (rid suoritus)))))
    (loop :for i :upfrom 1
          :for sid :in sid-lista
          :do (query "UPDATE suoritukset SET sija=~A WHERE sid=~A" i sid))))


(defmethod poista ((arvosana arvosana))
  (query "DELETE FROM arvosanat WHERE sid=~A AND oid=~A"
         (sid arvosana) (oid arvosana)))


(defun erota-ensimmäinen-sana (mj)
  (setf mj (string-left-trim " " mj))
  (let* ((väli (or (position #\space mj) (length mj)))
         (ensimmäinen (subseq mj 0 väli))
         (loput (string-left-trim " " (subseq mj väli))))
    (values ensimmäinen loput)))


(defun pilko-erottimella (mj)
  (when (plusp (length mj))
    (split-sequence (elt mj 0) (subseq mj 1)
                    :remove-empty-subseqs nil)))


(defun komento-hae-oppilaat (arg)
  ;; /sukunimi/etunimi/ryhmät/lisätiedot
  (when (zerop (length arg))
    (setf arg "/"))
  (let ((jaettu (pilko-erottimella arg)))
    (tulosta (hae-oppilaat (nth 0 jaettu)
                           (nth 1 jaettu)
                           (nth 2 jaettu)
                           (nth 3 jaettu)))))


(defun komento-hae-oppilaat-arvottu (arg)
  ;; /sukunimi/etunimi/ryhmät/lisätiedot
  (when (zerop (length arg))
    (setf arg "/"))
  (let* ((jaettu (pilko-erottimella arg))
         (oppilaat (hae-oppilaat (nth 0 jaettu)
                                 (nth 1 jaettu)
                                 (nth 2 jaettu)
                                 (nth 3 jaettu))))
    (when oppilaat
      (setf (oppilaslista oppilaat)
            (arvottu-järjestys (oppilaslista oppilaat))))
    (tulosta oppilaat)))


(defun komento-hae-suoritukset (arg)
  ;; ryhmä
  (when (zerop (length arg))
    (virhe "Anna ryhmän tunnus."))
  (tulosta (hae-suoritukset (erota-ensimmäinen-sana arg))))


(defun komento-hae-ryhmät (&optional arg)
  ;; /ryhmä/lisätiedot
  (when (zerop (length arg))
    (setf arg "/"))
  (let ((jaettu (pilko-erottimella arg)))
    (tulosta (hae-ryhmät (nth 0 jaettu)
                         (nth 1 jaettu)))))


(defun komento-hae-arvosanat-suorituksista (arg)
  ;; ryhmä /suoritus/lyhenne
  (multiple-value-bind (ryhmä loput)
      (erota-ensimmäinen-sana arg)
    (when (zerop (length ryhmä))
      (virhe "Anna ryhmän tunnus."))
    (when (zerop (length loput))
      (setf loput "/"))
    (let ((jaettu (pilko-erottimella loput)))
      (tulosta (hae-arvosanat-suorituksista ryhmä (nth 0 jaettu)
                                            (nth 1 jaettu))))))


(defun komento-hae-arvosanat-oppilailta (arg)
  ;; /sukunimi/etunimi/ryhmät/lisätiedot
  (when (zerop (length arg))
    (setf arg "/"))
  (let ((jaettu (pilko-erottimella arg)))
    (tulosta (hae-arvosanat-oppilailta (nth 0 jaettu)
                                       (nth 1 jaettu)
                                       (nth 2 jaettu)
                                       (nth 3 jaettu)))))


(defun komento-hae-arvosanat-koonti (arg)
  ;; ryhmä
  (when(zerop (length arg))
    (virhe "Anna ryhmän tunnus."))
  (tulosta (hae-arvosanat-koonti (erota-ensimmäinen-sana arg))))


(defun komento-tilasto-jakauma (arg &optional painokerroin)
  ;; |/sukunimi/etunimi/ryhmä/lisätiedot/suoritus/lyhenne|/...
  (when (zerop (length arg))
    (setf arg "|"))
  (loop :for haku-mj :in (pilko-erottimella arg)
        :for haku := (pilko-erottimella haku-mj)
        :collect (list (nth 0 haku)
                       (nth 1 haku)
                       (nth 2 haku)
                       (nth 3 haku)
                       (nth 4 haku)
                       (nth 5 haku))
        :into haut
        :finally
        (tulosta (tilasto-jakauma haut painokerroin))))


(defun komento-tilasto-koonti ()
  (tulosta (tilasto-koonti)))


(defun komento-lisää-oppilas (arg)
  ;; /sukunimi/etunimi/ryhmät/lisätiedot
  (when (zerop (length arg))
    (virhe "Anna lisättävän oppilaan tiedot."))
  (let ((jaettu (pilko-erottimella arg)))
    (when (or (zerop (length (normalisoi-mj (nth 0 jaettu))))
              (zerop (length (normalisoi-mj (nth 1 jaettu))))
              (zerop (length (normalisoi-mj (nth 2 jaettu)))))
      (virhe "Pitää antaa vähintään sukunimi, etunimi ja ryhmä."))
    (with-transaction
      (lisää (make-instance 'oppilas
                            :sukunimi (normalisoi-mj (nth 0 jaettu))
                            :etunimi (normalisoi-mj (nth 1 jaettu))
                            :ryhmälista (normalisoi-ryhmät (nth 2 jaettu))
                            :lisätiedot (normalisoi-mj (nth 3 jaettu))))
      (lisää-muokkauslaskuriin 1))
    (eheytys)))


(defun on-sisältöä-p (mj)
  (notevery (lambda (el)
              (and (characterp el)
                   (or (not (graphic-char-p el))
                       (char= #\Space el))))
            mj))


(defun komento-lisää-suoritus (arg)
  ;; ryhmä /suoritus/lyhenne/painokerroin/sija
  (multiple-value-bind (ryhmä tiedot)
      (erota-ensimmäinen-sana arg)
    (setf tiedot (loop :for i :in (pilko-erottimella tiedot)
                       :collect (normalisoi-mj i)))
    (unless tiedot
      (virhe "Anna lisättävän suorituksen tiedot."))

    (let ((nimi (nth 0 tiedot))
          (lyh (nth 1 tiedot))
          (paino (nth 2 tiedot))
          (sija (nth 3 tiedot)))
      (when (or (not (and nimi (on-sisältöä-p nimi)))
                (not (and lyh (on-sisältöä-p lyh))))
        (virhe "Pitää antaa vähintään suorituksen nimi ja lyhenne."))
      (if (and paino (on-sisältöä-p paino))
          (let ((num (lue-numero paino)))
            (if (and (integerp num) (plusp num))
                (setf paino num)
                (virhe "Painokertoimen täytyy olla positiivinen kokonaisluku ~
                         (tai jättää pois).")))
          (setf paino nil))
      (if (and sija (on-sisältöä-p sija))
          (let ((num (lue-numero sija)))
            (if (and (integerp num) (plusp num))
                (setf sija num)
                (virhe "Sijainnin täytyy olla positiivinen kokonaisluku ~
                        (tai jättää pois).")))
          (setf sija nil))

      (with-transaction
        (lisää (make-instance 'suoritus
                              :ryhmä ryhmä
                              :nimi nimi
                              :lyhenne lyh
                              :painokerroin paino)
               :sija sija)
        (lisää-muokkauslaskuriin 1))
      (eheytys))))


(defun jäsennä-numeroluettelo (mj)
  ;; Luettelo on muotoa 1,2,3,5-7,9-12
  (loop :with valmis := nil
        :for i :in (split-sequence #\, mj :remove-empty-subseqs t)
        :do
        (if (every #'digit-char-p i)
            (push (lue-numero i) valmis)
            (let ((lista (split-sequence #\- i :remove-empty-subseqs nil)))
              (if (or (/= 2 (length lista))
                      (zerop (length (nth 0 lista)))
                      (zerop (length (nth 1 lista)))
                      (notevery #'digit-char-p (nth 0 lista))
                      (notevery #'digit-char-p (nth 1 lista)))
                  (return nil)
                  (destructuring-bind (eka toka)
                      (mapcar #'lue-numero lista)
                    (loop :for i :from (min eka toka)
                          :upto (max eka toka)
                          :collect i :into luvut
                          :finally
                          (if (>= eka toka)
                              (setf valmis (nconc luvut valmis))
                              (setf valmis (nconc (nreverse luvut)
                                                  valmis))))))))
        :finally
        (return (nreverse (delete-duplicates valmis :from-end nil)))))


(defun komento-poista (arg)
  ;; numeroluettelo
  (unless *muokattavat*
    (virhe "Edellinen komento ei sisällä poistettavia."))
  (let ((numeroluettelo (erota-ensimmäinen-sana arg)))
    (when (zerop (length numeroluettelo))
      (virhe "Anna poistettavien numerot. Ohjeita saa ?:llä."))
    (setf numeroluettelo (jäsennä-numeroluettelo numeroluettelo))
    (unless numeroluettelo
      (virhe "Sopimattomia numeroita. Ohjeita saa ?:llä."))
    (let ((suurin (length *muokattavat*)))
      (when (notevery (lambda (n)
                        (<= 1 n suurin))
                      numeroluettelo)
        (virhe "Vain seuraavat voi poistaa: 1~[~;~:;-~:*~A~]." suurin)))
    (when (> (length numeroluettelo) *poistoraja*)
      (virhe "Vain ~A kpl voi poistaa kerralla." *poistoraja*))

    (with-transaction
      (loop :for i :in numeroluettelo
            :for kohde := (elt *muokattavat* (1- i))
            :do (cond ((typep kohde 'ryhmä)
                       (virhe "Ryhmää ei voi poistaa näin. Ryhmä poistuu ~
                                itsestään,~%kun siltä poistaa kaikki ~
                                oppilaat ja suoritukset."))
                      ((and kohde (not (typep kohde 'arvosana)))
                       (poista kohde)
                       (setf (elt *muokattavat* (1- i)) nil))
                      ((and kohde (typep kohde 'arvosana))
                       (poista kohde))
                      (t (viesti "~&Tietue ~A on jo poistettu.~%" i))))
      (lisää-muokkauslaskuriin (length numeroluettelo)))
    (eheytys)))


(defun komento-muokkaa-oppilas (kentät kohde)
  ;; /sukunimi/etunimi/ryhmät/lisätiedot
  (let ((suku (nth 0 kentät))
        (etu (nth 1 kentät))
        (ryhmä (nth 2 kentät))
        (lisä (nth 3 kentät))
        (uusi-suku :tyhjä)
        (uusi-etu :tyhjä)
        (uusi-ryhmä :tyhjä)
        (uusi-lisä :tyhjä))

    (when (and suku (on-sisältöä-p suku))
      (setf uusi-suku suku))

    (when (and etu (on-sisältöä-p etu))
      (setf uusi-etu etu))

    (when ryhmä
      (cond ((on-sisältöä-p ryhmä)
             (setf uusi-ryhmä ryhmä))
            ((and (plusp (length ryhmä)) (not (on-sisältöä-p ryhmä)))
             (virhe "Oppilaan täytyy kuulua johonkin ryhmään."))))

    (when lisä
      (cond ((on-sisältöä-p lisä)
             (setf uusi-lisä lisä))
            ((and (plusp (length lisä)) (not (on-sisältöä-p lisä)))
             (setf uusi-lisä nil))))

    (unless (eql uusi-suku :tyhjä)
      (setf (sukunimi kohde) (normalisoi-mj uusi-suku)))
    (unless (eql uusi-etu :tyhjä)
      (setf (etunimi kohde) (normalisoi-mj uusi-etu)))
    (unless (eql uusi-ryhmä :tyhjä)
      (setf (ryhmälista kohde) (normalisoi-ryhmät uusi-ryhmä)))
    (unless (eql uusi-lisä :tyhjä)
      (setf (lisätiedot kohde) (normalisoi-mj uusi-lisä)))
    (muokkaa kohde)))


(defun komento-muokkaa-suoritus (kentät kohde)
  ;; /nimi/lyhenne/painokerroin/sija
  (let ((nimi (nth 0 kentät))
        (lyhenne (nth 1 kentät))
        (painokerroin (nth 2 kentät))
        (sija (nth 3 kentät))
        (uusi-nimi :tyhjä)
        (uusi-lyhenne :tyhjä)
        (uusi-painokerroin :tyhjä)
        (uusi-sija :tyhjä))

    (when (and nimi (on-sisältöä-p nimi))
      (setf uusi-nimi nimi))

    (when (and lyhenne (on-sisältöä-p lyhenne))
      (setf uusi-lyhenne lyhenne))

    (when painokerroin
      (let ((num (lue-numero painokerroin)))
        (cond
          ((and (plusp (length painokerroin))
                (not (on-sisältöä-p painokerroin)))
           (setf uusi-painokerroin nil))
          ((and (integerp num) (plusp num))
           (setf uusi-painokerroin num))
          ((and (on-sisältöä-p painokerroin)
                (or (not (integerp num))
                    (and (integerp num)
                         (not (plusp num)))))
           (virhe "Painokertoimen täytyy olla positiivinen kokonaisluku ~
                (tai välilyönti).")))))

    (when sija
      (let ((suurin (length *muokattavat*))
            (num (lue-numero sija)))
        (cond
          ((and (integerp num)
                (<= 1 num suurin))
           (setf uusi-sija num))
          ((and (integerp num)
                (not (<= 1 num suurin)))
           (virhe "Sopivia sijoja ovat seuraavat: ~A."
                  (if (> suurin 1) (format nil "1-~A" suurin) "1")))
          ((and (on-sisältöä-p sija)
                (not (integerp num)))
           (virhe "Sijan täytyy olla positiivinen kokonaisluku.")))))

    (unless (eql uusi-nimi :tyhjä)
      (setf (nimi kohde) (normalisoi-mj uusi-nimi)))
    (unless (eql uusi-lyhenne :tyhjä)
      (setf (lyhenne kohde) (normalisoi-mj uusi-lyhenne)))
    (unless (eql uusi-painokerroin :tyhjä)
      (setf (painokerroin kohde) (normalisoi-painokerroin uusi-painokerroin)))
    (muokkaa kohde :sija (if (eql uusi-sija :tyhjä) nil uusi-sija))))


(defun komento-muokkaa-arvosana (kentät kohde)
  ;; /arvosana/lisätiedot
  (let ((arvosana (nth 0 kentät))
        (lisätiedot (nth 1 kentät))
        (uusi-arvosana :tyhjä)
        (uusi-lisätiedot :tyhjä))
    (when arvosana
      (cond ((on-sisältöä-p arvosana)
             (setf uusi-arvosana arvosana))
            ((and (plusp (length arvosana)) (not (on-sisältöä-p arvosana)))
             (setf uusi-arvosana ""))))
    (when lisätiedot
      (cond ((on-sisältöä-p lisätiedot)
             (setf uusi-lisätiedot lisätiedot))
            ((and (plusp (length lisätiedot))
                  (not (on-sisältöä-p lisätiedot)))
             (setf uusi-lisätiedot nil))))

    (unless (eql uusi-arvosana :tyhjä)
      (setf (arvosana kohde) (normalisoi-mj uusi-arvosana)))
    (unless (eql uusi-lisätiedot :tyhjä)
      (setf (lisätiedot kohde) (normalisoi-mj uusi-lisätiedot)))
    (if (and (or (not (arvosana kohde))
                 (equal (arvosana kohde) ""))
             (or (not (lisätiedot kohde))
                 (equal (lisätiedot kohde) "")))
        (poista kohde)
        (muokkaa kohde))))


(defun komento-muokkaa-ryhmä (kentät kohde)
  ;; /nimi/lisätiedot
  (let ((nimi (nth 0 kentät))
        (lisätiedot (nth 1 kentät))
        (uusi-nimi :tyhjä)
        (uusi-lisätiedot :tyhjä))
    (when (and nimi (on-sisältöä-p nimi))
      (multiple-value-bind (ryhmä loput)
          (erota-ensimmäinen-sana nimi)
        (when (plusp (length loput))
          (virhe "Ryhmätunnuksen täytyy olla yksi sana."))
        (when (query "SELECT nimi FROM ryhmat WHERE nimi LIKE ~A"
                     (sql-like-suoja ryhmä))
          (virhe "Ryhmä nimeltä ~A on jo olemassa." ryhmä))
        (setf uusi-nimi ryhmä)))
    (when lisätiedot
      (cond ((on-sisältöä-p lisätiedot)
             (setf uusi-lisätiedot lisätiedot))
            ((and (plusp (length lisätiedot))
                  (not (on-sisältöä-p lisätiedot)))
             (setf uusi-lisätiedot nil))))
    (unless (eql uusi-nimi :tyhjä)
      (setf (nimi kohde) (normalisoi-mj uusi-nimi)))
    (unless (eql uusi-lisätiedot :tyhjä)
      (setf (lisätiedot kohde) (normalisoi-mj uusi-lisätiedot)))
    (muokkaa kohde)))


(defun komento-muokkaa-sarjana (arg)
  ;; numeroluettelo kentän_numero ////
  (cond
    ((not *muokattavat*)
     (virhe "Edellinen komento ei sisällä muokattavia."))
    ((zerop (length arg))
     (virhe "Anna tietueiden numerot, kentän numero ja uudet tiedot. ~
                Ohjeita saa ?:llä.")))

  (multiple-value-bind (numeroluettelo loput)
      (erota-ensimmäinen-sana arg)
    (setf numeroluettelo (jäsennä-numeroluettelo numeroluettelo))
    (let ((suurin (length *muokattavat*)))
      (cond
        ((or (not numeroluettelo)
             (notevery (lambda (nro)
                         (<= 1 nro suurin))
                       numeroluettelo))
         (virhe "Vain seuraavia voi muokata: 1~[~;~:;-~:*~A~]." suurin))
        ((not (on-sisältöä-p loput))
         (virhe "Anna muokkausta varten kentän numero. Ohjeita saa ?:llä."))))

    (multiple-value-bind (kentän-numero loput)
        (erota-ensimmäinen-sana loput)
      (setf kentän-numero (lue-numero kentän-numero))
      (cond
        ((not (and (integerp kentän-numero)
                   (plusp kentän-numero)))
         (virhe "Kentän numeron täytyy olla positiivinen kokonaisluku."))
        ((not (on-sisältöä-p loput))
         (virhe "Anna muokkausta varten uudet tiedot. Ohjeita saa ?:llä.")))

      (let ((arvot (pilko-erottimella loput)))
        (with-transaction
          (loop :for arvo :in arvot
                :for kentät := (append (make-list (1- kentän-numero)
                                                  :initial-element "")
                                       (list arvo))
                :for i :in numeroluettelo
                :for kohde := (elt *muokattavat* (1- i))
                :do (typecase kohde
                      (oppilas (komento-muokkaa-oppilas kentät kohde))
                      (suoritus (komento-muokkaa-suoritus kentät kohde))
                      (arvosana (komento-muokkaa-arvosana kentät kohde))
                      (ryhmä (komento-muokkaa-ryhmä kentät kohde))
                      (t (virhe "Tietue ~A on poistettu." i))))
          (lisää-muokkauslaskuriin (min (length numeroluettelo)
                                        (length arvot)))))
      (eheytys))))


(defun komento-muokkaa (arg)
  ;; numeroluettelo //// (tilannekohtaiset kentät)
  (cond
    ((not *muokattavat*)
     (virhe "Edellinen komento ei sisällä muokattavia."))
    ((zerop (length arg))
     (virhe "Anna tietueiden numerot ja uudet tiedot. Ohjeita saa ?:llä.")))

  (multiple-value-bind (numeroluettelo loput)
      (erota-ensimmäinen-sana arg)
    (setf numeroluettelo (jäsennä-numeroluettelo numeroluettelo))
    (let ((suurin (length *muokattavat*)))
      (cond
        ((or (not numeroluettelo)
             (notevery (lambda (nro)
                         (<= 1 nro suurin))
                       numeroluettelo))
         (virhe "Vain seuraavia voi muokata: 1~[~;~:;-~:*~A~]." suurin))
        ((not (on-sisältöä-p loput))
         (virhe "Anna muokkausta varten uudet tiedot. Ohjeita saa ?:llä."))))

    (with-transaction
      (loop :with kentät := (pilko-erottimella loput)
            :for i :in numeroluettelo
            :for kohde := (elt *muokattavat* (1- i))
            :do (typecase kohde

                  (oppilas
                   (let ((suku (nth 0 kentät))
                         (etu (nth 1 kentät)))
                     (if (and (> (length numeroluettelo) 1)
                              (or (and suku (on-sisältöä-p suku))
                                  (and etu (on-sisältöä-p etu))))
                         (virhe "Usealle oppilaalle ei voi vaihtaa kerralla ~
                                samaa suku- tai etunimeä.")
                         (komento-muokkaa-oppilas kentät kohde))))

                  (suoritus
                   (let ((sija (nth 3 kentät)))
                     (if (and (> (length numeroluettelo) 1)
                              sija (on-sisältöä-p sija))
                         (virhe "Usealle suoritukselle ei voi asettaa samaa ~
                                        sijaa yhtä aikaa.")
                         (komento-muokkaa-suoritus kentät kohde))))

                  (arvosana
                   (komento-muokkaa-arvosana kentät kohde))

                  (ryhmä
                   (komento-muokkaa-ryhmä kentät kohde))

                  (t (virhe "Tietue ~A on poistettu." i))))

      (lisää-muokkauslaskuriin (length numeroluettelo)))
    (eheytys)))


(defun ohjeet (&optional komento)
  (tulosta-taulu
   (list
    :viiva-alku
    (list (otsikko "Komento") (otsikko "Tarkoitus"))
    :viiva-otsikko
    '("ho /sukunimi/etunimi/ryhmät/lisätiedot" "Hae oppilaita.")
    '("hoa /sukunimi/etunimi/ryhmät/lisätiedot"
      "Hae oppilaita arvotussa järjestyksessä.")
    '("hr /ryhmä/lisätiedot" "Hae ryhmiä.")
    '("hs ryhmä" "Hae suoritukset ryhmältä.")
    '("hao /sukunimi/etunimi/ryhmät/lisätiedot" "Hae arvosanat oppilailta.")
    '("has ryhmä /suoritus/lyhenne" "Hae arvosanat suorituksista.")
    '("hak ryhmä" "Hae arvosanojen koonti.")
    :viiva
    '("tj  |/suku/etu/ryh/lisät/suor/lyh|/..." "Tulosta jakauma.")
    '("tjp |/suku/etu/ryh/lisät/suor/lyh|/..."
      "Tulosta jakauma (vain painokertoimelliset).")
    '("tk" "Tulosta tietokannasta koonti.")
    :viiva
    '("lo /sukunimi/etunimi/ryhmät/lisätiedot" "Lisää oppilas.")
    '("ls ryhmä /suoritus/lyhenne/painokerroin/sija"
      "Lisää ryhmälle suoritus.")
    :viiva
    '("m numerot /.../.../.../..." "Muokkaa valittuja tietueita ja kenttiä.")
    '("ms numerot kenttä /.../.../..." "Muokkaa tietueista samaa kenttää.")
    '("poista numerot" "Poista tietueet.")
    :viiva
    '("?" "Ohjeet.")
    '("??" "Tarkemmat ohjeet.")
    '("???" "Aloitusvinkkejä.")
    :viiva-loppu))

  (cond ((equal komento "?")
         (return-from ohjeet))
        ((equal komento "??")
         (viesti "~%~

Komento-sarakkeessa vinoviiva (/) tarkoittaa kenttien
erotinmerkkiä (/.../...). Ensimmäisenä oleva merkki määrittää komennossa
käytettävän erotinmerkin, joka voi olla mikä tahansa. Esimerkiksi alla
olevat komennot toimivat samalla tavalla. Niillä haetaan oppilaita (ho)
sukunimen ja etunimen perusteella. Kentät on erotettu toisistaan eri
erotinmerkeillä.

    ho /Meikäl/Mat
    ho ,Meikäl,Mat
    ho 3Meikäl3Mat

Tilastokomennoissa (\"tj\" ja \"tjp\") oleva pystyviiva (|) tarkoittaa
myös erotinmerkkiä. Näissä komennoissa on kaksitasoinen kenttien erotus,
joten niille voi määritellä useita hakulausekkeita. Ensin argumentit
jaetaan |-merkin avulla ryhmiin erillisiksi hakulausekeryhmiksi ja
sitten kukin ryhmä jaetaan hakukentiksi /-merkin avulla. Erotinmerkit
voi valita vapaasti. Kumpikin seuraavista komennoista toimii samalla
tavalla:

    tj |///2012:8a|///2013:8b
    tj @,,,2012:8a@...2013:8b

Komento \"tjp\" on muutoin samanlainen kuin \"tj\", mutta se huomioi
vain sellaiset suoritukset, joille on määritelty painokerroin.

Komentojen \"m\", \"ms\" ja \"poista\" kohdalla argumentti \"numerot\"
tarkoittaa kokonaislukujen luetteloa, esimerkiksi \"1,4\" tai \"2-5,9\".
Niiden avulla valitaan, mitä tietueita muokataan tai mitkä poistetaan.

Hakutoiminnot (h:lla alkavat komennot) tulostavat tietokannassa olevia
tietoja. Hakutoiminnon tulosteessa voi olla numeroituja tietueita.
Lisäksi tulosteen lopussa on esimerkiksi seuraavanlainen rivi:

    Tietueet: 1-22. Kentät: /arvosana/lisätiedot

Se tarkoittaa, että mainittuja tietueita (1-22) ja kenttiä (arvosana,
lisätiedot) on mahdollista muokata tai poistaa. Tietueita poistetaan
esimerkiksi seuraavilla komennoilla:

    poista 1
    poista 4,7,9
    poista 2-6,10,14-19

Tietueita muokataan m- tai ms-komennolla. Esimerkiksi arvosanahaun (hao-
ja has-komennot) jälkeen voidaan muokata jokaisesta tietueesta arvosana-
ja lisätiedot-kenttää. Eri kentät erotetaan toisistaan jollakin
erotinmerkillä. Tässä esimerkissä käytetään vinoviivaa (/):

    m 1 /8+/Eri koe kuin muilla
    m 3,9,14 /7½

Kaikkia kenttiä ei tarvitse asettaa. Esimerkiksi oppilashaun (ho- ja
hoa-komennot) jälkeen on mahdollista muokata neljää kenttää: sukunimi,
etunimi, ryhmät ja lisätiedot. Ne kentät, joita ei haluta muuttaa,
voidaan jättää tyhjiksi, eli erotinmerkkien välissä ei ole mitään. Alla
on esimerkki viiden oppilaan (tietueet 1-5) ryhmät-kentän (3.
vasemmalta) samanaikaisesta muokkaamisesta. Ryhmätunnukset erotetaan
toisistaan välilyönnillä.

    m 1-5 ///2013:7a 2014:8a

Kenttä tyhjennetään laittamalla kenttään pelkkä välilyönti:

    m 1 / / /

Toinen muokkauskomento on \"ms\". Myös sille annetaan ensimmäiseksi
argumentiksi luettelo muokattavista tietueista. Toiseksi argumentiksi
annetaan muokattavan kentän numero: ensimmäinen kenttä vasemmalta on 1,
toinen vasemmalta on 2 jne. Kolmantena argumenttina luetellaan
erotinmerkin avulla kyseiseen kenttään tulevat tiedot eri tietueissa.

Esimerkiksi jos halutaan kirjata arvosana usealle oppilaalle, haetaan
ensin halutut suoritustiedot has-komennolla ja annetaan sitten
seuraavanlainen muokkauskomento:

    ms 1-9,11 1 /8-/6½/8+/7-/7½/7-/7/6½/9-/8+

Yllä oleva komento muokkaa tietueita 1-9 ja 11. Kaikista tietueista
muokataan kenttää numero 1 (arvosana-kenttä). Kenttiin tulevat arvosanat
on lueteltu erotinmerkin \"/\" avulla samassa järjestyksessä kuin
tietueetkin on annettu (1-9,11).

Hakutoimintojen tulostusasua voi muuttaa kirjoittamalla komentorivin
alkuun tietyn avainsanan. Alla oleva taulukko ja esimerkki selventää
niitä.

")
         (tulosta-taulu
          (list
           :viiva-alku
           (list (otsikko "Sana") (otsikko "Selitys"))
           :viiva-otsikko
           '("wilma" "Wilma-viestiin sopiva taulukkomalli.")
           '("org" "Emacsin Org-tilaan sopiva taulukkomalli.")
           '("latex" "Tulosteet LaTeX-komentoina.")
           '("suppea" "Karsitaan tulostuksesta Lisätiedot-kentät yms.")
           :viiva-loppu))

         (viesti "~%~

Esimerkiksi

    wilma suppea hao /Meikäl/Mat/2013:7a
    org hak 2013:7a

"))

        ((equal komento "???")
         (viesti "~%~

Tietokantaohjelman käyttö kannattaa aloittaa lisäämällä oppilaita.
Lisäystoiminnossa (lo) syötettävät kentät ovat vasemmalta oikealle
sukunimi, etunimi, ryhmät ja lisätiedot. Ainakin sukunimi, etunimi ja
yksi ryhmä täytyy syöttää. Kentät erotetaan toisistaan jollakin
erotinmerkillä. Tässä esimerkissä käytetään vivoviivaa (/):

    lo /Meikäläinen/Matti/2013:7a
    lo /Oppilas/Oona/2013:7a
    lo /Koululainen/Kalle/2013:7a/lukivaikeus

Kannattaa nimetä ryhmät lukuvuoden aloitusvuoden ja ryhmätunnuksen
avulla, esimerkiksi \"2013:7a\". Näin ryhmät voi yksilöidä usean
lukuvuoden aikana. Oppilaan tiedoissa eri ryhmät erotetaan toisistaan
välilyönnein:

    lo /Meikäläinen/Maija/2013:7a 2014:8a 2015:9a

Sitten voi luoda ryhmälle suorituksia. Suoritustiedoissa kentät ovat
seuraavat: suorituksen nimi, lyhenne, painokerroin ja sija eli
järjestysnumero. Suorituksen lyhennettä käytetään arvosanojen
koonnissa (hak-komento). Painokerrointa käytetään suoritusten keskiarvon
laskennassa. Sen täytyy olla positiivinen kokonaisluku. Jos
painokerrointa ei ole, kyseistä suoritusta ei huomioida keskiarvon
laskennassa. Alla on esimerkkejä suoritusten lisäämisestä.

    ls 2013:7a /Kirje opettajalle/kir
    ls 2013:7a /Sanaluokkakoe/san/2
    ls 2013:7a /Kirjoitelma romaanista/rom/3
    ls 2013:7a /Välitodistus/vto

"))))


(defun käsittele-komentorivi (mj)
  (when (null mj)
    (viesti "~%")
    (error 'poistu-ohjelmasta))
  (handler-case
      (multiple-value-bind (komento arg)
          (erota-ensimmäinen-sana mj)
        (flet ((testaa (mj)
                 (equalp komento mj))
               (tuntematon ()
                 (virhe "Tuntematon komento. Ohjeita saa ?:llä.")))
          (cond
            ((testaa "wilma") (let ((*tulostusmuoto* :wilma))
                                (käsittele-komentorivi arg)))
            ((testaa "org") (let ((*tulostusmuoto* :org))
                              (käsittele-komentorivi arg)))
            ((testaa "latex") (let ((*tulostusmuoto* :latex))
                                (käsittele-komentorivi arg)))
            ((testaa "suppea") (let ((*suppea* t))
                                 (käsittele-komentorivi arg)))
            ((testaa "ho") (komento-hae-oppilaat arg))
            ((testaa "hoa") (komento-hae-oppilaat-arvottu arg))
            ((testaa "hs") (komento-hae-suoritukset arg))
            ((testaa "hr") (komento-hae-ryhmät arg))
            ((testaa "has") (komento-hae-arvosanat-suorituksista arg))
            ((testaa "hao") (komento-hae-arvosanat-oppilailta arg))
            ((testaa "hak") (komento-hae-arvosanat-koonti arg))
            ((testaa "lo") (komento-lisää-oppilas arg))
            ((testaa "ls") (komento-lisää-suoritus arg))
            ((testaa "tj") (komento-tilasto-jakauma arg))
            ((testaa "tjp") (komento-tilasto-jakauma arg t))
            ((testaa "tk") (komento-tilasto-koonti))
            ((or (testaa "?") (testaa "??") (testaa "???")) (ohjeet komento))
            (*vuorovaikutteinen*
             (cond
               ((testaa "") (signal 'poistu-ohjelmasta))
               ((testaa "poista") (komento-poista arg))
               ((testaa "m") (komento-muokkaa arg))
               ((testaa "ms") (komento-muokkaa-sarjana arg))
               (t (tuntematon))))
            (t (tuntematon)))))

    (virhe (tila)
      (format *error-output* "~&~A~%" tila))))


(defun main (&optional argv)
  (script:with-pp-errors
    (handler-case
        (tietokanta-käytössä
          (if (rest argv)
              (let ((*vuorovaikutteinen* nil))
                (käsittele-komentorivi (format nil "~{~A~^ ~}" (rest argv))))
              (let ((*vuorovaikutteinen* t))
                (loop (käsittele-komentorivi (lue-rivi "KOAS> " t))))))
      (poistu-ohjelmasta () nil)
      (sb-sys:interactive-interrupt ()
        (viesti "~%")))))


#+script
(let ((*readline* t)) (main (script:argv)))
