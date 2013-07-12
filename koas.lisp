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
(defvar *muokkaukset-kunnes-eheytys* 1000)


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


(defun alusta-tietokanta ()
  (let ((kaikki (mapcar #'first (query "select name from sqlite_master ~
                                        where type='table'")))
        (valmistellaan t))
    (flet ((löytyy (asia)
             (member asia kaikki :test #'string-equal))
           (teksti ()
             (when valmistellaan
               (viesti "~&Valmistellaan tietokanta (~A).~%~
        Ota tietokantatiedostosta varmuuskopio riittävän usein.~%~
        Ohjelman käyttöön saa apua ?-komennolla.~%"
                       (sb-ext:native-pathname *tiedosto*))
               (setf valmistellaan nil))))
      (unless (löytyy "oppilaat")
        (teksti)
        (query "create table oppilaat ~
                (oid integer unique primary key, ~
                sukunimi text, etunimi text, ~
                ryhmat text, lisatiedot text default '')"))
      (unless (löytyy "ryhmat")
        (teksti)
        (query "create table ryhmat ~
                (ryhma text unique, suoritukset text default '')"))
      (unless (löytyy "suoritukset")
        (teksti)
        (query "create table suoritukset ~
                (sid integer unique primary key, ~
                nimi text default '', ~
                lyhenne text default '', ~
                painokerroin integer)"))
      (unless (löytyy "hallinto")
        (query "create table hallinto (avain text unique, arvo text)"))

      (query "pragma case_sensitive_like = 0"))))


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


(defmacro with-transaction (&body body)
  `(sqlite:with-transaction *tietokanta* ,@body))


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
    (:wilma (format nil "**~A**" (if (equal "" mj) " " mj)))
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


(defun mj-lista-listaksi (mj-lista)
  (split-sequence #\space mj-lista :remove-empty-subseqs t))


(defun lista-mj-listaksi (lista)
  (format nil "~{~A~^ ~}" lista))


(defun lisää-mj-listaan (uusi mj-lista &optional ennen)
  (let ((lista (mj-lista-listaksi mj-lista)))
    (setf ennen (if ennen
                    (min (max 0 ennen) (length lista))
                    (length lista)))
    (loop :for el :in lista
          :for i :upfrom 0
          :if (= i ennen) :collect uusi :into uusi-lista
          :collect el :into uusi-lista
          :finally (setf lista (if (= ennen (length lista))
                                   (nconc uusi-lista (list uusi))
                                   uusi-lista)))
    (lista-mj-listaksi lista)))


(defun poista-mj-listasta (osa mj-lista)
  (let ((lista (split-sequence #\space mj-lista
                               :remove-empty-subseqs t)))
    (setf lista (remove osa lista :test #'string-equal))
    (lista-mj-listaksi lista)))


(defun siirrä-mj-listassa (osa mj-lista uusi-kohta)
  (let ((lista (split-sequence #\space mj-lista
                               :remove-empty-subseqs t)))
    (lisää-mj-listaan
     osa (lista-mj-listaksi (remove osa lista :test #'string-equal))
     uusi-kohta)))


(defun tulosta-taulu (taulu &key (virta *standard-output*))
  (when taulu
    (flet ((viivap (ob) (eql ob :viiva)))
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
              :do
              (format virta "~:[|~;+~]" (and (viivap rivi) (muoto nil)))
              (loop :for (osa . loput) :on uusi
                    :for leveys :in leveimmät-sarakkeet
                    :do
                    (cond
                      ((and (viivap osa) (or (muoto :org) (muoto nil)))
                       (format virta "--~V,,,'-<~>~:[|~;+~]" leveys
                               (or loput (and (not loput) (muoto nil)))))
                      ((and (viivap osa) (muoto :wilma))
                       (format virta " ~V<~> |" leveys))
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
   (sid :accessor sid :initarg :sid)
   (nimi :accessor nimi :initarg :nimi)
   (lyhenne :accessor lyhenne :initarg :lyhenne)
   (painokerroin :accessor painokerroin :initarg :painokerroin :initform nil)))

(defclass suoritukset ()
  ((ryhmä :reader ryhmä :initarg :ryhmä)
   (suorituslista :reader suorituslista :initarg :suorituslista)))

(defclass ryhmä ()
  ((ryhmä :accessor ryhmä :initarg :ryhmä)
   (vanha-ryhmä :accessor vanha-ryhmä :initarg :vanha-ryhmä)))

(defclass suoritusryhmät ()
  ((ryhmälista :reader ryhmälista :initarg :ryhmälista)))

(defclass arvosana ()
  ((oppilas :reader oppilas :initarg :oppilas)
   (sid :reader sid :initarg :sid)
   (arvosana :accessor arvosana :initarg :arvosana :initform "")
   (lisätiedot :accessor lisätiedot :initarg :lisätiedot :initform nil)))

(defclass arvosanat-suoritus ()
  ((sid :reader sid :initarg :sid)
   (ryhmä :reader ryhmä :initarg :ryhmä)
   (arvosanalista :reader arvosanalista :initarg :arvosanalista)))

(defclass arvosanat-suoritukset ()
  ((lista :reader lista :initarg :lista)))

(defclass arvosanat-oppilas ()
  ((oppilas :reader oppilas :initarg :oppilas)
   (ryhmä :reader ryhmä :initarg :ryhmä)
   (suorituslista :reader suorituslista :initarg :suorituslista)
   (arvosanalista :reader arvosanalista :initarg :arvosanalista)))

(defclass arvosanat-oppilaat ()
  ((lista :reader lista :initarg :lista)))

(defclass arvosanat-koonti ()
  ((ryhmä :reader ryhmä :initarg :ryhmä)
   (suorituslista :reader suorituslista :initarg :suorituslista)
   (taulukko :reader taulukko :initarg :taulukko)))


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


(defun muokkauslaskuri (muokkaukset)
  (let ((laskuri (query "select arvo from hallinto ~
                where avain='muokkauslaskuri'")))
    (unless laskuri
      (query "insert into hallinto (avain, arvo) ~
                                values ('muokkauslaskuri', '0')"))
    (setf laskuri (+ muokkaukset (or (lue-numero (first (first laskuri))) 0)))
    (query "update hallinto set arvo=~A where avain='muokkauslaskuri'"
           (sql-mj laskuri))
    laskuri))


(defun ehkä-eheytys ()
  (let ((laskuri (query "select arvo from hallinto ~
                                where avain='muokkauslaskuri'")))
    (setf laskuri (or (lue-numero (first (first laskuri))) 0))
    (if (>= laskuri *muokkaukset-kunnes-eheytys*)
        (ignore-errors
          (query "vacuum")
          (query "update hallinto set arvo='0' where avain='muokkauslaskuri'")
          t)
        nil)))


(defun hae-oppilaat (sukunimi &optional (etunimi "") (ryhmät "")
                                (lisätiedot ""))
  (let ((oppilaat
         (query "select oid,sukunimi,etunimi,ryhmat,lisatiedot from oppilaat ~
                where sukunimi like ~A and etunimi like ~A and ~
                ryhmat like ~A and lisatiedot like ~A group by ~
                sukunimi,etunimi,ryhmat,oid"
                (sql-like-suoja sukunimi "%" "%")
                (sql-like-suoja etunimi "%" "%")
                (sql-like-suoja ryhmät "%" "%")
                (sql-like-suoja lisätiedot "%" "%"))))
    (when oppilaat
      (make-instance
       'oppilaat
       :oppilaslista (loop :for (o s e r l) in oppilaat
                           :collect (make-instance 'oppilas
                                                   :oid o
                                                   :sukunimi s
                                                   :etunimi e
                                                   :ryhmälista
                                                   (mj-lista-listaksi r)
                                                   :lisätiedot l))))))


(defun hae-suoritukset (ryhmä)
  (let ((ryhmä-suoritukset
         (first (query "select * from ryhmat where ryhma like ~A"
                       (sql-like-suoja ryhmä)))))

    (when ryhmä-suoritukset
      (let ((suoritukset
             (loop :for id :in (split-sequence
                                #\space (second ryhmä-suoritukset)
                                :remove-empty-subseqs t)
                   :for (sid nimi lyh kerroin)
                   := (first (query "select sid,nimi,lyhenne,painokerroin ~
                                      from suoritukset where sid = ~A"
                                    id))
                   :collect (make-instance 'suoritus
                                           :ryhmä ryhmä
                                           :sid sid
                                           :nimi nimi
                                           :lyhenne lyh
                                           :painokerroin kerroin))))

        (when suoritukset
          (make-instance 'suoritukset
                         :ryhmä ryhmä
                         :suorituslista suoritukset))))))


(defun hae-suoritusryhmät ()
  (let ((ryhmät (mapcar #'first (query "select ryhma from ryhmat ~
                                        group by ryhma"))))
    (when ryhmät
      (make-instance
       'suoritusryhmät
       :ryhmälista (loop :for ryhmä :in ryhmät
                         :collect
                         (make-instance 'ryhmä :ryhmä ryhmä))))))


(defun hae-arvosanat-suoritukset (ryhmä &optional (nimi "") (lyhenne ""))
  (let ((suor (hae-suoritukset ryhmä))
        (kaikki))
    (when suor
      (loop :named valmis
            :for suoritus :in (suorituslista suor)
            :if (and (search nimi (nimi suoritus) :test #'char-equal)
                     (search lyhenne (lyhenne suoritus) :test #'char-equal))

            :do
            (let ((kysely (query "select oppilaat.oid,oppilaat.sukunimi,~
                        oppilaat.etunimi,oppilaat.ryhmat,oppilaat.lisatiedot,~
                        suoritus_~A.arvosana,suoritus_~A.lisatiedot ~
                        from oppilaat left join suoritus_~A ~
                        on oppilaat.oid = suoritus_~A.oid ~
                        where oppilaat.ryhmat like ~A group by ~
                        oppilaat.sukunimi,oppilaat.etunimi"
                                 (sid suoritus) (sid suoritus)
                                 (sid suoritus)
                                 (sid suoritus)
                                 (sql-like-suoja ryhmä "%" "%"))))

              (when kysely
                (loop :for i :upfrom 1
                      :for (oid suku etu ryh ol arv al) :in kysely
                      :for oppilas := (make-instance 'oppilas
                                                     :oid oid
                                                     :sukunimi suku
                                                     :etunimi etu
                                                     :ryhmälista
                                                     (mj-lista-listaksi ryh)
                                                     :lisätiedot ol)
                      :for arvosana := (make-instance 'arvosana
                                                      :oppilas oppilas
                                                      :sid (sid suoritus)
                                                      :arvosana arv
                                                      :lisätiedot al)
                      :collect arvosana :into arvosanat
                      :finally (push (make-instance 'arvosanat-suoritus
                                                    :ryhmä ryhmä
                                                    :sid (sid suoritus)
                                                    :arvosanalista arvosanat)
                                     kaikki))))
            :finally
            (return-from valmis
              (when kaikki
                (make-instance 'arvosanat-suoritukset
                               :lista (nreverse kaikki))))))))


(defun hae-arvosanat-oppilaat (sukunimi &optional (etunimi "") (ryhmät "")
                                          (lisätiedot ""))
  (let ((opp (hae-oppilaat sukunimi etunimi ryhmät lisätiedot))
        (kaikki))
    (when opp
      (loop :for oppilas :in (oppilaslista opp)
            :do
            (loop :with rl := (sort (copy-seq (ryhmälista oppilas))
                                    #'string-lessp)
                  :for ryhmä :in rl
                  :if (search ryhmät ryhmä :test #'char-equal)
                  :do
                  (let ((suor (hae-suoritukset ryhmä)))
                    (when suor
                      (let ((arvosanat
                             (loop :for suoritus :in (suorituslista suor)
                                   :for (as lt)
                                   := (first
                                       (query "select arvosana,lisatiedot ~
                                        from suoritus_~A where oid = ~A"
                                              (sid suoritus) (oid oppilas)))
                                   :collect
                                   (make-instance 'arvosana
                                                  :oppilas oppilas
                                                  :sid (sid suoritus)
                                                  :arvosana as
                                                  :lisätiedot lt))))
                        (push (make-instance 'arvosanat-oppilas
                                             :oppilas oppilas
                                             :ryhmä ryhmä
                                             :suorituslista (suorituslista suor)
                                             :arvosanalista arvosanat)
                              kaikki))))))
      (when kaikki
        (make-instance 'arvosanat-oppilaat
                       :lista (nreverse kaikki))))))


(defun hae-arvosanat-koonti (ryhmä)
  (let ((suorituslista (let ((suoritukset (hae-suoritukset ryhmä)))
                         (when suoritukset
                           (suorituslista suoritukset)))))

    (when suorituslista
      (let (taulut joinit taulukko)
        (loop :repeat 64 ;SQLiten suurin joinien määrä
              :for s :in suorituslista
              :collect (format nil "suoritus_~A.arvosana" (sid s))
              :into tau
              :collect (format nil "left join suoritus_~A on oppilaat.oid = ~
                        suoritus_~A.oid"
                               (sid s) (sid s))
              :into joi
              :finally (setf taulut tau joinit joi))

        (setf taulukko
              (query "select oppilaat.sukunimi,oppilaat.etunimi,~
                ~{~A~^,~} from oppilaat ~{~A~^ ~} ~
                where oppilaat.ryhmat like ~A ~
                group by oppilaat.sukunimi,oppilaat.etunimi"
                     taulut joinit (sql-like-suoja ryhmä "%" "%")))

        (when taulukko
          (make-instance 'arvosanat-koonti
                         :ryhmä ryhmä
                         :suorituslista suorituslista
                         :taulukko taulukko))))))


(defun tulosta-muokattavat (&rest kentät)
  (when *muokattavat*
    (viesti "~&~[~;Muokattava tietue: 1~:;Muokattavat tietueet: 1-~:*~A~]. ~
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
     (append (if (muoto :org nil) (list :viiva))
             (list (append (if *muokattavat* (list nil))
                           (list (otsikko "Sukunimi"))
                           (list (otsikko "Etunimi"))
                           (list (otsikko "Ryhmät"))
                           (unless *suppea*
                             (list (otsikko "Lisätiedot")))))
             (if (muoto :org nil) (list :viiva))
             (if *muokattavat* (numeroi taulu) taulu)
             (if (muoto :org nil) (list :viiva))))

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
     (append (if (muoto :org nil) (list :viiva))
             (list (list (otsikko "Ryhmä:") (ryhmä suo)))
             (if (muoto :org nil) (list :viiva))))
    (viesti "~%")

    (tulosta-taulu
     (append (if (muoto :org nil) (list :viiva))
             (list (append (if *muokattavat* (list nil))
                           (list (otsikko "Suoritus"))
                           (list (otsikko "Lyh"))
                           (list (otsikko "K"))))
             (if (muoto :org nil) (list :viiva))
             (if *muokattavat* (numeroi taulu) taulu)
             (if (muoto :org nil) (list :viiva))))

    (tulosta-muokattavat "suoritus" "lyhenne" "painokerroin"
                         (format nil "sija(1~[~;~:;-~:*~A~])"
                                 (length *muokattavat*)))

    (unless (muoto nil)
      (viesti "~%")
      (tulosta-taulu (list (list (otsikko "K") "= painokerroin"))))))


(defmethod tulosta ((arv arvosanat-suoritukset))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*)
                                 (= (length (lista arv)) 1))
                        (coerce (arvosanalista (first (lista arv))) 'vector)))

  (loop :for (arv-suo . lisää) :on (lista arv)
        :do
        (let* ((arvot)
               (taulu (loop :for arvosana :in (arvosanalista arv-suo)
                            :for suku := (sukunimi (oppilas arvosana))
                            :for etu := (etunimi (oppilas arvosana))
                            :collect (append (list (format nil "~A, ~A"
                                                           suku etu))
                                             (list (arvosana arvosana))
                                             (unless *suppea*
                                               (list (lisätiedot arvosana))))
                            :do (push (arvosana arvosana) arvot)))
               (suoritus
                (first (first (query "select nimi from suoritukset ~
                                        where sid = ~A"
                                     (sid arv-suo))))))

          (tulosta-taulu
           (append (if (muoto :org nil) (list :viiva))
                   (list (list (otsikko "Ryhmä:") (ryhmä arv-suo)))
                   (list (list (otsikko "Suoritus:") suoritus))
                   (if (muoto :org nil) (list :viiva))))
          (viesti "~%")

          (tulosta-taulu
           (append (if (muoto :org nil) (list :viiva))
                   (list (append (if *muokattavat* (list nil))
                                 (list (otsikko "Oppilas"))
                                 (list (otsikko "As"))
                                 (unless *suppea*
                                   (list (otsikko "Lisätiedot")))))
                   (if (muoto :org nil) (list :viiva))
                   (if *muokattavat* (numeroi taulu) taulu)
                   (list :viiva)
                   (list (append (if *muokattavat* (list nil))
                                 (list "Keskiarvo" (keskiarvo arvot))))
                   (if (muoto :org nil) (list :viiva))))

          (unless (muoto nil)
            (viesti "~%")
            (tulosta-taulu (list (list (otsikko "As") "= arvosana")))))

        :if lisää :do (viesti "~%~%"))

  (tulosta-muokattavat "arvosana" "lisätiedot"))


(defmethod tulosta ((arv arvosanat-oppilaat))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*)
                                 (= (length (lista arv)) 1))
                        (coerce (arvosanalista (first (lista arv))) 'vector)))

  (loop :with enintään := 30
        :for n :from 1 :upto enintään
        :for (arv-opp . lisää) :on (lista arv)
        :do
        (let* ((arvot)
               (kertoimet)
               (taulu (loop :for suoritus :in (suorituslista arv-opp)
                            :for arvosana :in (arvosanalista arv-opp)
                            :collect (append (list (nimi suoritus))
                                             (list (arvosana arvosana))
                                             (list (painokerroin suoritus))
                                             (unless *suppea*
                                               (list (lisätiedot arvosana))))
                            :do
                            (push (arvosana arvosana) arvot)
                            (push (painokerroin suoritus) kertoimet))))

          (tulosta-taulu
           (append (if (muoto :org nil) (list :viiva))
                   (list (list (otsikko "Oppilas:")
                               (format nil "~A, ~A"
                                       (sukunimi (oppilas arv-opp))
                                       (etunimi (oppilas arv-opp)))))
                   (list (list (otsikko "Ryhmä:") (ryhmä arv-opp)))
                   (let ((lis (lisätiedot (oppilas arv-opp))))
                     (if (or (not lis) (equal lis "") *suppea*)
                         nil
                         (list (list (otsikko "Lisätiedot:") lis))))
                   (if (muoto :org nil) (list :viiva))))
          (viesti "~%")

          (tulosta-taulu
           (append (if (muoto :org nil) (list :viiva))
                   (list (append (if *muokattavat* (list nil))
                                 (list (otsikko "Suoritus"))
                                 (list (otsikko "As"))
                                 (list (otsikko "K"))
                                 (unless *suppea*
                                   (list (otsikko "Lisätiedot")))))
                   (if (muoto :org nil) (list :viiva))
                   (if *muokattavat* (numeroi taulu) taulu)
                   (list :viiva)
                   (list (append (if *muokattavat* (list nil))
                                 (list "Keskiarvo"
                                       (keskiarvo arvot kertoimet 2))))
                   (if (muoto :org nil) (list :viiva))))

          (when (not (muoto nil))
            (viesti "~%")
            (tulosta-taulu
             (list (list (otsikko "As") "= arvosana"
                         (otsikko "K") "= painokerroin")))))

        :if (and lisää (< n enintään)) :do (viesti "~%~%")
        :finally (when (> n enintään)
                   (viesti "~%Enimmäismäärä ~A kpl tuli täyteen.~%" enintään)))

  (tulosta-muokattavat "arvosana" "lisätiedot"))


(defmethod tulosta ((koonti arvosanat-koonti))
  (setf *muokattavat* nil)
  (let* ((kertoimet)
         (lyhenteet)
         (taulu (make-array (list (+ 2 (length (suorituslista koonti)))
                                  (length (taulukko koonti)))
                            :initial-element nil))
         (ka (make-array (array-dimension taulu 0) :initial-element nil)))

    (loop :for s :in (suorituslista koonti)
          :collect (lyhenne s) :into lyh
          :collect (painokerroin s) :into ker
          :finally (setf lyhenteet lyh kertoimet ker))

    (loop :for (suku etu . loput) :in (taulukko koonti)
          :for y :upfrom 0
          :do (loop :initially (setf (aref taulu 0 y)
                                     (format nil "~A, ~A" suku etu))
                    :for as :in loput
                    :for x :upfrom 1
                    :collect as :into luvut
                    :do (setf (aref taulu x y) as)
                    :finally (setf (aref taulu (1+ x) y)
                                   (keskiarvo luvut kertoimet 2))))

    (loop :for x :from 1 :below (array-dimension taulu 0)
          :do (loop :for y :from 0 :below (array-dimension taulu 1)
                    :collect (aref taulu x y) :into luvut
                    :finally (setf (aref ka x) (keskiarvo luvut))))

    (tulosta-taulu
     (append (if (muoto :org nil) (list :viiva))
             (list (append (list (otsikko "Ryhmä:")) (list (ryhmä koonti))))
             (if (muoto :org nil) (list :viiva))))
    (viesti "~%")

    (tulosta-taulu
     (append (if (muoto :org nil) (list :viiva))
             (list (append (list (otsikko "Suoritus"))
                           (mapcar #'otsikko lyhenteet)
                           (list (otsikko "ka"))))
             (list (append (list (otsikko "Painokerroin"))
                           (mapcar #'otsikko kertoimet)
                           (list (otsikko ""))))
             (if (muoto :org nil) (list :viiva))
             (loop :for y :from 0 :below (array-dimension taulu 1)
                   :collect
                   (loop :for x :from 0 :below (array-dimension taulu 0)
                         :collect (aref taulu x y)))
             (if (muoto :wilma) (list nil) (list :viiva))
             (list (cons "Keskiarvo" (rest (coerce ka 'list))))
             (if (muoto :org nil) (list :viiva))))

    (unless *suppea*
      (viesti "~%")
      (tulosta-taulu
       (append (if (muoto :org nil) (list :viiva))
               (list (list (otsikko "Lyh") (otsikko "Suoritus")))
               (if (muoto :org nil) (list :viiva))
               (loop :for suoritus :in (suorituslista koonti)
                     :collect (list (lyhenne suoritus) (nimi suoritus)))
               '(("ka" "Keskiarvo"))
               (if (muoto :org nil) (list :viiva)))))))


(defmethod tulosta ((lista suoritusryhmät))
  (setf *muokattavat* (when (and *vuorovaikutteinen*
                                 (not *tulostusmuoto*)
                                 (not *suppea*))
                        (coerce (ryhmälista lista) 'vector)))

  (let* ((ryhmät (map 'vector #'ryhmä (ryhmälista lista)))
         (sarakkeita (min 7 (1+ (truncate (1- (length ryhmät)) 10))))
         (rivejä (multiple-value-bind (koko jäännös)
                     (truncate (length ryhmät) sarakkeita)
                   (if (plusp jäännös) (1+ koko) koko)))
         (taulukko (make-array (list sarakkeita rivejä) :initial-element "")))

    (loop :with i := 0
          :for x :from 0 :below sarakkeita
          :for numeron-leveys := (olion-mj-pituus
                                  (min (* (1+ x) rivejä) (length ryhmät)))
          :do (loop :for y :from 0 :below rivejä
                    :while (< i (length ryhmät))
                    :do
                    (setf (aref taulukko x y)
                          (if *muokattavat*
                              (format nil "~V@A. ~A" numeron-leveys (1+ i)
                                      (aref ryhmät i))
                              (aref ryhmät i)))
                    (incf i)))

    (tulosta-taulu
     (append (if (muoto :org nil) (list :viiva))
             (loop :for y :from 0 :below rivejä
                   :collect (loop :for x :from 0 :below sarakkeita
                                  :collect (aref taulukko x y)))
             (if (muoto :org nil) (list :viiva))))
    (tulosta-muokattavat "ryhmä")))


(defmethod tulosta ((object t))
  (setf *muokattavat* nil)
  (format *error-output* "~&Ei löytynyt.~%"))


(defun query-last-insert-rowid ()
  (sqlite:last-insert-rowid *tietokanta*))


(defgeneric lisää (asia &key &allow-other-keys))


(defmethod lisää ((opp oppilas) &key)
  (query "insert into oppilaat (sukunimi,etunimi,ryhmat,lisatiedot) ~
        values (~A,~A,~A,~A)"
         (sql-mj (sukunimi opp)) (sql-mj (etunimi opp))
         (sql-mj (lista-mj-listaksi (ryhmälista opp)))
         (sql-mj (lisätiedot opp)))
  (let ((oid (query-last-insert-rowid)))
    (setf (oid opp) oid)
    opp))


(defmethod lisää ((suo suoritus) &key sija)
  (query "insert into suoritukset (nimi,lyhenne,painokerroin) ~
        values (~A,~A,~A)"
         (sql-mj (nimi suo)) (sql-mj (lyhenne suo))
         (or (painokerroin suo) "NULL"))

  (let ((sid (query-last-insert-rowid)))
    (when sid
      (ignore-errors
        (query "create table suoritus_~A (oid integer, arvosana text, ~
                lisatiedot text)" sid))
      (setf (sid suo) sid)

      (let ((ryhmän-suoritukset
             (first (first (query "select suoritukset from ryhmat where ~
                                ryhma like ~A"
                                  (sql-like-suoja (ryhmä suo)))))))

        (if ryhmän-suoritukset
            (progn
              (setf ryhmän-suoritukset
                    (lisää-mj-listaan (princ-to-string sid)
                                      ryhmän-suoritukset sija))
              (query "update ryhmat set suoritukset=~A where ryhma like ~A"
                     (sql-mj ryhmän-suoritukset) (sql-like-suoja (ryhmä suo))))
            (query "insert into ryhmat (ryhma,suoritukset) values (~A,~A)"
                   (sql-mj (ryhmä suo)) (sql-mj sid))))))
  suo)


(defgeneric muokkaa (asia &key &allow-other-keys))


(defmethod muokkaa ((opp oppilas) &key)
  (query "update oppilaat set sukunimi=~A,etunimi=~A,ryhmat=~A,lisatiedot=~A ~
        where oid=~A"
         (sql-mj (sukunimi opp)) (sql-mj (etunimi opp))
         (sql-mj (lista-mj-listaksi (ryhmälista opp)))
         (sql-mj (lisätiedot opp)) (oid opp))
  opp)


(defmethod muokkaa ((suo suoritus) &key sija)
  (query "update suoritukset set nimi=~A,lyhenne=~A,painokerroin=~A ~
                where sid=~A"
         (sql-mj (nimi suo)) (sql-mj (lyhenne suo))
         (if (painokerroin suo) (painokerroin suo) "NULL")
         (sid suo))

  (when sija
    (let ((sid-mj (princ-to-string (sid suo)))
          (ryhmän-suoritukset
           (first (first (query "select suoritukset from ryhmat ~
                                where ryhma like ~A"
                                (sql-like-suoja (ryhmä suo)))))))

      (if ryhmän-suoritukset
          (progn
            (setf ryhmän-suoritukset
                  (siirrä-mj-listassa sid-mj ryhmän-suoritukset sija))
            (query "update ryhmat set suoritukset=~A where ryhma like ~A"
                   (sql-mj ryhmän-suoritukset) (sql-like-suoja (ryhmä suo))))
          (query "insert into ryhmat (ryhma,suoritukset) values (~A,~A)"
                 (sql-mj (ryhmä suo)) (sql-mj sid-mj))))))


(defmethod muokkaa ((ryh ryhmä) &key)
  (query "update ryhmat set ryhma=~A where ryhma=~A"
         (sql-mj (ryhmä ryh))
         (sql-mj (vanha-ryhmä ryh))))


(defmethod muokkaa ((arv arvosana) &key)
  (let ((oid (oid (oppilas arv)))
        (lisätiedot (let ((lisä (lisätiedot arv)))
                      (if (or (not lisä) (equal lisä ""))
                          "NULL"
                          (sql-mj lisä)))))
    (if (query "select oid from suoritus_~A where oid=~A" (sid arv) oid)
        (query "update suoritus_~A set arvosana=~A,lisatiedot=~A ~
                where oid=~A"
               (sid arv) (sql-mj (arvosana arv)) lisätiedot oid)
        (query "insert into suoritus_~A (oid,arvosana,lisatiedot) ~
                values (~A,~A,~A)"
               (sid arv) oid (sql-mj (arvosana arv))
               lisätiedot))
    arv))


(defgeneric poista (asia))


(defmethod poista ((opp oppilas))
  (let ((taulut
         (mapcar #'first
                 (query "select name from sqlite_master where type='table' ~
                        and name like 'suoritus\\_%' escape '\\'"))))
    (loop :for taulu :in taulut
          :do (ignore-errors
                (query "delete from ~A where oid=~A" taulu (oid opp))))
    (ignore-errors
      (query "delete from oppilaat where oid=~A" (oid opp)))))


(defmethod poista ((suo suoritus))
  (let ((ryhmän-suoritukset
         (first (first (query "select suoritukset from ryhmat ~
                                where ryhma like ~A"
                              (sql-like-suoja (ryhmä suo)))))))

    (when ryhmän-suoritukset
      (setf ryhmän-suoritukset
            (poista-mj-listasta (princ-to-string (sid suo))
                                ryhmän-suoritukset))
      (if (string= "" ryhmän-suoritukset)
          (query "delete from ryhmat where ryhma like ~A"
                 (sql-like-suoja (ryhmä suo)))
          (query "update ryhmat set suoritukset=~A where ryhma like ~A"
                 (sql-mj ryhmän-suoritukset)
                 (sql-like-suoja (ryhmä suo))))))

  (query "delete from suoritukset where sid=~A" (sid suo))
  (ignore-errors (query "drop table suoritus_~A" (sid suo))))


(defmethod poista ((arv arvosana))
  (let ((oid (oid (oppilas arv)))
        (sid (sid arv)))
    (ignore-errors
      (query "delete from suoritus_~A where oid=~A" sid oid))))


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
    (virhe "Anna ryhmän tunnus Ohjeita saa ?:llä."))
  (tulosta (hae-suoritukset (erota-ensimmäinen-sana arg))))


(defun komento-hae-suoritusryhmät (&optional arg)
  (declare (ignore arg))
  (tulosta (hae-suoritusryhmät)))


(defun komento-hae-arvosanat-suoritukset (arg)
  ;; ryhmä /suoritus/lyhenne
  (multiple-value-bind (ryhmä loput)
      (erota-ensimmäinen-sana arg)
    (when (zerop (length ryhmä))
      (virhe "Anna ryhmän tunnus. Ohjeita saa ?:llä."))
    (when (zerop (length loput))
      (setf loput "/"))
    (let ((jaettu (pilko-erottimella loput)))
      (tulosta (hae-arvosanat-suoritukset ryhmä (nth 0 jaettu)
                                          (nth 1 jaettu))))))


(defun komento-hae-arvosanat-oppilaat (arg)
  ;; /sukunimi/etunimi/ryhmät/lisätiedot
  (when (zerop (length arg))
    (setf arg "/"))
  (let ((jaettu (pilko-erottimella arg)))
    (tulosta (hae-arvosanat-oppilaat (nth 0 jaettu)
                                     (nth 1 jaettu)
                                     (nth 2 jaettu)
                                     (nth 3 jaettu)))))


(defun komento-hae-arvosanat-koonti (arg)
  ;; ryhmä
  (when(zerop (length arg))
    (virhe "Anna ryhmän tunnus. Ohjeita saa ?:llä."))
  (tulosta (hae-arvosanat-koonti (erota-ensimmäinen-sana arg))))


(defun komento-lisää-oppilas (arg)
  ;; /sukunimi/etunimi/ryhmät/lisätiedot
  (when (zerop (length arg))
    (virhe "Anna lisättävän oppilaan tiedot. Ohjeita saa ?:llä."))
  (let ((jaettu (pilko-erottimella arg)))
    (when (or (zerop (length (normalisoi-mj (nth 0 jaettu))))
              (zerop (length (normalisoi-mj (nth 1 jaettu)))))
      (virhe "Pitää antaa vähintään sukunimi ja etunimi. Ohjeita saa ?:llä."))
    (with-transaction
      (lisää (make-instance 'oppilas
                            :sukunimi (normalisoi-mj (nth 0 jaettu))
                            :etunimi (normalisoi-mj (nth 1 jaettu))
                            :ryhmälista (normalisoi-ryhmät (nth 2 jaettu))
                            :lisätiedot (normalisoi-mj (nth 3 jaettu))))
      (muokkauslaskuri 1))
    (ehkä-eheytys)))


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
                (setf sija (1- num))
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
        (muokkauslaskuri 1))
      (ehkä-eheytys))))


(defun jäsennä-numeroluettelo (mj)
  ;; Luettolo on muotoa 1,2,3,5-7,9-12
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
                       (virhe "Ryhmä poistetaan siten, että poistetaan siltä ~
                                kaikki suoritukset."))
                      ((and kohde (not (typep kohde 'arvosana)))
                       (poista kohde)
                       (setf (elt *muokattavat* (1- i)) nil))
                      ((and kohde (typep kohde 'arvosana))
                       (poista kohde))
                      (t (viesti "~&Tietue ~A on jo poistettu.~%" i))))
      (muokkauslaskuri (length numeroluettelo)))
    (ehkä-eheytys)))


(defun on-sisältöä-p (mj)
  (notevery (lambda (el)
              (and (characterp el)
                   (or (not (graphic-char-p el))
                       (char= #\Space el))))
            mj))


(defun komento-muokkaa-oppilas (kentät kohde)
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
             (setf uusi-ryhmä nil))))

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
           (setf uusi-sija (1- num)))
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
    (muokkaa kohde)))


(defun komento-muokkaa-ryhmä (uusi-ryhmä ryhmäolio)
  (setf uusi-ryhmä (first uusi-ryhmä))
  (when (query "select * from ryhmat where ryhma like ~A"
               (sql-like-suoja uusi-ryhmä))
    (virhe "Ryhmä nimeltä ~A on jo olemassa." uusi-ryhmä))
  (setf (vanha-ryhmä ryhmäolio) (ryhmä ryhmäolio)
        (ryhmä ryhmäolio) uusi-ryhmä)
  (muokkaa ryhmäolio))


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
          (muokkauslaskuri (min (length numeroluettelo) (length arvot)))))
      (ehkä-eheytys))))


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
                   (komento-muokkaa-ryhmä
                    (list (erota-ensimmäinen-sana loput)) kohde))

                  (t (virhe "Tietue ~A on poistettu." i))))

      (muokkauslaskuri (length numeroluettelo)))
    (ehkä-eheytys)))


(defun ohjeet (&optional komento)
  (tulosta-taulu
   '(:viiva
     ("Komento" "Lisätiedot" "Tarkoitus")
     :viiva
     ("ho" "/sukunimi/etunimi/ryhmät/lisätiedot" "Hae oppilaita.")
     ("hoa" "/sukunimi/etunimi/ryhmät/lisätiedot"
      "Hae oppilaita arvotussa järjestyksessä.")
     ("hs" "ryhmä" "Hae suoritukset ryhmältä.")
     ("hr" "" "Hae ryhmät, joilla on suorituksia.")
     ("hao" "/sukunimi/etunimi/ryhmät/lisätiedot" "Hae arvosanat oppilaalta.")
     ("has" "ryhmä /suoritus/lyhenne" "Hae arvosanat suorituksesta.")
     ("hak" "ryhmä" "Hae arvosanojen koonti.")
     :viiva
     ("lo" "/sukunimi/etunimi/ryhmät/lisätiedot" "Lisää oppilas.")
     ("ls" "ryhmä /suoritus/lyhenne/painokerroin/sija"
      "Lisää ryhmälle suoritus.")
     :viiva
     ("m" "numerot /.../.../.../..." "Muokkaa valittuja tietueita ja kenttiä.")
     ("ms" "numerot kenttä /.../.../..." "Muokkaa samaa kenttää sarjana.")
     ("poista" "numerot" "Poista valitut tietueet.")
     :viiva
     ("?" "" "Ohjeet.")
     ("??" "" "Tarkemmat ohjeet.")
     ("???" "" "Aloitusvinkkejä.")
     :viiva))

  (cond ((equal komento "?")
         (return-from ohjeet))
        ((equal komento "??")
         (viesti "~%~

Lisätiedot-sarakkeessa vinoviiva (/) tarkoittaa kenttien
erotinmerkkiä (/.../...). Ensimmäisenä oleva merkki määrittää komennossa
käytettävän erotinmerkin, joka voi olla mikä tahansa. Esimerkiksi alla
olevat komennot toimivat samalla tavalla. Niillä haetaan oppilaita (ho)
sukunimen ja etunimen perusteella. Kentät on erotettu toisistaan eri
erotinmerkeillä.

    ho /Meikäl/Mat
    ho ,Meikäl,Mat
    ho 3Meikäl3Mat

Komentojen \"m\", \"ms\" ja \"poista\" kohdalla lisätieto \"numerot\"
tarkoittaa kokonaislukujen luetteloa, esimerkiksi \"1,4\" tai \"2-5,9\".
Niiden avulla valitaan, mitä tietueita muokataan tai mitkä poistetaan.

Hakutoiminnot (h:lla alkavat komennot) tulostavat tietokannassa olevia
tietoja. Hakutoiminnon tulosteessa voi olla numeroituja tietueita.
Lisäksi tulosteen lopussa on esimerkiksi seuraavanlainen rivi:

    Muokattavat tietueet: 1-22. Kentät: /arvosana/lisätiedot

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
lisätiedoksi luettelo muokattavista tietueista. Toiseksi lisätiedoksi
annetaan muokattavan kentän numero: ensimmäinen kenttä vasemmalta on 1,
toinen vasemmalta on 2 jne. Kolmantena lisätietona luetellaan
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
          '(:viiva
            ("Sana" "Selitys")
            :viiva
            ("wilma" "Wilma-viestiin sopiva taulukkomalli.")
            ("org" "Emacsin Org-tilaan sopiva taulukkomalli.")
            ("suppea" "Karsitaan tulostuksesta Lisätiedot-kentät yms.")
            :viiva))

         (viesti "~%~

Esimerkiksi

    wilma suppea hao /Meikäl/Mat/2013:7a
    org hak 2013:7a

"))

        ((equal komento "???")
         (viesti "~%~

Tietokantaohjelman käyttö kannattaa aloittaa lisäämällä oppilaita.
Lisäystoiminnossa (lo) syötettävät kentät ovat vasemmalta oikealle
sukunimi, etunimi, ryhmät ja lisätiedot. Ainakin sukunimi ja etunimi
täytyy syöttää. Kentät erotetaan toisistaan jollakin erotinmerkillä.
Tässä esimerkissä käytetään vivoviivaa (/):

    lo /Meikäläinen/Matti/2013:7a
    lo /Oppilas/Oona/2013:7a
    lo /Koululainen/Kalle/2013:7a/lukivaikeus
    ...

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
    ...

Tiedot ryhmän suorituksista tulostetaan komennolla \"hs 2013:7a\".
Arvosanoja voi muokata siten, että ensin haetaan arvosanoista tiedot
esimerkiksi seuraavilla komennoilla:

    hao /Meikäl/Mai
    has 2013:7a /Sanaluokk

Sitten käytetään m-komentoa muokkaamiseen. Arvosanatiedoissa on
muokattavana kaksi kenttää: arvosana ja lisätiedot. Alla on esimerkkejä
muokkauskomennoista:

    m 1 /8½
    m 3 /6+
    m 4 /8-/Kirjoitelma palautettu myöhässä.
    m 6,14-15 /7½
    m 16 //Koe toistaiseksi tekemättä.

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
            ((testaa "suppea") (let ((*suppea* t))
                                 (käsittele-komentorivi arg)))
            ((testaa "ho") (komento-hae-oppilaat arg))
            ((testaa "hoa") (komento-hae-oppilaat-arvottu arg))
            ((testaa "hs") (komento-hae-suoritukset arg))
            ((testaa "hr") (komento-hae-suoritusryhmät arg))
            ((testaa "has") (komento-hae-arvosanat-suoritukset arg))
            ((testaa "hao") (komento-hae-arvosanat-oppilaat arg))
            ((testaa "hak") (komento-hae-arvosanat-koonti arg))
            ((testaa "lo") (komento-lisää-oppilas arg))
            ((testaa "ls") (komento-lisää-suoritus arg))
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
    (tietokanta-käytössä
      (if (rest argv)
          (let ((*vuorovaikutteinen* nil))
            (käsittele-komentorivi (format nil "~{~A~^ ~}" (rest argv))))
          (let ((*vuorovaikutteinen* t)
                (kehote (format nil "~:@(~A~)> "
                                (pathname-name (or (first argv) "koas")))))
            (handler-case
                (loop (käsittele-komentorivi (lue-rivi kehote t)))
              (poistu-ohjelmasta () nil)
              (sb-sys:interactive-interrupt ()
                (viesti "~%"))))))))


#+script
(let ((*readline* t)) (main (script:argv)))
