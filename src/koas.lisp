;;;; Koas - tietokantaohjelma kouluarvosanoille


;;; Copyright (C) 2013-2020 Teemu Likonen <tlikonen@iki.fi>
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
  (:use #:cl #:yhteinen #:tietokanta)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:main #:start))

(in-package #:koas)


(defvar *readline* nil)
(defvar *muokattavat* nil)
(defvar *vuorovaikutteinen* nil)
(defvar *tulostusmuoto* nil)
(defvar *suppea* nil)


(defun moni-sort (sarja &rest key-testit)
  (sort sarja (lambda (a b)
                (loop :for (key . test) :in key-testit
                      :unless (equalp (funcall key a) (funcall key b))
                      :return (funcall test (funcall key a) (funcall key b))))))


(defun poista-tyhjät-ryhmät ()
  (query "DELETE FROM ryhmat WHERE rid IN ~
        (SELECT r.rid FROM ryhmat AS r ~
        LEFT JOIN oppilaat_ryhmat AS j ON r.rid = j.rid ~
        LEFT JOIN suoritukset AS s ON s.rid = r.rid ~
        WHERE j.rid IS NULL AND s.rid IS NULL)"))


(defun arvottu-järjestys (lista)
  (loop :with pituus := (length lista)
        :with vektori := (make-array pituus :initial-contents lista)
        :with rs := (make-random-state t)
        :for i :from 0 :below pituus
        :do (rotatef (aref vektori i)
                     (aref vektori (+ i (random (- pituus i) rs))))
        :finally (return (coerce vektori 'list))))


(defun lue-rivi (kehote &optional muistiin)
  (if *readline*
      (cl-readline:readline :prompt kehote :add-history muistiin)
      (progn
        (format *query-io* "~A" kehote)
        (force-output *query-io*)
        (read-line *query-io*))))


(defun normalisoi-mj (mj)
  ;; Poistetaan välilyönnit alusta ja lopusta sekä yhtä useammat
  ;; välilyönnit keskeltä.
  (format nil "~{~A~^ ~}" (split-sequence #\space mj :remove-empty-subseqs t)))


(defun normalisoi-ryhmät (asia)
  (assert (or (stringp asia) (listp asia)))
  (when (stringp asia)
    (setf asia (mj-lista-listaksi asia)))
  (setf asia (mapcar #'normalisoi-mj asia))
  (setf asia (delete-duplicates asia :test #'equal))
  (sort asia #'string-lessp))


(defun normalisoi-painokerroin (asia)
  (typecase asia
    (integer asia)
    (string
     (let ((luku (lue-numero asia)))
       (if (integerp luku) luku)))))


(defun otsikko-sarake (mj)
  (setf mj (or mj ""))
  (case *tulostusmuoto*
    (:org (if (equal "" mj) "" (format nil "*~A*" mj)))
    (t mj)))


(defun otsikko-rivi (mj)
  (setf mj (or mj ""))
  (case *tulostusmuoto*
    (:org (if (equal "" mj) "" (format nil "*~A*" mj)))
    (t mj)))


(defun muoto (&rest tyypit)
  (member *tulostusmuoto* tyypit :test #'eql))


(defun luvun-leveys (luku)
  (check-type luku (integer 0 *))
  (loop :for l := (truncate luku 10) :then (truncate l 10)
        :for n :upfrom 1
        :while (plusp l)
        :finally (return n)))


(defun numeroi (taulu)
  (let ((suurin-leveys (luvun-leveys (length taulu))))
    (loop :with i := 0
       :for rivi :in taulu
       :collect
         (if (find :jatko rivi)
             (cons :jatko rivi)
             (cons (format nil "~V@A" suurin-leveys (incf i)) rivi)))))


(defun tulosta-taulu (taulu &key (virta *standard-output*))
  (when taulu
    (flet ((viivap (ob)
             (find ob '(:viiva :viiva-alku :viiva-loppu :viiva-otsikko)))
           (jatkop (solu)
             (eql :jatko solu)))
      (let* ((sarakkeiden-lkm (reduce #'max taulu :key (lambda (osa)
                                                         (if (viivap osa)
                                                             0
                                                             (length osa)))))
             (leveimmät-sarakkeet (make-list sarakkeiden-lkm
                                             :initial-element 0)))

        (loop :with uusi-taulu := nil
           :for rivi :in taulu
           :do (cond
                 ((viivap rivi)
                  (push rivi uusi-taulu))
                 (t
                  (let ((lisättävät (- sarakkeiden-lkm (length rivi)))
                        (rivi-mj (mapcar (lambda (solu)
                                           (cond ((null solu) "")
                                                 ((jatkop solu) "")
                                                 ((stringp solu) solu)
                                                 (t (princ-to-string solu))))
                                         rivi)))

                    (when (plusp lisättävät)
                      (setf rivi-mj (nconc rivi-mj
                                           (make-list lisättävät
                                                      :initial-element ""))))
                    (setf leveimmät-sarakkeet
                          (mapcar #'max (mapcar #'length rivi-mj)
                                  leveimmät-sarakkeet))

                    (push rivi-mj uusi-taulu))))

           :finally (setf taulu (nreverse uusi-taulu)))

        (format virta "~&")
        (loop :for rivi :in taulu
              :for rivi-uusi := (if (viivap rivi)
                                    (make-list sarakkeiden-lkm
                                               :initial-element :viiva)
                                    rivi)

              :if (or (muoto nil :org)
                      (and (or (muoto :latex)
                               (muoto :tab)
                               (muoto :csv))
                           (not (viivap rivi))))
              :do
              (cond ((muoto :latex)
                     (format virta "\\rivi"))
                    ((and (viivap rivi) (muoto nil))
                     (format virta "+"))
                    ((muoto nil :org)
                     (format virta "|")))

              (loop :for (solu . loput) :on rivi-uusi
                    :for leveys :in leveimmät-sarakkeet
                    :do
                    (cond
                      ((and (muoto :org nil) (viivap solu))
                       (format virta "--~V,,,'-<~>~:[|~;+~]" leveys
                               (or loput (and (not loput) (muoto nil)))))
                      ((and (muoto :tab) (not (viivap solu)))
                       (format virta "~A~A" solu (if loput #\Tab "")))
                      ((and (muoto :latex) (not (viivap solu)))
                       (loop :initially (princ #\{ virta)
                             :for m :across (string-trim " " solu)
                             :if (find m "%&{}") :do (princ #\\ virta)
                             :do (princ m virta)
                             :finally (princ #\} virta)))
                      ((and (muoto :csv) (not (viivap solu)))
                       (let ((solu (string-trim " " solu)))
                         (if (every #'digit-char-p solu)
                             (princ solu virta)
                             (loop :initially (princ #\" virta)
                                   :for m :across solu
                                   :if (eql m #\") :do (princ #\\ virta)
                                   :do (princ m virta)
                                   :finally (princ #\" virta)))
                         (if loput (princ #\, virta))))
                      (t (format virta " ~VA |" leveys solu))))
              (format virta "~%"))))))


(defclass oppilas ()
  ((oid :accessor oid :initarg :oid)
   (sukunimi :accessor sukunimi :initarg :sukunimi)
   (etunimi :accessor etunimi :initarg :etunimi)
   (ryhmälista :accessor ryhmälista :initarg :ryhmälista)
   (oppilas-lisätiedot :accessor oppilas-lisätiedot
                       :initarg :oppilas-lisätiedot)))

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
   (ryhmä-lisätiedot :reader ryhmä-lisätiedot :initarg :ryhmä-lisätiedot)
   (suorituslista :reader suorituslista :initarg :suorituslista)))

(defclass ryhmä ()
  ((rid :reader rid :initarg :rid)
   (nimi :accessor nimi :initarg :nimi)
   (ryhmä-lisätiedot :accessor ryhmä-lisätiedot :initarg :ryhmä-lisätiedot
                     :initform nil)))

(defclass ryhmät ()
  ((ryhmälista :reader ryhmälista :initarg :ryhmälista)))

(defclass arvosana ()
  ((oid :reader oid :initarg :oid)
   (sukunimi :reader sukunimi :initarg :sukunimi)
   (etunimi :reader etunimi :initarg :etunimi)
   (sid :reader sid :initarg :sid)
   (nimi :reader nimi :initarg :nimi); suorituksen nimi
   (lyhenne :reader lyhenne :initarg :lyhenne)
   (painokerroin :reader painokerroin :initarg :painokerroin :initform nil)
   (arvosana :accessor arvosana :initarg :arvosana :initform "")
   (arvosana-lisätiedot :accessor arvosana-lisätiedot
                        :initarg :arvosana-lisätiedot :initform nil)))

(defclass arvosanat-suorituksesta ()
  ((nimi :reader nimi :initarg :nimi); suorituksen nimi
   (ryhmä :reader ryhmä :initarg :ryhmä)
   (ryhmä-lisätiedot :reader ryhmä-lisätiedot :initarg :ryhmä-lisätiedot)
   (arvosanalista :reader arvosanalista :initarg :arvosanalista)))

(defclass arvosanat-suorituksista ()
  ((lista :reader lista :initarg :lista)))

(defclass arvosanat-oppilaalta ()
  ((oid :reader oid :initarg :oid)
   (sukunimi :reader sukunimi :initarg :sukunimi)
   (etunimi :reader etunimi :initarg :etunimi)
   (oppilas-lisätiedot :reader oppilas-lisätiedot :initarg :oppilas-lisätiedot)
   (rid :reader rid :initarg :rid)
   (ryhmä :reader ryhmä :initarg :ryhmä)
   (ryhmä-lisätiedot :reader ryhmä-lisätiedot :initarg :ryhmä-lisätiedot)
   (arvosanalista :reader arvosanalista :initarg :arvosanalista)))

(defclass arvosanat-oppilailta ()
  ((lista :reader lista :initarg :lista)))

(defclass arvosanat-koonti ()
  ((ryhmä :reader ryhmä :initarg :ryhmä)
   (ryhmä-lisätiedot :reader ryhmä-lisätiedot :initarg :ryhmä-lisätiedot)
   (oppilaslista :reader oppilaslista :initarg :oppilaslista)
   (suorituslista :reader suorituslista :initarg :suorituslista)
   (taulukko :reader taulukko :initarg :taulukko)))

(defclass tilasto-jakauma ()
  ((hajautustaulu :reader hajautustaulu :initarg :hajautustaulu)))

(defclass tilasto-koonti ()
  ((oppilaita :reader oppilaita :initarg :oppilaita)
   (ryhmiä :reader ryhmiä :initarg :ryhmiä)
   (suorituksia :reader suorituksia :initarg :suorituksia)
   (arvosanoja :reader arvosanoja :initarg :arvosanoja)))

(defclass tilasto-paremmuus ()
  ((lista :reader lista :initarg :lista)
   (kokonaiskeskiarvo :reader kokonaiskeskiarvo :initarg :kokonaiskeskiarvo)))


(defun pyöristä (luku &optional (tarkkuus 1))
  (* tarkkuus (decimals:round-half-away-from-zero luku tarkkuus)))


(defun muuta-arvosanaksi (luku)
  (let ((neliporras (pyöristä (abs luku) 1/4))
        (merkki (if (minusp luku) "-" "")))
    (multiple-value-bind (koko murto)
        (truncate neliporras)
      (let ((lisä (case murto
                    (1/4 "+")
                    (1/2 "½")
                    (3/4 (incf koko) "-")
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


(defun ryhmä-mj (ryhmä lisätiedot)
  (if (and (stringp lisätiedot)
           (plusp (length lisätiedot)))
      (concatenate 'string ryhmä " (" lisätiedot ")")
      ryhmä))


(defun oppilas-mj (sukunimi etunimi)
  (concatenate 'string sukunimi ", " etunimi))


(defun hae-oppilaat (sukunimi &optional (etunimi "") (ryhmä "")
                                (lisätiedot ""))
  (let ((kysely ;Ei karsita vielä ryhmän perusteella.
         (query "SELECT oid, sukunimi, etunimi, ryhma, olt ~
                FROM view_oppilaat ~
                WHERE sukunimi LIKE ~A ~
                AND etunimi LIKE ~A ~
                AND olt LIKE ~A ~
                ORDER BY sukunimi, etunimi, oid, ryhma DESC"
                (sql-like-suoja sukunimi t)
                (sql-like-suoja etunimi t)
                (sql-like-suoja lisätiedot t))))

    (loop :with oppilaat := nil
          :with ryhmät := nil
          :for (rivi . loput) :on kysely
          :for (oid sukunimi etunimi r-nimi lisätiedot) := rivi
          :for seuraava-oid := (caar loput)
          :do

          (push (or r-nimi "") ryhmät)
          (unless (eql oid seuraava-oid)
            (when (some (lambda (r) ;karsinta ryhmän perustella
                          (search ryhmä r :test #'equal))
                        ryhmät)
              (push (make-instance 'oppilas
                                   :oid oid
                                   :sukunimi sukunimi
                                   :etunimi etunimi
                                   :ryhmälista
                                   (sort (delete "" ryhmät :test #'equal)
                                         #'string-lessp)
                                   :oppilas-lisätiedot lisätiedot)
                    oppilaat))
            (setf ryhmät nil))

          :finally
          (return (when oppilaat
                    (make-instance 'oppilaat
                                   :oppilaslista
                                   (moni-sort (nreverse oppilaat)
                                              (cons #'sukunimi #'string-lessp)
                                              (cons #'etunimi #'string-lessp)
                                              (cons #'oid #'<))))))))


(defun hae-suoritukset (ryhmä)
  (let ((suoritukset
         (query "SELECT rid, ryhma, rlt, ~
                sid, suoritus, lyhenne, painokerroin ~
                FROM view_suoritukset ~
                WHERE ryhma LIKE ~A ~
                ORDER BY sija, sid"
                (sql-like-suoja ryhmä))))

    (when suoritukset
      (make-instance
       'suoritukset
       :ryhmä (nth 1 (first suoritukset))
       :ryhmä-lisätiedot (nth 2 (first suoritukset))
       :suorituslista
       (loop :for (rid r-nimi r-lisätiedot sid s-nimi lyhenne painokerroin)
             :in suoritukset
             :collect (make-instance 'suoritus
                                     :ryhmä r-nimi
                                     :rid rid
                                     :sid sid
                                     :nimi s-nimi
                                     :lyhenne lyhenne
                                     :painokerroin painokerroin))))))


(defun hae-ryhmät (&optional (ryhmä "") (lisätiedot ""))
  (let ((ryhmät (query "SELECT rid, nimi, lisatiedot FROM ryhmat ~
                WHERE nimi LIKE ~A AND lisatiedot LIKE ~A"
                       (sql-like-suoja ryhmä t)
                       (sql-like-suoja lisätiedot t))))
    (when ryhmät
      (setf ryhmät (moni-sort ryhmät
                              (cons (lambda (x) (nth 1 x)) #'string-lessp)
                              (cons (lambda (x) (nth 2 x)) #'string-lessp)
                              (cons (lambda (x) (nth 0 x)) #'<)))

      (make-instance
       'ryhmät
       :ryhmälista (loop :for (rid nimi lisätiedot) :in ryhmät
                         :collect
                         (make-instance 'ryhmä
                                        :rid rid
                                        :nimi nimi
                                        :ryhmä-lisätiedot lisätiedot))))))


(defun hae-arvosanat-suorituksista (&optional (ryhmä "") (nimi "") (lyhenne ""))
  (let ((kysely
         (query "SELECT ryhma, rid, rlt, ~
                sija, sid, suoritus, lyhenne, painokerroin,~
                oid, sukunimi, etunimi, arvosana, alt ~
                FROM view_arvosanat ~
                WHERE ryhma LIKE ~A ~
                AND suoritus LIKE ~A ~
                AND lyhenne LIKE ~A ~
                AND oid IS NOT NULL ~
                ORDER BY ryhma, rid, sija, sid, sukunimi, etunimi, oid"
                (sql-like-suoja ryhmä t)
                (sql-like-suoja nimi t)
                (sql-like-suoja lyhenne t))))

    (when kysely
      (setf kysely (moni-sort kysely
                              (cons (lambda (x) (nth 0 x)) #'string-lessp)
                              (cons (lambda (x) (nth 2 x)) #'<)
                              (cons (lambda (x) (nth 1 x)) #'<)
                              (cons (lambda (x) (nth 3 x)) #'<)
                              (cons (lambda (x) (nth 4 x)) #'<)
                              (cons (lambda (x) (nth 9 x)) #'string-lessp)
                              (cons (lambda (x) (nth 10 x)) #'string-lessp)
                              (cons (lambda (x) (nth 8 x)) #'<)))

      (loop :with suoritukset := nil
            :with arvosanat := nil
            :for (rivi . loput) :on kysely
            :for (r-nimi nil r-lisätiedot nil sid s-nimi lyhenne painokerroin
                         oid sukunimi etunimi arvosana a-lisätiedot) := rivi
            :for seuraava-sid := (nth 4 (first loput)) ;s.sid
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
                                 :arvosana-lisätiedot a-lisätiedot)
                  arvosanat)

            (unless (eql sid seuraava-sid)
              (push (make-instance 'arvosanat-suorituksesta
                                   :nimi s-nimi
                                   :ryhmä r-nimi
                                   :ryhmä-lisätiedot r-lisätiedot
                                   :arvosanalista (nreverse arvosanat))
                    suoritukset)
              (setf arvosanat nil))

            :finally
            (return (when suoritukset
                      (make-instance 'arvosanat-suorituksista
                                     :lista (nreverse suoritukset))))))))


(defun hae-arvosanat-oppilailta (sukunimi &optional (etunimi "") (ryhmä "")
                                            (lisätiedot ""))
  (let ((kysely
         (query "SELECT oid, sukunimi, etunimi, olt, ~
                rid, ryhma, rlt, ~
                sid, suoritus, lyhenne, painokerroin, ~
                arvosana, alt ~
                FROM view_arvosanat ~
                WHERE sukunimi LIKE ~A ~
                AND etunimi LIKE ~A ~
                AND ryhma LIKE ~A ~
                AND olt LIKE ~A ~
                AND sid IS NOT NULL ~
                ORDER BY sukunimi, etunimi, oid, ryhma, rid, sija, sid"
                (sql-like-suoja sukunimi t)
                (sql-like-suoja etunimi t)
                (sql-like-suoja ryhmä t)
                (sql-like-suoja lisätiedot t))))

    (loop :with oppilas-ryhmät := nil
          :with arvosanat := nil
          :for (rivi . loput) :on kysely
          :for (oid sukunimi etunimi o-lisätiedot rid r-nimi r-lisätiedot
                    sid s-nimi lyhenne painokerroin arvosana a-lisätiedot)
          := rivi
          :for seuraava-oid := (caar loput)
          :for seuraava-rid := (nth 4 (first loput)) ;r.rid
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
                               :arvosana-lisätiedot a-lisätiedot)
                arvosanat)

          (when (or (not (eql oid seuraava-oid))
                    (not (eql rid seuraava-rid)))
            (push (make-instance 'arvosanat-oppilaalta
                                 :oid oid
                                 :sukunimi sukunimi
                                 :etunimi etunimi
                                 :oppilas-lisätiedot o-lisätiedot
                                 :rid rid
                                 :ryhmä r-nimi
                                 :ryhmä-lisätiedot r-lisätiedot
                                 :arvosanalista (nreverse arvosanat))
                  oppilas-ryhmät)
            (setf arvosanat nil))

          :finally
          (return (when oppilas-ryhmät
                    (make-instance
                     'arvosanat-oppilailta
                     :lista (moni-sort (nreverse oppilas-ryhmät)
                                       (cons #'sukunimi #'string-lessp)
                                       (cons #'etunimi #'string-lessp)
                                       (cons #'oid #'<)
                                       (cons #'ryhmä #'string-lessp)
                                       (cons #'ryhmä-lisätiedot
                                             #'string-lessp))))))))


(defun hae-arvosanat-koonti (ryhmä)
  (let ((suorituslista
         (query "SELECT ryhma, rlt, sid, suoritus, lyhenne, painokerroin ~
                FROM view_suoritukset ~
                WHERE ryhma LIKE ~A ~
                ORDER BY sija, sid"
                (sql-like-suoja ryhmä)))
        (oppilasmäärä (query-1 "SELECT count(oid) ~
                FROM oppilaat_ryhmat AS j ~
                JOIN ryhmat AS r ON j.rid = r.rid ~
                WHERE r.nimi LIKE ~A"
                               (sql-like-suoja ryhmä))))

    (when suorituslista
      (let ((kysely
             (query "SELECT sukunimi, etunimi, oid, sija, arvosana ~
                        FROM view_arvosanat ~
                        WHERE ryhma LIKE ~A ~
                        ORDER BY sukunimi, etunimi, oid, sija"
                    (sql-like-suoja ryhmä))))

        (when kysely
          (setf kysely (moni-sort kysely
                                  (cons (lambda (x) (nth 0 x)) #'string-lessp)
                                  (cons (lambda (x) (nth 1 x)) #'string-lessp)
                                  (cons (lambda (x) (nth 2 x)) #'<)
                                  (cons (lambda (x) (nth 3 x)) #'<)))

          (let ((taulukko (make-array (list oppilasmäärä
                                            (length suorituslista))))
                (oppilaslista nil))

            (loop :with opp := 0
                  :with arv := 0
                  :for (rivi . loput) :on kysely
                  :for (sukunimi etunimi oid nil arvosana) := rivi
                  :for seuraava-oid := (nth 2 (first loput))
                  :do
                  (setf (aref taulukko opp arv) arvosana)
                  (incf arv)

                  (unless (eql oid seuraava-oid)
                    (push (oppilas-mj sukunimi etunimi) oppilaslista)
                    (incf opp)
                    (setf arv 0)))

            (make-instance 'arvosanat-koonti
                           :ryhmä (nth 0 (first suorituslista))
                           :ryhmä-lisätiedot (nth 1 (first suorituslista))
                           :oppilaslista (nreverse oppilaslista)
                           :suorituslista suorituslista
                           :taulukko taulukko)))))))


(defun tilasto-jakauma-1 (hajautustaulu painokerroin
                          &optional (ryhmä "") (suoritus "") (lyhenne "")
                            (sukunimi "") (etunimi "") (lisätiedot ""))
  (let ((kysely
         (query-nconc "SELECT arvosana FROM view_arvosanat ~
                WHERE sukunimi LIKE ~A ~
                AND etunimi LIKE ~A ~
                AND ryhma LIKE ~A ~
                AND olt LIKE ~A ~
                AND suoritus LIKE ~A ~
                AND lyhenne LIKE ~A ~A"
                      (sql-like-suoja sukunimi t)
                      (sql-like-suoja etunimi t)
                      (sql-like-suoja ryhmä t)
                      (sql-like-suoja lisätiedot t)
                      (sql-like-suoja suoritus t)
                      (sql-like-suoja lyhenne t)
                      (if painokerroin
                          "AND painokerroin >= 1"
                          ""))))
    (loop :for mj :in kysely
          :for as := (lue-numero mj)
          :if (numberp as)
          :do (incf (gethash (tulosta-luku as) hajautustaulu 0)))))


(defun tilasto-jakauma (hakulista &optional painokerroin)
  (loop :with taulu := (make-hash-table :test #'equal)
        :for hakutermit :in hakulista
        :do (apply #'tilasto-jakauma-1 taulu painokerroin hakutermit)
        :finally
        (when (loop :for n :being :each :hash-value :in taulu
                    :thereis (plusp n))
          (return (make-instance 'tilasto-jakauma :hajautustaulu taulu)))))


(defun tilasto-paremmuus-1 (hajautustaulu painokerroin
                            &optional (ryhmä "") (suoritus "") (lyhenne "")
                              (sukunimi "") (etunimi "") (lisätiedot ""))
  (let ((kysely
         (query "SELECT oid, sukunimi, etunimi, ryhma, ~
                arvosana, painokerroin ~
                FROM view_arvosanat ~
                WHERE sukunimi LIKE ~A ~
                AND etunimi LIKE ~A ~
                AND ryhma LIKE ~A ~
                AND olt LIKE ~A ~
                AND suoritus LIKE ~A ~
                AND lyhenne LIKE ~A ~A ~
                ORDER BY oid"
                (sql-like-suoja sukunimi t)
                (sql-like-suoja etunimi t)
                (sql-like-suoja ryhmä t)
                (sql-like-suoja lisätiedot t)
                (sql-like-suoja suoritus t)
                (sql-like-suoja lyhenne t)
                (if painokerroin "AND painokerroin >= 1" ""))))

    (loop :for rivi :in kysely
          :for (oid sukunimi etunimi ryhmä arvosana painokerroin) := rivi
          :for as := (lue-numero arvosana)
          :if (numberp as) :do
          (setf (getf (gethash oid hajautustaulu) :nimi)
                (oppilas-mj sukunimi etunimi))
          (pushnew ryhmä (getf (gethash oid hajautustaulu) :ryhmät)
                   :test #'equal)
          (loop :repeat (or painokerroin 1)
                :do (push as (getf (gethash oid hajautustaulu) :arvosanat))
                :finally (incf (getf (gethash oid hajautustaulu)
                                     :suoritusmäärä 0))))))


(defun tilasto-paremmuus (hakulista &optional painokerroin)
  (let ((hajautustaulu (make-hash-table))
        (rivit nil)
        (summa 0))

    (loop :for hakutermit :in hakulista
          :do (apply #'tilasto-paremmuus-1 hajautustaulu painokerroin
                     hakutermit))

    (when (plusp (hash-table-count hajautustaulu))

      (loop :with ka := nil
            :for oppilas :being :each :hash-value :in hajautustaulu
            :for suoritusmäärä := (getf oppilas :suoritusmäärä)
            :if (plusp suoritusmäärä)
            :do
            (setf ka (/ (reduce #'+ (getf oppilas :arvosanat))
                        (length (getf oppilas :arvosanat))))
            (incf summa ka)
            :and :collect
            (list (getf oppilas :nimi)
                  (lista-mj-listaksi (sort (getf oppilas :ryhmät)
                                           #'string-lessp))
                  (pyöristä ka 1/100)
                  suoritusmäärä)
            :into lista
            :finally (setf rivit lista))

      (when rivit
        (setf rivit (moni-sort rivit
                               (cons (lambda (x) (nth 2 x)) #'>)
                               (cons (lambda (x) (nth 0 x)) #'string-lessp)))

        (loop :for rivi :in rivit
              :do (setf (nth 2 rivi) (tulosta-luku (nth 2 rivi) 2)))
        (make-instance 'tilasto-paremmuus
                       :lista rivit
                       :kokonaiskeskiarvo
                       (tulosta-luku (/ summa (length rivit)) 2))))))


(defun tilasto-koonti ()
  (let ((oppilaita (query-1 "SELECT count(*) FROM oppilaat"))
        (ryhmiä (query-1 "SELECT count(*) FROM ryhmat"))
        (suorituksia (query-1 "SELECT count(*) FROM suoritukset"))
        (arvosanoja (query-1 "SELECT count(*) FROM arvosanat ~
                                WHERE arvosana LIKE '_%'")))
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


(defun taulukkoväli (&optional iso)
  (if iso
      (cond ((muoto :org) (viesti "~& ~%-----~% ~%"))
            (t (viesti "~&~%~%")))
      (cond ((muoto :org) (viesti "~& ~%"))
            (t (viesti "~&~%")))))


(defun muokkaustila ()
  (and *vuorovaikutteinen*
       (not *tulostusmuoto*)
       (not *suppea*)))


(defgeneric tulosta (object))


(defmethod tulosta ((opp oppilaat))
  (setf *muokattavat* (when (muokkaustila)
                        (coerce (oppilaslista opp) 'vector)))
  (let ((taulu nil))

    (loop :with ryhmä-rivi-lkm := 3
       :for oppilas :in (oppilaslista opp)
       :for ryhmät := (ryhmälista oppilas)
       :do
         (push (nconc (list (sukunimi oppilas))
                      (list (etunimi oppilas))
                      (list (lista-mj-listaksi
                             (loop :for i :upfrom 1
                                :while ryhmät
                                :if (and *vuorovaikutteinen*
                                         (> i ryhmä-rivi-lkm))
                                :do (loop-finish)
                                :collect (pop ryhmät))))
                      (unless *suppea*
                        (list (oppilas-lisätiedot oppilas))))
               taulu)

         (loop :while ryhmät
            :do (push (nconc (list :jatko)
                             (list :jatko)
                             (list (lista-mj-listaksi
                                    (loop :repeat ryhmä-rivi-lkm
                                       :while ryhmät
                                       :collect (pop ryhmät))))
                             (unless *suppea*
                               (list :jatko)))
                      taulu))

       :finally (setf taulu (nreverse taulu)))

    (tulosta-taulu
     (nconc (list :viiva-alku)
            (list (nconc (if *muokattavat* (list nil))
                         (list (otsikko-sarake "Sukunimi"))
                         (list (otsikko-sarake "Etunimi"))
                         (list (otsikko-sarake "Ryhmät"))
                         (unless *suppea*
                           (list (otsikko-sarake "Lisätiedot")))))
            (list :viiva-otsikko)
            (if *muokattavat* (numeroi taulu) taulu)
            (list :viiva-loppu)))

    (tulosta-muokattavat "sukunimi" "etunimi" "ryhmät" "lisätiedot")))


(defmethod tulosta ((suo suoritukset))
  (setf *muokattavat* (when (muokkaustila)
                        (coerce (suorituslista suo) 'vector)))

  (let ((taulu (loop :for suoritus :in (suorituslista suo)
                     :collect (list (nimi suoritus)
                                    (lyhenne suoritus)
                                    (painokerroin suoritus)))))

    (tulosta-taulu
     (nconc (list :viiva-alku)
            (list (list (otsikko-rivi "Ryhmä:")
                        (ryhmä-mj (ryhmä suo) (ryhmä-lisätiedot suo))))
            (list :viiva-loppu)))
    (viesti "~%")

    (tulosta-taulu
     (nconc (list :viiva-alku)
            (list (nconc (if *muokattavat* (list nil))
                         (list (otsikko-sarake "Suoritus"))
                         (list (otsikko-sarake "Lyh"))
                         (list (otsikko-sarake "K"))))
            (list :viiva-otsikko)
            (if *muokattavat* (numeroi taulu) taulu)
            (list :viiva-loppu)))

    (tulosta-muokattavat "suoritus" "lyhenne" "painokerroin"
                         (format nil "sija(1~[~;~:;-~:*~A~])"
                                 (length *muokattavat*)))

    (unless (muoto nil :latex)
      (taulukkoväli)
      (tulosta-taulu (list (list (otsikko-rivi "K") "painokerroin"))))))


(defmethod tulosta ((lista ryhmät))
  (setf *muokattavat* (when (muokkaustila)
                        (coerce (ryhmälista lista) 'vector)))

  (let ((taulu (loop :for ryhmä :in (ryhmälista lista)
                     :collect (list (nimi ryhmä)
                                    (ryhmä-lisätiedot ryhmä)))))
    (tulosta-taulu
     (nconc (list :viiva-alku)
            (list (nconc (if *muokattavat* (list nil))
                         (list (otsikko-sarake "Nimi"))
                         (list (otsikko-sarake "Lisätiedot"))))
            (list :viiva-otsikko)
            (if *muokattavat* (numeroi taulu) taulu)
            (list :viiva-loppu))))
  (tulosta-muokattavat "nimi" "lisätiedot"))


(defmethod tulosta ((arv arvosanat-suorituksista))
  (setf *muokattavat* (when (and (muokkaustila)
                                 (= (length (lista arv)) 1))
                        (coerce (arvosanalista (first (lista arv))) 'vector)))

  (loop :for (arv-suo . lisää) :on (lista arv)
        :do
        (let* ((luvut)
               (taulu (loop :for arvosana :in (arvosanalista arv-suo)
                            :for suku := (sukunimi arvosana)
                            :for etu := (etunimi arvosana)
                            :collect
                            (nconc (list (oppilas-mj suku etu))
                                   (list (arvosana arvosana))
                                   (unless *suppea*
                                     (list (arvosana-lisätiedot arvosana))))
                            :do (push (arvosana arvosana) luvut))))

          (tulosta-taulu
           (nconc (list :viiva-alku)
                  (list (list (otsikko-rivi "Ryhmä:")
                              (ryhmä-mj (ryhmä arv-suo)
                                        (ryhmä-lisätiedot arv-suo))))
                  (list (list (otsikko-rivi "Suoritus:") (nimi arv-suo)))
                  (list :viiva-loppu)))

          (taulukkoväli)

          (tulosta-taulu
           (nconc (list :viiva-alku)
                  (list (nconc (if *muokattavat* (list nil))
                               (list (otsikko-sarake "Oppilas"))
                               (list (otsikko-sarake "As"))
                               (unless *suppea*
                                 (list (otsikko-sarake "Lisätiedot")))))
                  (list :viiva-otsikko)
                  (if *muokattavat* (numeroi taulu) taulu)
                  (list :viiva)
                  (list (nconc (if *muokattavat* (list nil))
                               (list "Keskiarvo" (keskiarvo luvut))))
                  (list :viiva-loppu)))

          (unless (muoto nil :latex)
            (taulukkoväli)
            (tulosta-taulu (list (list (otsikko-rivi "As") "arvosana")))))

        :if lisää :do (taulukkoväli t))

  (tulosta-muokattavat "arvosana" "lisätiedot"))


(defmethod tulosta ((arv arvosanat-oppilailta))
  (setf *muokattavat* (when (and (muokkaustila)
                                 (= (length (lista arv)) 1))
                        (coerce (arvosanalista (first (lista arv))) 'vector)))

  (loop :for (arv-opp . lisää) :on (lista arv)
        :do
        (let* ((arvot)
               (kertoimet)
               (taulu (loop :for arvosana :in (arvosanalista arv-opp)
                            :collect
                            (nconc (list (nimi arvosana))
                                   (list (arvosana arvosana))
                                   (list (painokerroin arvosana))
                                   (unless *suppea*
                                     (list (arvosana-lisätiedot arvosana))))
                            :do
                            (push (arvosana arvosana) arvot)
                            (push (painokerroin arvosana) kertoimet))))

          (tulosta-taulu
           (nconc (list :viiva-alku)
                  (list (list (otsikko-rivi "Oppilas:")
                              (oppilas-mj (sukunimi arv-opp)
                                          (etunimi arv-opp))))
                  (list (list (otsikko-rivi "Ryhmä:")
                              (ryhmä-mj (ryhmä arv-opp)
                                        (ryhmä-lisätiedot arv-opp))))
                  (let ((lis (oppilas-lisätiedot arv-opp)))
                    (if (or (not lis) (equal lis "") *suppea*)
                        nil
                        (list (list (otsikko-rivi "Lisätiedot:") lis))))
                  (list :viiva-loppu)))

          (taulukkoväli)

          (tulosta-taulu
           (nconc (list :viiva-alku)
                  (list (nconc (if *muokattavat* (list nil))
                               (list (otsikko-sarake "Suoritus"))
                               (list (otsikko-sarake "As"))
                               (list (otsikko-sarake "K"))
                               (unless *suppea*
                                 (list (otsikko-sarake "Lisätiedot")))))
                  (list :viiva-otsikko)
                  (if *muokattavat* (numeroi taulu) taulu)
                  (list :viiva)
                  (list (nconc (if *muokattavat* (list nil))
                               (list "Keskiarvo"
                                     (keskiarvo arvot kertoimet 2))))
                  (list :viiva-loppu)))

          (unless (muoto nil :latex)
            (taulukkoväli)
            (tulosta-taulu
             (list (list (otsikko-rivi "As") "arvosana"
                         (otsikko-rivi "K") "painokerroin")))))

        :if lisää :do (taulukkoväli t))

  (tulosta-muokattavat "arvosana" "lisätiedot"))


(defmethod tulosta ((koonti arvosanat-koonti))
  (setf *muokattavat* nil)
  (let ((kertoimet)
        (lyhenteet)
        (ka-oppilas (make-array (array-dimension (taulukko koonti) 0)
                                :initial-element nil))
        (ka-suoritus (make-array (array-dimension (taulukko koonti) 1)
                                 :initial-element nil)))

    (loop :for (nil nil nil nil lyhenne painokerroin)
          :in (suorituslista koonti)
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
     (nconc (list :viiva-alku)
            (list (nconc (list (otsikko-rivi "Ryhmä:"))
                         (list (ryhmä-mj (ryhmä koonti)
                                         (ryhmä-lisätiedot koonti)))))
            (list :viiva-loppu)))

    (taulukkoväli)

    (tulosta-taulu
     (nconc
      (list :viiva-alku)
      (list (nconc (list (otsikko-rivi "Suoritus"))
                   (mapcar #'otsikko-sarake lyhenteet)
                   (list (otsikko-sarake "ka"))))
      (list (nconc (list (otsikko-rivi "Painokerroin"))
                   (mapcar #'otsikko-sarake kertoimet)
                   (list (otsikko-sarake ""))))
      (list :viiva-otsikko)
      (loop :for nimi :in (oppilaslista koonti)
            :for oppilas :from 0 :below (array-dimension (taulukko koonti) 0)
            :collect (loop :for suoritus :from 0
                           :below (array-dimension (taulukko koonti) 1)
                           :collect (aref (taulukko koonti) oppilas suoritus)
                           :into rivi
                           :finally (return (nconc (list nimi)
                                                   rivi
                                                   (list (aref ka-oppilas
                                                               oppilas))))))
      (list :viiva)
      (list (nconc (list "Keskiarvo")
                   (coerce ka-suoritus 'list)
                   (list (keskiarvo (coerce ka-oppilas 'list)))))
      (list :viiva-loppu)))

    (unless *suppea*
      (taulukkoväli)
      (tulosta-taulu
       (nconc (list :viiva-alku)
              (list (list (otsikko-sarake "Lyh") (otsikko-sarake "Suoritus")))
              (list :viiva-otsikko)
              (loop :for (nil nil nil nimi lyhenne nil)
                    :in (suorituslista koonti)
                    :collect (list lyhenne nimi))
              (list '("ka" "Keskiarvo"))
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
    (loop :with lkm-leveys := (max 3 (luvun-leveys suurin-arvo))
          :for i :from (floor as-pienin) :upto (ceiling as-suurin) :by 1/4
          :for as := (tulosta-luku i)
          :for määrä := (gethash as (hajautustaulu jakauma) 0)
          :collect (list as (format nil "~V@A" lkm-leveys määrä)
                         (let* ((suhde (/ määrä suurin-arvo))
                                (pituus (pyöristä (* suhde leveys))))
                           (make-string pituus :initial-element #\#)))
          :into taulu
          :finally
          (tulosta-taulu
           (nconc (list :viiva-alku)
                  (list (list (otsikko-sarake "As")
                              (otsikko-sarake "Lkm")
                              (otsikko-sarake "")))
                  (list :viiva-otsikko)
                  taulu
                  (list :viiva-loppu)))

          (unless (muoto nil :latex)
            (taulukkoväli)
            (tulosta-taulu
             (list (list (otsikko-rivi "As") "arvosana"
                         (otsikko-rivi "Lkm") "lukumäärä")))))))


(defmethod tulosta ((paremmuus tilasto-paremmuus))
  (setf *muokattavat* nil)
  (let ((sija-leveys)
        (lkm-leveys)
        (rivit))

    (loop :for (nil nil nil lkm) :in (lista paremmuus)
          :for i :upfrom 1
          :maximize lkm :into suurin-lkm
          :finally (setf sija-leveys (luvun-leveys i)
                         lkm-leveys (max 3 (luvun-leveys suurin-lkm))))

    (loop :with edellinen-ka := nil
          :for (nimi ryhmät ka lkm) :in (lista paremmuus)
          :for sija :upfrom 1
          :collect (list (format nil "~V@A" sija-leveys
                                 (if (equal edellinen-ka ka) "" sija))
                         nimi ryhmät ka
                         (format nil "~V@A" lkm-leveys lkm))
          :into valmis
          :do (setf edellinen-ka ka)
          :finally (setf rivit valmis))

    (tulosta-taulu
     (nconc (list :viiva-alku)
            (list (list (otsikko-sarake "")
                        (otsikko-sarake "Oppilas")
                        (otsikko-sarake "Ryhmät")
                        (otsikko-sarake "Ka")
                        (otsikko-sarake "Lkm")))
            (list :viiva-otsikko)
            rivit
            (list :viiva)
            (list (list nil "Keskiarvo" nil (kokonaiskeskiarvo paremmuus)))
            (list :viiva-loppu)))

    (unless (muoto nil :latex)
      (taulukkoväli)
      (tulosta-taulu
       (list (list (otsikko-rivi "Ka") "keskiarvo"
                   (otsikko-rivi "Lkm") "suoritusten lukumäärä"))))))


(defmethod tulosta ((koonti tilasto-koonti))
  (setf *muokattavat* nil)
  (let ((suurin (reduce #'max (list (oppilaita koonti)
                                    (ryhmiä koonti)
                                    (suorituksia koonti)
                                    (arvosanoja koonti))
                        :key #'luvun-leveys)))
    (flet ((rivi (otsikko olio)
             (list (otsikko-rivi otsikko) (format nil "~V@A" suurin olio))))
      (tulosta-taulu
       (nconc (list :viiva-alku)
              (list (rivi "Oppilaita:" (oppilaita koonti)))
              (list (rivi "Ryhmiä:" (ryhmiä koonti)))
              (list (rivi "Suorituksia:" (suorituksia koonti)))
              (list (rivi "Arvosanoja:" (arvosanoja koonti)))
              (list :viiva-loppu))))))


(defmethod tulosta ((object t))
  (setf *muokattavat* nil)
  (virhe "Ei löytynyt."))


(defgeneric lisää (asia &key &allow-other-keys))


(defmethod lisää ((oppilas oppilas) &key)
  (query "INSERT INTO oppilaat (sukunimi, etunimi, lisatiedot) ~
        VALUES (~A, ~A, ~A)"
         (sql-mj (sukunimi oppilas))
         (sql-mj (etunimi oppilas))
         (sql-mj (oppilas-lisätiedot oppilas)))
  (let ((oid (query-last-insert-rowid)))
    (loop :for ryhmä :in (ryhmälista oppilas)
          :for rid := (query-1 "SELECT rid FROM ryhmat WHERE nimi LIKE ~A"
                               (sql-like-suoja ryhmä))

          :unless rid :do
          (query "INSERT INTO ryhmat (nimi, lisatiedot) VALUES (~A, '')"
                 (sql-mj ryhmä))
          (setf rid (query-last-insert-rowid))

          :do
          (query "INSERT INTO oppilaat_ryhmat (oid, rid) VALUES (~A, ~A)"
                 oid rid))
    oid))


(defmethod lisää ((suoritus suoritus) &key sija)
  (let ((rid (query-1 "SELECT rid FROM ryhmat WHERE nimi LIKE ~A"
                      (sql-like-suoja (ryhmä suoritus)))))
    (unless rid
      (query "INSERT INTO ryhmat (nimi) VALUES (~A)" (sql-mj (ryhmä suoritus)))
      (setf rid (query-last-insert-rowid)))

    (setf (rid suoritus) rid)

    (query "INSERT INTO suoritukset (rid, nimi, lyhenne, painokerroin) ~
                VALUES (~A, ~A, ~A, ~A)"
           rid (sql-mj (nimi suoritus)) (sql-mj (lyhenne suoritus))
           (or (painokerroin suoritus) "NULL"))

    (let* ((uusi-sid (query-last-insert-rowid))
           (sid-lista
            (query-nconc "SELECT sid FROM suoritukset ~
                                WHERE rid = ~A AND NOT sid = ~A ~
                                ORDER BY sija, sid"
                         rid uusi-sid)))
      (setf (sid suoritus) uusi-sid)
      (cond ((or (not sija)
                 (> sija (1+ (length sid-lista))))
             (setf sija (1+ (length sid-lista))))
            ((< sija 1) (setf sija 1)))
      (query "UPDATE suoritukset SET sija = ~A WHERE sid = ~A" sija uusi-sid)
      (loop :with i := 0
            :for sid :in sid-lista
            :do (incf i)
            :if (= i sija) :do (incf i)
            :do (query "UPDATE suoritukset SET sija = ~A ~
                                WHERE sid = ~A" i sid)))))


(defgeneric muokkaa (asia &key &allow-other-keys))


(defmethod muokkaa ((oppilas oppilas) &key)
  (let ((vanha-rid-lista
         (query-nconc "SELECT rid FROM oppilaat_ryhmat WHERE oid = ~A"
                      (oid oppilas)))
        (uusi-rid-lista nil))

    (query "UPDATE oppilaat SET sukunimi = ~A, etunimi = ~A, lisatiedot = ~A ~
                WHERE oid = ~A"
           (sql-mj (sukunimi oppilas))
           (sql-mj (etunimi oppilas))
           (sql-mj (oppilas-lisätiedot oppilas))
           (oid oppilas))

    (loop :for ryhmä :in (ryhmälista oppilas)
          :for rid := (query-1 "SELECT rid FROM ryhmat ~
                                        WHERE nimi LIKE ~A"
                               (sql-like-suoja ryhmä))

          :unless rid :do
          (query "INSERT INTO ryhmat (nimi) VALUES (~A)" (sql-mj ryhmä))
          (setf rid (query-last-insert-rowid))

          :unless (member rid vanha-rid-lista) :do
          (query "INSERT INTO oppilaat_ryhmat (oid, rid) VALUES (~A, ~A)"
                 (oid oppilas) rid)

          :collect rid :into rid-lista
          :finally (setf uusi-rid-lista rid-lista))

    (loop :with ero := (set-difference vanha-rid-lista uusi-rid-lista)
          :for rid :in ero
          :do (query "DELETE FROM oppilaat_ryhmat ~
                        WHERE oid = ~A AND rid = ~A"
                     (oid oppilas) rid)
          :finally (when ero (poista-tyhjät-ryhmät)))

    oppilas))


(defmethod muokkaa ((suoritus suoritus) &key sija)
  (query "UPDATE suoritukset SET nimi = ~A, lyhenne = ~A, painokerroin = ~A ~
                WHERE sid = ~A"
         (sql-mj (nimi suoritus)) (sql-mj (lyhenne suoritus))
         (or (painokerroin suoritus) "NULL")
         (sid suoritus))

  (when sija
    (let ((sid-lista
           (query-nconc "SELECT sid FROM suoritukset ~
                                WHERE rid = ~A AND NOT sid = ~A ~
                                ORDER BY sija, sid"
                        (rid suoritus) (sid suoritus))))

      (setf sija (min sija (1+ (length sid-lista)))
            sija (max sija 1))
      (query "UPDATE suoritukset SET sija = ~A WHERE sid = ~A"
             sija (sid suoritus))
      (loop :with i := 0
            :for sid :in sid-lista
            :do (incf i)
            :if (= i sija) :do (incf i)
            :do (query "UPDATE suoritukset SET sija = ~A WHERE sid = ~A"
                       i sid)))))


(defmethod muokkaa ((ryhmä ryhmä) &key)
  (query "UPDATE ryhmat SET nimi = ~A, lisatiedot = ~A WHERE rid = ~A"
         (sql-mj (nimi ryhmä))
         (sql-mj (ryhmä-lisätiedot ryhmä))
         (rid ryhmä)))


(defmethod muokkaa ((arvosana arvosana) &key)
  (let ((lisätiedot (let ((lisä (arvosana-lisätiedot arvosana)))
                      (if (or (not lisä) (equal lisä ""))
                          "NULL"
                          (sql-mj lisä)))))
    (if (query "SELECT oid FROM arvosanat WHERE sid = ~A AND oid = ~A"
               (sid arvosana) (oid arvosana))
        (query "UPDATE arvosanat SET arvosana = ~A, lisatiedot = ~A ~
                WHERE sid = ~A AND oid = ~A"
               (sql-mj (arvosana arvosana)) lisätiedot (sid arvosana)
               (oid arvosana))
        (query "INSERT INTO arvosanat (sid, oid, arvosana, lisatiedot) ~
                VALUES (~A, ~A, ~A, ~A)"
               (sid arvosana) (oid arvosana) (sql-mj (arvosana arvosana))
               lisätiedot))))


(defgeneric poista (asia))


(defmethod poista ((oppilas oppilas))
  (query "DELETE FROM oppilaat WHERE oid = ~A" (oid oppilas))
  (poista-tyhjät-ryhmät))


(defmethod poista ((suoritus suoritus))
  (query "DELETE FROM suoritukset WHERE sid = ~A" (sid suoritus))
  (poista-tyhjät-ryhmät)
  (let ((sid-lista
         (query-nconc "SELECT sid FROM suoritukset ~
                        WHERE rid = ~A ORDER BY sija, sid"
                      (rid suoritus))))
    (loop :for i :upfrom 1
          :for sid :in sid-lista
          :do (query "UPDATE suoritukset SET sija= ~A WHERE sid = ~A" i sid))))


(defmethod poista ((arvosana arvosana))
  (query "DELETE FROM arvosanat WHERE sid = ~A AND oid = ~A"
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
  ;; /ryhmä/suoritus/lyhenne
  (when (zerop (length arg))
    (setf arg "/"))
  (let ((jaettu (pilko-erottimella arg)))
    (tulosta (hae-arvosanat-suorituksista (nth 0 jaettu)
                                          (nth 1 jaettu)
                                          (nth 2 jaettu)))))


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
  (when (zerop (length arg))
    (virhe "Anna ryhmän tunnus."))
  (tulosta (hae-arvosanat-koonti (erota-ensimmäinen-sana arg))))


(defun komento-tilasto-jakauma (arg &optional painokerroin)
  ;; @/ryhmä/suoritus/lyhenne/sukunimi/etunimi/lisätiedot@/...
  (when (zerop (length arg))
    (setf arg "@"))
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


(defun komento-tilasto-paremmuus (arg &optional painokerroin)
  ;; @/ryhmä/suoritus/lyhenne/sukunimi/etunimi/lisätiedot@/...
  (when (zerop (length arg))
    (setf arg "@"))
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
        (tulosta (tilasto-paremmuus haut painokerroin))))


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
                            :oppilas-lisätiedot (normalisoi-mj (nth 3 jaettu))))
      (lisää-muokkauslaskuriin 1))
    (eheytys)))


(defun on-sisältöä-p (mj)
  (notevery (lambda (el)
              (and (characterp el)
                   (or (not (graphic-char-p el))
                       (char= #\Space el))))
            mj))


(defun komento-lisää-suoritus (arg)
  ;; /ryhmä/suoritus/lyhenne/painokerroin/sija
  (let* ((jaettu (mapcar #'normalisoi-mj (pilko-erottimella arg)))
         (ryhmä (nth 0 jaettu))
         (nimi (nth 1 jaettu))
         (lyh (nth 2 jaettu))
         (paino (nth 3 jaettu))
         (sija (nth 4 jaettu)))

    (when (or (not (and ryhmä (on-sisältöä-p ryhmä)))
              (not (and nimi (on-sisältöä-p nimi)))
              (not (and lyh (on-sisältöä-p lyh))))
      (virhe "Pitää antaa vähintään ryhmä, suorituksen nimi ja lyhenne."))

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
    (eheytys)))


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

    (when (and ryhmä (on-sisältöä-p ryhmä))
      (setf ryhmä (string-trim " " ryhmä))
      (cond
        ((char= #\+ (aref ryhmä 0))
         (setf uusi-ryhmä (normalisoi-ryhmät
                           (nconc (split-sequence #\space ryhmä :start 1
                                                  :remove-empty-subseqs t)
                                  (ryhmälista kohde)))))
        ((char= #\- (aref ryhmä 0))
         (setf uusi-ryhmä (normalisoi-ryhmät (ryhmälista kohde)))
         (loop :for r :in (normalisoi-ryhmät (subseq ryhmä 1))
               :do (setf uusi-ryhmä (delete r uusi-ryhmä :test #'equal))))
        (t (setf uusi-ryhmä (normalisoi-ryhmät ryhmä))))

      (unless (plusp (length uusi-ryhmä))
        (virhe "Oppilaan täytyy kuulua johonkin ryhmään.")))

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
      (setf (ryhmälista kohde) uusi-ryhmä)) ;Normalisoidaan jo aiemmin.
    (unless (eql uusi-lisä :tyhjä)
      (setf (oppilas-lisätiedot kohde) (normalisoi-mj uusi-lisä)))
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
      (setf (arvosana-lisätiedot kohde) (normalisoi-mj uusi-lisätiedot)))
    (if (and (or (not (arvosana kohde))
                 (equal (arvosana kohde) ""))
             (or (not (arvosana-lisätiedot kohde))
                 (equal (arvosana-lisätiedot kohde) "")))
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
      (setf (ryhmä-lisätiedot kohde) (normalisoi-mj uusi-lisätiedot)))
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
                :for kentät := (nconc (make-list (1- kentän-numero)
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


(defun ohjeet-komennot ()
  (tulosta-taulu
   (list
    :viiva-alku
    (list (otsikko-sarake "Komento") (otsikko-sarake "Tarkoitus"))
    :viiva-otsikko
    '("ho /sukunimi/etunimi/ryhmät/lisätiedot" "Hae oppilaita.")
    '("hoa /sukunimi/etunimi/ryhmät/lisätiedot"
      "Hae oppilaita arvotussa järjestyksessä.")
    '("hr /ryhmä/lisätiedot" "Hae ryhmiä.")
    '("hs ryhmä" "Hae suoritukset ryhmältä.")
    '("hao /sukunimi/etunimi/ryhmät/lisätiedot" "Hae arvosanat oppilailta.")
    '("has /ryhmä/suoritus/lyhenne" "Hae arvosanat suorituksista.")
    '("hak ryhmä" "Hae arvosanojen koonti.")
    :viiva
    '("tj  @/ryh/suor/lyh/suku/etu/lisät@/..."
      "Jakauma arvosanoista (painokertoimelliset).")
    '("tjk @/ryh/suor/lyh/suku/etu/lisät@/..."
      "Jakauma arvosanoista (kaikki).")
    '("tp  @/ryh/suor/lyh/suku/etu/lisät@/..."
      "Oppilaat paremmuusjärjestyksessä (painokert.).")
    '("tpk @/ryh/suor/lyh/suku/etu/lisät@/..."
      "Oppilaat paremmuusjärjestyksessä (kaikki).")
    '("tk" "Tulosta tietokannasta koonti.")
    :viiva
    '("lo /sukunimi/etunimi/ryhmät/lisätiedot" "Lisää oppilas.")
    '("ls /ryhmä/suoritus/lyhenne/painokerroin/sija"
      "Lisää ryhmälle suoritus.")
    :viiva
    '("m numerot /.../.../.../..." "Muokkaa valittuja tietueita ja kenttiä.")
    '("ms numerot kenttä /.../.../..." "Muokkaa tietueista samaa kenttää.")
    '("poista numerot" "Poista tietueet.")
    :viiva
    '("?" "Komennot.")
    '("??" "Käyttöohjeita.")
    '("???" "Aloitusvinkkejä.")
    :viiva-loppu)))


(defun ohjeet-käyttö ()
  (viesti "~

Komennon argumenteissa oleva vinoviiva (/.../...) tarkoittaa kenttien
erotinmerkkiä. Merkki voi ola mikä tahansa, ja ensimmäisenä oleva merkki
määrittää erottimen.

Joissakin komennoissa oleva @-merkki tarkoittaa myös erotinmerkkiä,
jonka voi valita vapaasti. Näissä komennoissa on kaksitasoinen kenttien
erotus: ensin yhden erottimen (@) avulla erotetaan kenttäryhmät
toisistaan ja sitten ryhmien sisällä toisen erottimen (/) avulla kentät
toisistaan.

Komentojen \"m\", \"ms\" ja \"poista\" kohdalla argumentti \"numerot\"
tarkoittaa kokonaislukujen luetteloa, esimerkiksi \"1,4\" tai \"2-5,9\".
Niiden avulla valitaan, mitä tietueita muokataan tai mitkä poistetaan.

Hakukomentojen tulosteessa voi olla numeroituja tietueita. Lisäksi
tulosteen lopussa on esimerkiksi seuraavanlainen rivi:

    Tietueet: 1-22. Kentät: /arvosana/lisätiedot

Mainittuja tietueita (1-22) ja kenttiä (arvosana, lisätiedot) voi
muokata tai poistaa. Esimerkkejä:

    m 1 /8+/Eri koe kuin muilla
    m 3,9,14 /7½
    ms 10-12 1 /7½/8+/9-
    poista 3,15

Komento \"m\" muokkaa yksittäisiä tietueita, ja kaikille tietueille
asetetaan sama sisältö. Komento \"ms\" muokkaa kaikista tietueista vain
yhtä kenttää, ja kaikille tietueille voi määrittää kenttään eri arvon.
Komennon \"ms\" toisena argumenttina annetaan muokattavan kentän numero.
Ensimmäinen kenttä vasemmalta on 1, toinen vasemmalta on 2 jne.

Muokkauskomennoissa kentän voi jättää tyhjäksi, jos sitä ei halua
muokata. Jos kenttään laittaa pelkän välilyönnin, kentän sisältö
poistetaan.

Oppilashaun jälkeen on muokattavissa neljä kenttää: sukunimi, etunimi,
ryhmät ja lisätiedot. Ryhmät-kentässä oppilaan eri ryhmät erotetaan
toisistaan välilyönnillä. Kentän voi aloittaa plusmerkillä (+) tai
miinusmerkillä (-). Tällöin oppilas lisätään kentässä lueteltuihin
ryhmiin tai poistetaan niistä.

    m 1-5 ///2013:suk:7a 2014:suk:8a
    m 6 ///+2014:suk:8a

Kun ohjelman käynnistää ilman komentoriviargumentteja, se käynnistyy
vuorovaikutteiseen tilaan. Jos ohjelmalle antaa argumentiksi \"-\",
luetaan komennot rivi kerrallaan standardisyötteestä. Muussa tapauksessa
komentoriviargumentit tulkitaan ohjelman komennoiksi. Komento
suoritetaan ja ohjelma sulkeutuu. Muokkaus- ja poistokomennot toimivat
vain vuorovaikutteisessa tilassa.

"))


(defun ohjeet-aloitus ()
  (viesti "~

Ohjelman käyttö kannattaa aloittaa lisäämällä oppilaita.
Lisäystoiminnossa (lo) syötettävät kentät ovat vasemmalta oikealle
sukunimi, etunimi, ryhmät ja lisätiedot. Ainakin sukunimi, etunimi ja
yksi ryhmä täytyy syöttää. Kentät erotetaan toisistaan jollakin
erotinmerkillä. Tässä esimerkissä käytetään vivoviivaa (/):

    lo /Meikäläinen/Matti/2013:suk:7a
    lo /Oppilas/Oona/2013:suk:7a
    lo /Koululainen/Kalle/2013:suk:7a/lukivaikeus

Kannattaa nimetä ryhmät lukuvuoden aloitusvuoden, aine- ja
ryhmätunnuksen avulla, esimerkiksi \"2013:suk:7a\". Näin ryhmät voi
yksilöidä usean lukuvuoden aikana. Oppilaan tiedoissa eri ryhmät
erotetaan toisistaan välilyönnein:

    lo /Meikäläinen/Maija/2013:suk:7a 2014:suk:8a 2015:suk:9a

Sitten voi luoda ryhmälle suorituksia. Suoritustiedoissa kentät ovat
seuraavat: suorituksen nimi, lyhenne, painokerroin ja sija eli
järjestysnumero. Suorituksen lyhennettä käytetään arvosanojen
koonnissa (hak-komento). Painokerrointa käytetään suoritusten keskiarvon
laskennassa. Sen täytyy olla positiivinen kokonaisluku. Jos
painokerrointa ei ole, kyseistä suoritusta ei huomioida keskiarvon
laskennassa. Alla on esimerkkejä suoritusten lisäämisestä.

    ls /2013:suk:7a/Kirje opettajalle/kir
    ls /2013:suk:7a/Sanaluokkakoe/san/2
    ls /2013:suk:7a/Kirjoitelma romaanista/rom/3
    ls /2013:suk:7a/Välitodistus/vto

"))


(defun ohjeet-komentorivi ()
  (viesti "Käyttö: koas [valitsimet] [--] [komennot]

Koas eli kouluarvosanatietokanta. Ohjelma käynnistyy vuorovaikutteiseen
tilaan, kun sen käynnistää ilman \"komentoja\". Vuorovaikutteisessa
tilassa saa apua komennolla \"?\".

Valitsimet:

  --muoto=tulostusmuoto
        Vaihtaa ohjelman tulostusmuodon. Tämä valitsin ei vaikuta
        vuorovaikutteisen tilan tulostusmuotoon.

        Tulostusmuodot ovat: \"csv\" eli pilkuilla erotetut taulukon
        solut (csv = comma-separated values); \"tab\" eli
        sarkainmerkeillä erotetut taulukon solut; \"org\" eli Emacsin
        org-tilan taulukkomuoto; \"latex\" eli Latex-komennon näköiset
        taulukon rivit.

  --suppea
        Tulostaa taulukoiden tiedot suppeammin. Esimerkiksi taulukoiden
        Lisätiedot-sarake jätetään pois. Tämä valitsin ei vaikuta
        vuorovaikutteiseen tilaan.

  -h, --ohje[=aihe]
        Tulostaa ohjelman ohjeita. Jos ohjeen aihetta ei ole mainittu,
        tulostetaan nämä komentorivin ohjeet. Ohjeen aiheita ovat
        \"komennot\", \"käyttö\" ja \"aloitus\".

Ohjelman komennot saa näkyviin valitsimella \"--ohje=komennot\" tai
vuorovaikutteisessa tilassa komennolla \"?\". Jos komentorivillä
komentona on vain yhdysmerkki \"-\", luetaan komennot
standardisyötteestä, niin että yhdellä rivillä on aina yksi komento.
Muokkauskomennot eivät ole tällöin käytössä.

"))


(defun käsittele-komentorivi (mj)
  (when (null mj)
    (viesti "~%")
    (error 'poistu-ohjelmasta))

  (multiple-value-bind (komento arg)
      (erota-ensimmäinen-sana mj)
    (flet ((testaa (mj)
             (equalp komento mj))
           (tuntematon (mj)
             (if *vuorovaikutteinen*
                 (virhe "Tuntematon komento \"~A\". Ohjeita saa ?:llä." mj)
                 (virhe "Tuntematon komento \"~A\". Ohjeita saa ~
                        valitsimella \"-h\" tai \"--ohje\"." mj))))

      (cond
        ((testaa "ho") (komento-hae-oppilaat arg))
        ((testaa "hoa") (komento-hae-oppilaat-arvottu arg))
        ((testaa "hs") (komento-hae-suoritukset arg))
        ((testaa "hr") (komento-hae-ryhmät arg))
        ((testaa "has") (komento-hae-arvosanat-suorituksista arg))
        ((testaa "hao") (komento-hae-arvosanat-oppilailta arg))
        ((testaa "hak") (komento-hae-arvosanat-koonti arg))
        ((testaa "lo") (komento-lisää-oppilas arg))
        ((testaa "ls") (komento-lisää-suoritus arg))
        ((testaa "tj") (komento-tilasto-jakauma arg t))
        ((testaa "tjk") (komento-tilasto-jakauma arg))
        ((testaa "tp") (komento-tilasto-paremmuus arg t))
        ((testaa "tpk") (komento-tilasto-paremmuus arg))
        ((testaa "tk") (komento-tilasto-koonti))
        ((and (testaa "") (not *vuorovaikutteinen*)))
        (*vuorovaikutteinen*
         (cond
           ((testaa "") (error 'poistu-ohjelmasta))
           ((testaa "?") (ohjeet-komennot))
           ((testaa "??") (ohjeet-käyttö))
           ((testaa "???") (ohjeet-aloitus))
           ((testaa "poista") (komento-poista arg))
           ((testaa "m") (komento-muokkaa arg))
           ((testaa "ms") (komento-muokkaa-sarjana arg))
           (t (tuntematon mj))))
        (t (tuntematon mj))))))


(defun main (&rest args)
  (flet ((tulosta-valitsin (valitsin)
           (etypecase valitsin
             (character (format nil "-~C" valitsin))
             (string (format nil "--~A" valitsin)))))

    (handler-bind
        ((just-getopt-parser:unknown-option
           (lambda (tila)
             (virheviesti "Tuntematon valitsin \"~A\".~%"
                          (tulosta-valitsin
                           (just-getopt-parser:option-name tila)))
             (invoke-restart 'just-getopt-parser:skip-option)))

         (just-getopt-parser:required-argument-missing
           (lambda (tila)
             (virhe "Valitsin \"~A\" tarvitsee lisäksi argumentin."
                    (tulosta-valitsin (just-getopt-parser:option-name tila)))))

         (just-getopt-parser:argument-not-allowed
           (lambda (tila)
             (virhe "Valitsimelle \"~A\" ei hyväksytä argumenttia."
                    (tulosta-valitsin (just-getopt-parser:option-name tila)))))

         (poistu-ohjelmasta
           (lambda (tila)
             (declare (ignore tila))
             (return-from main))))

      (multiple-value-bind (valitsimet argumentit tuntemattomat)
          (just-getopt-parser:getopt args '((:help #\h)
                                            (:help "ohje" :optional)
                                            (:muoto "muoto" :required)
                                            (:suppea "suppea"))
                                     :error-on-unknown-option t
                                     :error-on-argument-missing t
                                     :error-on-argument-not-allowed t)

        (when (and tuntemattomat (not (assoc :help valitsimet)))
          (virhe "Ohjeita saa valitsimella \"-h\" tai \"--ohje\"."))

        (when (assoc :help valitsimet)
          (when argumentit
            (virheviesti "(Annettuja komentoja \"~{~A~^ ~}\" ei huomioida, ~
                kun näytetään ohjeet.)~%~%" argumentit))
          (let ((tapa (cdr (assoc :help valitsimet))))
            (cond ((null tapa) (ohjeet-komentorivi))
                  ((string= tapa "komennot") (ohjeet-komennot))
                  ((string= tapa "käyttö") (ohjeet-käyttö))
                  ((string= tapa "aloitus") (ohjeet-aloitus))
                  (t (virhe "Tuntematon ohjeen aihe \"~A\"." tapa))))
          (error 'poistu-ohjelmasta))

        (let ((muoto nil))

          (when (assoc :muoto valitsimet)
            (setf muoto (cdr (assoc :muoto valitsimet)))
            (if (find muoto '("tab" "csv" "org" "latex") :test #'equal)
                (setf muoto (intern (string-upcase muoto) "KEYWORD"))
                (virhe "Tuntematon tulostusmuoto \"~A\"." muoto)))

          (tietokanta-käytössä
            (cond
              ((and (equal (nth 0 argumentit) "-")
                    (not (rest argumentit)))
               (let ((*vuorovaikutteinen* nil)
                     (*tulostusmuoto* muoto)
                     (*suppea* (assoc :suppea valitsimet)))
                 (loop :for rivi := (read-line *standard-input* nil)
                       :while rivi :do (käsittele-komentorivi rivi))))

              (argumentit
               (let ((*vuorovaikutteinen* nil)
                     (*tulostusmuoto* muoto)
                     (*suppea* (assoc :suppea valitsimet)))
                 (käsittele-komentorivi
                  (format nil "~{~A~^ ~}" argumentit))))

              (t (let ((*vuorovaikutteinen* t)
                       (*tulostusmuoto* nil)
                       (*suppea* nil))
                   (when (or (assoc :muoto valitsimet)
                             (assoc :suppea valitsimet))
                     (virheviesti "(Vuorovaikutteisessa tilassa ei huomioida ~
                        kaikkia valitsimia.)~%"))
                   (loop
                     (handler-case
                         (käsittele-komentorivi (lue-rivi "KOAS> " t))
                       (koas-virhe (tila)
                         (viesti "~A~%" tila)))))))))))))


(defun start ()
  (handler-case (let ((*readline* t))
                  (apply #'main (rest sb-ext:*posix-argv*)))
    (sb-int:simple-stream-error ()
      (sb-ext:exit :code 0))
    (sb-sys:interactive-interrupt ()
      (terpri)
      (sb-ext:exit :code 1))
    (serious-condition (c)
      (virheviesti "~A~%" c)
      (sb-ext:exit :code 1))))
