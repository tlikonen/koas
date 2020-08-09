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


(defpackage #:yhteinen
  (:use #:cl)
  (:import-from #:split-sequence #:split-sequence)
  (:export
   #:poistu-ohjelmasta
   #:koas-virhe
   #:virhe
   #:viesti #:virheviesti
   #:lue-numero
   #:mj-lista-listaksi
   #:lista-mj-listaksi
   #:sanalista-riveiksi
   ))

(in-package #:yhteinen)


(define-condition poistu-ohjelmasta () nil)

(define-condition koas-virhe (error)
  ((teksti :reader teksti :initarg :teksti))
  (:report (lambda (tila virta)
             (format virta "~A" (teksti tila)))))


(defun virhe (fmt &rest args)
  (error 'koas-virhe :teksti (apply #'format nil fmt args)))


(defun viesti (fmt &rest args)
  (apply #'format t fmt args))


(defun virheviesti (fmt &rest args)
  (apply #'format *error-output* fmt args)
  (finish-output *error-output*))


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

       (setf objekti (nsubstitute #\. #\, (subseq objekti alku loppu)))
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


(defun mj-lista-listaksi (mj-lista)
  (split-sequence #\space mj-lista :remove-empty-subseqs t))


(defun lista-mj-listaksi (lista)
  (format nil "~{~A~^ ~}" lista))


(defun sanalista-riveiksi (sanalista rivin-pituus)
  (loop :with rivit := nil
     :with rivi := nil
     :while sanalista
     :if (or (null rivi)
             (<= (length (lista-mj-listaksi
                          (append rivi (list (first sanalista)))))
                 rivin-pituus))
     :do
       (push (pop sanalista) rivi)
       (when (null sanalista)
         (push (lista-mj-listaksi (nreverse rivi)) rivit))

     :else :do
       (push (lista-mj-listaksi (nreverse rivi)) rivit)
       (setf rivi nil)
     :finally (return (nreverse rivit))))
