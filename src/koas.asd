(defsystem "koas"
  :description "Koas - tietokantaohjelma kouluarvosanoille"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 3"
  :depends-on ((:require "sb-posix")
               "cl-readline" "sqlite" "decimals" "split-sequence"
                             "just-getopt-parser")
  :components
  ((:file "koas" :depends-on ("yhteinen" "pathconv" "tietokanta"))
   (:file "tietokanta" :depends-on ("yhteinen" "pathconv" "string-io"))
   (:file "yhteinen")
   (:file "pathconv")
   (:file "string-io")))
