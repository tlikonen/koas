(defsystem "koas"
  :description "Koas - tietokantaohjelma kouluarvosanoille"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 3"
  :depends-on ((:require "sb-posix") "cl-readline" "sqlite" "cl-postgres"
               "decimals" "split-sequence" "just-getopt-parser")
  :components
  ((:file "koas" :depends-on ("yhteinen" "pathconv" "tietokanta"))
   (:file "tietokanta"
    :depends-on ("yhteinen" "pathconv" "string-io" "xdg-dirs" "fstools"
                            "pgsql"))
   (:file "yhteinen")
   (:file "pathconv")
   (:file "string-io")
   (:file "xdg-dirs" :depends-on ("pathconv"))
   (:file "fstools" :depends-on ("pathconv"))
   (:file "pgsql")))
