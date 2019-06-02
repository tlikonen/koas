(defsystem "koas"
  :description "Koas - tietokantaohjelma kouluarvosanoille"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 3"
  :depends-on ("cl-readline" "sqlite" "decimals" "split-sequence")
  :components ((:file "koas" :depends-on ("pathconv"))
               (:file "pathconv")))
