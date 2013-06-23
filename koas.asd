(defsystem :koas
  :description "Tietokantaohjelma oppilaiden arvosanoille"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "The GNU General Public License version 3"
  :depends-on (:cffi :sqlite :decimals :split-sequence)
  :components
  ((:file "koas" :depends-on ("script-lib"))
   (:file "script-lib")))
