(defsystem :arviointi
  :description "Tietokantaohjelma oppilaiden arvosanoille"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "GPL 3+"
  :depends-on (:cffi :sqlite :decimals :split-sequence)
  :components
  ((:file "arviointi" :depends-on ("script-lib"))
   (:file "script-lib")))
