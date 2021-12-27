(load "asdf.conf")
(load "quicklisp/setup.lisp")
(ql:quickload "koas")

(let ((versio (with-open-file (v "versio.txt" :direction :input)
                (read-line v))))
  (sb-ext:save-lisp-and-die
   "koas"
   :executable t
   :save-runtime-options t
   :compression (if (member :sb-core-compression *features*) 9)
   :toplevel (lambda () (koas:start :versio versio))))
