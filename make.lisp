(load "asdf.conf")
(load "quicklisp/setup.lisp")
(ql:quickload "koas")

(sb-ext:save-lisp-and-die
   "koas"
   :executable t
   :save-runtime-options t
   :compression (if (member :sb-core-compression *features*) 9)
   :toplevel #'koas:start)
