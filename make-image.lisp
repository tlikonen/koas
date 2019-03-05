(load "quicklisp/setup.lisp")

(ql:quickload "koas")

(sb-ext:save-lisp-and-die
 "koas"
 :executable t
 :toplevel #'koas:start
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
