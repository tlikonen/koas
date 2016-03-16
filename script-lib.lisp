;;; Copyright (C) 2013-2016 Teemu Likonen <tlikonen@iki.fi>
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


(defpackage #:script
  (:use #:cl)
  (:export #:argv #:readline #:with-pp-errors))

(in-package #:script)


(defun argv ()
  (cons (or *load-pathname*
            (first sb-ext:*posix-argv*))
        (rest sb-ext:*posix-argv*)))


(defun msgerr (control-string &rest args)
  (apply #'format *error-output* control-string args))

(eval-when (:load-toplevel)
  (handler-case
      (progn
        (cffi:load-foreign-library '(:default "libreadline"))
        (format t "~&Readline library loaded.~%")
        (cffi:defcvar "rl_inhibit_completion" :int)
        (setf *rl-inhibit-completion* 1))

    (cffi:load-foreign-library-error ()
      (msgerr "~%WARNING: Readline library is not available. ~
                    Install libreadline-dev.~%"))))

(defun readline (prompt &optional add-history)
  (handler-case
      (let ((string-ptr
             (cffi:foreign-funcall "readline" :string prompt :pointer)))
        (when (and (cffi:pointerp string-ptr)
                   (not (cffi:null-pointer-p string-ptr)))
          (unwind-protect
               (let ((string (cffi:foreign-string-to-lisp string-ptr)))
                 (when add-history
                   (cffi:foreign-funcall "add_history" :string string :void))
                 string)
            (cffi:foreign-funcall "free" :pointer string-ptr :void))))

    (sb-kernel::undefined-alien-function-error ()
      (format *query-io* "~A" prompt)
      (force-output *query-io*)
      (read-line *query-io*))))


(defmacro with-pp-errors (&body body)
  `(handler-case (progn ,@body)
     (error (c)
       (msgerr "~&~A~%" c))))
