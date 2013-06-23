(defparameter *system* (nth 1 sb-ext:*posix-argv*))
(defparameter *image* (nth 2 sb-ext:*posix-argv*))

(when (or (not *image*)
          (zerop (length *image*)))
  (write-line "Image file name missing." *error-output*)
  (sb-ext:exit :code 1))

(format t "Creating SBCL image file: ~A~%~A ~A~%~%"
        *image*
        (lisp-implementation-type)
        (lisp-implementation-version))

(handler-case
    (let ((home (user-homedir-pathname))
          (pwd *default-pathname-defaults*))
      (flet ((probe-load (path &optional (default home))
               (let ((path (merge-pathnames path default)))
                 (when (probe-file path) (load path)))))
        (or (probe-load #p"quicklisp/setup.lisp" home)
            (probe-load #p".quicklisp/setup.lisp" home)
            (probe-load #p"quicklisp/setup.lisp" pwd)
            (let ((init "quicklisp.lisp")
                  (url "http://beta.quicklisp.org/quicklisp.lisp"))
              (sb-ext:run-program "wget" (list "-O" init url)
                                  :search t :output t)
              (when (probe-load init pwd)
                (funcall (read-from-string "quicklisp-quickstart:install")
                         :path (merge-pathnames #p"quicklisp/" pwd)))))))

  (sb-sys:interactive-interrupt ()
    (sb-ext:exit :code 1))
  (error (c)
    (format *error-output* "~&~A~%" c)
    (sb-ext:exit :code 1)))

(handler-case (ql:quickload *system*)
  (sb-sys:interactive-interrupt ()
    (sb-ext:exit :code 1))
  (error (c)
    (format *error-output* "~&~A~%" c)
    (sb-ext:exit :code 1)))

(sb-ext:save-lisp-and-die
 *image*
 :executable t
 :toplevel (lambda ()
             (let ((koas::*readline* t))
               (koas::main (script:argv))))
 :save-runtime-options t
 :compression (and (member :sb-core-compression *features*) t))
