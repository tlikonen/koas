(let ((image (nth 1 sb-ext:*posix-argv*)))
  (when (or (not image)
            (zerop (length image)))
    (write-line "Image file name missing." *error-output*)
    (sb-ext:exit :code 1)))

(format t "Creating SBCL image file: ~A~%~A ~A~%~%"
        (nth 1 sb-ext:*posix-argv*)
        (lisp-implementation-type)
        (lisp-implementation-version))

(handler-case
    (let ((home (user-homedir-pathname))
          (pwd *default-pathname-defaults*))
      (flet ((probe-load (path &optional (default home))
               (let ((path (merge-pathnames path default)))
                 (when (probe-file path) (load path)))))
        (or ;; (probe-load #p"quicklisp/setup.lisp" home)
            ;; (probe-load #p".quicklisp/setup.lisp" home)
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

(pushnew :interactive *features*)

(handler-case (ql:quickload (nth 1 sb-ext:*posix-argv*))
  (sb-sys:interactive-interrupt ()
    (sb-ext:exit :code 1))
  (error (c)
    (format *error-output* "~&~A~%" c)
    (sb-ext:exit :code 1)))

;; (push (lambda ()
;;         (asdf:initialize-source-registry)
;;         (asdf:initialize-output-translations))
;;       sb-ext:*init-hooks*)

(sb-ext:save-lisp-and-die
 (nth 1 sb-ext:*posix-argv*)
 :executable t
 :toplevel (lambda ()
             (arviointi::main (script:argv))
             (sb-ext:exit))
 :save-runtime-options t
 :compression (and (member :sb-core-compression *features*) t))
