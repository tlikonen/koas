(defparameter *sbcl* (nth 0 sb-ext:*posix-argv*))
(defparameter *lib* (sb-ext:native-pathname (nth 1 sb-ext:*posix-argv*)))

(load "asdf.conf")
(load "quicklisp/setup.lisp")
(ql:quickload "koas")

(asdf:operate 'asdf:monolithic-deliver-asd-op "koas")

(with-open-file (f "build/koas" :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* 78)
          (*print-case* :downcase))
      (format f "#!~A --script~%" *sbcl*)
      (format f "~{~S~%~}"
              (list '(require "asdf")
                    (list 'asdf:initialize-source-registry
                          (list 'quote
                                (list :source-registry
                                      :ignore-inherited-configuration
                                      (list :directory *lib*))))
                    '(asdf:operate 'asdf:monolithic-load-bundle-op "koas")
                    '(koas:start))))))