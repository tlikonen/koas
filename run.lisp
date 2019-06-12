;; sbcl --script run.lisp ...

(require "asdf")

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory (merge-pathnames "build/src/"))))

(handler-case
    (asdf:operate 'asdf:monolithic-load-bundle-op "koas")
  (serious-condition (c)
    (format *error-output* "~A~%" c)
    (sb-ext:exit :code 1)))

(koas:start)
