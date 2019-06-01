(require 'asdf)
(asdf:disable-output-translations)
(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory *default-pathname-defaults*)
       (list :tree (merge-pathnames "quicklisp/dists/"))))

(asdf:load-system "koas")

(sb-ext:save-lisp-and-die
 "koas"
 :executable t
 :toplevel #'koas:start
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
