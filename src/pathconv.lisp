;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:pathconv
  (:use #:cl)
  (:shadow #:pathname
           #:namestring
           #:file-namestring
           #:directory-namestring
           #:enough-namestring)
  (:export #:pathname
           #:namestring
           #:file-namestring
           #:directory-namestring
           #:enough-namestring))

(in-package #:pathconv)

(defun pathname-as-file (pathname)
  (cond
    ((pathname-name pathname)
     pathname)
    ((equal (pathname-directory pathname) '(:absolute))
     pathname)
    (t (let* ((directory (pathname-directory pathname))
              (name-type (sb-ext:native-pathname (first (last directory)))))
         (merge-pathnames
          name-type (make-pathname :directory (butlast directory)))))))

(defun pathname-as-directory (pathname)
  (if (not (pathname-name pathname))
      pathname
      (make-pathname
       :directory
       (append (or (pathname-directory pathname)
                   (list :relative))
               (list (sb-ext:native-namestring
                      (make-pathname :name (pathname-name pathname)
                                     :type (pathname-type pathname)))))
       :name nil :type nil :defaults pathname)))

(defun pathname-as-type (pathname type)
  (cond ((null type) pathname)
        ((eql :file type)
         (pathname-as-file pathname))
        ((eql :directory type)
         (pathname-as-directory pathname))
        (t (error "Invalid target type ~A." type))))

(defgeneric pathname (pathspec &key type)
  (:documentation
   "Convert PATHSPEC to a PATHNAME object and return it.

PATHSPEC is taken literally without interpreting any wild characters
like *?[].

If the optional TYPE argument is NIL (the default) the returned
PATHNAME's last component is of the same type as the source
PATHSPEC (file or directory).

IF the TYPE argument is :FILE the last component of PATHSPEC will be
converted to a file name component even if it was a directory component
in PATHSPEC.

If the TYPE argument is :DIRECTORY the last component of PATHSPEC will
be converted to a directory component and there will be no file name
component."))

(defmethod pathname ((pathspec cl:pathname) &key type)
  (pathname-as-type pathspec type))

(defmethod pathname ((pathspec string) &key type)
  (pathname-as-type (sb-ext:native-pathname pathspec) type))

(defgeneric namestring (pathspec &key type))

(defmethod namestring ((pathspec cl:pathname) &key type)
  (sb-ext:native-namestring (pathname-as-type pathspec type)))

(defmethod namestring ((pathspec string) &key type)
  (namestring (pathname pathspec :type type)))

(defgeneric file-namestring (pathspec)
  (:documentation
   "Return PATHSPEC's last component as a file name string.

The last component of PATHSPEC is interpreted as a file name and it is
returned as a string."))

(defmethod file-namestring ((pathspec cl:pathname))
  (setf pathspec (pathname-as-file pathspec))
  (sb-ext:native-namestring
   (make-pathname :name (pathname-name pathspec)
                  :type (pathname-type pathspec))))

(defmethod file-namestring ((pathspec string))
  (file-namestring (pathname pathspec)))

(defgeneric directory-namestring (pathspec)
  (:documentation
   "Return PATHSPEC as a directory string.

The last component of PATHSPEC is interpreted as a file name and is
stripped. All the preceeding components are returned as a directory name
string."))

(defmethod directory-namestring ((pathspec cl:pathname))
  (setf pathspec (pathname-as-file pathspec))
  (namestring (make-pathname :directory (pathname-directory pathspec))))

(defmethod directory-namestring ((pathspec string))
  (directory-namestring (pathname pathspec)))

(defgeneric enough-namestring (pathspec &optional defaults))

(defmethod enough-namestring
    ((pathspec cl:pathname) &optional (defaults *default-pathname-defaults*))
  (cl:enough-namestring (namestring pathspec) defaults))

(defmethod enough-namestring
    ((pathspec string) &optional (defaults *default-pathname-defaults*))
  (cl:enough-namestring pathspec defaults))
