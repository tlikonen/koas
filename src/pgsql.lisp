;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:pgsql
  (:use #:cl)
  (:import-from #:cl-postgres #:database-connection)
  (:export
   #:connect #:reconnect #:disconnect #:connectedp #:connectionp
   #:database-connection
   #:query #:query-1 #:query-nconc
   #:with-connection #:with-transaction
   #:*query-reader* #:line-reader #:line-reader-null
   ))

(in-package #:pgsql)

(defun connect (database user password host &key (port 5432))
  (cl-postgres:open-database database user password host port))

(defun reconnect (connection)
  (cl-postgres:reopen-database connection))

(defun disconnect (connection)
  (cl-postgres:close-database connection))

(defun connectedp (connection)
  (cl-postgres:database-open-p connection))

(defun connectionp (connection)
  (typep connection 'cl-postgres:database-connection))

(cl-postgres:def-row-reader line-reader (fields)
  (loop :while (cl-postgres:next-row)
        :collect (loop :for field :across fields
                       :for f := (cl-postgres:next-field field)
                       :collect (if (eql f :null) nil f))))

(cl-postgres:def-row-reader line-reader-null (fields)
  (loop :while (cl-postgres:next-row)
        :collect (loop :for field :across fields
                       :for f := (cl-postgres:next-field field)
                       :collect f)))

(defvar *query-reader* 'line-reader)

(defun query (connection fmt &rest args)
  (cl-postgres:exec-query connection
                          (apply #'format nil fmt args)
                          *query-reader*))

(defun query-1 (connection fmt &rest args)
  (caar (apply #'query connection fmt args)))

(defun query-nconc (connection fmt &rest args)
  (reduce #'nconc (apply #'query connection fmt args)))

(defmacro with-connection ((var database user password host &key (port 5432))
                           &body body)
  (let ((connection (gensym "CONNECTION")))
    `(let* ((,connection (connect ,database ,user ,password
                                  ,host :port ,port))
            (,var ,connection))
       (declare (ignorable ,var))
       (unwind-protect (progn ,@body)
         (disconnect ,connection)))))

(defmacro with-transaction (connection &body body)
  (let ((conn (gensym "CONNECTION"))
        (commit (gensym "COMMIT")))
    `(let ((,conn ,connection)
           (,commit nil))
       (query ,conn "BEGIN TRANSACTION")
       (unwind-protect (multiple-value-prog1 (progn ,@body)
                         (setf ,commit t))
         (query ,conn (if ,commit
                          "COMMIT TRANSACTION"
                          "ROLLBACK TRANSACTION"))))))
