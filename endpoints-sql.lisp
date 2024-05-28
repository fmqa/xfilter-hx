(defpackage endpoints-sql
  (:use cl)
  (:export
   #:endpoint-search
   #:endpoint-by-uri))
(in-package :endpoints-sql)

(defun endpoint-search (query text &optional (limit 200))
  (if (zerop (length text))
      ;; Omit LIKE clause if TEXT is empty
      (funcall query
               "SELECT * FROM endpoint LIMIT ?"
               limit)
      ;; Include LIKE clause if TEXT is non-empty
      (funcall query
               "SELECT * FROM endpoint WHERE uri LIKE ? LIMIT ?"
               (format nil "%~A%" (remove #\% text))
               limit)))

(defun endpoint-by-uri (query uri)
  (funcall query "SELECT * FROM endpoint WHERE uri = ?" uri))
