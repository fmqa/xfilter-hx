(defpackage endpoints-sql
  (:use cl)
  (:export
   #:endpoint-search
   #:endpoint-by-uri))
(in-package :endpoints-sql)

(defun wordp (char)
  (or (alphanumericp char) (member char '(#\_ #\. #\: #\Space))))

(defun endpoint-search (text &optional (limit 200))
  (setf text (remove-if (lambda (chr) (not (wordp chr))) text))
  (if (zerop (length text))
      ;; Omit LIKE clause if TEXT is empty
      (sql-db:call-with-db
       #'sql-db:execute-to-list
       "SELECT * FROM endpoint LIMIT ?"
       limit)
      ;; Include LIKE clause if TEXT is non-empty
      (sql-db:call-with-db
       #'sql-db:execute-to-list
       "SELECT * FROM endpoint WHERE uri LIKE ? LIMIT ?"
       (format nil "%~A%" text)
       limit)))

(defun endpoint-by-uri (uri)
  (car (sql-db:call-with-db
        #'sql-db:execute-to-list
        "SELECT * FROM endpoint WHERE uri = ?"
        uri)))
