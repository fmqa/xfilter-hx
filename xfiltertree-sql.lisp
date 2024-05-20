(defpackage xfiltertree-sql
  (:use :cl)
  (:export
   #:compute-aggregations))
(in-package :xfiltertree-sql)

(defun compute-aggregations (tree)
  (let (expressions aggregations)
    (xfiltertree:traverse-if
     #'xfiltertree:aggregation-p
     (lambda (node)
       (xfiltertree:aggregation-foreach
        (lambda (id bins)
          (push id expressions)
          (push bins aggregations))
        node))
     tree)
    (loop with selections = (mapcar #'eqvalg-sql:sqlize-aggregation expressions)
          with statement = (format nil "SELECT ~{(~A)~^, ~}" selections)
          with row = (multiple-value-list
                      (sql-db:call-with-db #'sql-db:execute-one-row-m-v statement))
          for (count pair) in (mapcar #'cons row aggregations)
          do (setf (cdr pair) count))
      tree))
