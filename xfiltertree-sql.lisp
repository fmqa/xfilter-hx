(defpackage xfiltertree-sql
  (:use :cl)
  (:export
   #:compute-aggregations))
(in-package :xfiltertree-sql)

(defun compute-aggregations (tree query)
  "For all AGGREGATION nodes in TREE, compute aggregation counts for all bins
   based on an SQL data store with the given QUERY function"
  (let (expressions aggregations)
    (xfiltertree:traverse-if
     ;; Visit aggregation nodes only
     #'xfiltertree:aggregation-p
     ;; Collect aggregation predicate expressions,
     ;; aggregation bin CONSes into lists
     (lambda (node)
       (xfiltertree:aggregation-foreach
        (lambda (id bins)
          (push id expressions)
          (push bins aggregations))
        node))
     tree)
    ;; Convert EQVALG predicate expressions to SQL statements computing the
    ;; count of items that fulfill each expression.
    (loop with selections = (mapcar #'eqvalg-sql:sqlize-aggregation expressions)
          with statement = (format nil "SELECT ~{(~A)~^, ~}" selections)
          with row = (funcall query statement)
          for (count pair) in (mapcar #'cons row aggregations)
          do (setf (cdr pair) count))
      tree))
