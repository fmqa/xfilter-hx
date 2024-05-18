(defpackage xfiltertree-fql
  (:use :cl)
  (:export
   #:queries
   #:collect-node-queries
   #:constrained-tree))
(in-package :xfiltertree-fql)

(defgeneric queries (node))

(defmethod queries ((node xfiltertree:node)))

(defmethod queries ((node xfiltertree:aggregation))
  (mapcar
   (lambda (column-aggregations)
     (destructuring-bind (column . aggregations) column-aggregations
       (cons (fql:parse-filter column) aggregations)))
   (xfiltertree:aggregation-bins node)))

(defun collect-node-queries (node)
  (let ((collection nil))
    (xfiltertree:traverse
     (lambda (node)
       (let ((ndqueries (queries node)))
         (when ndqueries
           (push (cons node ndqueries) collection))))
     node)
    collection))

(defun related-subjectp (column subject)
  (if (listp subject)
      (equalp column (car (last subject)))
      (equalp column subject)))

(defun relatedp (column expression)
  (related-subjectp column (eqvalg:subject expression)))

(defun constrained-tree (tree constraints)
  (loop for (node . unconstrained) in (collect-node-queries tree)
        for column = (car (fql:parse-column (xfiltertree:node-name node)))
        for applicable = (remove-if (lambda (cst) (relatedp column cst)) constraints)
        for constrained = (loop for (expression . aggregations) in unconstrained
                                collect (cons (reduce #'eqvalg:coalesce
                                                      applicable
                                                      :initial-value expression)
                                              aggregations))
        collect (cons node constrained)))
