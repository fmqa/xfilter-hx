(defpackage xfiltertree-sql
  (:use :cl)
  (:export
   #:compute-aggregations
   #:populate-aggregations))
(in-package :xfiltertree-sql)

(defun populate-aggregations (tree)
  (xfiltertree:traverse-if
   #'xfiltertree:auto-p
   (lambda (node)
     ;; Assign aggregations. The aggregations are constructed as equality
     ;; predicates of the form COLUMN=X, where each X is a distinct value
     ;; of COLUMN
     (setf (xfiltertree:aggregation-bins node)
           (mapcar
            ;; If id is a conditional column of the form (COLUMN . CONDITION)
            ;; then the resulting aggregation clauses should be
            ;; COLUMN=X & CONDITION. Otherwise, it should just be COLUMN=X.
            ;;
            ;; Select the projection function for MAPCAR dynamically by using
            ;; CONSP to check whether the id is a pair indicating a conditional
            ;; column
            (if (consp (xfiltertree:node-id node))
                (lambda (distinct)
                  (list (eqvalg:conjunction-of
                         (eqvalg:equality-of (car (xfiltertree:node-id node)) distinct)
                         (cdr (xfiltertree:node-id node)))
                        (list "ALL")))
                (lambda (distinct)
                  (list (eqvalg:equality-of (xfiltertree:node-id node) distinct)
                        (list "ALL"))))
            (funcall query (if (consp (xfiltertree:node-id node))
                               (funcall (eqvalg-query:distinct (car (xfiltertree:node-id node)))
                                        (cdr (xfiltertree:node-id node))
                                        (xfiltertree:auto-offset node)
                                        (and (xfiltertree:auto-limit node)
                                             (1+ (xfiltertree:auto-limit node))))
                               (funcall (eqvalg-query:distinct (xfiltertree:node-id node))
                                        nil
                                        (xfiltertree:auto-offset node)
                                        (and (xfiltertree:auto-limit node)
                                          (1+ (xfiltertree:auto-limit node))))))))
     ;; NIL out the pagination info if we're below the pagination limit
     (when (xfiltertree:auto-limit node)
       (if (> (length (xfiltertree:aggregation-bins node)) (xfiltertree:auto-limit node))
           (setf (xfiltertree:aggregation-bins node)
                 (nbutlast (xfiltertree:aggregation-bins node))
                 (xfiltertree:auto-offset node)
                 (+ (or (xfiltertree:auto-offset node) 0) (xfiltertree:auto-limit node)))
           (setf (xfiltertree:auto-next node) nil))))
   tree)
  tree)

(defun compute-aggregations (tree)
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
    (loop with row = (eqvalg-query:cardinality expressions)
          for (count pair) in (mapcar #'cons row aggregations)
          do (setf (cdr pair) count))
    tree))
