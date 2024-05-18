(defpackage xfiltertree-fql
  (:use :cl)
  (:export
   #:queries
   #:collect-node-queries
   #:constrain-tree-queries
   #:nodecolp))
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

(defun find-table (pred expressions table)
  (find-if
   (lambda (expr)
     (and (funcall pred (car expr))
          (equal table
                 (fql-util:expr-first-table expr))))
   expressions))

(defun call-with-table (pred func expressions table)
  (let ((found (find-table pred expressions table)))
    (when found
      (funcall func found table))))

(defun eqinp (obj)
  (or (eq :EQ obj)
      (eq :IN obj)))

(defun combine-eq-in (constraints expression)
  (call-with-table
   #'eqinp
   (lambda (constraint table)
     (setf
      ;; Set CAR to :IN
      (car constraint)
      :IN
      ;; Set CDR to all union of all non-table operands in the
      ;; constraint and the expression to be combined with it
      (cdr constraint)
      (cons table
            (delete-duplicates
             (append (remove table (cdr constraint) :test #'equal)
                     (remove table (cdr expression) :test #'equal))))))
   constraints
   (fql-util:expr-first-table expression)))

(defun combine-expression (constraints expression)
  (unless (and (eqinp (car expression)) (combine-eq-in constraints expression))
    (push expression constraints))
  constraints)

(defun combine-constraints (constraints additional)
  (loop for additional-expression in additional
        unless (member additional-expression constraints :test #'equal)
          do (setf constraints
                   (combine-expression constraints additional-expression))
        finally (return constraints)))

(defun nodecolp (node column)
  (equal column
         (car (fql:parse-column (xfiltertree:node-name node)))))

(defun constrain-tree-queries (tree constraint-groups)
  (loop with node-aggregation-groups = (collect-node-queries tree)
        for conjunction-constraint in constraint-groups
        for subject = (fql-util:group-first-table conjunction-constraint)
        when subject do
          (loop for (node . aggregation-groups) in node-aggregation-groups
                unless (nodecolp node subject)
                  do (loop for node-conjunctions-aggregations in aggregation-groups
                           for node-conjunctions = (car node-conjunctions-aggregations)
                           do (setf (car node-conjunctions-aggregations)
                                    (combine-constraints node-conjunctions conjunction-constraint))))
        finally (return node-aggregation-groups)))
