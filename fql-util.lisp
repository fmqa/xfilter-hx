(defpackage fql-util
  (:use :cl)
  (:export
   #:expr-first-table
   #:group-first-table
   #:expr-table-names
   #:group-table-names))
(in-package :fql-util)

(defun expr-first-table (expression)
  "Finds the first table term in the expression"
  (find-if #'consp expression))

(defun group-first-table (group)
  "Finds the first table term in the expression list"
  (loop for expression in group
        for subject = (expr-first-table expression)
        until subject
        finally (return subject)))

(defun expr-table-names (expression)
  (destructuring-bind (operator . operands) expression
    (declare (ignore operator))
    (delete-duplicates
     (loop for operand in operands
           when (consp operand) collect (car operand))
     :test #'equal)))

(defun group-table-names (group)
  (delete-duplicates
   (apply #'append (mapcar #'expr-table-names group))
   :test #'equal))
