(defpackage fql-util
  (:use :cl)
  (:export
   #:expr-first-table
   #:group-first-table
   #:expr-table-names
   #:group-table-names
   #:constrain-tree-queries))
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

(defun single-quote (obj)
  (cond ((numberp obj) obj)
        (t (format nil "'~A'" (remove #\' obj)))))

(defun conjunction-table-column (conjunction)
  (let ((equality (car conjunction))
        (discriminator (cadr conjunction)))
    (when (and (eq :EQ (car equality))
               (or (not discriminator)
                   (eq :STRICT-EQ (car discriminator))))
      (let* ((table-pair (fql-util:expr-first-table equality))
             (table (car table-pair))
             (column (cdr table-pair)))
        (if discriminator
            (destructuring-bind (left right) (cdr discriminator)
              (let* ((d-table-pair (if (consp left) left (when (consp right) right)))
                     (d-table (car d-table-pair))
                     (d-column (cdr d-table-pair))
                     (d-table-operand (if (eq d-table-pair left) right left)))
                (when (equal table d-table)
                  (format nil "~A[~A=~A]"
                          table d-column (single-quote d-table-operand)))))
            (format nil "~A.~A" table column))))))

(defun conjunction-name (conjunction)
  (let ((equality (car conjunction))
        (discriminator (cadr conjunction)))
    (when (and (eq :EQ (car equality))
               (or (not discriminator)
                   (eq :STRICT-EQ (car discriminator))))
      (let* ((table-pair (fql-util:expr-first-table equality))
             (table (car table-pair))
             (column (cdr table-pair))
             (name (format nil "~A.~A" table column))
             (equality-operand (destructuring-bind (left right) (cdr equality)
                                 (if (eq table-pair left) right left))))
        (if discriminator
            (destructuring-bind (left right) (cdr discriminator)
              (let* ((d-table-pair (if (consp left) left (when (consp right) right)))
                     (d-table (car d-table-pair))
                     (d-column (cdr d-table-pair))
                     (d-table-operand (if (eq d-table-pair left) right left)))
                (when (equal table d-table)
                  (format nil "~A[~A=~A]=~A"
                          table d-column (single-quote d-table-operand)
                          (single-quote equality-operand)))))
            (format nil "~A=~A" name (single-quote equality-operand)))))))
