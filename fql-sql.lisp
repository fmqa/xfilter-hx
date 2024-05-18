(defpackage fql-sql
  (:use :cl)
  (:export
   #:sql-quote
   #:sqlize-term
   #:sqlize-expression
   #:sqlize-conjugate-group
   #:sqlize-count-conjoined-group
   #:sqlize-aggregated-conjoined-group
   #:sqlize-aggregated-conjoined-groups
   #:sqlize-aggregated-conjoined-group-list))
(in-package :fql-sql)

(defparameter *substitutions* nil)

(defun duplicate-single-quotes (string)
  (with-output-to-string (out)
    (loop for char across string
          do (progn (when (eq #\' char) (write-char char out))
                    (write-char char out)))))

(defun substitute-table (name)
  (or (cdr (assoc name *substitutions* :test #'equal)) name))

(defun sql-quote (string)
  (format nil "'~A'" (duplicate-single-quotes string)))

(defun sqlize-table (pair)
  (format nil "~A.~A" (substitute-table (car pair)) (cdr pair)))

(defun sqlize-term (term)
  (cond
    ((consp term) (sqlize-table term))
    ((stringp term) (sql-quote term))
    (t term)))

(defun sqlize-expression (expression)
  (destructuring-bind (operator . operands) expression
    (ecase operator
      ((:EQ :STRICT-EQ) (destructuring-bind (left right) operands
                         (format nil "~A = ~A" (sqlize-term left) (sqlize-term right))))
      (:IN (destructuring-bind (container . contained) operands
             (format nil "~A IN (~{~A~^,~})"
                     (sqlize-table container)
                     (mapcar #'sqlize-term contained)))))))

(defun sqlize-conjugate-group (group)
  (format nil "~{~A~^ AND ~}" (mapcar #'sqlize-expression group)))

(defun sqlize-count-conjoined-group (group)
  (format nil "SELECT COUNT(*) FROM ~A WHERE ~A"
          (format nil "~{~A~^, ~}"
                  (mapcar #'substitute-table
                          (fql-util:group-table-names group)))
          (sqlize-conjugate-group group)))

(defun sqlize-aggregated-conjoined-group (agroup)
  (destructuring-bind (group . aggs) agroup
    (apply
     #'append
     (mapcar
      (lambda (agg)
        (cond
          ((equal "ALL" (car agg))
           (cons (sqlize-count-conjoined-group group)
                 (lambda (count) (setf (cdr agg) count))))
          (t (error "Unsupported aggregation: ~A" (car agg)))))
      aggs))))

(defun sqlize-aggregated-conjoined-groups (group)
  (mapcar #'sqlize-aggregated-conjoined-group group))

(defun sqlize-aggregated-conjoined-group-list (groups)
  (apply #'append (mapcar #'sqlize-aggregated-conjoined-groups groups)))
