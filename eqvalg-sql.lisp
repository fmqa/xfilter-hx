(defpackage eqvalg-sql
  (:use :cl)
  (:export
   #:*join*
   #:sqlize
   #:sqlize-aggregation))
(in-package :eqvalg-sql)

(defparameter *join* nil)

(defgeneric sqlize (obj))

(defun duplicate-single-quotes (string)
  (with-output-to-string (out)
    (loop for char across string
          do (progn (when (eq #\' char) (write-char char out))
                    (write-char char out)))))

(defun join-tables (tables)
  (loop with joiners = nil
        for left = (car tables) then (car rest)
        for rest = (cdr tables) then (cdr rest)
        while rest
        do (loop for right in rest
                 for join = (cdr (or (assoc (cons left right) *join* :test #'equal)
                                     (assoc (cons right left) *join* :test #'equal)))
                 do (when join (push join joiners)))
        finally (return joiners)))

(defmethod sqlize ((obj string))
  (format nil "'~A'" (duplicate-single-quotes obj)))

(defmethod sqlize ((obj number))
  (format nil "~A" obj))

(defmethod sqlize ((obj eqvalg:column))
  (format nil "`~A`.`~A`" (eqvalg:column-table obj) (eqvalg:column-name obj)))

(defmethod sqlize ((obj eqvalg:equality))
  (format nil "~A = ~A" (sqlize (eqvalg:equality-left obj)) (sqlize (eqvalg:equality-right obj))))

(defmethod sqlize ((obj eqvalg:membership))
  (format nil "~A IN (~{~A~^,~})"
          (sqlize (eqvalg:membership-operand obj))
          (mapcar #'sqlize (eqvalg:membership-collection obj))))

(defmethod sqlize ((obj eqvalg:conjunction))
  (format nil "~{~A~^ AND ~}" (mapcar #'sqlize (eqvalg:conjunction-operands obj))))

(defun subject-tables (subject)
  (cond ((eqvalg:column-p subject) (list (eqvalg:column-table subject)))
        (t (mapcar #'eqvalg:column-table subject))))

(defun sqlize-aggregation (obj)
  (setf obj (reduce #'eqvalg:coalesce
                    (join-tables (remove-duplicates (subject-tables (eqvalg:subject obj))))
                    :initial-value obj))
  (format nil "SELECT COUNT(*) FROM ~{~A~^, ~} WHERE ~A"
          (remove-duplicates (subject-tables (eqvalg:subject obj)) :test #'equal)
          (sqlize obj)))
