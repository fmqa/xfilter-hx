(defpackage eqvalg-sql
  (:use :cl)
  (:export
   #:*join*
   #:sqlize
   #:sqlize-count
   #:sqlize-distinct))
(in-package :eqvalg-sql)

(defparameter *join* nil "associative list of column pairs to EQVALG expressions")

(defgeneric sqlize (obj)
  (:documentation "Translate an EQVALG object obj to SQL"))

;; Finds the value associated with (LEFT . RIGHT) in *JOIN*, or if not found,
;; the value associated with (RIGHT . LEFT)
(defun join-table-for (left right)
  (cdr (or (assoc (cons left right) *join* :test #'equal)
           (assoc (cons right left) *join* :test #'equal))))

;; Returns a list of EQVALG expressions joining the given table columns
(defun join-tables (tables)
  (loop with joiners = nil
        for left = (car tables) then (car rest)
        for rest = (cdr tables) then (cdr rest)
        while rest
        do (loop for right in rest
                 for join = (join-table-for left right)
                 do (when join (push join joiners)))
        finally (return joiners)))

(defmethod sqlize ((obj string))
  (with-output-to-string (out)
    (loop initially (write-char #\' out)
          for char across obj
          do (progn (when (eq #\' char) (write-char char out))
                    (write-char char out))
          finally (write-char #\' out))))

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

(defun sqlize-count (obj)
  ;; Fuse the aggregation filter with the joining predicates
  (setf obj (reduce #'eqvalg:coalesce
                    (join-tables (eqvalg:table-names obj))
                    :initial-value obj))
  ;; Construct SQL selection statement from comma-delimited table names in the FROM
  ;; clause, and the stringified aggregation filter in the WHERE clause
  (format nil "SELECT COUNT(*) FROM ~{`~A`~^, ~} WHERE ~A"
          (eqvalg:table-names obj)
          (sqlize obj)))

(defun sqlize-distinct (column &optional where offset limit)
  (format nil "SELECT DISTINCT `~A` FROM `~A`~@[ WHERE ~A~]~@[ LIMIT ~A~]~@[ OFFSET ~A~]"
          (eqvalg:column-name column)
          (eqvalg:column-table column)
          (and where (sqlize where))
          limit offset))
