(defpackage eqvalg
  (:use :cl)
  (:export
   ;; COLUMN
   #:column
   #:make-column
   #:column-p
   #:copy-column
   #:column-table
   #:column-name
   ;; EQUALITY
   #:equality
   #:make-equality
   #:equality-p
   #:copy-equality
   #:equality-left
   #:equality-right
   #:equality-strict
   ;; MEMBERSHIP
   #:membership
   #:make-membership
   #:membership-p
   #:copy-membership
   #:membership-operand
   #:membership-collection
   ;; CONJUNCTION
   #:conjunction
   #:make-conjunction
   #:conjunction-p
   #:copy-conjunction
   #:conjunction-operands
   #:conjunction-singleton-p
   ;; CONSTRUCTORS
   #:column-of
   #:equality-of
   #:strict-equality-of
   #:membership-of
   #:conjunction-from
   #:conjunction-of
   ;; QUERIES
   #:subject
   ;; TRANSFORMS
   #:coalesce))
(in-package :eqvalg)

(defstruct column table name)

(defstruct equality left right (strict nil))

(defstruct membership operand collection)

(defstruct conjunction operands)

(defun column-of (table name)
  (make-column :table table :name name))

(defun equality-of (left right &optional strict)
  (make-equality :left left :right right :strict strict))

(defun strict-equality-of (left right)
  (equality-of left right t))

(defun membership-of (operand collection)
  (make-membership :operand operand :collection collection))

(defun conjunction-from (operands)
  (make-conjunction :operands operands))

(defun conjunction-of (&rest operands)
  (conjunction-from operands))

(defun conjunction-singleton-p (conj)
  "Returns a true value if CONJ consists of a single element"
  (destructuring-bind (head . tail) (conjunction-operands conj)
    (and (not tail) head)))

(defgeneric coalesce (left right)
  (:documentation "Combines LEFT and RIGHT"))

(defgeneric subject (term)
  (:documentation "Retrieves the subject column of TERM"))

(defmethod subject ((term equality))
  (cond ((column-p (equality-left term)) (values (equality-left term)
                                                 (equality-right term)))
        ((column-p (equality-right term)) (values (equality-right term)
                                                  (equality-left term)))))

(defmethod subject ((term membership))
  (and (column-p (membership-operand term))
       (membership-operand term)))

(defmethod subject ((term conjunction))
  (loop with cols = nil
        for operand in (conjunction-operands term)
        for col = (subject operand)
        do (when (and (column-p col) (not (member col cols :test #'equalp)))
             (push col cols))
        finally (return cols)))

(defmethod coalesce ((left equality) (right equality))
  (multiple-value-bind (left-subject left-target) (subject left)
    (multiple-value-bind (right-subject right-target) (subject right)
      (if (equalp left-subject right-subject)
          (if (equalp left-target right-target)
              (if (eq (equality-strict left) (equality-strict right))
                  left
                  (conjunction-of left right))
              (if (and (not (equality-strict left)) (not (equality-strict right)))
                  (membership-of left-subject (list left-target right-target))
                  (conjunction-of left right)))
          (conjunction-of left right)))))

(defmethod coalesce ((left membership) (right equality))
  (multiple-value-bind (right-subject right-target) (subject right)
    (let ((left-subject (subject left)))
      (if (equality-strict right)
          (if (equalp left-subject right-subject)
              (if (equalp (list right-target) (membership-collection left))
                  left
                  (conjunction-of left right))
              (conjunction-of left right))
          (if (equalp left-subject right-subject)
              (if (member right-target (membership-collection left) :test #'equalp)
                  left
                  (membership-of right-subject
                                 (cons right-target (membership-collection left))))
              (conjunction-of left right))))))

(defmethod coalesce ((left equality) (right membership))
  (coalesce right left))

(defmethod coalesce ((left membership) (right membership))
  (let ((left-subject (subject left))
        (right-subject (subject right)))
    (if (equalp left-subject right-subject)
        (membership-of left-subject (remove-duplicates
                                     (append (membership-collection left)
                                             (membership-collection right))
                                     :test #'equalp))
        (conjunction-of left right))))

(defmethod coalesce ((left conjunction) (right equality))
  (if (conjunction-singleton-p left)
      (coalesce (car (conjunction-operands left)) right)
      (conjunction-from
       (loop for term in (conjunction-operands left)
             for candidate = (coalesce term right)
             when (not (conjunction-p candidate))
               return (substitute candidate term (conjunction-operands left))
             finally (return (cons right (conjunction-operands left)))))))

(defmethod coalesce ((left equality) (right conjunction))
  (coalesce right left))

(defmethod coalesce ((left conjunction) (right membership))
  (reduce #'coalesce
          (conjunction-operands left)
          :initial-value right))

(defmethod coalesce ((right membership) (left conjunction))
  (coalesce left right))

(defmethod coalesce ((left conjunction) (right conjunction))
  (reduce
   #'coalesce
   (remove-duplicates (append (conjunction-operands left)
                              (conjunction-operands right))
                      :test #'equalp)))
