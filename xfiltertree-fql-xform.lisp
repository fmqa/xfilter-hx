(defpackage xfiltertree-fql
  (:use :cl)
  (:export
   #:queries
   #:collect-node-queries
   #:constrained-tree))
(in-package :xfiltertree-fql)

(defun quote-value (value)
  (cond ((numberp value) value)
        (t (format nil "'~A'" (remove #\' value)))))

(defmethod xfiltertree:translate ((id eqvalg:column))
  (format nil "~A.~A" (eqvalg:column-table id) (eqvalg:column-name id)))

(defmethod xfiltertree:translate ((id eqvalg:equality))
  (format nil "~A=~A"
          (xfiltertree:translate (eqvalg:equality-left id))
          (quote-value (xfiltertree:translate (eqvalg:equality-right id)))))

(defmethod xfiltertree:translate ((id eqvalg:conjunction))
  (destructuring-bind (eqlt discriminator) (eqvalg:conjunction-operands id)
    (if (and (eqvalg:equality-p eqlt)
             (eqvalg:equality-p discriminator)
             (eqvalg:equality-strict discriminator))
        (multiple-value-bind (discriminator-subject discriminator-target)
            (eqvalg:subject discriminator)
          (multiple-value-bind (eqlt-subject eqlt-target) (eqvalg:subject eqlt)
            (if (equal (eqvalg:column-table eqlt-subject)
                       (eqvalg:column-table discriminator-subject))
                (format nil "~A[~A=~A].~A=~A"
                        (eqvalg:column-table eqlt-subject)
                        (eqvalg:column-name discriminator-subject)
                        discriminator-target
                        (eqvalg:column-name eqlt-subject)
                        (quote-value eqlt-target))))))))

(defmethod xfiltertree:translate ((id cons))
  (destructuring-bind (eqlt-subject . discriminator) id
    (multiple-value-bind (discriminator-subject discriminator-target)
        (eqvalg:subject discriminator)
      (if (equal (eqvalg:column-table eqlt-subject)
                 (eqvalg:column-table discriminator-subject))
          (format nil "~A[~A=~A].~A"
                  (eqvalg:column-table eqlt-subject)
                  (eqvalg:column-name discriminator-subject)
                  discriminator-target
                  (eqvalg:column-name eqlt-subject))))))

(defgeneric queries (node))

(defmethod queries ((node xfiltertree:node)))

(defmethod queries ((node xfiltertree:aggregation))
  (xfiltertree:aggregation-bins node))

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
  (equalp column
          (if (listp subject) (car (last subject)) subject)))

(defun relatedp (column expression)
  (related-subjectp column (eqvalg:subject expression)))

(defun constrained-tree (tree constraints)
  (loop for (node . unconstrained) in (collect-node-queries tree)
        for column = (if (eqvalg:column-p (xfiltertree:node-id node))
                         (xfiltertree:node-id node)
                         (car (xfiltertree:node-id node)))
        for applicable = (remove-if (lambda (cst) (relatedp column cst)) constraints)
        for constrained = (loop for (expression . aggregations) in unconstrained
                                collect (cons (reduce #'eqvalg:coalesce
                                                      applicable
                                                      :initial-value expression)
                                              aggregations))
        collect (cons node constrained)))
