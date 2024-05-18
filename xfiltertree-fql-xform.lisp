(defpackage xfiltertree-fql
  (:use :cl)
  (:export
   #:constrain
   #:extend))
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

(defun related-subjectp (column subject)
  (equalp column
          (if (listp subject) (car (last subject)) subject)))

(defun relatedp (column expression)
  (related-subjectp column (eqvalg:subject expression)))

(defun constrain (tree constraints)
  (xfiltertree:traverse-if
   #'xfiltertree:aggregation-p
   (lambda (node)
     (let* ((column (if (eqvalg:column-p (xfiltertree:node-id node))
                        (xfiltertree:node-id node)
                        (car (xfiltertree:node-id node))))
            (applicable (remove-if (lambda (cst) (relatedp column cst)) constraints)))
       (setf (xfiltertree:aggregation-bins node)
             (xfiltertree:aggregation-map-id
              (lambda (expression aggregations)
                (cons (reduce #'eqvalg:coalesce applicable
                              :initial-value expression)
                      aggregations))
              node))))
   tree)
  tree)

(defun extend (tree dynamic)
  (xfiltertree:traverse-if
   #'xfiltertree:dynamic-p
   (lambda (node)
     (loop for (clause . bins) in dynamic
           for clause-subject = (eqvalg:subject clause)
           for node-subject = (xfiltertree:node-id node)
           do (progn (when (consp node-subject)
                       (setf node-subject (car node-subject)))
                     (when (consp clause-subject)
                       (setf clause-subject (car (last clause-subject))))
                     (when (equalp node-subject clause-subject)
                       (push (cons clause (mapcar #'list bins))
                             (xfiltertree:aggregation-bins node))))))
   tree)
  tree)
