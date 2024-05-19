(defpackage xfiltertree-eqvalg
  (:use :cl)
  (:export
   #:constrain
   #:extend))
(in-package :xfiltertree-eqvalg)

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
             (xfiltertree:aggregation-map
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
