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
  "Constrains the EQVALG filters for all aggregations in TREE, coalescing
   them with CONSTRAINTS. This modifies TREE."
  (xfiltertree:traverse-if
   ;; Traverse AGGREGATION nodes only
   #'xfiltertree:aggregation-p
   ;; AGGREGATION node visitor
   (lambda (node)
     ;; If the ID is an EQVALG column, simply retrieve it. If not,
     ;; assume it is a (COLUMN . DISCRIMINATOR) pair, and retrieve the
     ;; COLUMN (CAR) of the pair.
     (let* ((column (if (eqvalg:column-p (xfiltertree:node-id node))
                        (xfiltertree:node-id node)
                        (car (xfiltertree:node-id node))))
            ;; Filter out from CONSTRAINTS all constraints related to this
            ;; node's column - this is to avoid applying a node's own constraints
            ;; to itself
            (applicable (remove-if (lambda (cst) (relatedp column cst)) constraints)))
       ;; Reassign the node's aggregations after coalescing the filter expression
       ;; with all applicable (non-reflexive) constraints. Note that only the CAR
       ;; of the aggregation bin is changed. The CDR is retained from the original
       ;; aggregation pair.
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
  "Extend dynamic nodes in TREE with dynamic aggregation pairs from DYNAMIC.
   An aggregation pair of the form (AGG . BINS) will only extend a dynamic node
   if the SUBJECT of AGG matches the generalized SUBJECT of the NODE-ID.
   This modifies TREE."
  (xfiltertree:traverse-if
   ;; Traverse DYNAMIC nodes only
   #'xfiltertree:dynamic-p
   ;; DYNAMIC node visitor
   (lambda (node)
     ;; For each DYNAMIC node, loop through all given aggregation pairs
     ;; and push them into the node's AGGREGATION-BINS if compatibility
     ;; between the aggregation's EQVALG object and the NODE-ID is
     ;; established
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
