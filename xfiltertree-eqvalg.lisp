(defpackage xfiltertree-eqvalg
  (:use :cl)
  (:export
   #:constrain
   #:extend
   #:populate-aggregations
   #:compute-aggregations))
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
     (loop with node-subject = (if (eqvalg:column-p (xfiltertree:node-id node))
                                   (xfiltertree:node-id node)
                                   (car (xfiltertree:node-id node)))
           for (clause . bins) in dynamic
           for clause-subject = (eqvalg:subject clause)
           do (progn (when (consp clause-subject)
                       (setf clause-subject (car (last clause-subject))))
                     (when (equalp node-subject clause-subject)
                       (push (cons clause (mapcar #'list bins))
                             (xfiltertree:aggregation-bins node))))))
   tree)
  tree)

(defun populate-aggregations (tree distinct)
  (xfiltertree:traverse-if
   #'xfiltertree:auto-p
   (lambda (node)
     ;; Assign aggregations. The aggregations are constructed as equality
     ;; predicates of the form COLUMN=X, where each X is a distinct value
     ;; of COLUMN
     (setf (xfiltertree:aggregation-bins node)
           (if (consp (xfiltertree:node-id node))
               ;; If id is a conditional column of the form (COLUMN . CONDITION)
               ;; then the resulting aggregation clauses should be
               ;; COLUMN=X & CONDITION.
               (mapcar
                (lambda (distinct)
                  (list (eqvalg:conjunction-of
                         (eqvalg:equality-of (car (xfiltertree:node-id node)) distinct)
                         (cdr (xfiltertree:node-id node)))
                        (list "ALL")))
                (funcall (funcall distinct (car (xfiltertree:node-id node)))
                         ; where
                         (cdr (xfiltertree:node-id node))
                         ; offset
                         (xfiltertree:auto-offset node)
                         ; limit
                         (and (xfiltertree:auto-limit node)
                              (1+ (xfiltertree:auto-limit node)))))
               ;; Otherwise, if the id is a simple column, the resulting
               ;; aggregation clauses should be of the form COLUMN=X.
               (mapcar
                (lambda (distinct)
                  (list (eqvalg:equality-of (xfiltertree:node-id node) distinct)
                        (list "ALL")))
                (funcall (funcall distinct (xfiltertree:node-id node))
                         ; where
                         nil
                         ; offset
                         (xfiltertree:auto-offset node)
                         ; limit
                         (and (xfiltertree:auto-limit node)
                              (1+ (xfiltertree:auto-limit node)))))))
     ;; Update pagination information if a limit exists
     (when (xfiltertree:auto-limit node)
       (if (> (length (xfiltertree:aggregation-bins node)) (xfiltertree:auto-limit node))
           (setf (xfiltertree:aggregation-bins node)
                 (nbutlast (xfiltertree:aggregation-bins node))
                 ;; Set offset to offset+limit, i.e. boundary of the next page
                 (xfiltertree:auto-offset node)
                 (+ (or (xfiltertree:auto-offset node) 0) (xfiltertree:auto-limit node)))
           ;; NIL out paging information if no next page exists
           (setf (xfiltertree:auto-next node) nil))))
   tree)
  tree)

(defun compute-aggregations (tree cardinalities)
  "For all AGGREGATION nodes in TREE, compute aggregation counts for all bins
   based on an SQL data store with the given QUERY function"
  (let (expressions aggregations)
    (xfiltertree:traverse-if
     ;; Visit aggregation nodes only
     #'xfiltertree:aggregation-p
     ;; Collect aggregation predicate expressions,
     ;; aggregation bin CONSes into lists
     (lambda (node)
       (xfiltertree:aggregation-foreach
        (lambda (id bins)
          (push id expressions)
          (push bins aggregations))
        node))
     tree)
    ;; Determine cardinality of EQVALG predicate expressions, assign
    ;; cardinalities to counts
    (loop with counts = (funcall cardinalities expressions)
          for (count pair) in (mapcar #'cons counts aggregations)
          do (setf (cdr pair) count))
    tree))
