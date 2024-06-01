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

(defun node-subject (node)
  "Returns COLUMN if the NODE-ID of NODE is a (COLUMN . DISCRIMINATOR) pair,
   otherwise returns NODE-ID"
  (let ((id (xfiltertree:node-id node)))
    (if (eqvalg:column-p id) id (car id))))

(defun node-where (node)
  "Returns DISCRIMINATOR if the NODE-ID is a (COLUMN . DISCRIMINATOR) pair"
  (let ((id (xfiltertree:node-id node)))
    (when (consp id) (cdr id))))

(defun node-subject-where (node)
  "Returns a COLUMN if NODE-ID is an EQVALG:COLUMN, otherwise returns the
   values DISCRIMINATOR and PAIR if NODE-ID is a pair of the former two"
  (let ((id (xfiltertree:node-id node)))
    (if (eqvalg:column-p id)
        id
        (values (car id) (cdr id)))))

(defun node-equality-of (node to)
  "Return an EQVALG:EQUALITY term of the NODE's column to TO, ANDing
   any conditional clauses should they exist"
  (multiple-value-bind (subject where) (node-subject-where node)
    (if where
        (eqvalg:coalesce (eqvalg:equality-of subject to) where)
        (eqvalg:equality-of subject to))))

(defun constrain (tree constraints)
  "Constrains the EQVALG filters for all aggregations in TREE, coalescing
   them with CONSTRAINTS. This modifies TREE."
  (xfiltertree:traverse-if
   ;; Traverse AGGREGATION nodes only
   #'xfiltertree:aggregation-p
   ;; AGGREGATION node visitor
   (lambda (node)
     ;; Filter out from CONSTRAINTS all constraints related to this
     ;; node's subject column - this is to avoid applying a node's own
     ;; constraints onto itself
     (let* ((subject (node-subject node))
            (applicable (remove-if (lambda (cst) (relatedp subject cst)) constraints)))
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
     ;; between the aggregation's EQVALG object and the NODE's subject
     ;; column is established
     (loop with nsubject = (node-subject node)
           for (clause . bins) in dynamic
           for csubject = (eqvalg:subject clause)
           do (progn (when (consp csubject)
                       (setf csubject (car (last csubject))))
                     (when (equalp nsubject csubject)
                       (push (cons clause (mapcar #'list bins))
                             (xfiltertree:aggregation-bins node))))))
   tree)
  tree)

(defun populate-aggregations (tree distinct)
  "Populate all AUTO nodes in TREE with aggregations based on DISTINCT values
   of the nodes' subject columns. DISTINCT should take a COLUMN and return a
   function taking three optional parameters WHERE OFFSET LIMIT, where WHERE
   is an EQVALG term for filtering and OFFSET, LIMIT control pagination"
  (xfiltertree:traverse-if
   #'xfiltertree:auto-p
   (lambda (node)
     ;; Assign aggregations. The aggregations are constructed as equality
     ;; predicates of the form COLUMN=X, where each X is a distinct value
     ;; of COLUMN
     (setf (xfiltertree:aggregation-bins node)
           (mapcar
            (lambda (distinct)
              (list (node-equality-of node distinct)
                    (list "ALL")))
            (funcall (funcall distinct (node-subject node))
                     ; where
                     (node-where node)
                     ; offset
                     (xfiltertree:auto-offset node)
                     ; limit
                     (and (xfiltertree:auto-limit node)
                          (1+ (xfiltertree:auto-limit node))))))
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
   using a CARDINALITIES function. CARDINALITIES should take a list of terms
   to compute the cardinalities for"
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
