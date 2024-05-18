(in-package :xfiltertree-html)

(defun sort-filter-clauses (clauses)
  "Sort filter clauses into a tree of the form
  ((attribute (mode (op args...) ...) ...) ...)"
  (loop with tree = nil
        for (mode . triples) in clauses
        do (loop for (operator column value) in triples
                 for cell = (or (assoc column tree :test #'equal)
                                (let ((head (cons column nil)))
                                  (setf tree (cons head tree))
                                  head))
                 for modal = (or (assoc mode (cdr cell))
                                 (let ((head (cons mode nil)))
                                   (setf (cdr cell) (cons head (cdr cell)))
                                   head))
                 for op = (or (assoc operator (cdr modal))
                              (let ((head (cons operator nil)))
                                (setf (cdr modal) (cons head (cdr modal)))
                                head))
                 do (unless (member value (cdr op) :test #'equal)
                      (setf (cdr op) (cons value (cdr op)))))
        finally (return tree)))
