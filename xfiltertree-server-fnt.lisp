(in-package :xfiltertree-server)

(defun filter-navigation-tree-html (clauses &key update dynamic)
  (let ((tree (xfiltertree-eqvalg:extend (xfiltertree-bom:make-tree) dynamic)))
    (let ((eqvalg-sql:*join* *default-join*))
      (xfiltertree-sql:compute-aggregations
       (xfiltertree-eqvalg:constrain (xfiltertree:copy-node tree)
                                     (mapcar #'car clauses))))
    (let ((xfiltertree-html:*form-post* (hunchentoot:request-uri*))
          (xfiltertree-html:*form-update* update)
          (xfiltertree-html:*translate* #'fql:stringify))
      (xfiltertree-html:htmlize tree))))
