(in-package :xfiltertree-server)

(defun endpoint-search-html (needle)
  (uiop:reduce/strcat
   (mapcar
    (lambda (row)
      (destructuring-bind (id uri) row
        (declare (ignore id))
        (cl-who:with-html-output-to-string (s)
          (:option :value (cl-who:escape-string uri)))))
    (xfiltertree-sql:endpoint-search needle))))

(defun endpoint-filter-html (key clauses)
  (let ((endpoint (xfiltertree-sql:endpoint-by-uri key)))
    (when endpoint
      (let* ((name (eqvalg:equality-of (eqvalg:column-of "endpoint" "uri") key))
             (tree (xfiltertree-bom:make-singleton-endpoint-node name '("ALL"))))
        (let ((eqvalg-sql:*join* *default-join*))
          (xfiltertree-sql:compute-aggregations
           (xfiltertree-eqvalg:constrain (xfiltertree:copy-node tree)
                                         (mapcar #'car clauses))))
        (let ((xfiltertree-html:*translate* #'fql:stringify))
          (xfiltertree-html:htmlize-dynamic-bins tree))))))
