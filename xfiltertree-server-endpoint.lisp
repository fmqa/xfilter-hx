(in-package :xfiltertree-server)

(defun endpoint-search-html (needle)
  (let ((result (endpoints-sql:endpoint-search #'sql-query-multi needle)))
    (when result
      (uiop:reduce/strcat
       (mapcar
        (lambda (row)
          (destructuring-bind (id uri) row
            (declare (ignore id))
            (cl-who:with-html-output-to-string (s)
              (:option
               :value (cl-who:escape-string uri)
               :data-key (cl-who:escape-string
                          (fql:stringify
                           (eqvalg:equality-of
                            (eqvalg:column-of "endpoint" "uri") uri)))))))
        result)))))

(defun endpoint-filter-html (key clauses)
  (let ((endpoint (endpoints-sql:endpoint-by-uri #'sql-query key)))
    (when endpoint
      (let* ((name (eqvalg:equality-of (eqvalg:column-of "endpoint" "uri") key))
             (tree (xfiltertree-bom:make-singleton-endpoint-node name '("ALL"))))
        (compute-aggregations
         (xfiltertree-eqvalg:constrain (xfiltertree:copy-node tree)
                                       (mapcar #'car clauses)))
        (let ((xfiltertree-html:*translate* #'fql:stringify))
          (xfiltertree-html:htmlize-dynamic-bins tree))))))
