(defpackage xfiltertree-server
  (:use :cl)
  (:export
   #:*acceptor*))
(in-package :xfiltertree-server)

(defparameter *default-join*
  '((("event" . "endpoint")
     ("egression")
     (:EQ ("egression" . "egressor") ("endpoint" . "id"))
     (:EQ ("egression" . "egressed") ("event" . "id")))))

(defparameter *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port 8080))

(defparameter *static-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (format nil "~Awww/" (uiop:getcwd))))

(defun respond-with-filter-tree (clauses update &optional dynamic)
  (let ((tree (let ((fql-sql:*join* *default-join*))
                 (xfiltertree-sql:compute-aggregations #'xfiltertree-bom:make-tree
                                                       (mapcar #'car clauses))))
        (xfiltertree-html:*form-post* (hunchentoot:request-uri*))
        (xfiltertree-html:*form-update* update))
    (xfiltertree-html:htmlize tree)))

(defun allow-methods (allowed-methods handler &optional ondisallowed)
  (if (member (hunchentoot:request-method*) allowed-methods)
      (funcall handler)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+HTTP-METHOD-NOT-ALLOWED+)
        (and ondisallowed (funcall ondisallowed)))))

(defun parse-filter-bin-clause (clause)
  (destructuring-bind (filter &rest options) (fql:parse-filter-with-options clause)
    (cons filter (cdr (assoc "bin" options :test #'equal)))))

(hunchentoot:define-easy-handler (fnt-route :uri "/fnt")
    ((clause :parameter-type 'list)
     (update :parameter-type 'boolean)
     (dynamic :parameter-type 'list))
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (prin1 (mapcar #'parse-filter-bin-clause dynamic))
     (respond-with-filter-tree (mapcar #'parse-filter-bin-clause clause)
                               update dynamic))))

(hunchentoot:define-easy-handler (endpoint-search-route :uri "/endpoints/search") (q)
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (uiop:reduce/strcat
      (mapcar
       (lambda (row)
         (destructuring-bind (id uri) row
           (declare (ignore id))
           (cl-who:with-html-output-to-string (s)
             (:option :value (cl-who:escape-string uri)))))
       (xfiltertree-sql:endpoint-search q))))))

(hunchentoot:define-easy-handler (endpoint-query-route :uri "/endpoints/query")
    ((endpoint-uri :real-name "q")
     (clause :parameter-type 'list))
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (let ((clauses (mapcar #'parse-filter-bin-clause clause))
           (endpoint (xfiltertree-sql:endpoint-by-uri endpoint-uri)))
       (when endpoint
         (let* ((name (format nil "endpoint.uri='~A'" endpoint-uri))
                (tree (let ((fql-sql:*join* *default-join*))
                        (xfiltertree-sql:compute-aggregations
                         (lambda ()
                           (xfiltertree-bom:make-singleton-endpoint-node
                            name
                            '("ALL")))
                         (mapcar #'car clauses)))))
           (xfiltertree-html:htmlize-dynamic-bins (xfiltertree:aggregation-bins tree))))))))

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (hunchentoot:redirect "/static/index.html"))

(unless (member *static-dispatcher* hunchentoot:*dispatch-table*)
  (push *static-dispatcher* hunchentoot:*dispatch-table*))
