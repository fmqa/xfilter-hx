(defpackage xfiltertree-server
  (:use :cl)
  (:export
   #:*acceptor*))
(in-package :xfiltertree-server)

(defparameter *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port 8080))

(defparameter *static-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (format nil "~Awww/" (uiop:getcwd))))

(defun respond-with-filter-tree (clauses update)
  (let ((tree (xfiltertree-sql:compute-aggregations #'xfiltertree-bom:make-tree
                                                    (mapcar #'car clauses)))
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
     (update :parameter-type 'boolean))
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (respond-with-filter-tree (mapcar #'parse-filter-bin-clause clause)
                               update))))

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

(hunchentoot:define-easy-handler (endpoint-query-route :uri "/endpoints/query") (q)
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (let ((endpoint (xfiltertree-sql:endpoint-by-uri q)))
       (if endpoint
           (destructuring-bind (id uri) endpoint
             (declare (ignore id))
             (let ((name (format nil "endpoint.uri='~A'" uri)))
               (xfiltertree-html::htmlize-dynamic-bin name '(("ALL" . 0))))))))))

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (hunchentoot:redirect "/static/index.html"))

(unless (member *static-dispatcher* hunchentoot:*dispatch-table*)
  (push *static-dispatcher* hunchentoot:*dispatch-table*))
