(in-package :xfiltertree-server)

(defparameter *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port 8080))

(defparameter *static-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (format nil "~Awww/" (uiop:getcwd))))

(defun allow-methods (allowed-methods handler &optional ondisallowed)
  (if (member (hunchentoot:request-method*) allowed-methods)
      (funcall handler)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+HTTP-METHOD-NOT-ALLOWED+)
        (and ondisallowed (funcall ondisallowed)))))

(defun parse-filter-bin-clause (clause)
  (destructuring-bind (filter &rest options) (fql:parse-filter-with-options clause)
    (cons filter (list (cdr (assoc "bin" options :test #'equal))))))

(hunchentoot:define-easy-handler (fnt-route :uri "/fnt")
    ((clause :parameter-type 'list)
     (update :parameter-type 'boolean)
     (dynamic :parameter-type 'list))
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (filter-navigation-tree-html
      (mapcar #'parse-filter-bin-clause clause)
      :update update
      :dynamic (mapcar #'parse-filter-bin-clause dynamic)))))

(hunchentoot:define-easy-handler (endpoint-search-route :uri "/endpoints/search") (q)
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (endpoint-search-html q))))

(hunchentoot:define-easy-handler (endpoint-query-route :uri "/endpoints/query")
    ((key :real-name "q")
     (clause :parameter-type 'list))
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (endpoint-filter-html key (mapcar #'parse-filter-bin-clause clause)))))

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (hunchentoot:redirect "/static/index.html"))

(unless (member *static-dispatcher* hunchentoot:*dispatch-table*)
  (push *static-dispatcher* hunchentoot:*dispatch-table*))
