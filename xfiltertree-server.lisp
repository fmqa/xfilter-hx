(defpackage xfiltertree-server
  (:use :cl)
  (:export
   #:*acceptor*))
(in-package :xfiltertree-server)

(defparameter *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port 8080))

(defparameter *static-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (format nil "~Awww/" (uiop:getcwd))))

(hunchentoot:define-easy-handler (fnt-route :uri "/fnt") ()
  (unless (member (hunchentoot:request-method*) '(:HEAD :GET :POST))
    (setf (hunchentoot:return-code*) hunchentoot:+HTTP-METHOD-NOT-ALLOWED+)
    (setf (hunchentoot:content-type*) "text/plain")
    (return-from fnt-route "Invalid Request Method"))
  (let* ((form-parameters (hunchentoot:post-parameters*))
         (lru (assoc "lru" form-parameters :test 'equal))
         (clause-parameters (remove lru form-parameters))
         (string-clauses (xfiltertree-html:unescape-filter-clauses clause-parameters))
         (sorted-clauses (xfiltertree-html:sort-filter-clauses
                          (xfiltertree-html:parse-filter-clauses string-clauses)))
         (tree (xfiltertree-sql:query-aggregation-tree sorted-clauses)))
    ;; Ensure lru is a string
    (if (and lru (not (zerop (length (cdr lru)))))
        (setf lru (cdr lru))
        (setf lru nil))
    ;; Generate HTML form
    (let ((xfiltertree-html:*form-post* (hunchentoot:request-uri*))
          (xfiltertree-html:*form-update* lru))
      (xfiltertree-html:htmlize tree))))

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (hunchentoot:redirect "/static/index.html"))

(unless (member *static-dispatcher* hunchentoot:*dispatch-table*)
  (push *static-dispatcher* hunchentoot:*dispatch-table*))
