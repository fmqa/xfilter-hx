(defpackage xfiltertree-server
  (:use :cl)
  (:export
   #:*acceptor*))
(in-package :xfiltertree-server)

(defparameter *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port 8080))

(defparameter *static-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (format nil "~Awww/" (uiop:getcwd))))

(defun make-directives (haystack)
  (lambda (needle)
    (member (if (symbolp needle) (symbol-name needle) (string-upcase needle))
            haystack
            :test #'equal)))

(defun collect-form-parameters (parameters)
  (loop for pair in parameters
        for key = (car pair)
        for discriminator = (char key 0)
        if (eq #\$ discriminator)
          for value = (string-downcase (cdr pair))
          when (member value '("true" "t" "1" "on") :test #'equal)
            collect (string-upcase (subseq key 1)) into directives
          end
        else
          collect (cons key value) into clauses
        finally (return (values clauses (make-directives directives)))))

(defun respond-with-filter-tree (clauses directives)
  (let* ((parsed-clauses (xfiltertree-html:parse-filter-clauses clauses))
         (hier-clauses (xfiltertree-html:sort-filter-clauses parsed-clauses))
         (tree (xfiltertree-sql:query-aggregation-tree hier-clauses)))
    (let ((xfiltertree-html:*form-post* (hunchentoot:request-uri*))
          (xfiltertree-html:*form-update* (funcall directives :update)))
      (xfiltertree-html:htmlize tree))))

(defun allow-methods (allowed-methods handler &optional result)
  (if (member (hunchentoot:request-method*) allowed-methods)
      (funcall handler)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+HTTP-METHOD-NOT-ALLOWED+)
        result)))

(hunchentoot:define-easy-handler (fnt-route :uri "/fnt") ()
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (multiple-value-call #'respond-with-filter-tree
       (collect-form-parameters (hunchentoot:post-parameters*))))))

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (hunchentoot:redirect "/static/index.html"))

(unless (member *static-dispatcher* hunchentoot:*dispatch-table*)
  (push *static-dispatcher* hunchentoot:*dispatch-table*))
