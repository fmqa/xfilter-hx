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
        for value = (cdr pair)
        for discriminator = (char key 0)
        if (eq #\$ discriminator)
          do (setf value (string-downcase value)) and
          when (member value '("true" "t" "1" "on") :test #'equal)
            collect (string-upcase (subseq key 1)) into directives
          end
        else
          collect pair into clauses
        finally (return (values clauses (make-directives directives)))))

(defun respond-with-filter-tree (clauses directives)
  (let* ((parsed-clauses (xfiltertree-html:parse-filter-clauses clauses))
         (hier-clauses (xfiltertree-html:sort-filter-clauses parsed-clauses))
         (tree (xfiltertree-sql:query-aggregation-tree hier-clauses)))
    (let ((xfiltertree-html:*form-post* (hunchentoot:request-uri*))
          (xfiltertree-html:*form-update* (funcall directives :update)))
      (xfiltertree-html:htmlize tree))))

(defun allow-methods (allowed-methods handler &optional ondisallowed)
  (if (member (hunchentoot:request-method*) allowed-methods)
      (funcall handler)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+HTTP-METHOD-NOT-ALLOWED+)
        (and ondisallowed (funcall ondisallowed)))))

(hunchentoot:define-easy-handler (fnt-route :uri "/fnt") ()
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (multiple-value-call #'respond-with-filter-tree
       (collect-form-parameters (hunchentoot:post-parameters*))))))

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
             (let* ((name (format nil "endpoint.uri=~A" uri))
                    (escaped (webstr:escape name)))
               (cl-who:with-html-output-to-string (s)
                 (:fieldset
                  :id (format nil "fieldset--~A" escaped)
                  :data-leaf "true"
                  (:legend (cl-who:str name))
                  (:input :type "hidden" :name (cl-who:escape-string (format nil "@~A" name))
                          :value "add")
                  (:input :id escaped
                          :type "checkbox"
                          :name name
                          :value "ALL")
                  (:label :id (format nil "label--~A" escaped)
                          :for escaped
                          (:span :data-bin "ALL" (cl-who:str "0")))
                  (:button :type "button" :|hx-on:click| (cl-who:escape-string "((elt, id) => { if (!elt) return; const attrs = elt.getAttribute('hx-select-oob'); if (!attrs) return; elt.setAttribute('hx-select-oob', attrs.split(',').filter(attr => attr !== id).join(',')); this.parentElement.remove(); })(htmx.closest(this, '[hx-select-oob]'), '#' + this.parentElement.id)") "x")
                  (:script
                   (cl-who:str
                    (format nil "((selector) => { const elt = htmx.find(selector); if (!elt || !elt.parentElement) return; const attr = elt.parentElement.getAttribute('hx-select-oob'); (attr && attr.split(',').includes(selector)) || elt.parentElement.setAttribute('hx-select-oob', attr ? attr + ',' + selector : selector); })('#fieldset--~A')" escaped))))))))))))

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (hunchentoot:redirect "/static/index.html"))

(unless (member *static-dispatcher* hunchentoot:*dispatch-table*)
  (push *static-dispatcher* hunchentoot:*dispatch-table*))
