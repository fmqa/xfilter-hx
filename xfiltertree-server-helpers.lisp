(in-package :xfiltertree-server)

(defun allow-methods (allowed-methods handler &optional ondisallowed)
  (if (member (hunchentoot:request-method*) allowed-methods)
      (funcall handler)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+HTTP-METHOD-NOT-ALLOWED+)
        (and ondisallowed (funcall ondisallowed)))))

(defun no-content-unless (value)
  (or value
      (progn (setf (hunchentoot:return-code*) hunchentoot:+HTTP-NO-CONTENT+)
             value)))

(defun parse-filter-bin-clause (clause)
  (destructuring-bind (filter &rest options) (fql:parse-filter-with-options clause)
    (cons filter (list (cdr (assoc "bin" options :test #'equal))))))
