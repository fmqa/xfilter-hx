(in-package :xfiltertree-server)

(defparameter *static-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (format nil "~Awww/" (uiop:getcwd))))

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (hunchentoot:redirect "/static/index.html"))

(unless (member *static-dispatcher* hunchentoot:*dispatch-table*)
  (push *static-dispatcher* hunchentoot:*dispatch-table*))
