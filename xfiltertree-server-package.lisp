(defpackage xfiltertree-server
  (:use :cl)
  (:export
   #:*acceptor*))

(in-package :xfiltertree-server)

(defparameter *default-join*
  (list (cons '("event" . "endpoint")
              (eqvalg:conjunction-of
               (eqvalg:equality-of (eqvalg:column-of "egression" "egressor")
                                   (eqvalg:column-of "endpoint" "id"))
               (eqvalg:equality-of (eqvalg:column-of "egression" "egressed")
                                   (eqvalg:column-of "event" "id"))))))

(defun log-sql (sql parameters)
  (hunchentoot:log-message* :INFO "Performing SQL query: ~A ~@[with parameters: ~A~]" sql parameters))