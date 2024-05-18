(defpackage xfiltertree-bom
  (:use :cl)
  (:export
   #:make-event-type-node
   #:make-connection-status-node
   #:make-endpoint-node
   #:make-tree))
(in-package :xfiltertree-bom)

(defun make-event-type-node ()
  (make-instance
   'xfiltertree:aggregation
   :name "event.type"
   :bins (list (list "event.type=PHONE_CALL" (list "ALL"))
               (list "event.type=E_MAIL" (list "ALL")))))

(defun make-connection-status-node ()
  (make-instance
   'xfiltertree:aggregation
   :name "event[type=PHONE_CALL].connectionStatus"
   :bins (list (list "event[type=PHONE_CALL].connectionStatus=ACCEPTED" (list "ALL"))
               (list "event[type=PHONE_CALL].connectionStatus=REJECTED" (list "ALL")))))

(defun make-endpoint-node ()
  (make-instance
   'xfiltertree:dynamic
   :name "endpoint.uri"
   :searcher "/endpoints/search"
   :querier "/endpoints/query"))

(defun make-tree ()
  (make-instance
   'xfiltertree:node
   :name "basic"
   :children (list (make-event-type-node)
                   (make-connection-status-node)
                   (make-endpoint-node))))
