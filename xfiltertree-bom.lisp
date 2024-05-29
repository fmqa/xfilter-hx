(defpackage xfiltertree-bom
  (:use :cl)
  (:export
   #:make-event-type-node
   #:make-connection-status-node
   #:make-endpoint-node
   #:make-singleton-endpoint-node
   #:make-tree
   #:make-marking-node))
(in-package :xfiltertree-bom)

(defun make-event-type-node ()
  (make-instance
   'xfiltertree:aggregation
   :id (eqvalg:column-of "event" "type")
   :bins (list (list (eqvalg:equality-of (eqvalg:column-of "event" "type") "PHONE_CALL")
                     (list "ALL"))
               (list (eqvalg:equality-of (eqvalg:column-of "event" "type") "E_MAIL")
                     (list "ALL")))))

(defun make-connection-status-node ()
  (make-instance
   'xfiltertree:aggregation
   :id (cons (eqvalg:column-of "event" "connectionStatus")
             (eqvalg:strict-equality-of (eqvalg:column-of "event" "type") "PHONE_CALL"))
   :bins (list (list (eqvalg:conjunction-of
                      (eqvalg:equality-of
                       (eqvalg:column-of "event" "connectionStatus") "ACCEPTED")
                      (eqvalg:strict-equality-of (eqvalg:column-of "event" "type") "PHONE_CALL"))
                     (list "ALL"))
               (list (eqvalg:conjunction-of
                      (eqvalg:equality-of
                       (eqvalg:column-of "event" "connectionStatus") "REJECTED")
                      (eqvalg:strict-equality-of (eqvalg:column-of "event" "type") "PHONE_CALL"))
                     (list "ALL")))))

(defun make-endpoint-node (&optional bins)
  (make-instance
   'xfiltertree:dynamic
   :id (eqvalg:column-of "endpoint" "uri")
   :searcher "/endpoints/search"
   :querier "/endpoints/query"
   :bins bins))

(defun make-marking-node ()
  (make-instance
   'xfiltertree:auto
   :id (eqvalg:column-of "marking" "mark")
   :limit 10))

(defun make-singleton-endpoint-node (name aggregations)
  (make-endpoint-node (list (cons name (mapcar #'list aggregations)))))

(defun make-tree ()
  (make-instance
   'xfiltertree:node
   :id "basic"
   :children (list (make-event-type-node)
                   (make-connection-status-node)
                   (make-endpoint-node)
                   (make-marking-node))))
