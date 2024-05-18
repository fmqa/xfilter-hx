(defpackage xfiltertree-bom
  (:use :cl)
  (:export
   #:event-type
   #:phone-call-connection-status
   #:consume-event-type
   #:consume-phone-call-connection-status
   #:consume-aggregation-tree))
(in-package :xfiltertree-bom)

(defun event-type (phonecalls emails)
  (make-instance
   'xfiltertree:aggregation
   :name "event.type"
   :bins `(("event.type=PHONE_CALL" . (("ALL" . ,phonecalls)))
           ("event.type=E_MAIL" . (("ALL" . ,emails))))))

(defun phone-call-connection-status (accepted rejected)
  (make-instance
   'xfiltertree:aggregation
   :name "event[type=PHONE_CALL].connectionStatus"
   :bins `(("event[type=PHONE_CALL].connectionStatus=ACCEPTED" . (("ALL" . ,accepted)))
           ("event[type=PHONE_CALL].connectionStatus=REJECTED" . (("ALL" . ,rejected))))))

(defun consume-event-type (consume)
  "Make an event type aggregation from calls to the given function CONSUME.
   The values from each CONSUME call fill the aggregation bins in the following order:
   - PHONE_CALL
   - E_MAIL"
  (event-type (funcall consume) (funcall consume)))

(defun consume-phone-call-connection-status (consume)
  "Make an connection status aggregation from calls to the given function CONSUME.
   The values from each CONSUME call fill the aggregation bins in the following order:
   - ACCEPTED
   - REJECTED"
  (phone-call-connection-status (funcall consume) (funcall consume)))

(defun consume-aggregation-tree (consume)
  "Make an aggregation tree from calls to the given function CONSUME.
   The values from each CONSUME call fill the aggregation bins in the following order:
   - type=PHONE_CALL
   - type=E_MAIL
   - connectionStatus=ACCEPTED
   - connectionStatus=REJECTED"
  (make-instance
   'xfiltertree:node
   :name "Basic"
   :items (list (consume-event-type consume)
                (consume-phone-call-connection-status consume))))
