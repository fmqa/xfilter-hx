(in-package :xfiltertree-server)

(defparameter *acceptor*
  (make-instance
   'hunchentoot:easy-acceptor
   :address (server-address)
   :port (server-port)))
