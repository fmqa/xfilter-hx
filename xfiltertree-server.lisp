(in-package :xfiltertree-server)

(defparameter *acceptor*
  (make-instance
   'hunchentoot:easy-acceptor
   :address (or (uiop:getenv "SERVER_ADDRESS") "127.0.0.1")
   :port (let ((port (uiop:getenv "SERVER_PORT"))) (if port (parse-integer port) 8080))))
