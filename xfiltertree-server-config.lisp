(in-package :xfiltertree-server)

(defun server-address (&optional (default "127.0.0.1"))
  (or (uiop:getenv "SERVER_ADDRESS") default))

(defun server-port (&optional (default 8081))
  (let ((variable (uiop:getenv "SERVER_PORT")))
    (if variable
        (parse-integer variable)
        default)))
