(asdf:defsystem xfiltertree-server
  :depends-on ("webstr" "xfiltertree-html" "xfiltertree-sql" "hunchentoot")
  :components ((:file "xfiltertree-server")))
