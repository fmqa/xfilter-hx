(asdf:defsystem xfiltertree-server
  :depends-on ("webstr" "fql" "xfiltertree-html" "xfiltertree-sql" "hunchentoot")
  :components ((:file "xfiltertree-server")))
