(asdf:defsystem xfiltertree-server
  :depends-on ("fql" "xfiltertree-bom" "xfiltertree-eqvalg" "xfiltertree-html" "xfiltertree-sql" "hunchentoot")
  :components ((:file "xfiltertree-server")))
