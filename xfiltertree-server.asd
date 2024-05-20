(asdf:defsystem xfiltertree-server
  :depends-on ("fql" "xfiltertree-bom" "xfiltertree-eqvalg" "xfiltertree-html" "xfiltertree-sql" "hunchentoot")
  :components ((:file "xfiltertree-server-package")
               (:file "xfiltertree-server-fnt"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-endpoint"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server"
                :depends-on ("xfiltertree-server-fnt"
                             "xfiltertree-server-endpoint"))))
