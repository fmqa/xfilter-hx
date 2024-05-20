(asdf:defsystem xfiltertree-server
  :depends-on ("fql"
               "xfiltertree-bom"
               "xfiltertree-eqvalg"
               "xfiltertree-html"
               "endpoints-sql"
               "xfiltertree-sql"
               "hunchentoot")
  :components ((:file "xfiltertree-server-package")
               (:file "xfiltertree-server-helpers"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-fnt"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-endpoint"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-static"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-routes"
                :depends-on ("xfiltertree-server-fnt"
                             "xfiltertree-server-endpoint"))
               (:file "xfiltertree-server"
                :depends-on ("xfiltertree-server-routes"))))
