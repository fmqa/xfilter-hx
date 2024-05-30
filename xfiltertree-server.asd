(asdf:defsystem xfiltertree-server
  :depends-on ("fql"
               "xfiltertree-bom"
               "xfiltertree-eqvalg"
               "xfiltertree-html"
               "endpoints-sql"
               "hunchentoot"
               "sql-client-sqlite"
               "eqvalg-sql")
  :components ((:file "xfiltertree-server-package")
               (:file "xfiltertree-server-helpers"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-sql"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-fnt"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-endpoint"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-static"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server-routes"
                :depends-on ("xfiltertree-server-helpers"
                             "xfiltertree-server-fnt"
                             "xfiltertree-server-endpoint"))
               (:file "xfiltertree-server-config"
                :depends-on ("xfiltertree-server-package"))
               (:file "xfiltertree-server"
                :depends-on ("xfiltertree-server-config"
                             "xfiltertree-server-static"
                             "xfiltertree-server-routes"))))
