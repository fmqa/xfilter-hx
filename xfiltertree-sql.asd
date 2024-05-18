(asdf:defsystem xfiltertree-sql
  :depends-on ("xfiltertree-bom" "xfiltertree-fql" "eqvalg-sql" "sqlite")
  :components ((:file "xfiltertree-sql")))
