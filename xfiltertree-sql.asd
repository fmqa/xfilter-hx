(asdf:defsystem xfiltertree-sql
  :depends-on ("xfiltertree-bom" "xfiltertree-eqvalg" "eqvalg-sql" "sqlite")
  :components ((:file "xfiltertree-sql")))
