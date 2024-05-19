(asdf:defsystem xfiltertree-sql
  :depends-on ("xfiltertree" "eqvalg-sql" "sqlite")
  :components ((:file "xfiltertree-sql")))
