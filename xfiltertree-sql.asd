(asdf:defsystem xfiltertree-sql
  :depends-on ("xfiltertree-bom" "xfiltertree-fql" "fql-sql" "sqlite")
  :components ((:file "xfiltertree-sql")))
