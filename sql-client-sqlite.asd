(asdf:defsystem sql-client-sqlite
  :depends-on ("sql-client" "sqlite")
  :components ((:file "sql-client-sqlite")))
