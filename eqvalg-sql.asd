(asdf:defsystem eqvalg-sql
  :depends-on ("eqvalg")
  :components ((:file "eqvalg-sql"))
  :in-order-to ((test-op (test-op :eqvalg-sql/test))))

(asdf:defsystem eqvalg-sql/test
  :depends-on ("eqvalg-sql")
  :components ((:file "eqvalg-sql-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :eqvalg-sql :eqvalg-sql/test))))
