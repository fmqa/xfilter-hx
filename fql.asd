(asdf:defsystem fql
  :depends-on ("esrap" "eqvalg")
  :components ((:file "fql-parse"))
  :in-order-to ((test-op (test-op :fql/test))))

(asdf:defsystem fql/test
  :depends-on ("fiveam" "fql")
  :components ((:file "fql-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :fql :fql/test))))
