(asdf:defsystem fql
  :depends-on ("esrap" "eqvalg")
  :serial t
  :components ((:file "fql-package")
               (:file "fql-parse")
               (:file "fql-output"))
  :in-order-to ((test-op (test-op :fql/test))))

(asdf:defsystem fql/test
  :depends-on ("fiveam" "fql")
  :components ((:file "fql-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :fql :fql/test))))
