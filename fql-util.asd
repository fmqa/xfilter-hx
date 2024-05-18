(asdf:defsystem fql-util
  :components ((:file "fql-util"))
  :in-order-to ((test-op (test-op :fql-util/test))))

(asdf:defsystem fql-util/test
  :depends-on ("fiveam" "fql-util")
  :components ((:file "fql-util-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :fql-util :fql-util/test))))

