(asdf:defsystem eqvalg
  :components ((:file "eqvalg"))
  :in-order-to ((test-op (test-op :eqvalg/test))))

(asdf:defsystem eqvalg/test
  :depends-on ("fiveam" "eqvalg")
  :components ((:file "eqvalg-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :eqvalg :eqvalg/test))))
