(asdf:defsystem webstr
  :components ((:file "webstr"))
  :in-order-to ((test-op (test-op :webstr/test))))

(asdf:defsystem webstr/test
  :depends-on ("fiveam" "webstr")
  :components ((:file "webstr-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :webstr :webstr/test))))
