(asdf:defsystem xfiltertree-eqvalg
  :depends-on ("xfiltertree" "eqvalg" "eqvalg-query")
  :components ((:file "xfiltertree-eqvalg"))
  :in-order-to ((test-op (test-op :xfiltertree-eqvalg/test))))

(asdf:defsystem xfiltertree-eqvalg/test
  :depends-on ("fiveam" "xfiltertree-eqvalg")
  :components ((:file "xfiltertree-eqvalg-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :xfiltertree-eqvalg :xfiltertree-eqvalg/test))))
