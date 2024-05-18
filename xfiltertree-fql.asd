(asdf:defsystem xfiltertree-fql
  :depends-on ("xfiltertree" "fql" "fql-util")
  :components ((:file "xfiltertree-fql-xform"))
  :in-order-to ((test-op (test-op :xfiltertree-fql/test))))

(asdf:defsystem xfiltertree-fql/test
  :depends-on ("fiveam" "xfiltertree-fql")
  :components ((:file "xfiltertree-fql-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :xfiltertree-fql :xfiltertree-fql/test))))

