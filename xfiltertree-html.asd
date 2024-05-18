(asdf:defsystem xfiltertree-html
  :serial t
  :depends-on ("uiop" "cl-who" "xfiltertree" "webstr" "fql")
  :components ((:file "xfiltertree-html-package")
               (:file "xfiltertree-html")
               (:file "xfiltertree-html-forms"))
  :in-order-to ((test-op (test-op :xfiltertree-html/test))))

(asdf:defsystem xfiltertree-html/test
  :depends-on ("fiveam" "xfiltertree-html")
  :components ((:file "xfiltertree-html-forms-test"))
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :xfiltertree-html :xfiltertree-html/test))))
