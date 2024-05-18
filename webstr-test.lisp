(defpackage webstr/test
  (:use :cl :fiveam))
(in-package :webstr/test)

(def-suite webstr :description "Web string utilities test")

(def-suite* integration :in webstr)

(test inversibility
  (is (equal "a.b.c[x-1]=10" (webstr:unescape (webstr:escape "a.b.c[x-1]=10")))))
