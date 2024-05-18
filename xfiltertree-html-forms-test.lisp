(defpackage xfiltertree-html/test
  (:use :cl :fiveam))
(in-package :xfiltertree-html/test)

(def-suite xfiltertree-html :description "Filter tree forms processing tests")

(def-suite* transformation :in xfiltertree-html)

(test clauses
  (is (equal '(("x.y" (:all (:eq 1))))
             (xfiltertree-html:sort-filter-clauses '((:all (:eq "x.y" 1))))))
  (is (equal '(("x.y" (:all (:eq 2 1))))
             (xfiltertree-html:sort-filter-clauses '((:all (:eq "x.y" 1) (:eq "x.y" 2))))))
  (is (equal '(("x.z" (:all (:eq 2))) ("x.y" (:all (:eq 1))))
             (xfiltertree-html:sort-filter-clauses '((:all (:eq "x.y" 1) (:eq "x.z" 2))))))
  (is (equal '(("x.z" (:all (:eq 2))) ("x.y" (:all (:eq 15 1))))
             (xfiltertree-html:sort-filter-clauses '((:all (:eq "x.y" 1) (:eq "x.z" 2) (:eq "x.y" 15)))))))
