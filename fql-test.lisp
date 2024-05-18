(defpackage fql/test
  (:use :cl :fiveam))
(in-package :fql/test)

(def-suite fql :description "Filter query DSL test")

(def-suite* parsing :in fql)

(test clauses
  (is (equal '((:eq "table.column" 1))
             (fql:parse-filter "table.column=1")))
  (is (equal '((:eq "table.column" "x"))
             (fql:parse-filter "table.column=x")))
  (is (equal '((:eq "table.column" 15.1))
             (fql:parse-filter "table.column=15.1")))
  (is (equal '((:eq "table.column" -2))
             (fql:parse-filter "table.column=-2")))
  (is (equal '((:eq "table.column" -2.5))
             (fql:parse-filter "table.column=-2.5")))
  (is (equal '((:eq "table.column" 46) (:strict-eq "table.var" "x"))
             (fql:parse-filter "table[var=x].column=46")))
  (is (equal '((:eq "table.column" 46) (:strict-eq "table.var" 11))
             (fql:parse-filter "table[var=11].column=46")))
  (is (equal '((:eq "table.column" 46) (:strict-eq "table.var" 11.8))
             (fql:parse-filter "table[var=11.8].column=46")))
  (is (equal '((:eq "table.column" 46) (:strict-eq "table.var" -17))
             (fql:parse-filter "table[var=-17].column=46")))
  (is (equal '((:eq "table.column" 46) (:strict-eq "table.var" -17.3))
             (fql:parse-filter "table[var=-17.3].column=46"))))
