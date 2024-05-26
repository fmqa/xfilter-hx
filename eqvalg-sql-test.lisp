(defpackage eqvalg-sql/test
  (:use :cl :fiveam))
(in-package :eqvalg-sql/test)

(def-suite eqvalg-sql :description "EQVALG-to-SQL translation module")

(def-suite* sqlize :in eqvalg-sql)

(test string-sqlization
  (is (equal "''" (eqvalg-sql:sqlize "")))
  (is (equal "'abc'" (eqvalg-sql:sqlize "abc")))
  (is (equal "'they''re'" (eqvalg-sql:sqlize "they're"))))

(test number-sqlization
  (is (equal "-3" (eqvalg-sql:sqlize -3)))
  (is (equal "-3.1" (eqvalg-sql:sqlize -3.1)))
  (is (equal "1" (eqvalg-sql:sqlize 1)))
  (is (equal "1.5" (eqvalg-sql:sqlize 1.5))))

(test column-sqlization
  (is (equal "`table`.`column`" (eqvalg-sql:sqlize (eqvalg:column-of "table" "column")))))
