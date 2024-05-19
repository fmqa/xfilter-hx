(defpackage xfiltertree-eqvalg/test
  (:use :cl :fiveam))
(in-package :xfiltertree-eqvalg/test)

(def-suite xfiltertree-eqvalg :description "Filter tree transform tests")

(def-suite* transforms :in xfiltertree-eqvalg)

(test constraining
  (let* ((column (eqvalg:column-of "table" "column"))
         (other-column (eqvalg:column-of "other" "column"))
         (equality (eqvalg:equality-of column "X"))
         (other-equality (eqvalg:equality-of other-column 12))
         (bins '(("ALL")))
         (aggregation (cons equality bins))
         (aggregation-bins (list aggregation))
         (node (make-instance 'xfiltertree:aggregation
                              :id column
                              :bins aggregation-bins))
         (related-constraint (eqvalg:equality-of column "Y")))
    ;; No change if identical equality is added as constraint
    (is (equalp aggregation-bins
                (xfiltertree:aggregation-bins
                 (xfiltertree-eqvalg:constrain node (list equality)))))
    ;; No change if equality with same subject but different value is
    ;; added as constraint
    (is (equalp aggregation-bins
                (xfiltertree:aggregation-bins
                 (xfiltertree-eqvalg:constrain node (list related-constraint)))))
    ;; Bin predicate coalesced with equality constraint of differing subject table
    (is (equalp (list (cons (eqvalg:coalesce equality other-equality) bins))
                (xfiltertree:aggregation-bins
                 (xfiltertree-eqvalg:constrain node (list other-equality)))))))

(test extension
  (let* ((column (eqvalg:column-of "table" "column"))
         (node (make-instance 'xfiltertree:dynamic
                             :querier "/q"
                             :searcher "/s"
                             :id column))
         (column-equality (eqvalg:equality-of column "x"))
         (aggregation (list column-equality "ALL")))
    ;; Adds aggregation with related subject table to column
    (is (equalp (list (list column-equality (list "ALL")))
                (xfiltertree:aggregation-bins
                 (xfiltertree-eqvalg:extend node (list aggregation))))))
  (let* ((column (eqvalg:column-of "table" "column"))
         (other-column (eqvalg:column-of "other" "column"))
         (node (make-instance 'xfiltertree:dynamic
                             :querier "/q"
                             :searcher "/s"
                             :id column))
         (other-equality (eqvalg:equality-of other-column "x"))
         (other-aggregation (list other-equality "ALL")))
    ;; Does not add aggregation with unrelated subject table to column
    (is (not (xfiltertree:aggregation-bins
              (xfiltertree-eqvalg:extend node (list other-aggregation)))))))
