(defpackage eqvalg/test
  (:use :cl :fiveam))
(in-package :eqvalg/test)

(def-suite eqvalg :description "Equivalency alegbra test")

(def-suite* subjects :in eqvalg)

(test equality-subject-of-column
  (let ((column (eqvalg:column-of "table" "column"))
        (otherc (eqvalg:column-of "abc" "def")))
    ;; Returns the left side of the equality if it is a column
    (is (equalp column
                (eqvalg:subject (eqvalg:equality-of column 12))))
    ;; Returns the right side of the equality if it is a column
    (is (equalp column
                (eqvalg:subject (eqvalg:equality-of 12 column))))
    ;; Returns both sides of the equality if they are both columns
    (is (equalp (list column otherc)
                (eqvalg:subject (eqvalg:equality-of column otherc))))))

(test equality-subject-no-columns
  ;; Returns nil if no columns are in the equality
  (is (not (eqvalg:subject (eqvalg:equality-of 12 17)))))

(test membership-subject
  (let ((column (eqvalg:column-of "table" "column")))
    ;; Returns the membership operand if it is a column
    (is (equalp column
                (eqvalg:subject (eqvalg:membership-of column (list 12)))))))

(test membership-subject-no-columns
  (is (not (eqvalg:subject (eqvalg:membership-of 12 (list 11 12))))))

(test conjunction-subject
  (let ((column-a (eqvalg:column-of "table" "column"))
        (column-b (eqvalg:column-of "abcde" "fghijk")))
    ;; Returns the column of the singleton conjunction's equality
    (is (equalp (list column-a)
                (eqvalg:subject (eqvalg:conjunction-of (eqvalg:equality-of column-a "x")))))
    ;; Returns the columns of a multi-element conjunction of equalities
    (is (equalp (list column-b column-a)
                (eqvalg:subject (eqvalg:conjunction-of (eqvalg:equality-of column-a "x")
                                                       (eqvalg:equality-of "y" column-b)))))
    ;; Ignores non-column conjunction terms
    (is (equalp (list column-a)
                (eqvalg:subject (eqvalg:conjunction-of (eqvalg:equality-of column-a "x")
                                                       (eqvalg:equality-of "y" "x")))))
    ;; Removes duplicates
    (is (equalp (list column-a)
                (eqvalg:subject (eqvalg:conjunction-of (eqvalg:equality-of column-a "x")
                                                       (eqvalg:equality-of "y" column-a)))))))

(test table-names
  (is (equal (list "table")
             (eqvalg:table-names (eqvalg:column-of "table" "column"))))
  (is (equal (list "taba" "tabb")
             (eqvalg:table-names (eqvalg:equality-of (eqvalg:column-of "taba" "cola")
                                                     (eqvalg:column-of "tabb" "colb")))))
  (is (equal (list "table")
             (eqvalg:table-names (eqvalg:membership-of (eqvalg:column-of "table" "column")
                                                       '(1 2)))))
  (is (equal (list "taba" "tabb")
             (eqvalg:table-names (eqvalg:conjunction-of
                                  (eqvalg:equality-of (eqvalg:column-of "taba" "cola")
                                                      (eqvalg:column-of "tabb" "colb"))))))
  (is (equal (list "tabc" "taba" "tabb")
             (eqvalg:table-names (eqvalg:conjunction-of
                                  (eqvalg:equality-of (eqvalg:column-of "taba" "cola")
                                                      (eqvalg:column-of "tabb" "colb"))
                                  (eqvalg:equality-of (eqvalg:column-of "tabc" "def")
                                                      "value")))))
  (is (equal (list "tabc" "tabd" "taba" "tabb")
             (eqvalg:table-names (eqvalg:conjunction-of
                                  (eqvalg:equality-of (eqvalg:column-of "taba" "cola")
                                                      (eqvalg:column-of "tabb" "colb"))
                                  (eqvalg:equality-of (eqvalg:column-of "tabc" "def")
                                                      (eqvalg:column-of "tabd" "xxx"))))))
  (is (equal (list "taba" "tabb")
             (eqvalg:table-names (eqvalg:conjunction-of
                                  (eqvalg:equality-of (eqvalg:column-of "taba" "cola")
                                                      (eqvalg:column-of "tabb" "colb"))
                                  (eqvalg:equality-of (eqvalg:column-of "taba" "yyy")
                                                      (eqvalg:column-of "tabb" "xxx")))))))

(def-suite* coalescing :in eqvalg)

(test coalesce-equalities
  (let ((column-a-b (eqvalg:column-of "a" "b"))
        (column-x-y (eqvalg:column-of "x" "y")))
    ;; AND-joins together two disjunct equalities
    ;;
    ;; COALESCE(X=1, Y=1) -> X=1 & Y=1
    (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of column-a-b  "1")
                                       (eqvalg:equality-of column-x-y "2"))
                (eqvalg:coalesce (eqvalg:equality-of column-a-b "1")
                                 (eqvalg:equality-of column-x-y "2")))))
  (let ((column (eqvalg:column-of "table" "column")))
    ;; Coalesces identical equalities to the same equality
    ;;
    ;; COALESCE(X=1,X=1) -> X=1
    (is (equalp (eqvalg:equality-of column 22)
                (eqvalg:coalesce (eqvalg:equality-of column 22)
                                 (eqvalg:equality-of column 22))))
    ;; ANDs equalities with same left/right sides that differ in strictness
    ;;
    ;; COALESCE (X=1,X==1) -> X=1 & X==1
    ;; COALESCE (X==1,X=1) -> X==1 & X=1
    (is (equalp (eqvalg:conjunction-of (eqvalg:strict-equality-of column 12)
                                       (eqvalg:equality-of column 12))
                (eqvalg:coalesce (eqvalg:strict-equality-of column 12)
                                 (eqvalg:equality-of column 12))))
    (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of column 12)
                                       (eqvalg:strict-equality-of column 12))
                (eqvalg:coalesce (eqvalg:equality-of column 12)
                                 (eqvalg:strict-equality-of column 12))))
    ;; Coalesces equalities with the same subject column into
    ;; a set membership
    ;;
    ;; COALESCE(X=1,X=2) -> X IN (1,2)
    (is (equalp (eqvalg:membership-of column '(1 2))
                (eqvalg:coalesce (eqvalg:equality-of column 1)
                                 (eqvalg:equality-of column 2))))
    (is (equalp (eqvalg:membership-of column '(2 1))
                (eqvalg:coalesce (eqvalg:equality-of column 2)
                                 (eqvalg:equality-of column 1))))
    ;; ANDs equalities with the same subject column that differ in
    ;; strictness
    ;;
    ;; COALESCE(X=1,X==10) -> X=1 & X==10
    ;; COALESCE(X==10,X=1) -> X==10 & X=1
    (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of column 1)
                                       (eqvalg:strict-equality-of column "x"))
                (eqvalg:coalesce (eqvalg:equality-of column 1)
                                 (eqvalg:strict-equality-of column "x"))))))

(test coalesce-membership
  (let ((table-column (eqvalg:column-of "table" "column"))
        (column-ab (eqvalg:column-of "a" "b"))
        (column-aaa-bbb (eqvalg:column-of "aaa" "bbb")))
    ;; Subsumes an equality with the same subject column as a set
    ;; membership into the set
    ;;
    ;; COALESCE(X IN (12),X=1) -> X IN (1,12)
    (is (equalp (eqvalg:membership-of table-column (list 1 12))
                (eqvalg:coalesce (eqvalg:membership-of table-column (list 12))
                                 (eqvalg:equality-of table-column 1))))
    (is (equalp (eqvalg:membership-of table-column (list 1 12))
                (eqvalg:coalesce (eqvalg:equality-of table-column 1)
                                 (eqvalg:membership-of table-column (list 12)))))
    ;; ANDs an equality with a differing subject column to the membership
    ;;
    ;; COALESCE(X IN (12), Y=1) -> (X IN (12)) & Y=1
    (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of table-column (list 12))
                                       (eqvalg:equality-of column-ab "x"))
                (eqvalg:coalesce (eqvalg:membership-of table-column (list 12))
                                 (eqvalg:equality-of column-ab "x"))))
    ;; ANDs a strict equality with the same subject column to the membership
    ;;
    ;; COALESCE(X IN (12), X==1) -> X IN (12) & X==1
    (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of table-column (list 12))
                                       (eqvalg:strict-equality-of table-column "x"))
                (eqvalg:coalesce (eqvalg:membership-of table-column (list 12))
                                 (eqvalg:strict-equality-of table-column "x"))))
    ;; Return the same membership set if coalesced with a strict equality
    ;; that shares the same subject column, and has a target value which is
    ;; already included in the membership set
    ;;
    ;; COALESCE(X IN (12), X==12) -> X IN (12)
    (is (equalp (eqvalg:membership-of table-column (list 12))
                (eqvalg:coalesce (eqvalg:membership-of table-column (list 12))
                                 (eqvalg:strict-equality-of table-column 12))))
    ;; Coalesces two memberships that share the same subject column into their union
    ;;
    ;; COALESCE(X IN (12), X IN (1)) -> X IN (12, 1)
    (is (equalp (eqvalg:membership-of table-column (list 12 1))
                (eqvalg:coalesce (eqvalg:membership-of table-column (list 12))
                                 (eqvalg:membership-of table-column (list 1)))))
    ;; Deduplicates the set union
    ;;
    ;; COALESCE(X IN (12), X IN (12)) -> X IN (12)
    (is (equalp (eqvalg:membership-of table-column (list 12))
                (eqvalg:coalesce (eqvalg:membership-of table-column (list 12))
                                 (eqvalg:membership-of table-column (list 12)))))
    ;; ANDs memberships that differ in their subject column
    ;;
    ;; COALESCE(X IN (12), Y IN (8)) -> X IN (12) & Y IN (8)
    (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of table-column (list 12))
                                       (eqvalg:membership-of column-aaa-bbb (list 8)))
                (eqvalg:coalesce (eqvalg:membership-of table-column (list 12))
                                 (eqvalg:membership-of column-aaa-bbb (list 8)))))))

(test coalesce-conjunction
  (let ((table-column (eqvalg:column-of "table" "column"))
        (table-abc-def (eqvalg:column-of "abc" "def"))
        (table-foo-bar (eqvalg:column-of "foo" "bar")))
    ;; Coalesces singleton conjunction of an equality with another equality
    ;; sharing the same subject column into a set membership
    ;;
    ;; COALESCE((X=1),X=2) -> X IN (1,2)
    (is (equalp (eqvalg:membership-of table-column '(1 2))
                (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:equality-of table-column 1))
                                 (eqvalg:equality-of table-column 2))))
    ;; Coalesces singleton conjunction of a membership with an equality
    ;; sharing the same subject
    ;;
    ;; COALESCE((X IN (1)), X=2) -> X IN (2, 1)
    (is (equalp (eqvalg:membership-of table-column '(2 1))
                (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:membership-of table-column '(1)))
                                 (eqvalg:equality-of table-column 2))))
    ;; Coalesces an equality into a multi-element conjunction, subsuming it into
    ;; the membership set that shares with it the same subject column
    ;;
    ;; COALESCE((X IN (1) & Y=2), X=2) -> X IN (2,1) & Y=2
    (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of table-column '(2 1))
                                       (eqvalg:equality-of table-abc-def "x"))
                (eqvalg:coalesce (eqvalg:conjunction-of
                                  (eqvalg:membership-of table-column '(1))
                                  (eqvalg:equality-of table-abc-def "x"))
                                 (eqvalg:equality-of table-column 2))))
    (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of table-abc-def "x")
                                       (eqvalg:membership-of table-column '(2 1)))
                (eqvalg:coalesce (eqvalg:conjunction-of
                                  (eqvalg:equality-of table-abc-def "x")
                                  (eqvalg:membership-of table-column '(1)))
                                 (eqvalg:equality-of table-column 2))))
    ;; Coalesces a disjunct equality into the conjunction
    ;;
    ;; COALESCE((X=1 & Y IN (1)), Z=3) -> X=1 & Y IN (1) & Z=3
    (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of table-foo-bar "baz")
                                       (eqvalg:equality-of table-abc-def "x")
                                       (eqvalg:membership-of table-column '(1)))
                (eqvalg:coalesce (eqvalg:conjunction-of
                                  (eqvalg:equality-of table-abc-def "x")
                                  (eqvalg:membership-of table-column '(1)))
                                 (eqvalg:equality-of table-foo-bar "baz"))))
    ;; COALESCE(X=1 & Y=2, X=10 & Y=20) -> X IN (1,10) & Y IN (2,20)
    (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of table-foo-bar '(1 2))
                                       (eqvalg:membership-of table-abc-def '(10 20)))
                (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:equality-of table-foo-bar 1)
                                                        (eqvalg:equality-of table-abc-def 10))
                                 (eqvalg:conjunction-of (eqvalg:equality-of table-foo-bar 2)
                                                        (eqvalg:equality-of table-abc-def 20)))))
    (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of table-foo-bar '(2 1))
                                       (eqvalg:membership-of table-abc-def '(20 10)))
                (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:equality-of table-foo-bar 2)
                                                        (eqvalg:equality-of table-abc-def 20))
                                 (eqvalg:conjunction-of (eqvalg:equality-of table-foo-bar 1)
                                                        (eqvalg:equality-of table-abc-def 10)))))))
