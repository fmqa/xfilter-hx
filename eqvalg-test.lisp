(defpackage eqvalg/test
  (:use :cl :fiveam))
(in-package :eqvalg/test)

(def-suite eqvalg :description "Equivalency alegbra test")

(def-suite* subjects :in eqvalg)

(test equality-subject
  (is (equalp (eqvalg:column-of "table" "column")
              (eqvalg:subject (eqvalg:equality-of (eqvalg:column-of "table" "column") 12))))
  (is (equalp (eqvalg:column-of "table" "column")
              (eqvalg:subject (eqvalg:equality-of 12 (eqvalg:column-of "table" "column")))))
  (is (not (eqvalg:subject (eqvalg:equality-of 12 17)))))

(test membership-subject
  (is (equalp (eqvalg:column-of "table" "column")
              (eqvalg:subject (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12)))))
  (is (not (eqvalg:subject (eqvalg:membership-of 12 (list 11 12))))))

(test conjunction-subject
  (is (equalp (list (eqvalg:column-of "table" "column"))
              (eqvalg:subject (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "table" "column") "x")))))
  (is (equalp (list (eqvalg:column-of "abcde" "fgab")
                    (eqvalg:column-of "table" "column"))
              (eqvalg:subject (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "table" "column") "x")
                                                     (eqvalg:equality-of "y" (eqvalg:column-of "abcde" "fgab"))))))
  (is (equalp (list (eqvalg:column-of "table" "column"))
              (eqvalg:subject (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "table" "column") "x")
                                                     (eqvalg:equality-of "y" "x"))))))

(def-suite* coalescing :in eqvalg)

(test coalesce-equalities
  (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "a" "b") "1")
                                     (eqvalg:equality-of (eqvalg:column-of "x" "y") "2"))
              (eqvalg:coalesce (eqvalg:equality-of (eqvalg:column-of "a" "b") "1")
                               (eqvalg:equality-of (eqvalg:column-of "x" "y") "2"))))
  (is (equalp (eqvalg:membership-of (eqvalg:column-of "table" "column") '(1 2))
              (eqvalg:coalesce (eqvalg:equality-of (eqvalg:column-of "table" "column") 1)
                               (eqvalg:equality-of (eqvalg:column-of "table" "column") 2))))
  (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "table" "column") 1)
                                     (eqvalg:strict-equality-of (eqvalg:column-of "table" "column") "x"))
              (eqvalg:coalesce (eqvalg:equality-of (eqvalg:column-of "table" "column") 1)
                               (eqvalg:strict-equality-of (eqvalg:column-of "table" "column") "x")))))

(test coalesce-membership
  (is (equalp (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 1 12))
              (eqvalg:coalesce (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                               (eqvalg:equality-of (eqvalg:column-of "table" "column") 1))))
  (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                                     (eqvalg:equality-of (eqvalg:column-of "a" "b") "x"))
              (eqvalg:coalesce (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                               (eqvalg:equality-of (eqvalg:column-of "a" "b") "x"))))
  (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                                     (eqvalg:strict-equality-of (eqvalg:column-of "table" "column") "x"))
              (eqvalg:coalesce (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                               (eqvalg:strict-equality-of (eqvalg:column-of "table" "column") "x"))))
  (is (equalp (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
              (eqvalg:coalesce (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                               (eqvalg:strict-equality-of (eqvalg:column-of "table" "column") 12))))
  (is (equalp (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12 1))
              (eqvalg:coalesce (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                               (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 1)))))
  (is (equalp (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
              (eqvalg:coalesce (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                               (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12)))))
  (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                                     (eqvalg:membership-of (eqvalg:column-of "aaa" "bbb") (list 8)))
              (eqvalg:coalesce (eqvalg:membership-of (eqvalg:column-of "table" "column") (list 12))
                               (eqvalg:membership-of (eqvalg:column-of "aaa" "bbb") (list 8))))))

(test coalesce-conjunction
  (is (equalp (eqvalg:membership-of (eqvalg:column-of "table" "column") '(1 2))
              (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "table" "column") 1))
                               (eqvalg:equality-of (eqvalg:column-of "table" "column") 2))))
  (is (equalp (eqvalg:membership-of (eqvalg:column-of "table" "column") '(2 1))
              (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:membership-of (eqvalg:column-of "table" "column") '(1)))
                               (eqvalg:equality-of (eqvalg:column-of "table" "column") 2))))
  (is (equalp (eqvalg:conjunction-of (eqvalg:membership-of (eqvalg:column-of "table" "column") '(2 1))
                                     (eqvalg:equality-of (eqvalg:column-of "abc" "def") "x"))
              (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:membership-of
                                                       (eqvalg:column-of "table" "column") '(1))
                                                      (eqvalg:equality-of
                                                       (eqvalg:column-of "abc" "def") "x"))
                               (eqvalg:equality-of (eqvalg:column-of "table" "column") 2))))
  (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "abc" "def") "x")
                                     (eqvalg:membership-of (eqvalg:column-of "table" "column") '(2 1)))
              (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:equality-of
                                                       (eqvalg:column-of "abc" "def") "x")
                                                      (eqvalg:membership-of
                                                       (eqvalg:column-of "table" "column") '(1)))
                               (eqvalg:equality-of (eqvalg:column-of "table" "column") 2))))
  (is (equalp (eqvalg:conjunction-of (eqvalg:equality-of (eqvalg:column-of "foo" "bar") "baz")
                                     (eqvalg:equality-of (eqvalg:column-of "abc" "def") "x")
                                     (eqvalg:membership-of (eqvalg:column-of "table" "column") '(1)))
              (eqvalg:coalesce (eqvalg:conjunction-of (eqvalg:equality-of
                                                       (eqvalg:column-of "abc" "def") "x")
                                                      (eqvalg:membership-of
                                                       (eqvalg:column-of "table" "column") '(1)))
                               (eqvalg:equality-of (eqvalg:column-of "foo" "bar") "baz")))))
