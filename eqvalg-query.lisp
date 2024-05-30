(defpackage eqvalg-query
  (:use :cl)
  (:export #:*db*
           #:cardinality-in
           #:distinct-in
           #:cardinality
           #:distinct))
(in-package :eqvalg-query)

(defparameter *db* nil "Active database descriptor")

(defgeneric cardinality-in (db term)
  (:documentation "Computes the cardinality (count) of TERM in the given DB"))

(defgeneric distinct-in (db column)
  (:documentation "Computes the set of distinct values of COLUMN in the given DB"))

(defun cardinality (term)
  (cardinality-in *db* term))

(defun distinct (column)
  (distinct-in *db* column))
