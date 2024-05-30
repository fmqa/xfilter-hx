(defpackage eqvalg-query
  (:use :cl)
  (:export #:*store*
           #:cardinality-in
           #:distinct-in
           #:cardinality
           #:distinct))
(in-package :eqvalg-query)

(defparameter *store* nil "Active database descriptor")

(defgeneric cardinality-in (db term)
  (:documentation "Computes the cardinality (count) of TERM in the given DB"))

(defgeneric distinct-in (db column)
  (:documentation "Computes the set of distinct values of COLUMN in the given DB"))

(defun cardinality (term &optional (db *store*))
  (cardinality-in db term))

(defun distinct (column &optional (db *store*))
  (distinct-in db column))
