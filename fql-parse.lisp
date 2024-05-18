(defpackage fql
  (:use :cl :esrap)
  (:export
   #:parse-filter
   #:parse-column))
(in-package :fql)

;; Parses simple filter strings of the form
;;
;;   table.column=X
;;   table.column[variable=A]=X
;;

(defrule pos #\+
  (:constant #'+))

(defrule neg #\-
  (:constant #'-))

(defrule sign (or pos neg))

(defrule natural (+ (character-ranges (#\0 #\9)))
  (:text t)
  (:lambda (text) (parse-integer text)))

(defrule integer (and (? sign) natural)
  (:destructure (sign n)
    (funcall (or sign #'identity) n)))

(defrule float (and (? sign) natural #\. (? natural))
  (:destructure (sign integral dot fractional)
    (with-input-from-string (in (format nil "~A~A~@[~A~]" integral dot fractional))
      (funcall (or sign #'identity) (read in)))))

(defrule number (or float integer))

(defrule word (and (or #\_ (alpha-char-p character)) (* (or #\_ (alphanumericp character))))
  (:text t))

(defrule text (+ character)
  (:text t))

(defrule constant (or number word))

(defrule clause (and word #\= (or number word))
  (:destructure (left equality right)
    (declare (ignore equality))
    (list :strict-eq left right)))

(defrule column (and word (? (and #\[ clause #\])) #\. word)
  (:destructure (table clause dot name)
    (cons (format nil "~A~A~A" table dot name)
          (when clause
            (destructuring-bind (open (op left right) close) clause
              (declare (ignore open close))
              (list op (format nil "~A.~A" table left) right))))))

(defrule filter (and column #\= constant)
  (:destructure (left op right)
    (declare (ignore op))
    (destructuring-bind (column &rest subclause) left
      (cons (list :eq column right)
            (when subclause (list subclause))))))

(defun parse-filter (text)
  (parse 'filter text))

(defun parse-column (text)
  (parse 'column text))
