(defpackage fql
  (:use :cl :esrap)
  (:export
   #:parse-filter
   #:parse-column
   #:parse-filter-with-options))
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

(defrule single-quote-escape (and #\' #\')
  (:constant #\'))

(defrule single-quoted-string (and #\' (* (or single-quote-escape (not #\'))) #\')
  (:destructure (open text close)
    (declare (ignore open close))
    text)
  (:text t))

(defrule constant (or number word single-quoted-string))

(defrule clause (and word #\= constant)
  (:destructure (left equality right)
    (declare (ignore equality))
    (cons left right)))

(defrule column (and word (? (and #\[ clause #\])) #\. word)
  (:destructure (table clause dot name)
    (declare (ignore dot))
    (cons (eqvalg:column-of table name)
          (when clause
            (destructuring-bind (open (left . right) close) clause
              (declare (ignore open close))
              (eqvalg:strict-equality-of (eqvalg:column-of table left)
                                         right))))))

(defrule filter (and column #\= constant)
  (:destructure (left op right)
    (declare (ignore op))
    (destructuring-bind (column . subclause) left
      (if subclause
          (eqvalg:coalesce (eqvalg:equality-of column right) subclause)
          (eqvalg:equality-of column right)))))

(defrule option (and word #\= constant)
  (:destructure (left eqls right)
    (declare (ignore eqls))
    (cons left right)))

(defrule option-list (and option (* (and  #\, option)))
  (:destructure (head tail)
    (cons head (mapcar #'cadr tail))))

(defrule filter-with-options (and filter (? (and #\; option-list)))
  (:destructure (flt options)
    (cons flt (cadr options))))

(defun parse-filter-with-options (text)
  (parse 'filter-with-options text))

(defun parse-filter (text)
  (parse 'filter text))

(defun parse-column (text)
  (parse 'column text))
