(in-package :fql)

;; Parses simple filter strings of the form
;;
;;   table.column=X
;;   table.column[variable=A]=X
;;
;; Into corresponding EQVALG objects, where:
;;
;;  table.column=X is translated to
;;
;;    (EQGVALG:EQUALITY :LEFT (EQVALG:COLUMN :TABLE "table" :COLUMN "column")
;;                      :RIGHT X)
;;
;; and table.column[variable=A]=X is translated to
;;
;;    (EQVALG:CONJUNCTION
;;      :OPERANDS (EQGVALG:EQUALITY :LEFT (EQVALG:COLUMN :TABLE "table" :COLUMN "column")
;;                                  :RIGHT X)
;;                 EQVALG:EQUALITY :LEFT (EQVALG:COLUMN :TABLE "table" :COLUMN "variable"
;;                                 :RIGHT X))

;; Parse + and - as their corresponding sign functions
(defrule pos #\+
  (:constant #'+))

(defrule neg #\-
  (:constant #'-))

;; A sign is either + or -
(defrule sign (or pos neg))

;; A natural number is a sequence of digits
(defrule natural (+ (character-ranges (#\0 #\9)))
  (:text t)
  (:lambda (text) (parse-integer text)))

;; A signed natural number is an integer
(defrule integer (and (? sign) natural)
  (:destructure (sign n)
    (funcall (or sign #'identity) n)))

;; Two dot-separated natural numbers with a preceding sign are a decimal (float)
;; number
(defrule float (and (? sign) natural #\. (? natural))
  (:destructure (sign integral dot fractional)
    (with-input-from-string (in (format nil "~A~A~@[~A~]" integral dot fractional))
      (funcall (or sign #'identity) (read in)))))

;; A number is either a float or an integer
(defrule number (or float integer))

;; A word is any string that starts with an alphabetical character or an underscore,
;; followed by any number of alphanumeric characters
(defrule word (and (or #\_ (alpha-char-p character)) (* (or #\_ (alphanumericp character))))
  (:text t))

;; Predicate version of the WORD rule
(defun wordp (s)
  (and (handler-case (parse 'word s) (parse-error nil)) s))

;; A duplicated single quote is an escape sequence for a single quote
(defrule single-quote-escape (and #\' #\')
  (:constant #\'))

;; A single-quoted string is a string enclosed by the ' character, with '' as
;; an escape sequence, so 'x''y' corresponds to the string x'y
(defrule single-quoted-string (and #\' (* (or single-quote-escape (not #\'))) #\')
  (:destructure (open text close)
    (declare (ignore open close))
    text)
  (:text t))

;; A constrant is either a number, a word, or a quoted string
(defrule constant (or number word single-quoted-string))

;; A clause is an equality with a word on the left-hand side and
;; a constant on the right hand side
(defrule clause (and word #\= constant)
  (:destructure (left equality right)
    (declare (ignore equality))
    (cons left right)))

;; A table column designator is a word denoting the table name, followed by a
;; square-bracketed discriminator clause, followed by a dot, followed by a word
;; denoting the column name
(defrule column (and word (? (and #\[ clause #\])) #\. word)
  (:destructure (table clause dot name)
    (declare (ignore dot))
    (cons (eqvalg:column-of table name)
          (when clause
            (destructuring-bind (open (left . right) close) clause
              (declare (ignore open close))
              (eqvalg:strict-equality-of (eqvalg:column-of table left)
                                         right))))))

;; A filter is a table column designator, followed by an equivalence operator,
;; followed by a constant
(defrule filter (and column #\= constant)
  (:destructure (left op right)
    (declare (ignore op))
    (destructuring-bind (column . subclause) left
      (if subclause
          (eqvalg:coalesce (eqvalg:equality-of column right) subclause)
          (eqvalg:equality-of column right)))))

;; A key=value option consisting of a world, followed by an equality sign,
;; followed by a constant
(defrule option (and word #\= constant)
  (:destructure (left eqls right)
    (declare (ignore eqls))
    (cons left right)))

;; A comma-delimited sequence of key=value options
(defrule option-list (and option (* (and  #\, option)))
  (:destructure (head tail)
    (cons head (mapcar #'cadr tail))))

;; A filter, optionally followed by a semicolon and a sequence of key=value options
(defrule filter-with-options (and filter (? (and #\; option-list)))
  (:destructure (flt options)
    (cons flt (cadr options))))

(defun parse-filter-with-options (text)
  (parse 'filter-with-options text))

(defun parse-filter (text)
  (parse 'filter text))

(defun parse-column (text)
  (parse 'column text))
