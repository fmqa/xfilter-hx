(in-package :fql)

;; Predicate version of the WORD parser in FQL-PARSE
(defun wordp (s)
  (and (handler-case (parse 'word s) (parse-error nil)) s))

;; If the given value is a non-wordp string, quote it, otherwise
;; behave as an identity function
(defun quote-value (value)
  (cond ((or (numberp value) (wordp value)) value)
        (t (with-output-to-string (out)
             (loop initially (write-char #\' out)
                   for char across value
                   do (progn (when (eq #\' char)
                               (write-char char out))
                             (write-char char out))
                   finally (write-char #\' out))))))

(defgeneric stringify (obj)
  (:documentation "Stringify the given object as an FQL string"))

;; Default to an identity if not special implementation is found
(defmethod stringify (obj) obj)

;; Stringify a table column designator as TABLE.COLUMN
(defmethod stringify ((obj eqvalg:column))
  (format nil "~A.~A" (eqvalg:column-table obj) (eqvalg:column-name obj)))

;; Stringify an equality
(defmethod stringify ((obj eqvalg:equality))
  (format nil "~A=~A"
          (stringify (eqvalg:equality-left obj))
          (quote-value (stringify (eqvalg:equality-right obj)))))

;; Stringify an equality with a discriminated column. This is the inverse of
;; the FILTER parser if the existence of a discriminator clause is assumed
(defmethod stringify ((obj eqvalg:conjunction))
  (destructuring-bind (eqlt discriminator) (eqvalg:conjunction-operands obj)
    (if (and (eqvalg:equality-p eqlt)
             (eqvalg:equality-p discriminator)
             (eqvalg:equality-strict discriminator))
        (multiple-value-bind (discriminator-subject discriminator-target)
            (eqvalg:subject discriminator)
          (multiple-value-bind (eqlt-subject eqlt-target) (eqvalg:subject eqlt)
            (if (equal (eqvalg:column-table eqlt-subject)
                       (eqvalg:column-table discriminator-subject))
                (format nil "~A[~A=~A].~A=~A"
                        (eqvalg:column-table eqlt-subject)
                        (eqvalg:column-name discriminator-subject)
                        discriminator-target
                        (eqvalg:column-name eqlt-subject)
                        (quote-value eqlt-target))))))))

;; Stringify a column-discriminator pair
(defmethod stringify ((obj cons))
  (destructuring-bind (eqlt-subject . discriminator) obj
    (multiple-value-bind (discriminator-subject discriminator-target)
        (eqvalg:subject discriminator)
      (if (equal (eqvalg:column-table eqlt-subject)
                 (eqvalg:column-table discriminator-subject))
          (format nil "~A[~A=~A].~A"
                  (eqvalg:column-table eqlt-subject)
                  (eqvalg:column-name discriminator-subject)
                  discriminator-target
                  (eqvalg:column-name eqlt-subject))))))
