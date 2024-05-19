(in-package :fql)

(defgeneric stringify (obj))

(defmethod stringify (obj) obj)

(defun wordp (s)
  (every (lambda (c) (or (alphanumericp c) (eq #\_ c))) s))

(defun quote-value (value)
  (cond ((or (numberp value) (wordp value)) value)
        (t (with-output-to-string (out)
             (loop initially (write-char #\' out)
                   for char across value
                   do (progn (when (eq #\' char)
                               (write-char char out))
                             (write-char char out))
                   finally (write-char #\' out))))))

(defmethod stringify ((obj eqvalg:column))
  (format nil "~A.~A" (eqvalg:column-table obj) (eqvalg:column-name obj)))

(defmethod stringify ((obj eqvalg:equality))
  (format nil "~A=~A"
          (stringify (eqvalg:equality-left obj))
          (quote-value (stringify (eqvalg:equality-right obj)))))

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
