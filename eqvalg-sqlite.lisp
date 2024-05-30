(in-package :eqvalg-sqlite)

(defmethod eqvalg-query:distinct-in ((db sqlite:handle) column)
  (lambda (where offset limit)
    (mapcar
     #'car
     (sqlite:execute-to-list db (eqvalg-sql:sqlize-distinct column where offset limit)))))

(defmethod eqvalg-query:cardinality-in ((db sqlite:handle) term)
  (if (consp term)
      (multiple-value-list
       (sqlite:execute-one-row-m-v
        db
        (format nil "SELECT ~{(~A)~^, ~}" (mapcar #'eqvalg-sql:sqlize-count term))))
      (sqlite:execute-one-row-m-v db (eqvalg-sql:sqlize-count term))))
