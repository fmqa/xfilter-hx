(defpackage sql-client
  (:use :cl)
  (:export
   #:*tap*
   #:query
   #:parametrized-query
   #:bind-querier
   #:bind-parametrized-querier))
(in-package :sql-client)

(defparameter *tap* #'identity)

(defgeneric query (db sql)
  (:method :before (db sql)
    (funcall *tap* sql)))

(defun parametrized-query (db sql &rest parameters)
  (query db (cons sql parameters)))

(defun bind-querier (open &key (tap #'identity))
  (lambda (sql)
    (funcall open (lambda (db)
                    (let ((*tap* tap))
                      (query db sql))))))

(defun bind-parametrized-querier (open &key (tap #'identity))
  (lambda (sql &rest parameters)
    (funcall open (lambda (db)
                    (let ((*tap* tap))
                      (query db (cons sql parameters)))))))
