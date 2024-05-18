(defpackage xfiltertree
  (:use :cl)
  (:export #:node #:node-name #:node-children
           #:aggregation #:aggregation-bins
           #:dynamic #:dynamic-searcher #:dynamic-querier
           #:traverse))
(in-package :xfiltertree)

(defclass node ()
  ((name
    :reader node-name
    :initarg :name)
   (children
    :accessor node-children
    :initarg :children
    :initform nil)))

(defclass aggregation (node)
  ((bins
    :accessor aggregation-bins
    :initarg :bins
    :initform nil)))

(defclass dynamic (aggregation)
  ((searcher
    :reader dynamic-searcher
    :initarg :searcher)
   (querier
    :reader dynamic-querier
    :initarg :querier)))

(defun traverse (func node)
  (funcall func node)
  (dolist (child (node-children node)) (traverse func child)))
