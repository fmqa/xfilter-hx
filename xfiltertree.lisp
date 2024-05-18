(defpackage xfiltertree
  (:use :cl)
  (:export #:node #:node-id #:node-children
           #:aggregation #:aggregation-bins
           #:dynamic #:dynamic-searcher #:dynamic-querier #:dynamic-p
           #:traverse
           #:traverse-if
           #:translate
           #:node-name
           #:aggregation-map-id
           #:aggregation-map))
(in-package :xfiltertree)

(defclass node ()
  ((id
    :reader node-id
    :initarg :id)
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

(defun dynamic-p (node)
  (typep node 'dynamic))

(defun traverse (func node)
  (funcall func node)
  (dolist (child (node-children node)) (traverse func child)))

(defun traverse-if (pred func node)
  (traverse (lambda (nd) (when (funcall pred nd) (funcall func nd)))
            node))

(defgeneric translate (id))

(defmethod translate (id) id)

(defun node-name (node)
  (translate (node-id node)))

(defun aggregation-map-id (function aggregation)
  (mapcar (lambda (pair)
            (destructuring-bind (id . bins) pair
              (funcall function id bins)))
          (aggregation-bins aggregation)))

(defun aggregation-map (function aggregation)
  (aggregation-map-id (lambda (id bins) (funcall function (translate id) bins))
                      aggregation))
