(defpackage xfiltertree
  (:use :cl)
  (:export #:node #:node-id #:node-children
           #:aggregation #:aggregation-bins #:aggregation-p
           #:dynamic #:dynamic-searcher #:dynamic-querier #:dynamic-p
           #:copy-node
           #:traverse
           #:traverse-if
           #:aggregation-map
           #:aggregation-foreach))
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

(defgeneric copy-node (nd))

(defmethod copy-node ((nd node))
  (make-instance 'node
                 :id (node-id nd)
                 :children (mapcar #'copy-node (node-children nd))))

(defmethod copy-node ((nd aggregation))
  (make-instance 'aggregation
                 :id (node-id nd)
                 :children (mapcar #'copy-node (node-children nd))
                 :bins (aggregation-bins nd)))

(defmethod copy-node ((nd dynamic))
  (make-instance 'dynamic
                 :id (node-id nd)
                 :children (mapcar #'copy-node (node-children nd))
                 :bins (aggregation-bins nd)
                 :searcher (dynamic-searcher nd)
                 :querier (dynamic-querier nd)))

(defun aggregation-p (node)
  (typep node 'aggregation))

(defun dynamic-p (node)
  (typep node 'dynamic))

(defun traverse (func node)
  (funcall func node)
  (dolist (child (node-children node)) (traverse func child)))

(defun traverse-if (pred func node)
  (traverse (lambda (nd) (when (funcall pred nd) (funcall func nd)))
            node))

(defun aggregation-map (function aggregation)
  (mapcar (lambda (pair)
            (destructuring-bind (id . bins) pair
              (funcall function id bins)))
          (aggregation-bins aggregation)))

(defun aggregation-foreach (function aggregation)
  (loop for (id . bins) in (aggregation-bins aggregation)
        do (funcall function id bins)))
