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

(defgeneric copy-node (nd)
  (:documentation "Clones the given node recursively"))

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
  "Returns t if NODE is an AGGREGATION"
  (typep node 'aggregation))

(defun dynamic-p (node)
  "Return t if NODE is a DYNAMIC node"
  (typep node 'dynamic))

(defun traverse (func node)
  "Calls FUNC on NODE and all its children in depth-first order"
  (funcall func node)
  (dolist (child (node-children node)) (traverse func child)))

(defun traverse-if (pred func node)
  "Calls FUNC on both NODE and any children that satisfy the predicate PRED"
  (traverse (lambda (nd) (when (funcall pred nd) (funcall func nd)))
            node))

(defun aggregation-map (bifunction aggregation)
  "Returns a list with the results of mapping BIFUNCTION onto
   the given AGGREGATION node's aggregation pairs"
  (mapcar (lambda (pair)
            (destructuring-bind (id . bins) pair
              (funcall bifunction id bins)))
          (aggregation-bins aggregation)))

(defun aggregation-foreach (bifunction aggregation)
  "Calls BIFUNCTION with the id and bins of each aggregation
   in AGGREGATION"
  (loop for (id . bins) in (aggregation-bins aggregation)
        do (funcall bifunction id bins)))
