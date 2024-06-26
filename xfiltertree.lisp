(defpackage xfiltertree
  (:use :cl)
  (:export #:node #:node-id #:node-children
           #:aggregation #:aggregation-bins #:aggregation-p
           #:dynamic #:dynamic-searcher #:dynamic-querier #:dynamic-p
           #:auto #:auto-limit #:auto-offset #:auto-next #:auto-p #:auto-next-uri
           #:copy-node
           #:traverse
           #:traverse-if
           #:aggregation-map
           #:aggregation-mapf
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

(defclass auto (aggregation)
  ((limit
    :reader auto-limit
    :initarg :limit
    :initform nil)
   (offset
    :accessor auto-offset
    :initarg :offset
    :initform nil)
   (next
    :accessor auto-next
    :initarg :next
    :initform nil)))

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

(defmethod copy-node ((nd auto))
  (make-instance 'auto
                 :id (node-id nd)
                 :children (mapcar #'copy-node (node-children nd))
                 :bins (aggregation-bins nd)
                 :offset (auto-offset nd)
                 :limit (auto-limit nd)
                 :next (auto-next nd)))

(defun aggregation-p (node)
  "Returns t if NODE is an AGGREGATION"
  (typep node 'aggregation))

(defun dynamic-p (node)
  "Return t if NODE is a DYNAMIC node"
  (typep node 'dynamic))

(defun auto-p (node)
  "Return t if NODE is an AUTO node"
  (typep node 'auto))

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

(defun aggregation-mapf (trifunction aggregation)
  "Returns a list with the results of mapping TRIFUNCTION onto
   the given AGGREGATION node's aggregation pairs. The third argument
   passed to TRIFUNCTION indicates whether this is the last aggregation"
  (maplist (lambda (lst)
             (destructuring-bind (id . bins) (car lst)
               (funcall trifunction id bins (not (cdr lst)))))
           (aggregation-bins aggregation)))

(defun aggregation-foreach (bifunction aggregation)
  "Calls BIFUNCTION with the id and bins of each aggregation
   in AGGREGATION"
  (loop for (id . bins) in (aggregation-bins aggregation)
        do (funcall bifunction id bins)))

(defun auto-next-uri (node)
  (when (auto-next node)
    (format nil "~A?offset=~A&limit=~A"
            (auto-next node)
            (auto-offset node)
            (auto-limit node))))
