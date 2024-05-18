(defpackage xfiltertree
  (:use :cl)
  (:export #:node #:node-name #:node-children
           #:aggregation #:aggregation-bins))
(in-package :xfiltertree)

(defclass node ()
  ((name
    :reader node-name
    :initarg :name)
   (children
    :reader node-children
    :initarg :items
    :initform nil)))

(defclass aggregation (node)
  ((bins
    :reader aggregation-bins
    :initarg :bins
    :initform nil)))
