(defpackage xfiltertree
  (:use :cl)
  (:export #:node #:node-name #:node-children
           #:aggregation #:aggregation-bins
           #:dynamic
           #:dynamic-search-uri
           #:dynamic-query-uri))
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

(defclass dynamic (aggregation)
  ((search-uri
    :reader dynamic-search-uri
    :initarg :search-uri)
   (query-uri
    :reader dynamic-query-uri
    :initarg :query-uri)))
