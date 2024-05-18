(in-package :xfiltertree-html)

(defparameter *form-post* "/" "Form POST endpoint")

(defparameter *form-update* nil "Update existing form")

(defparameter *epilogue* nil)

(defgeneric htmlize-content (node))

(defmethod htmlize-content ((node xfiltertree:node)))

(defun htmlize (node)
  (let ((*epilogue* nil))
    (cl-who:with-html-output-to-string (s)
      (:form
       :hx-post *form-post* :hx-swap "none" :hx-trigger "change"
       (:input :type "hidden" :name "$update" :value "true")
       ;; Recursively produce HTML for each node.
       (cl-who:str (htmlize-level node)))
      ;; Write out auxiliary HTML required by the emitted
      ;; HTML elements
      (cl-who:str (uiop:reduce/strcat *epilogue*)))))

(defun htmlize-level (node &optional (level 0))
  (cl-who:with-html-output-to-string (s)
    (:fieldset
     :data-name (xfiltertree:node-name node)
     (:legend :data-i18n ""
              (cl-who:str (xfiltertree:node-name node)))
     (cl-who:str (htmlize-content node))
     (cl-who:str
      (uiop:reduce/strcat
       (mapcar (lambda (x) (htmlize-level x (1+ level)))
               (xfiltertree:node-children node)))))))

(defun htmlize-aggregation-triplet (predicate aggregation value)
  (let ((escaped (webstr:escape predicate)))
    (cl-who:with-html-output-to-string (s)
      (:input :type "checkbox" :id escaped :name predicate :value aggregation)
      (:label
       :id (format nil "label--~A" escaped)
       ;; If we're updating an existing DOM form, mark this
       ;; element as swappable.
       :hx-swap-oob (if *form-update* "true" "")
       :for escaped (:span :data-bin aggregation (cl-who:str value))))))

(defun htmlize-aggregation-bin (bin)
  (destructuring-bind (name &rest value) bin
    (cl-who:with-html-output-to-string (s)
      (:fieldset :data-leaf "true"
       (:legend :data-i18n "" (cl-who:str name))
       (cl-who:str
        (uiop:reduce/strcat
         (mapcar (lambda (pair)
                   (destructuring-bind (cluster &rest count) pair
                     (htmlize-aggregation-triplet name cluster count)))
                 value)))))))

(defmethod htmlize-content ((node xfiltertree:aggregation))
  (uiop:reduce/strcat
   (mapcar #'htmlize-aggregation-bin (xfiltertree:aggregation-bins node))))

(defmethod htmlize-content ((node xfiltertree:dynamic))
  (let* ((name (xfiltertree:node-name node))
         (escaped (webstr:escape name))
         (id (format nil "search--~A" escaped))
         (data (format nil "data--search-~A" escaped))
         (form (format nil "form--~A" escaped)))
    (push (cl-who:with-html-output-to-string (s)
            (:form :id form :hx-post (xfiltertree:dynamic-search-uri node)
                   :onsubmit "return false;"
                   :hx-target "next"
                   :hx-trigger (format nil "input changed from:#~A, focus once from:#~A, focus changed from:#~A" id id id))
            (:datalist :id data))
          *epilogue*)
    (cl-who:with-html-output-to-string (s)
      (:input :id id :name name :form form :type "search" :list data :|hx-on:change| "event.stopPropagation();")
      (:button :hx-post (xfiltertree:dynamic-query-uri node)
               :hx-include "previous input" "+"))))
