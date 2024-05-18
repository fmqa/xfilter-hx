(in-package :xfiltertree-html)

(defparameter *form-post* "/" "Form POST endpoint")

(defparameter *form-update* nil "Update existing form")

(defgeneric htmlize-content (node))

(defmethod htmlize-content ((node xfiltertree:node)))

(defun htmlize (node)
  (cl-who:with-html-output-to-string (s)
    (:form
     :hx-post *form-post* :hx-swap "none" :hx-trigger "change"
	 (:input :type "hidden" :name "$update" :value "true")
     ;; Recursively produce HTML for each node.
     (cl-who:str (htmlize-level node)))))

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
