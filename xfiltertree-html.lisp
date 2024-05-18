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
       (:input :type "hidden" :name "update" :value "true")
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
  (let ((escaped (webstr:escape predicate))
        (clause (format nil "~A;bin=~A" predicate aggregation)))
    (cl-who:with-html-output-to-string (s)
      (:input :type "checkbox" :id escaped :name "clause" :value (cl-who:escape-string clause))
      (:label
       :id (format nil "label--~A" escaped)
       ;; If we're updating an existing DOM form, mark this
       ;; element as swappable.
       :hx-swap-oob (if *form-update* "true" "")
       :for escaped (:span :data-bin aggregation (cl-who:str value))))))

(defun make-aggregation-bin-htmlizer (name)
  (lambda (pair)
    (destructuring-bind (cluster &rest count) pair
      (htmlize-aggregation-triplet name cluster count))))

(defun htmlize-aggregation-bin (bin)
  (destructuring-bind (name &rest value) bin
    (cl-who:with-html-output-to-string (s)
      (:fieldset :data-leaf "true"
       (:legend :data-i18n "" (cl-who:str name))
       (cl-who:str
        (uiop:reduce/strcat
         (mapcar (make-aggregation-bin-htmlizer name)
                 value)))))))

(defmethod htmlize-content ((node xfiltertree:aggregation))
  (uiop:reduce/strcat
   (mapcar #'htmlize-aggregation-bin (xfiltertree:aggregation-bins node))))

(defun htmlize-dynamic-bin (name bins)
  (let* ((clause (format nil "~A;bin=ALL" name))
         (escaped (webstr:escape name)))
    (cl-who:with-html-output-to-string (s)
      (:fieldset
       :id (format nil "fieldset--~A" escaped)
       :data-leaf "true"
       (:legend :data-i18n "" (cl-who:str name))
       (:input :type "hidden" :name "dynamic"
               :value (cl-who:escape-string clause))
       (cl-who:str
        (uiop:reduce/strcat
         (mapcar (make-aggregation-bin-htmlizer name)
                 bins)))
       (:button
        :type "button"
        ;; Remove this fieldset in addition to any OOB settings
        ;; related to this element.
        :|hx-on:click|
        (cl-who:escape-string
         (format
          nil
          "((elt, id) => {~
                           if (!elt) return;~
                           const attrs = elt.getAttribute('hx-select-oob');~
                           if (!attrs) return;~
                           elt.setAttribute(~
                             'hx-select-oob',~
                             attrs.split(',').filter(attr => attr !== id).join(','));~
                           htmx.remove('#fieldset--~A');~
                         })(htmx.closest(this, '[hx-select-oob]'),~
                             '#fieldset--~A')" escaped escaped))
        "x")
       ;; Enable OOB swapping for this element to avoid adding duplicates.
       (:script
        (cl-who:str
         (format
          nil
          "((selector) => {~
                            const elt = htmx.find(selector);~
                            if (!elt || !elt.parentElement) return;~
                            const attr = elt.parentElement.getAttribute('hx-select-oob');~
                            (attr && attr.split(',').includes(selector)) ||~
                              elt.parentElement.setAttribute(~
                                'hx-select-oob',~
                                attr ? attr + ',' + selector : selector);~
                          })('#fieldset--~A')" escaped)))))))

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
                   :hx-trigger (format nil "input changed from:#~A, ~
                                            focus once from:#~A, ~
                                            focus changed from:#~A"
                                       id id id))
            (:datalist :id data))
          *epilogue*)
    (cl-who:with-html-output-to-string (s)
      (:div
       (:input :id id :name "q" :form form :type "search" :list data :|hx-on:change| "event.stopPropagation();")
       (:button :hx-post (xfiltertree:dynamic-query-uri node)
                :hx-include "previous input"
                :hx-swap "afterend"
                "+")
       (cl-who:str
        (uiop:reduce/strcat
         (mapcar (lambda (pair)
                   (destructuring-bind (clause &rest bins) pair
                     (htmlize-dynamic-bin clause bins)))
                 (xfiltertree:aggregation-bins node))))))))
