(in-package :xfiltertree-server)

(hunchentoot:define-easy-handler (fnt-route :uri "/fnt")
    ((clause :parameter-type 'list)
     (update :parameter-type 'boolean)
     (dynamic :parameter-type 'list))
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (filter-navigation-tree-html
      (mapcar #'parse-filter-bin-clause clause)
      :update update
      :dynamic (mapcar #'parse-filter-bin-clause dynamic)))))

(hunchentoot:define-easy-handler (endpoint-search-route :uri "/endpoints/search") (q)
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (no-content-unless (endpoint-search-html q)))))

(hunchentoot:define-easy-handler (endpoint-query-route :uri "/endpoints/query")
    ((key :real-name "q")
     (clause :parameter-type 'list))
  (allow-methods
   '(:HEAD :GET :POST)
   (lambda ()
     (endpoint-filter-html key (mapcar #'parse-filter-bin-clause clause)))))
