(defpackage xfiltertree-fql/test
  (:use :cl :fiveam))
(in-package :xfiltertree-fql/test)

(def-suite xfiltertree-fql :description "Filter tree FQL transform tests")

(def-suite* transforms :in xfiltertree-fql)

(test queries
  (is (not (xfiltertree-fql:queries (make-instance 'xfiltertree:node :name "root"))))
  (is (equal (list (list (fql:parse-filter "event.type=PHONE_CALL") '("ALL")))
             (xfiltertree-fql:queries
              (make-instance 'xfiltertree:aggregation
                             :name "root"
                             :bins '(("event.type=PHONE_CALL" ("ALL"))))))))

(test collect-node-queries
  (is (not (xfiltertree-fql:collect-node-queries
            (make-instance 'xfiltertree:node :name "root"))))
  (is-true (let* ((child (make-instance
                          'xfiltertree:aggregation
                          :name "child"
                          :bins '(("event.type=PHONE_CALL" ("ALL")))))
                  (root (make-instance
                         'xfiltertree:node
                         :name "root"
                         :children (list child))))
             (equal
              (list (list child (list (fql:parse-filter "event.type=PHONE_CALL") '("ALL"))))
              (xfiltertree-fql:collect-node-queries root)))))

(test constrain-tree-queries
  (is-true (let* ((child (make-instance
                          'xfiltertree:aggregation
                          :name "child"
                          :bins '(("event.type=PHONE_CALL" ("ALL")))))
                  (root (make-instance
                         'xfiltertree:node
                         :name "root"
                         :children (list child)))
                  (c1 '((:EQ ("event" . "id") "x")))
                  (groups (xfiltertree-fql:constrain-tree-queries root (list c1))))
             (equal
              (list (list child
                          (list (append (fql:parse-filter "event.type=PHONE_CALL") c1)
                                '("ALL"))))
              groups))))
