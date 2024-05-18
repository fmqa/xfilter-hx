(defpackage xfiltertree-eqvalg/test
  (:use :cl :fiveam))
(in-package :xfiltertree-eqvalg/test)

(def-suite xfiltertree-eqvalg :description "Filter tree transform tests")

(def-suite* transforms :in xfiltertree-eqvalg)

(test queries
  (is (not (xfiltertree-eqvalg:queries (make-instance 'xfiltertree:node :name "root"))))
  (is (equal (list (list (fql:parse-filter "event.type=PHONE_CALL") '("ALL")))
             (xfiltertree-eqvalg:queries
              (make-instance 'xfiltertree:aggregation
                             :name "root"
                             :bins '(("event.type=PHONE_CALL" ("ALL"))))))))

(test collect-node-queries
  (is (not (xfiltertree-eqvalg:collect-node-queries
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
              (xfiltertree-eqvalg:collect-node-queries root)))))

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
                  (groups (xfiltertree-eqvalg:constrain-tree-queries root (list c1))))
             (equal
              (list (list child
                          (list (append (fql:parse-filter "event.type=PHONE_CALL") c1)
                                '("ALL"))))
              groups))))
