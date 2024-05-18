(defpackage fql-util/test
  (:use :cl :fiveam))
(in-package :fql-util/test)

(def-suite fql-util :description "Filter query DSL test")

(def-suite* queries :in fql-util)

(test expr-first-table
  (is (equal '("event" . "type")
             (fql-util:expr-first-table '(:EQ ("event" . "type") "PHONE_CALL"))))
  (is (equal '("event" . "type")
             (fql-util:expr-first-table '(:EQ "PHONE_CALL" ("event" . "type")))))
  (is (not (fql-util:expr-first-table '(:EQ "PHONE_CALL" "PHONE_CALL")))))

(test group-first-table
  (is (equal '("event" . "type")
             (fql-util:group-first-table '((:EQ ("event" . "type") "PHONE_CALL")))))
  (is (equal '("event" . "type")
             (fql-util:group-first-table '((:EQ "PHONE_CALL" ("event" . "type"))))))
  (is (not (fql-util:group-first-table '((:EQ "PHONE_CALL" "PHONE_CALL"))))))

(test expr-table-names
  (is (equal '("event")
             (fql-util:expr-table-names '(:EQ ("event" . "type") "PHONE_CALL"))))
  (is (equal '("event" "endpoint")
             (fql-util:expr-table-names '(:EQ ("event" . "id") ("endpoint" "egressor")))))
  (is (equal '("event")
             (fql-util:expr-table-names '(:EQ ("event" . "id") ("event" . "dummy")))))
  (is (not (fql-util:expr-table-names '(:EQ 1 1)))))

(test group-table-names
  (is (equal '("event")
             (fql-util:group-table-names '((:EQ ("event" . "connectionStatus") "ACCEPTED")))))
  (is (equal '("event")
             (fql-util:group-table-names '((:EQ ("event" . "connectionStatus") "ACCEPTED")
                                           (:EQ ("event" . "type") "PHONE_CALL")))))
  (is (equal '("event" "endpoint")
             (fql-util:group-table-names '((:EQ ("event" . "id") "x")
                                           (:EQ ("endpoint" . "egressor") "x")))))
  (is (not (fql-util:group-table-names nil))))
