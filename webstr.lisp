(defpackage webstr
  (:use cl)
  (:export #:escape #:unescape))
(in-package :webstr)

(defun css (char)
  (if (alphanumericp char)
      char
      (format nil "_~X_" (char-code char))))

(defun uncss (string)
  (code-char (parse-integer (remove #\_ string) :radix 16)))

(defun write-string-or-char (val out)
  (cond ((stringp val) (write-string val out))
        ((characterp val) (write-char val out))))

(defun escape (string)
  "Hex-escape all non-alphanumeric characters in the given string, using an
   underscore _ as the escape character."
  (with-output-to-string (out)
    (loop for char across string
          for replacement = (css char)
          do (write-string-or-char replacement out))))

(defun unescape-offsets (string)
  (loop for start = (position #\_ string) then (position #\_ string :start (1+ end))
        while start
        for end = (position #\_ string :start (1+ start))
        if (and start end) collect (cons start end)))

(defun unescape (string)
  "Unescape a hex-escaped string."
  (with-output-to-string (out)
    (loop with offsets = (unescape-offsets string)
          with start = 0
          for end = (if offsets (caar offsets) (length string))
          while (< start (length string))
          do (progn (write-string string out :start start :end end)
                    (setf start end)
                    (when offsets
                      (setf start (caar offsets))
                      (setf end (1+ (cdar offsets)))
                      (write-string-or-char (uncss (subseq string start end)) out)
                      (setf start end)
                      (setf offsets (cdr offsets)))))))
