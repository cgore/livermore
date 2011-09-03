;;;; Copyright (C) 2005 -- 2008, all rights reserved.
;;;; Christopher Mark Gore.
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281.
;;;; WWW: <http://www.cgore.com>.
;;;; E-mail: <chris-gore@earthlink.net>.
;;;; $Date: 2008-05-22 19:35:07 -0500 (Thu, 22 May 2008) $
;;;; $HeadURL: file:///var/svn/trading/trunk/csv.lisp $
;;;; $Revision: 484 $

(unless (find-package 'utilities) (load "utilities/utilities"))
(unless (find-package 'csv)
  (defpackage :csv
    (:use :common-lisp :utilities)
    (:export :*separator*
             :*quote*
             :character-list-from-stream
             :parse-character-list
             :parse-stream
             :parse-file)))
(in-package :csv)

(defparameter *separator* #\,
  "Column separater character.")
(defparameter *quote* #\"
  "Delimiter of string data; pascal-like quoted as double itself in a string.")

(defun character-list-from-stream (stream)
  "This reads all of the characters from a stream until the stream is ended.
  It has the obvious problem of requiring the stream to have an ending."
  (loop for character = (read-char stream nil) while character
        collect character))

(defun parse-character-list (list)
  "This is the real parser for CSV files.  It doesn't allow for escaped
  characters in its current form."
  (let ((result nil)
        (current-row nil)
        (current-column nil)
        (quoted? nil)
        (last-character nil))
    (flet ((push-column nil
                        (push
                          (let* ((s (concatenate 'string
                                                 (reverse current-column)))
                                 (r (read-from-string s)))
                            ;; We want to parse numbers inline.
                            (if (numberp r) r s))
                              current-row)
                        (setf current-column nil))
           (push-row nil
                     (push (reverse current-row) result)
                     (setf current-column nil
                           current-row nil)))
      (mapc #'(lambda (character)
                (cond ((and (not quoted?) (char= character *quote*))
                       ;; We don't want to deal with stray quotes.
                       (assert (null current-column))
                       (setf quoted? t))
                      ((and (not quoted?) (char= character *separator*))
                       (if (or current-column
                               (and last-character
                                    (char= last-character *separator*)))
                         (push-column)))
                      ((and (not quoted?)
                            (or (char= character #\Newline)
                                (char= character #\Return)))
                       (if (or current-column
                               (char= last-character *separator*))
                         (push-column))
                       (if (and last-character
                                (not (char= last-character #\Newline))
                                (not (char= last-character #\Return)))
                         (push-row)))
                      ((and quoted? (char= character *quote*))
                       (push-column)
                       (setf quoted? nil))
                      (t (push character current-column)))
                (setf last-character character))
            list))
    (reverse result)))

(defun parse-stream (stream)
  "This is the main CSV stream parser function.  It will return a list of lists
  containing the data.  There is no special consideration given to the first
  row, which may or may not contain a list of headers."
  (assert (streamp stream))
  (parse-character-list (character-list-from-stream stream)))

(defun parse-file (pathname)
  "This is the main CSV file parser function.  It will return a list of lists
  containing the data.  There is no special consideration given to the first
  row, which may or may not contain a list of headers."
  (with-open-file (stream pathname
                          :direction :input
                          :if-does-not-exist :error)
    (parse-stream stream)))
