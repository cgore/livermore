;;;; Christopher Mark Gore.
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281.
;;;; WWW: <http://www.cgore.com>.
;;;; E-mail: <chris-gore@earthlink.net>.
;;;;
;;;; Last edited Saturday, July 15, AD 2006; xb.
;;;;
;;;; Copyright (C) 2006.
;;;; All rights reserved.
;;;;
;;;; The compilation of software is distributed under the following terms:
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.

(load "utilities")
(unless (find-package 'csv)
  (defpackage "CSV"
    (:use "COMMON-LISP" "UTILITIES")
    (:export "PARSE-STREAM" "PARSE-FILE")))
(in-package "CSV")

(defparameter *csv-quote* #\"
  "Delimiter of string data; pascal-like quoted as double itself in a string.")

(defun stream-to-char-list (stream)
  "This reads all of the characters from a stream until the stream is ended.
  It has the obvious problem of requiring the stream to have an ending."
  (loop for c = (read-char stream nil)
        while c collect c))

(defun collapse-strings (list)
  "Quoted strings in CSV files are enclosed by nondirected double-quote
  characters.  A literal nondirected double-quote character is represented by
  two in immediate succession."
  (let ((result nil)
        (quoted? nil)
        (nested-quotes 0)
        (quoted-list nil))
    (mapc #'(lambda (character)
              (cond ((and (plusp nested-quotes)
                          (not (char= *csv-quote* character)))
                     ;; We have encountered escaped nondirectional double-quote
                     ;; characters embedded in the quoted string, or possibly
                     ;; just the end of the quoted string.  Escaped quotes are
                     ;; represented by two nondirected double-quote characters
                     ;; in immediate succession, so we may just push the
                     ;; appropriate number of literal nondirected double-quote
                     ;; characters onto the QUOTED-LIST.
                     (dotimes (i (floor (/ (1- nested-quotes) 2)))
                       (push *csv-quote* quoted-list))
                     (when (oddp nested-quotes)
                       ;; This can only be an odd number of characters if we
                       ;; are terminating the quoted string.
                       (push (coerce (reverse quoted-list) 'string) result)
                       (setf quoted-list nil))
                     (setf nested-quotes 0))
                    ((and quoted?  (char= *csv-quote* character))
                     ;; We count the number of quotes seen."
                     (incf nested-quotes))
                    ((and (not quoted?) (char= *csv-quote* character))
                     ;; Opening of a quoted string."
                     (setf quoted? t))
                    (quoted? (push character quoted-list))
                    (t (push character result))))
          list)
    (reverse result)))

(defun split-records (list)
  (remove nil (split list (list #\Return #\Linefeed #\Newline) :test #'equalp)))

(defun split-fields (list)
  (split list (list #\, #\;) :test #'equalp))

(defun parse-field (field)
  "As far as I can tell, the only two data types in CSV files are numbers and
  strings.  If more are needed, then you need to write your own parser and then
  pass it in to the main functions.  This one is the default."
  ;; TODO: This doesn't correctly handle quoted strings.
  (setf field (apply #'string-concatenate field))
  (let ((x (multiple-value-list (read-from-string field))))
    (if (and (numberp (first x))
             (= (length field)
                (second x)))
      (first x)
      field)))

(defun parse-stream (stream &key (field-parser #'parse-field))
  "This is the main CSV stream parser function.  It will return a list of lists
  containing the data.  There is no special consideration given to the first
  row, which may or may not contain a list of headers."
  (assert (streamp stream))
  (assert (functionp field-parser))
  (mapcar #'(lambda (record)
              (mapcar field-parser record))
          (mapcar #'split-fields
                  (split-records
                    (collapse-strings
                      (stream-to-char-list stream))))))

(defun parse-file (pathname &key (field-parser #'parse-field))
  "This is the main CSV file parser function.  It will return a list of lists
  containing the data.  There is no special consideration given to the first
  row, which may or may not contain a list of headers."
  (assert (functionp field-parser))
  (with-open-file (stream pathname :direction :input :if-does-not-exist :error)
    (parse-stream stream :field-parser field-parser)))
