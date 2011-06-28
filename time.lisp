;;;; Christopher Mark Gore.
;;;; 8729 Lower Marine Road,
;;;; Saint Jacob, Illinois 62281.
;;;; Copyright (C) 2005, 2006, all rights reserved.
;;;;
;;;; This file provides several useful time-related functions.

(load "utilities")
(unless (find-package "TIME")
  (defpackage "TIME"
    (:use "COMMON-LISP" "UTILITIES")))
(in-package "TIME")
(export '(decoded-time-list
          get-decoded-time-list
          time-second
          time-minute
          time-hour
          time-date
          time-month
          time-month-name
          time-month-short-name
          time-year
          time-day
          time-daylight-p
          time-zone
          month-to-number
          month-name
          month-short-name
          dd-month-yyyy-to-universal-time
          dd-month-yyyy-to-decoded-time
          dd-month-yyyy-to-decoded-time-list
          dd-mon-yyyy-string
          date-time-string))

(defgeneric decoded-time-list (time))

(defmethod decoded-time-list ((time integer))
  (multiple-value-list (decode-universal-time time)))

(defmethod decoded-time-list ((time list))
  time)

(defun get-decoded-time-list ()
  (multiple-value-list (get-decoded-time)))

;;;; These functions provide named access to the elements of the specified time.
(defun time-second (&optional (time (get-decoded-time-list)))
  (first (decoded-time-list time)))

(defun time-minute (&optional (time (get-decoded-time-list)))
  (second (decoded-time-list time)))

(defun time-hour (&optional (time (get-decoded-time-list)))
  (third (decoded-time-list time)))

(defun time-date (&optional (time (get-decoded-time-list)))
  (fourth (decoded-time-list time)))

(defun time-month (&optional (time (get-decoded-time-list)))
  (fifth (decoded-time-list time)))

(defun time-month-name (&optional (time (get-decoded-time-list)))
  (month-name (time-month time)))

(defun time-month-short-name (&optional (time (get-decoded-time-list)))
  (month-short-name (time-month time)))

(defun time-year (&optional (time (get-decoded-time-list)))
  (sixth (decoded-time-list time)))

(defun time-day (&optional (time (get-decoded-time-list)))
  (seventh (decoded-time-list time)))

(defun time-daylight-p (&optional (time (get-decoded-time-list)))
  (eighth (decoded-time-list time)))

(defun time-zone (&optional (time (get-decoded-time-list)))
  (ninth (decoded-time-list time)))

;;; The function MONTH-TO-NUMBER takes a string that is an (English) month name
;;; and returns the number of the month, where January=1, ..., December=12.
(defun month-to-number (month)
  (let ((months '(("jan" 1) ("january" 1)
                  ("feb" 2) ("february" 2)
                  ("mar" 3) ("march" 3)
                  ("apr" 4) ("april" 4)
                  ("may" 5)
                  ("jun" 6) ("june" 6)
                  ("jul" 7) ("july" 7)
                  ("aug" 8) ("august" 8)
                  ("sep" 9) ("sept" 9) ("september" 9)
                  ("oct" 10) ("october" 10)
                  ("nov" 11) ("november" 11)
                  ("dec" 12) ("december" 12))))
    (second (assoc month months :test #'string-equal))))

;;; The function MONTH-NAME, given a month number (1=January, ..., 12=December),
;;; returns the full month name.
(defun month-name (month-number)
  (svref #("January" "February" "March" "April" "May" "June"
           "July" "August" "September" "October" "November" "December")
         (1- month-number)))

(defun month-short-name (month-number)
  "The function MONTH-SHORT-NAME, given a month number
  (1=January, ..., 12=December), returns the abbreviated month name."
  (svref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
         (1- month-number)))

(defun dd-month-yyyy-to-universal-time
  (date-string 
    &key (second 0) (minute 0) (hour 0) (time-zone (time-zone)))
  "This takes a date string of the style \"[D]D-(Mon|Monthname)-YYYY\" or of
  the style \"[D]D-(Mon|Monthname)-YY\", and returns a Lisp universal time
  integer for that date."
  (let ((result))
    (setf result (split date-string #\-))
    (setf result (list (read-from-string (first result))
                       (month-to-number (second result))
                       (read-from-string (third result))))
    (encode-universal-time second minute hour
                           (first result) (second result) (third result)
                           time-zone)))

(defun dd-month-yyyy-to-decoded-time
  (date-string
    &key (second 0) (minute 0) (hour 0) (time-zone (time-zone)))
  "This takes a date string of the style \"[D]D-(Mon|Monthname)-YYYY\" or of
  the style \"[D]D-(Mon|Monthname)-YY\", and returns a Lisp decoded time
  for that date."
    (decode-universal-time
      (dd-month-yyyy-to-universal-time
        date-string
        :second second
        :minute minute
        :hour hour
        :time-zone time-zone)))

(defun dd-month-yyyy-to-decoded-time-list
  (date-string
    &key (second 0) (minute 0) (hour 0) (time-zone (time-zone)))
  "This takes a date string of the style \"[D]D-(Mon|Monthname)-YYYY\" or of
  the style \"[D]D-(Mon|Monthname)-YY\", and returns a Lisp decoded time
  for that date."
  (multiple-value-list
      (dd-month-yyyy-to-decoded-time
        date-string
        :second second
        :minute minute
        :hour hour
        :time-zone time-zone)))

(defun dd-mon-yyyy-string (&optional (time (get-decoded-time-list)))
  (format nil "~2D-~3A-~4D"
          (time-date time)
          (time-month-short-name time)
          (time-year time)))

(defun date-time-string (&optional (time (get-decoded-time-list)))
  (format nil "~D/~D/~D ~2,'0D:~2,'0D:2,'0D"
          (time-month time)
          (time-day time)
          (time-year time)
          (time-hour time)
          (time-minute time)
          (time-second time)))
