;;;; Copyright (C) 2005 -- 2008, all rights reserved.
;;;; Christopher Mark Gore.
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281.
;;;; WWW: <http://www.cgore.com>.
;;;; E-mail: <chris-gore@earthlink.net>.
;;;; $Date: 2008-07-31 22:13:24 -0500 (Thu, 31 Jul 2008) $
;;;; $HeadURL: file:///var/svn/trading/trunk/time.lisp $
;;;; $Revision: 539 $

(unless (find-package 'utilities) (load "utilities/utilities"))
(unless (find-package 'time)
  (defpackage :time
    (:use :common-lisp :utilities)
    (:export :decoded-time-list
             :get-decoded-time-list
             :month-to-number
             :month-name
             :month-short-name
             :time-second
             :time-minute
             :time-hour
             :time-date
             :time-month
             :time-month-name
             :time-month-short-name
             :time-quarter
             :time-year
             :time-day
             :time-daylight-p
             :time-zone
             :encode-month-year
             :encode-year
             :yyyy-mm-dd-to-universal-time
             :dd-month-yyyy-to-universal-time
             :date-string-to-universal-time
             :dd-month-yyyy-to-decoded-time
             :dd-month-yyyy-to-decoded-time-list
             :time-julian-day-number
             :time-jdn
             :time-julian-date
             :time-jd
             :time-ordinal-date
             :time-day-of-year
             :yyyy-string
             :qqyyyy-string
             :qq-yyyy-string
             :mon-yyyy-string
             :dd-mon-yyyy-string
             :date-time-string
             :yyyy-mm-dd-string
             :iso-extended-date-string
             :iso-date-string
             :yyyymmdd-string
             :iso-short-date-string)))
(in-package :time)

(defgeneric decoded-time-list (time))

(defmethod decoded-time-list ((time integer))
  (multiple-value-list (decode-universal-time time)))

(defmethod decoded-time-list ((time list))
  time)

(defun get-decoded-time-list ()
  (multiple-value-list (get-decoded-time)))

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

(defun time-quarter (&optional (time (get-decoded-time-list)))
  ;; N.B.: We use the IDENTITY function to get rid of the fractional part of
  ;; the result from the FLOOR function.
  (identity (floor (1+ (/ (1- (time-month time)) 3)))))

(defun time-year (&optional (time (get-decoded-time-list)))
  (sixth (decoded-time-list time)))

(defun time-day (&optional (time (get-decoded-time-list)))
  (seventh (decoded-time-list time)))

(defun time-daylight-p (&optional (time (get-decoded-time-list)))
  (eighth (decoded-time-list time)))

(defun time-zone (&optional (time (get-decoded-time-list)))
  (ninth (decoded-time-list time)))

(defun encode-month-year (month year)
  (encode-universal-time
    0 0 0 1; Second, minute, hour, day.
    month year (time-zone)))

(defun encode-year (year)
  (encode-month-year 1 year))

(defun yyyy-mm-dd-to-universal-time
  (date-string 
    &key (second 0) (minute 0) (hour 0) (time-zone (time-zone)))
  "This takes a date string of the style \"[D]D-(Mon|Monthname)-YYYY\" or of
  the style \"[D]D-(Mon|Monthname)-YY\", and returns a Lisp universal time
  integer for that date."
  (let* ((split (split date-string #\-))
         (year (read-from-string (first split)))
         (month (read-from-string (second split)))
         (day (read-from-string (third split))))
    (encode-universal-time second minute hour day month year time-zone)))

(defun dd-month-yyyy-to-universal-time
  (date-string 
    &key (second 0) (minute 0) (hour 0) (time-zone (time-zone)))
  "This takes a date string of the style \"[D]D-(Mon|Monthname)-YYYY\" or of
  the style \"[D]D-(Mon|Monthname)-YY\", and returns a Lisp universal time
  integer for that date."
  (let* ((split (split date-string #\-))
         (day (read-from-string (first split)))
         (month (month-to-number (second split)))
         (year (read-from-string (third split))))
    (encode-universal-time second minute hour day month year time-zone)))

(defun date-string-to-universal-time
  (date-string 
    &key (second 0) (minute 0) (hour 0) (time-zone (time-zone)))
  (if (= 3 (length (second (split date-string #\-))))
    (dd-month-yyyy-to-universal-time date-string
                                     :second second
                                     :minute minute
                                     :hour hour
                                     :time-zone time-zone)
    (yyyy-mm-dd-to-universal-time date-string
                                  :second second
                                  :minute minute
                                  :hour hour
                                  :time-zone time-zone)))

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

(defun time-julian-day-number (&optional (time (get-decoded-time-list)))
  "This algorithm is from Wikipedia.  It returns the Julian Day Number at noon
  for a Gregorian date. Years work as: 1BC = 0, 2BC = -1, ..."
  (flet ((floor/ (&rest rest) (identity (floor (apply #'/ rest)))))
    (let* ((a (floor/ (- 14 (time-month time)) 12))
           (y (+ (time-year time) 4800 (- a)))
           (m (+ (time-month time) (* 12 a) -3)))
      (+ (time-day time)
         (floor/ (+ (* 153 m) 2) 5)
         (* 365 y)
         (floor/ y 4)
         (- (floor/ y 100))
         (floor/ y 400)
         32045))))

(function-alias 'time-jdn 'time-julian-day-number)

(defun time-julian-date (&optional (time (get-decoded-time-list)))
  (+ (time-julian-day-number time)
     (/ (- (time-hour time) 12) 24)
     (/ (time-minute time) 1440)
     (/ (time-second time) 86400)))

(function-alias 'time-jd 'time-julian-date)

(defun time-ordinal-date (&optional (time (get-decoded-time-list)))
  (1+ (- (time-julian-day-number time)
         (time-julian-day-number
           (decoded-time-list (encode-year (time-year time)))))))

(function-alias 'time-day-of-year 'time-ordinal-date)



(defun yyyy-string (&optional (time (get-decoded-time-list)))
  (format nil "~4D" (time-year time)))

(defun qqyyyy-string (&optional (time (get-decoded-time-list)))
  (format nil "~AQ~4D" (time-quarter time) (time-year time)))

(defun qq-yyyy-string (&optional (time (get-decoded-time-list)))
  (format nil "Q~A-~4D" (time-quarter time) (time-year time)))

(defun mon-yyyy-string (&optional (time (get-decoded-time-list)))
  (format nil "~3A-~4D" (time-month-short-name time) (time-year time)))

(defun dd-mon-yyyy-string (&optional (time (get-decoded-time-list)))
  (format nil "~2D-~A" (time-date time) (mon-yyyy-string time)))

(defun date-time-string (&optional (time (get-decoded-time-list)))
  (format nil "~D/~D/~D ~2,'0D:~2,'0D:~2,'0D"
          (time-month time)
          (time-day time)
          (time-year time)
          (time-hour time)
          (time-minute time)
          (time-second time)))

(defun yyyy-mm-dd-string (&optional (time (get-decoded-time-list)))
  (format nil "~4D-~2D-~2D"
          (time-year time) (time-month time) (time-date time)))

(function-alias 'iso-extended-date-string 'yyyy-mm-dd-string)

(function-alias 'iso-date-string 'iso-extended-date-string)

(defun yyyymmdd-string (&optional (time (get-decoded-time-list)))
  (format nil "~4D~2D~2D"
          (time-year time) (time-month time) (time-date time)))

(function-alias 'iso-short-date-string 'yyyymmdd-string)
