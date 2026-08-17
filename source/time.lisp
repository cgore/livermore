;;;; Copyright (c) 2005 -- 2026, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.


(defpackage :livermore/time
  (:use :common-lisp
        :sigma/behave
        :sigma/control
        :sigma/string)
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
           :iso-short-date-string))
(in-package :livermore/time)

(defgeneric decoded-time-list (time)
  (:documentation
   "This returns TIME as a decoded-time list of nine values, the same
   values DECODE-UNIVERSAL-TIME produces."))

(defmethod decoded-time-list ((time integer))
  "This decodes a Lisp universal time."
  (multiple-value-list (decode-universal-time time)))

(defmethod decoded-time-list ((time list))
  "This returns TIME unchanged when it is already a decoded-time list."
  time)

(defun get-decoded-time-list ()
  "This returns the current time as a decoded-time list."
  (multiple-value-list (get-decoded-time)))

(defun month-to-number (month)
  "This returns the month number for an English month name, January = 1
  through December = 12.  Short names such as \"Jan\" are accepted."
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

(defun month-name (month-number)
  "This returns the full English month name for MONTH-NUMBER, where
  January = 1 and December = 12."
  (svref #("January" "February" "March" "April" "May" "June"
           "July" "August" "September" "October" "November" "December")
         (1- month-number)))

(defun month-short-name (month-number)
  "The function MONTH-SHORT-NAME, given a month number
  (1=January, ..., 12=December), returns the abbreviated month name."
  (svref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
         (1- month-number)))

(defun time-second (&optional (time (get-decoded-time-list)))
  "This returns the second field of TIME, 0 through 59."
  (first (decoded-time-list time)))

(defun time-minute (&optional (time (get-decoded-time-list)))
  "This returns the minute field of TIME, 0 through 59."
  (second (decoded-time-list time)))

(defun time-hour (&optional (time (get-decoded-time-list)))
  "This returns the hour field of TIME, 0 through 23."
  (third (decoded-time-list time)))

(defun time-date (&optional (time (get-decoded-time-list)))
  "This returns the day of the month of TIME, 1 through 31."
  (fourth (decoded-time-list time)))

(defun time-month (&optional (time (get-decoded-time-list)))
  "This returns the month number of TIME, January = 1 through December = 12."
  (fifth (decoded-time-list time)))

(defun time-month-name (&optional (time (get-decoded-time-list)))
  "This returns the full English month name of TIME."
  (month-name (time-month time)))

(defun time-month-short-name (&optional (time (get-decoded-time-list)))
  "This returns the abbreviated English month name of TIME."
  (month-short-name (time-month time)))

(defun time-quarter (&optional (time (get-decoded-time-list)))
  "This returns the calendar quarter of TIME, 1 through 4."
  ;; N.B.: We use the IDENTITY function to get rid of the fractional part of
  ;; the result from the FLOOR function.
  (identity (floor (1+ (/ (1- (time-month time)) 3)))))

(defun time-year (&optional (time (get-decoded-time-list)))
  "This returns the year of TIME."
  (sixth (decoded-time-list time)))

(defun time-day (&optional (time (get-decoded-time-list)))
  "This returns the day of the week of TIME, Monday = 0 through Sunday = 6."
  (seventh (decoded-time-list time)))

(defun time-daylight-p (&optional (time (get-decoded-time-list)))
  "This is true when TIME is in daylight-saving time."
  (eighth (decoded-time-list time)))

(defun time-zone (&optional (time (get-decoded-time-list)))
  "This returns the time zone of TIME as hours west of GMT."
  (ninth (decoded-time-list time)))

(defun encode-month-year (month year)
  "This returns the universal time of the first of MONTH in YEAR."
  (encode-universal-time
    0 0 0 1; Second, minute, hour, day.
    month year (time-zone)))

(defun encode-year (year)
  "This returns the universal time of 1 January of YEAR."
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
  "This parses DATE-STRING as either YYYY-MM-DD or DD-Mon-YYYY, choosing
  by the length of the middle field."
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

(function-alias 'time-julian-day-number 'time-jdn)

(defun time-julian-date (&optional (time (get-decoded-time-list)))
  "This returns the Julian Date of TIME, including the fraction of the day
  past noon."
  (+ (time-julian-day-number time)
     (/ (- (time-hour time) 12) 24)
     (/ (time-minute time) 1440)
     (/ (time-second time) 86400)))

(function-alias 'time-julian-date 'time-jd)

(defun time-ordinal-date (&optional (time (get-decoded-time-list)))
  "This returns the day of the year of TIME, 1 through 366."
  (1+ (- (time-julian-day-number time)
         (time-julian-day-number
           (decoded-time-list (encode-year (time-year time)))))))

(function-alias 'time-ordinal-date 'time-day-of-year)



(defun yyyy-string (&optional (time (get-decoded-time-list)))
  "This returns the year of TIME as a four-digit string."
  (format nil "~4D" (time-year time)))

(defun qqyyyy-string (&optional (time (get-decoded-time-list)))
  "This returns TIME as a string such as \"1Q2020\"."
  (format nil "~AQ~4D" (time-quarter time) (time-year time)))

(defun qq-yyyy-string (&optional (time (get-decoded-time-list)))
  "This returns TIME as a string such as \"Q1-2020\"."
  (format nil "Q~A-~4D" (time-quarter time) (time-year time)))

(defun mon-yyyy-string (&optional (time (get-decoded-time-list)))
  "This returns TIME as a string such as \"Mar-2020\"."
  (format nil "~3A-~4D" (time-month-short-name time) (time-year time)))

(defun dd-mon-yyyy-string (&optional (time (get-decoded-time-list)))
  "This returns TIME as a string such as \"15-Mar-2020\"."
  (format nil "~2D-~A" (time-date time) (mon-yyyy-string time)))

(defun date-time-string (&optional (time (get-decoded-time-list)))
  "This returns TIME as a string of month, weekday (Monday = 0), year, and
  zero-padded hours, minutes, and seconds."
  (format nil "~D/~D/~D ~2,'0D:~2,'0D:~2,'0D"
          (time-month time)
          (time-day time)
          (time-year time)
          (time-hour time)
          (time-minute time)
          (time-second time)))

(defun yyyy-mm-dd-string (&optional (time (get-decoded-time-list)))
  "This returns TIME as a string such as \"2020-3-15\"."
  (format nil "~4D-~2D-~2D"
          (time-year time) (time-month time) (time-date time)))

(function-alias 'yyyy-mm-dd-string 'iso-extended-date-string)

(function-alias 'iso-extended-date-string 'iso-date-string)

(defun yyyymmdd-string (&optional (time (get-decoded-time-list)))
  "This returns TIME as a string such as \"2020315\"."
  (format nil "~4D~2D~2D"
          (time-year time) (time-month time) (time-date time)))

(function-alias 'yyyymmdd-string 'iso-short-date-string)

(behavior 'month-to-number
  (should= 1 (month-to-number "jan"))
  (should= 1 (month-to-number "January"))
  (should= 9 (month-to-number "sept"))
  (should= 12 (month-to-number "DECEMBER"))
  (should-be-null (month-to-number "not-a-month")))

(behavior 'month-name
  (should-string= "January" (month-name 1))
  (should-string= "March" (month-name 3))
  (should-string= "December" (month-name 12)))

(behavior 'month-short-name
  (should-string= "Jan" (month-short-name 1))
  (should-string= "Mar" (month-short-name 3))
  (should-string= "Dec" (month-short-name 12)))

(behavior 'decoded-time-accessors
  (let ((time '(5 10 15 20 3 2020 6 nil 0)))
    (should= 5 (time-second time))
    (should= 10 (time-minute time))
    (should= 15 (time-hour time))
    (should= 20 (time-date time))
    (should= 3 (time-month time))
    (should= 2020 (time-year time))
    (should= 1 (time-quarter time))
    (should-string= "March" (time-month-name time))
    (should-string= "Mar" (time-month-short-name time))))

(behavior 'time-quarter
  (should= 1 (time-quarter '(0 0 0 1 1 2020 2 nil 0)))
  (should= 1 (time-quarter '(0 0 0 1 3 2020 6 nil 0)))
  (should= 2 (time-quarter '(0 0 0 1 4 2020 2 nil 0)))
  (should= 3 (time-quarter '(0 0 0 1 7 2020 2 nil 0)))
  (should= 4 (time-quarter '(0 0 0 1 10 2020 3 nil 0))))

(behavior 'date-string-parsers
  (let ((zone 0))
    (spec "YYYY-MM-DD and DD-Mon-YYYY parse to the same instant"
      (should= (yyyy-mm-dd-to-universal-time "2020-3-15" :time-zone zone)
               (dd-month-yyyy-to-universal-time "15-Mar-2020" :time-zone zone)))
    (spec "date-string-to-universal-time dispatches on the middle field"
      (should= (yyyy-mm-dd-to-universal-time "2020-03-15" :time-zone zone)
               (date-string-to-universal-time "2020-03-15" :time-zone zone))
      (should= (dd-month-yyyy-to-universal-time "15-Mar-2020" :time-zone zone)
               (date-string-to-universal-time "15-Mar-2020" :time-zone zone)))))

(behavior 'date-formatters
  (let ((time '(0 0 0 15 3 2020 6 nil 0)))
    (should-string= "2020" (yyyy-string time))
    (should-string= "1Q2020" (qqyyyy-string time))
    (should-string= "Q1-2020" (qq-yyyy-string time))
    (should-string= "Mar-2020" (mon-yyyy-string time))
    (should-string= "15-Mar-2020" (dd-mon-yyyy-string time))))
