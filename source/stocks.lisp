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


(defpackage :livermore/stocks
  (:use :common-lisp
        #+cmu :extensions #+sbcl :sb-ext
        :livermore/time
        :livermore/statistics
        :livermore/stock-ticker-descriptions
        :sigma/control
        :sigma/numeric
        :sigma/os
        :sigma/sequence)
  (:export :*initial-money*
           :canonical-ticker
           :stock-description
           :table-filename
           :components-filename
           :retrieve-stock-data
           :retrieve-stocks-data
           :retrieve-index-components
           :index-components
           :buy-and-hold-gains
           :b&h-gains
           :buy-and-hold-performance
           :b&h-performance
           :cash-optimal-gains
           :cash-optimal-performance
           :buy-and-hold
           :report-results
           :table-record
           :adjustment
           :opening-time
           :closing-time
           :opening-price
           :adjusted-opening-price
           :high-price
           :adjusted-high-price
           :low-price
           :adjusted-low-price
           :closing-price
           :trading-volume
           :adjusted-closing-price
           :table
           :preferred-records
           :records-start
           :records-end
           :daily-records
           :weekly-records
           :monthly-records
           :quarterly-records
           :yearly-records
           :ticker-symbol
           :records
           :elt-record
           :stock-description
           :load-table
           :difference
           :value-ratio
           :percent-change
           :spread-difference
           :spread-ratio
           :spread-percent-change
           :arithmetic-mean
           :sample-variance
           :unbiased-sample-variance
           :sample-standard-deviation
           :unbiased-sample-standard-deviation
           :minimum
           :maximum
           :minimum?
           :maximum?
           :sample-z-score
           :unbiased-sample-z-score
           :plot
           :shares-buyable
           :simple-moving-average
           :sma
           :arithmetically-weighted-moving-average
           :awma
           :exponential-moving-average
           :ema
           :moving-average-convergence-divergence
           :macd
           :macd-12-26-9
           :macd-textbook
           :macd-signal
           :upward-changes
           :downward-changes
           :relative-strength
           :rs
           :ema-rs
           :wilder-rs
           :ema-rs-27
           :rs-textbook
           :sma-rs
           :cutler-rs
           :relative-strength-index
           :rsi
           :ema-rsi
           :wilder-rsi
           :ema-rsi-27
           :sma-rsi
           :cutler-rsi
           :rsi-textbook
           :simple-directional-signal
           :simple-directional-gains
           :simple-directional-performance
           :dual-simple-directional-signal
           :dual-simple-directional-gains
           :dual-simple-directional-performance
           :sma-crossover-signal
           :directional-sma-crossover-signal
           :sma-crossover-gains
           :sma-crossover-long/short-gains
           :directional-sma-crossover-gains
           :sma-crossover-performance
           :sma-crossover-long/short-performance
           :directional-sma-crossover-performance
           :sma-crossover-optimal
           :sma-crossover-long/short-optimal
           :directional-sma-crossover-optimal
           :^dji
           :^gspc
           :^ixic))
(in-package :livermore/stocks)

;; Connect to the database.
;; (clsql:connect '("localhost" "livermore" "livermore" "stupidhead")
;;                  :database-type :postgresql-socket)

(defgeneric records (table)
  (:documentation
   "This returns the preferred records of TABLE, clipped to RECORDS-START
   and RECORDS-END."))

(defgeneric adjustment (table-record)
  (:documentation
   "This is the split-adjustment factor: adjusted close over close."))

(defgeneric adjusted-opening-price (table-record)
  (:documentation "This is the opening price times the adjustment factor."))

(defgeneric adjusted-high-price (table-record)
  (:documentation "This is the high price times the adjustment factor."))

(defgeneric adjusted-low-price (table-record)
  (:documentation "This is the low price times the adjustment factor."))

(defgeneric generate-daily-records (table)
  (:documentation "This returns the daily records of TABLE."))

(defgeneric daily-records (table)
  (:documentation "This returns the daily records of TABLE."))

(defgeneric generate-monthly-records (table)
  (:documentation
   "This builds and stores monthly OHLC records from the daily records."))

(defgeneric monthly-records (table)
  (:documentation
   "This returns the monthly records, generating them if needed."))

(defgeneric generate-yearly-records (table)
  (:documentation
   "This builds and stores yearly OHLC records from the daily records."))

(defgeneric yearly-records (table)
  (:documentation
   "This returns the yearly records, generating them if needed."))

(defgeneric elt-record (table position &optional key)
  (:documentation
   "This returns the record at POSITION, or KEY applied to it.  A NIL
   position is the last record."))

(defgeneric difference (table &key key start end)
  (:documentation
   "This is KEY at END minus KEY at START."))

(defgeneric value-ratio (table &key key start end)
  (:documentation
   "This is KEY at END divided by KEY at START."))

(defgeneric percent-change (table &key key start end)
  (:documentation
   "This is 100 times VALUE-RATIO from START to END."))

(defgeneric spread-difference (table &key key position spread)
  (:documentation
   "This is the difference of KEY between POSITION+SPREAD and POSITION."))

(defgeneric spread-ratio (table &key key position spread)
  (:documentation
   "This is the ratio of KEY between POSITION+SPREAD and POSITION."))

(defgeneric spread-percent-change (table &key key position spread)
  (:documentation
   "This is the percent change of KEY between POSITION+SPREAD and POSITION."))

(defgeneric sample-z-score (table &key key start end)
  (:documentation
   "This is the sample z-score of KEY at START over the range START to END."))

(defgeneric unbiased-sample-z-score (table &key key start end)
  (:documentation
   "This is the unbiased sample z-score of KEY at START over START to END."))

(defgeneric shares-buyable (record money)
  (:documentation
   "This is how many whole shares MONEY buys at the adjusted close."))

(defgeneric buy-and-hold (table initial-money &key start end)
  (:documentation
   "This is INITIAL-MONEY grown by the value ratio from START to END."))

;; This is the initial quantity of money for the various analyzer methods.
(defparameter *initial-money* 100000.00)

;; I just copied these user agent strings from my Apache access logs; they all
;; are real-world examples.  If we don't specify an agent then Yahoo! Finance
;; won't give us any data, presumably to try to prevent data mining.
(let (;;(msie-5.5 "Mozilla/4.0 (compatible; MSIE 5.5; Windows NT 5.0)")
      ;;(msie-6.0 "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1. 0.3705; .NET CLR 1.1.4322; Media Center PC 4.0; BO1IE8_v1;ENUS)")
      ;;(msie-7.0 "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.2;  SLCC1;  .NET CLR 1.1.4322;  . NET CLR 2.0.40607;  .NET CLR 3.0.04506.648)")
      (msie-8.0 "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET    CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; yie8)")
      ;;(msie-9.0 "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)")
      ;;(ubuntu-firefox-5 "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.10) Gecko/20100915 Ubuntu/10.04 (lucid)  Firefox/3.6.10 GTB7.1")
      )
  (defun wget (&rest arguments)
    "A simple wrapper for GNU wget."
    (#+cmu extensions:run-program #+sbcl sb-ext:run-program
      "/usr/bin/wget"
      (append (list "-q" "-U" msie-8.0) arguments)
      :output t)
    (values)))

(defun canonical-ticker (ticker)
  "Turns a ticker symbol into its canonical form.  We are following Yahoo!
Finance's style of tickers for now."
  (if (symbolp ticker)
    (setf ticker (symbol-name ticker)))
  (string-upcase ticker))

;; This is the directory where all of the stock trading data should be stored.
(defparameter *livermore-data-directory*
  (merge-pathnames #P"data/yahoo-finance-data/"
                   (asdf:system-source-directory :livermore))
  "Absolute path to the yahoo-finance-data directory.")

(defun description-filename (ticker-symbol)
  "This is the path of the Yahoo description CSV for TICKER-SYMBOL."
  (merge-pathnames (make-pathname
                    :name (concatenate 'string (canonical-ticker ticker-symbol) "-description")
                    :type "csv")
                   *livermore-data-directory*))

(defun table-filename (ticker-symbol)
  "This is the path of the Yahoo price table CSV for TICKER-SYMBOL."
  (merge-pathnames (make-pathname
                    :name (concatenate 'string (canonical-ticker ticker-symbol) "-table")
                    :type "csv")
                   *livermore-data-directory*))

(defun components-filename (index-symbol)
  "This is the path of the index-components CSV for INDEX-SYMBOL."
  (merge-pathnames (make-pathname
                    :name (concatenate 'string (canonical-ticker index-symbol) "-components")
                    :type "csv")
                   *livermore-data-directory*))

(defclass table-record ()
  ((opening-time
    :accessor opening-time
    :initarg :opening-time
    :type integer
    :documentation "The session open as a Lisp universal time.")
   (closing-time
    :accessor closing-time
    :initarg :closing-time
    :type integer
    :documentation "The session close as a Lisp universal time.")
   (descriptive-time
    :accessor descriptive-time
    :initarg :descriptive-time
    :type string
    :documentation "A printable date for this record.")
   (opening-price
    :accessor opening-price
    :initarg :opening-price
    :type float
    :documentation "The unadjusted opening price.")
   (high-price
    :accessor high-price
    :initarg :high-price
    :type float
    :documentation "The unadjusted high price.")
   (low-price
    :accessor low-price
    :initarg :low-price
    :type float
    :documentation "The unadjusted low price.")
   (closing-price
    :accessor closing-price
    :initarg :closing-price
    :type float
    :documentation "The unadjusted closing price.")
   (trading-volume
    :accessor trading-volume
    :initarg :trading-volume
    :type (integer 0 *)
    :documentation "The session volume.")
   (adjusted-closing-price
    :accessor adjusted-closing-price
    :initarg :adjusted-closing-price
    :type float
    :documentation "The split-adjusted closing price."))
  (:documentation
   "One OHLC bar: times, prices, volume, and an adjusted close."))

(defmethod print-object ((table-record table-record) stream)
  (format stream "(stocks:table-record ~%:opening-time ~A :closing-time ~A :descriptive-time ~A ~%:opening-price ~A :high-price ~A :low-price ~A ~%:closing-price ~A :trading-volume ~A :adjusted-closing-price ~A)"
          (opening-time table-record)
          (closing-time table-record)
          (descriptive-time table-record)
          (opening-price table-record)
          (high-price table-record)
          (low-price table-record)
          (closing-price table-record)
          (trading-volume table-record)
          (adjusted-closing-price table-record)))

(defun table-record (&rest rest)
  "This is a simple constructor for the TABLE-RECORD class."
  (apply #'make-instance 'table-record rest))

(defmethod duplicate ((table-record table-record))
  "This returns a newly created copy of TABLE-RECORD."
  (make-instance 'table-record
                 :opening-time (opening-time table-record)
                 :closing-time (closing-time table-record)
                 :descriptive-time (descriptive-time table-record)
                 :opening-price (opening-price table-record)
                 :high-price (high-price table-record)
                 :low-price (low-price table-record)
                 :closing-price (closing-price table-record)
                 :trading-volume (trading-volume table-record)
                 :adjusted-closing-price (adjusted-closing-price table-record)))

(defmethod adjustment ((table-record table-record))
  (/ (adjusted-closing-price table-record) (closing-price table-record)))


(defmethod adjusted-opening-price ((table-record table-record))
  (* (opening-price table-record) (adjustment table-record)))


(defmethod adjusted-high-price ((table-record table-record))
  (* (high-price table-record) (adjustment table-record)))


(defmethod adjusted-low-price ((table-record table-record))
  (* (low-price table-record) (adjustment table-record)))

(defclass table ()
  ((ticker-symbol
     :accessor ticker-symbol
     :initform ""
     :initarg :ticker-symbol
     :type string
     :documentation "The ticker this table holds, in canonical form.")
   (preferred-records
     :accessor preferred-records
     :initform #'daily-records
     :initarg :preferred-records
     :documentation
     "A function of the table that returns the working bar series.")
   (records-start
     :accessor records-start
     :initform 0
     :initarg :records-start
     :documentation "The first index of RECORDS to use.")
   (records-end
     :accessor records-end
     :initform nil
     :initarg :records-end
     :documentation "The exclusive end index of RECORDS, or NIL for the end.")
   (weekly-records
     :initform nil
     :initarg weekly-records
     :type list)
   (monthly-records
     :initform nil
     :initarg monthly-records
     :type list)
   (quarterly-records
     :initform nil
     :initarg quarterly-records
     :type list)
   (yearly-records
     :initform nil
     :initarg yearly-records
     :type list)
   (records
     :initform nil
     :initarg :records
     :type list))
  (:documentation
   "A price table for one ticker, with daily bars and optional aggregates."))


(defmethod records ((table table))
  (subseq (funcall (preferred-records table) table)
          (records-start table)
          (records-end table)))

(defmethod duplicate ((table table))
  "This returns a newly created copy of TABLE, including its records."
  (make-instance 'table
                 :ticker-symbol (duplicate (ticker-symbol table))
                 :preferred-records (duplicate (preferred-records table))
                 :weekly-records (duplicate (slot-value table 'weekly-records))
                 :monthly-records (duplicate
                                    (slot-value table 'monthly-records))
                 :quarterly-records (duplicate
                                      (slot-value table 'quarterly-records))
                 :yearly-records (duplicate (slot-value table 'yearly-records))
                 :records (duplicate (slot-value table 'records))))

(defmethod generate-daily-records ((table table))
  ;; Effectively a no-op for now, until we start defaulting to intraday data.
  (slot-value table 'records))

(defmethod daily-records ((table table))
  ;; XXX: This will need to be changed whenever we get intraday data.
  (slot-value table 'records))

(defmethod generate-monthly-records ((table table))
  (with-slots (monthly-records) table
    (setf monthly-records nil)
    (let ((current-record (first (daily-records table)))
          (current-month (time-month (opening-time (first (daily-records table)))))
          (current-year (time-year (opening-time (first (daily-records table))))))
      (mapcar #'(lambda (record)
                  (if (or (< current-month
                             (time-month (opening-time record)))
                          (< current-year
                             (time-year (opening-time record))))
                    (progn (push current-record monthly-records)
                           (psetf current-month
                                  (time-month (opening-time record))
                                  current-year
                                  (time-year (opening-time record))
                                  current-record record
                                  (opening-time current-record)
                                  (opening-time record)
                                  (closing-time current-record)
                                  (closing-time record)
                                  (descriptive-time current-record)
                                  (mon-yyyy-string
                                    (opening-time current-record))))
                    (setf
                      ;; Opening time is the earliest time over all.
                      (opening-time current-record)
                      (min (opening-time current-record)
                           (opening-time record))
                      ;; Closing time is the latest time over all.
                      (closing-time current-record)
                      (max (closing-time current-record)
                           (closing-time record))
                      ;; High price is the highest over all.
                      (high-price current-record)
                      (max (high-price current-record)
                           (high-price record))
                      ;; Low price is the lowest over all.
                      (low-price current-record)
                      (min (low-price current-record)
                           (low-price record))
                      ;; The closing price is with the newer closing price, not
                      ;; the older one.
                      (closing-price current-record)
                      (closing-price record)
                      ;; The opening price would be the older one.
                      (adjusted-closing-price current-record)
                      (adjusted-closing-price record)
                      ;; Add the new valume in to the volume we have so far.
                      (trading-volume current-record)
                      (+ (trading-volume current-record)
                         (trading-volume record)))))
              (daily-records table))
      (push current-record monthly-records)
      (setf monthly-records (reverse monthly-records)))
    monthly-records))

(defmethod monthly-records ((table table))
  (aif (slot-value table 'monthly-records) it (generate-monthly-records table)))

(defmethod generate-yearly-records ((table table))
  (with-slots (yearly-records) table
    (setf yearly-records nil)
    (let ((current-record (first (daily-records table)))
          (current-year (time-year (opening-time (first (daily-records table))))))
      (mapcar #'(lambda (record)
                  (if (< current-year (time-year (opening-time record)))
                    (progn (push current-record yearly-records)
                           (psetf current-year
                                  (time-year (opening-time record))
                                  current-record record
                                  (opening-time current-record)
                                  (opening-time record)
                                  (closing-time current-record)
                                  (closing-time record)
                                  (descriptive-time current-record)
                                  (yyyy-string
                                    (opening-time current-record)))))
                    (setf
                      ;; Opening time is the earliest time over all.
                      (opening-time current-record)
                      (min (opening-time current-record)
                           (opening-time record))
                      ;; Closing time is the latest time over all.
                      (closing-time current-record)
                      (max (closing-time current-record)
                           (closing-time record))
                      ;; High price is the highest over all.
                      (high-price current-record)
                      (max (high-price current-record)
                           (high-price record))
                      ;; Low price is the lowest over all.
                      (low-price current-record)
                      (min (low-price current-record)
                           (low-price record))
                      ;; The closing price is with the newer closing price, not
                      ;; the older one.
                      (closing-price current-record)
                      (closing-price record)
                      ;; The opening price would be the older one.
                      (adjusted-closing-price current-record)
                      (adjusted-closing-price record)
                      ;; Add the new valume in to the volume we have so far.
                      (trading-volume current-record)
                      (+ (trading-volume current-record)
                         (trading-volume record))))
              (daily-records table))
      (push current-record yearly-records)
      (setf yearly-records (reverse yearly-records)))
    yearly-records))

(defmethod yearly-records ((table table))
  (aif (slot-value table 'yearly-records) it (generate-yearly-records table)))

(defmethod opening-time ((table table))
  "This returns the opening time of each record in TABLE."
  (mapcar #'opening-time (records table)))

(defmethod closing-time ((table table))
  (mapcar #'opening-time (records table)))

(defmethod descriptive-time ((table table))
  "This returns the descriptive time of each record in TABLE."
  (mapcar #'descriptive-time (records table)))

(defmethod opening-price ((table table))
  "This returns the opening price of each record in TABLE."
  (mapcar #'opening-price (records table)))

(defmethod adjustment ((table table))
  "This returns the adjustment factor of each record in TABLE."
  (mapcar #'adjustment (records table)))

(defmethod adjusted-opening-price ((table table))
  "This returns the adjusted opening price of each record in TABLE."
  (mapcar #'adjusted-opening-price (records table)))

(defmethod high-price ((table table))
  "This returns the high price of each record in TABLE."
  (mapcar #'high-price (records table)))

(defmethod adjusted-high-price ((table table))
  "This returns the adjusted high price of each record in TABLE."
  (mapcar #'adjusted-high-price (records table)))

(defmethod low-price ((table table))
  "This returns the low price of each record in TABLE."
  (mapcar #'low-price (records table)))

(defmethod adjusted-low-price ((table table))
  "This returns the adjusted low price of each record in TABLE."
  (mapcar #'adjusted-low-price (records table)))

(defmethod closing-price ((table table))
  "This returns the closing price of each record in TABLE."
  (mapcar #'closing-price (records table)))

(defmethod trading-volume ((table table))
  "This returns the trading volume of each record in TABLE."
  (mapcar #'trading-volume (records table)))

(defmethod adjusted-closing-price ((table table))
  "This returns the adjusted closing price of each record in TABLE."
  (mapcar #'adjusted-closing-price (records table)))

(defmethod elt-record ((table table) position &optional (key #'identity))
  ;; We allow for NIL as a position indicating the very end of the list.
  (when (null position)
    (setf position (1- (length (records table)))))
  (assert (nonnegative-integer? position))
  (funcall key (elt (records table) position)))

(defmethod print-object ((table table) stream)
  (format stream
          "#<stocks:table for ~A from ~A through ~A>"
          (ticker-symbol table)
          (descriptive-time (elt-record table 0))
          (descriptive-time (elt-record table nil))))

(defmethod stock-description ((table table))
  "This is the long name of TABLE's ticker."
  (stock-description (ticker-symbol table)))

(defun parse-yahoo-finance-stock-csv-record (record)
  "This reads in a line from a Yahoo! Finance CSV file about a stock and returns
an equivalent stock table record."
  (make-instance 'table-record
                 :opening-time (date-string-to-universal-time
                                 (first record)
                                 ;; XXX: The NYSE opens 9:30 AM Eastern.  This
                                 ;; is a cheap hack for now, eventually we will
                                 ;; want support for non-American markets.
                                 :hour 9 :minute 30 :time-zone 5)
                 :closing-time (date-string-to-universal-time
                                 (first record)
                                 ;; XXX: The NYSE closes 4:00 PM Eastern.  This
                                 ;; is a cheap hack for now, eventually we will
                                 ;; want support for non-American markets.
                                 :hour 16 :minute 0 :time-zone 5)
                 :descriptive-time (dd-mon-yyyy-string
                                     (date-string-to-universal-time
                                       (first record)))
                 :opening-price (second record)
                 :high-price (third record)
                 :low-price (fourth record)
                 :closing-price (fifth record)
                 :trading-volume (sixth record)
                 :adjusted-closing-price (seventh record)))

(defun load-table-from-yahoo (ticker-symbol &key (preferred-records #'daily-records))
  "This loads a price table from the local Yahoo CSV for TICKER-SYMBOL."
  (make-instance 'table
                 :ticker-symbol ticker-symbol
                 :preferred-records preferred-records
                 :records
                 (sort (mapcar #'parse-yahoo-finance-stock-csv-record
                               (rest (livermore/csv:parse-file
                                       (table-filename ticker-symbol))))
                       #'< :key #'opening-time)))

(function-alias 'load-table-from-yahoo 'load-table)

(defmethod difference
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This reports the DIFFERENCE between the value of the KEY at the START
position and at the END position."
  (- (elt-record table end key)
     (elt-record table start key)))

(defmethod value-ratio
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This reports the RATIO between the value of the KEY at the START position
and at the END position."
  (/ (elt-record table end key)
     (elt-record table start key)))

(defmethod percent-change
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This reports the PERCENT DIFFERENCE between the value of the KEY at the
START position and at the END position."
  (* 100.0 (value-ratio table :key key :start start :end end)))

(defmethod spread-difference
  ((table table) &key (key #'adjusted-closing-price) (position 0) (spread -1))
  "This reports the DIFFERENCE between the value of the KEY at the POSITION and
at the POSITION + SPREAD."
  (difference table :key key :start (+ position spread) :end position))

(defmethod spread-ratio
  ((table table) &key (key #'adjusted-closing-price) (position 0) (spread -1))
  "This reports the RATIO between the value of the KEY at the POSITION and at
  the POSITION + SPREAD."
  (value-ratio table :key key :start (+ position spread) :end position))

(defmethod spread-percent-change
  ((table table) &key (key #'adjusted-closing-price) (position 0) (spread -1))
  "This reports the PERCENT DIFFERENCE between the value of the KEY at the
POSITION and at the POSITION + SPREAD."
  (percent-change table :key key :start (+ position spread) :end position))

(defmethod arithmetic-mean
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This is the ARITHMETIC-MEAN of the stock table's records."
  (arithmetic-mean (records table) :key key :start start :end end))

(defmethod sample-variance
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This is the SAMPLE-VARIANCE of the stock table's records."
  (sample-variance (records table) :key key :start start :end end))

(defmethod unbiased-sample-variance
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This is the UNBIASED-SAMPLE-VARIANCE of the stock table's records."
  (unbiased-sample-variance (records table) :key key :start start :end end))

(defmethod sample-standard-deviation
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This is the SAMPLE-STANDARD-DEVIATION of the stock table's records."
  (sample-standard-deviation (records table) :key key :start start :end end))

(defmethod unbiased-sample-standard-deviation
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This is the UNBIASED-SAMPLE-STANDARD-DEVIATION of the stock table records."
  (unbiased-sample-standard-deviation (records table)
                                      :key key :start start :end end))

(defmethod minimum ((table table)
                    &key
                    (key #'adjusted-closing-price)
                    (start 0) (end nil))
  "This is the MINIMUM of the stock table's records."
  (minimum (records table) :key key :start start :end end))

(defmethod maximum ((table table)
                    &key
                    (key #'adjusted-closing-price)
                    (start 0) (end nil))
  "This is the MAXIMUM of the stock table's records."
  (maximum (records table) :key key :start start :end end))

;;; XXX: this is broken.
(defmethod minimum?  ((table table)
                      &key
                      (position nil)
                      (key #'adjusted-closing-price)
                      (start 0) (end nil))
  "This predicate specifies if element at the specified position is the
MINIMUM of the stock table's records."
  (minimum? (records table) :position position :key key :start start :end end))

;;; XXX: this is broken.
(defmethod maximum?  ((table table)
                      &key
                      (position nil)
                      (key #'adjusted-closing-price)
                      (start 0) (end nil))
  "This predicate specifies if element at the specified position is the
MAXIMUM of the stock table's records."
  (maximum? (records table) :position position :key key :start start :end end))

(defmethod sample-z-score
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This is (KEY at START minus the mean) divided by the sample standard
  deviation, or 0.0 if the deviation is zero."
  (let ((sdev (sample-standard-deviation
                table :key key :start start :end end)))
    (if (zerop sdev)
      0.0
      (/ (- (elt-record table start key)
            (arithmetic-mean table :key key :start start :end end))
         sdev))))

(defmethod unbiased-sample-z-score
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This is (KEY at START minus the mean) divided by the unbiased sample
  standard deviation, or 0.0 if the deviation is zero."
  (let ((sdev (unbiased-sample-standard-deviation
                table :key key :start start :end end)))
    (if (zerop sdev)
      0.0
      (/ (- (elt-record table start key)
            (arithmetic-mean table :key key :start start :end end))
         sdev))))

(defmethod shares-buyable ((record table-record) money)
  "This is how many whole shares MONEY buys at the adjusted close of RECORD."
  (floor (/ money (adjusted-closing-price record))))

(defmethod buy-and-hold ((table table) initial-money
                         &key (start 0) (end nil))
  "This is INITIAL-MONEY grown by the value ratio of TABLE from START to END."
  (* initial-money (value-ratio table :start start :end end)))

(defun retrieve-stock-description (ticker-symbol)
  "This fetches the Yahoo name for TICKER-SYMBOL and returns it."
  (wget "-O" (description-filename ticker-symbol)
        (concatenate 'simple-string
                     "http://quote.yahoo.com/d/quotes.csv"
                     "?s=" (canonical-ticker ticker-symbol)
                     "&f=sn")) ; format s: stock ticker n: stock name
  (second (first (livermore/csv:parse-file (description-filename ticker-symbol)))))

(defun description (ticker-symbol)
  (a?if database-result
        (clsql:query
          (format nil "SELECT description FROM tickers WHERE ticker = '~A';"
                  (canonical-ticker ticker-symbol))
          :flatp t
          :field-names nil)
        (first database-result)
        (a?if yahoo-result
              (retrieve-stock-description ticker-symbol)
              (progn (clsql:query
                       (format nil
                "INSERT INTO tickers (ticker, description) VALUES ('~A', '~A');"
                               (canonical-ticker ticker-symbol)
                               yahoo-result))
                     yahoo-result)
              nil)))

(defun retrieve-stock-data (ticker-symbol
                             &key
                             (filename (table-filename ticker-symbol))
                             ;; "d": daily, "w": weekly, "m": monthly.
                             (frequency "d")
                             (start-month (time-month))
                             (start-date (time-date))
                             (start-year (- (time-year) 50))
                             (end-month (time-month))
                             (end-date (time-date))
                             (end-year (time-year))
                             ;; Supress user messages, T or NIL.
                             (quiet nil))
  "This function retrieves the table.csv file for the specified stock from
Yahoo! Finance's webserver.  The table.csv file is in the format:
  date,open,high,low,close,volume,adjusted-close
An example:
  19-Nov-04,10571.63,10588.29,10419.89,10456.91,15266000,10456.91"
  (setf ticker-symbol (canonical-ticker ticker-symbol)
        start-date (princ-to-string start-date)
        start-year (princ-to-string start-year)
        end-date (princ-to-string end-date)
        end-year (princ-to-string end-year))
  (unless quiet
    (format t 
      "~&Retrieving the ~a trading data for ~a (~a) from ~a/~a/~a to ~a/~a/~a."
            (cond ((string-equal frequency "d") "daily")
                  ((string-equal frequency "w") "weekly")
                  ((string-equal frequency "m") "monthly"))
            (stock-description ticker-symbol)
            ticker-symbol
            (princ-to-string start-month) start-date start-year
            (princ-to-string end-month) end-date end-year))
  (wget "-O" filename
        (concatenate 'simple-string
                     "http://ichart.finance.yahoo.com/table.csv"
                     "?s=" (canonical-ticker ticker-symbol)
                     "&a=" (princ-to-string (1- start-month))
                     "&b=" start-date
                     "&c=" start-year
                     "&d=" (princ-to-string (1- end-month))
                     "&e=" end-date
                     "&f=" end-year
                     "&g=" frequency
                     "&ignore=.csv")))

(defun retrieve-stocks-data ()
  "This function calls RETRIEVE-TABLE on all of the ticker symbols in the
*TICKER-DESCRIPTIONS* hash table."
  (maphash #'(lambda (ticker description)
               ;; We tell Lisp that we don't really use DESCRIPTION so that it
               ;; won't give us warning messages about it.
               (declare (ignore description))
               (retrieve-stock-data ticker))
           *ticker-descriptions*))

(defun retrieve-index-components
  (index-symbol &key (filename (components-filename index-symbol)))
  "This function retrieves the components included in the specified index from
Yahoo! Finance's backend servers."
  ;; TODO: this doesn't work yet.
  (wget "-O" filename
        (concatenate 'simple-string
                     "http://finance.yahoo.com/d/quotes.csv"
                     "?s=@" (canonical-ticker index-symbol)
                     "&f=sl1d1t1c1ohgv"
                     "&e=.csv")))

(defun index-components (index-symbol
                          &key (filename (components-filename index-symbol)))
  "This function returns the components included in the specified index from
the file retrieved from Yahoo! Finance's backend servers."
  (retrieve-index-components index-symbol :filename filename)
  (mapcar (compose #'first #'parse-yahoo-finance-stock-csv-record)
          (read-lines filename)))

(defun buy-and-hold-gains (sequence)
  "This returns the successive price ratios of SEQUENCE."
  (mapcar #'/ (rest sequence) sequence))

(function-alias 'buy-and-hold-gains 'b&h-gains)

(defun buy-and-hold-performance (sequence)
  "This is the last price of SEQUENCE divided by the first."
  (/ (car (last sequence)) (first sequence)))

(function-alias 'buy-and-hold-performance 'b&h-performance)

(defun cash-optimal-gains (sequence)
  "This is the gains possible if we magically know when to keep our money out,
and instead keep it in cash those days so that there is no loss or gain on down
days but always gain on up days."
  (mapcar (curry #'max 1) (buy-and-hold-gains sequence)))

(defun cash-optimal-performance (sequence)
  "This is the product of the cash-optimal gains of SEQUENCE."
  (product (cash-optimal-gains sequence)))

(defun simple-moving-average (period sequence)
  "This function calculates the simple moving average (SMA) of a sequence."
  (assert (positive-integer? period))
  (assert (sequence? sequence))
  (loop for end from period to (length sequence) collect
        (/ (sum sequence :start (- end period) :end end)
           period)))

(function-alias 'simple-moving-average 'sma)

(defun arithmetically-weighted-moving-average (period sequence)
  "This function calculates the arithmetically weighted moving average (WMA) of
a sequence, which is the specific WMA generally meant in technical analysis
whenever WMA is discussed."
  (assert (positive-integer? period))
  (assert (sequence? sequence))
  (loop for end from period to (length sequence) collect
        (/ (sum (mapcar #'*
                        ;; These are the weights.  The oldest data point is
                        ;; weighted at 1 and the newest at n for an n-period
                        ;; arithmetically weighted moving average.
                        (loop for i from 1 to period collect i)
                        (subseq sequence (- end period) end)))
           ;; The denominator is actually n+(n-1)+(n-2)+...+2+1, but this is a
           ;; triangular number, which can be determined by this formula.
           (* period (1+ period) 1/2))))

(function-alias 'arithmetically-weighted-moving-average 'awma)

(defun exponential-moving-average (period sequence)
  "This function calculates the exponential moving average (EMA) of a sequence."
  ;;; XXX: This is too slow!
  (assert (positive-integer? period))
  (assert (sequence? sequence))
  (let ((smoothing-factor (/ 2 (1+ period)))) ; This is alpha.
    (loop for end from period to (length sequence) collect
          ;; This should actually be divided by 1+(1-a)+(1-a)**2+... as an
          ;; infinite summation, but this approaches 1/a.
          (* smoothing-factor
             (sum (mapcar #'*
                          ;; These are the weights.
                          (loop for e from 0 to (length sequence) collect
                                (expt (1- smoothing-factor) e))
                          (subseq sequence (- end period) end)))))))

(function-alias 'exponential-moving-average 'ema)

(defun moving-average-convergence-divergence (period-a period-b sequence)
  "This is the moving average convergence/divergence (MACD) of a sequence."
  (mapcar #'- (ema period-a sequence) (ema period-b sequence)))

(function-alias 'moving-average-convergence-divergence 'macd)

;(defun macd-signal-line (period-a period-b signal-period sequence)
;  "This is the signal line for the moving average convergence/divergence (MACD)
;of a sequence."
;  (
;
;(defun macd-12-26-9 (sequence)
;  "The MACD[12,26,9] is the textbook standard version."
;  (macd 12 26 9 sequence))
;
;(function-alias 'macd-12-26-9 'macd-textbook)

(defun macd-signal (smoothing-period period-a period-b sequence)
  "This is the MACD of a sequence with an EMA applied to smooth the results.
It is routinely used as a signal/trigger."
  (ema smoothing-period (macd period-a period-b sequence)))

(defun upward-changes (sequence)
  "This returns the positive day-to-day changes of SEQUENCE, else 0."
  (mapcar #'(lambda (yesterday today)
              (if (< yesterday today) ; Are we upward?
                (- today yesterday)
                0))
          sequence
          (rest sequence)))

(defun downward-changes (sequence)
  "This returns the positive day-to-day drops of SEQUENCE, else 0."
  (mapcar #'(lambda (yesterday today)
              (if (> yesterday today) ; Are we downward?
                (- yesterday today)
                0))
          sequence
          (rest sequence)))

(defun relative-strength (moving-average sequence)
  "This is the ratio of the moving average of upward changes to that of
  downward changes."
  (mapcar #'/
          (funcall moving-average (upward-changes sequence))
          (funcall moving-average (downward-changes sequence))))

(function-alias 'relative-strength 'rs)

(defun ema-rs (period sequence)
  "This is relative strength using an EMA of PERIOD."
  (relative-strength (curry #'ema period) sequence))

(function-alias 'ema-rs 'wilder-rs)

(defun ema-rs-27 (sequence)
  "This is the RS that Wilder recommended originally."
  (ema-rs 27 sequence))

(function-alias 'ema-rs-27 'rs-textbook)

(defun sma-rs (period sequence)
  (relative-strength (curry #'ema period) sequence))

(function-alias 'sma-rs 'cutler-rs)

(defun relative-strength-index (rs sequence)
  (- 100 (* 100 (/ (1+ (funcall rs sequence))))))

(function-alias 'relative-strength-index 'rsi)

(defun ema-rsi (period sequence)
  (rsi (curry #'ema-rs period) sequence))

(function-alias 'ema-rsi 'wilder-rsi)

(defun ema-rsi-27 (sequence)
  (rsi #'ema-rs-27 sequence))

(function-alias 'ema-rsi-27 'rsi-textbook)

(defun sma-rsi (period sequence)
  (rsi (curry #'ema-rs period) sequence))

(function-alias 'sma-rsi 'cutler-rsi)

(defun simple-directional-signal (period sequence)
  "This is true at each point where the price PERIOD steps later is higher."
  (assert (positive-integer? period))
  (mapcar #'< sequence (nthcdr period sequence)))

(defun simple-directional-gains (period sequence)
  "This returns the day-to-day gains of a strategy that is long when
  SIMPLE-DIRECTIONAL-SIGNAL is true and otherwise holds cash."
  (let* ((signals (simple-directional-signal period sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  1))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun simple-directional-performance (period sequence)
  "This is the product of the simple-directional gains."
  (product (simple-directional-gains period sequence)))

(defun dual-simple-directional-signal (period-a period-b sequence)
  "This is true when both PERIOD-A and PERIOD-B simple directional signals
  are true."
  (reverse (mapcar #'(lambda (a b) (and a b))
                   (reverse (simple-directional-signal period-a sequence))
                   (reverse (simple-directional-signal period-b sequence)))))

(defun dual-simple-directional-gains (period-a period-b sequence)
  "This returns the day-to-day gains of a strategy that is long when both
  directional signals are true and otherwise holds cash."
  (let* ((signals (dual-simple-directional-signal period-a period-b sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  1))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun dual-simple-directional-performance (period-a period-b sequence)
  "This is the product of the dual simple-directional gains."
  (product (dual-simple-directional-gains period-a period-b sequence)))

(defun sma-crossover-signal (short-period long-period ratio sequence)
  "This is true when the short SMA exceeds RATIO times the long SMA."
  (assert (positive-integer? short-period))
  (assert (positive-integer? long-period))
  (assert (< short-period long-period))
  (let* ((short-sma (sma short-period sequence))
         (long-sma (sma long-period sequence))
         (start (- (length short-sma) (length long-sma))))
    (mapcar #'(lambda (short long)
                (< (* ratio long) short))
            (nthcdr start short-sma)
            long-sma)))

(defun directional-sma-crossover-signal
  (short-period long-period ratio sequence)
  "This returns :LONG, :SHORT, or :CASH from the short and long SMAs and
  the slope of the long SMA."
  (assert (positive-integer? short-period))
  (assert (positive-integer? long-period))
  (assert (< short-period long-period))
  (let* ((short-sma (sma short-period sequence))
         (long-sma (sma long-period sequence))
         (start (1+ (- (length short-sma) (length long-sma)))))
    (mapcar #'(lambda (short long previous-long)
                (cond ((and (< (* ratio long) short)
                            (< previous-long long))
                       :long)
                      ((and (> (/ long ratio) short)
                            (> previous-long long))
                       :short)
                      (t :cash)))
            (nthcdr start short-sma)
            (cdr long-sma)
            long-sma)))

(defun sma-crossover-gains (short-period long-period ratio sequence)
  "This returns the day-to-day gains of a long-or-cash SMA crossover."
  (let* ((signals (sma-crossover-signal short-period long-period ratio
                                        sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  1))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun directional-sma-crossover-gains (short-period long-period ratio sequence)
  (let* ((signals (directional-sma-crossover-signal
                    short-period long-period ratio sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (case signal
                  (:long (/ tomorrow today))
                  (:cash 1)
                  (:short (/ today tomorrow))))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun sma-crossover-long/short-gains (short-period long-period ratio sequence)
  (let* ((signals (sma-crossover-signal short-period long-period ratio
                                        sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  (/ today tomorrow)))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun sma-crossover-performance (short-period long-period ratio sequence)
  "This is the product of the long-or-cash SMA crossover gains."
  (product (sma-crossover-gains short-period long-period ratio sequence)))

(defun directional-sma-crossover-performance
  (short-period long-period ratio sequence)
  "This is the product of the directional SMA crossover gains."
  (product (directional-sma-crossover-gains
             short-period long-period ratio sequence)))

(defun sma-crossover-long/short-performance
  (short-period long-period ratio sequence)
  "This is the product of the long-or-short SMA crossover gains."
  (product (sma-crossover-long/short-gains short-period long-period ratio
                                           sequence)))

(defun sma-crossover-optimal (sequence)
  "This searches short period, long period, and ratio for the best
  long-or-cash SMA crossover on SEQUENCE."
  (let ((optimal-short-period nil)
        (optimal-long-period nil)
        (optimal-ratio nil)
        (optimal-performance nil)
        (b&h-performance (buy-and-hold-performance sequence)))
    (loop for ratio from 1.00 to 1.05 by 0.01 do
          (loop for long-period from 2 to 200 do
                (loop for short-period from 1 to (1- long-period) do
                      (let ((performance (sma-crossover-performance short-period
                                                                    long-period
                                                                    ratio
                                                                    sequence)))
                        (when (or (null optimal-performance)
                                (< optimal-performance performance))
                          (format t "New optimal: (SMA-CROSSOVER ~A ~A ~A) => ~A ~A B&H => ~A.~%"
                                  short-period long-period ratio
                                  performance
                                  (cond ((< performance b&h-performance) "<")
                                        ((= performance b&h-performance) "=")
                                        ((> performance b&h-performance) ">"))
                                  b&h-performance)
                          (setf optimal-short-period short-period
                                optimal-long-period long-period
                                optimal-ratio ratio
                                optimal-performance performance))))))
    (list optimal-short-period optimal-long-period optimal-ratio
          optimal-performance)))

(defun sma-crossover-long/short-optimal (sequence)
  "This searches short period, long period, and ratio for the best
  long-or-short SMA crossover on SEQUENCE."
  (let ((optimal-short-period nil)
        (optimal-long-period nil)
        (optimal-ratio nil)
        (optimal-performance nil)
        (b&h-performance (buy-and-hold-performance sequence)))
    (loop for ratio from 1.00 to 1.05 by 0.01 do
          (loop for long-period from 2 to 200 do
                (loop for short-period from 1 to (1- long-period) do
                      (let ((performance
                              (sma-crossover-long/short-performance
                                short-period long-period ratio sequence)))
                        (when (or (null optimal-performance)
                                (< optimal-performance performance))
                          (format t "New optimal: (SMA-CROSSOVER-LONG/SHORT ~A ~A ~A) => ~A ~A B&H => ~A.~%"
                                  short-period long-period ratio
                                  performance
                                  (cond ((< performance b&h-performance) "<")
                                        ((= performance b&h-performance) "=")
                                        ((> performance b&h-performance) ">"))
                                  b&h-performance)
                          (setf optimal-short-period short-period
                                optimal-long-period long-period
                                optimal-ratio ratio
                                optimal-performance performance))))))
    (list optimal-short-period optimal-long-period optimal-ratio
          optimal-performance)))

(defun directional-sma-crossover-optimal (sequence)
  "This searches short period, long period, and ratio for the best
  directional SMA crossover on SEQUENCE."
  (let ((optimal-short-period nil)
        (optimal-long-period nil)
        (optimal-ratio nil)
        (optimal-performance nil)
        (b&h-performance (buy-and-hold-performance sequence)))
    (loop for ratio from 1.00 to 1.05 by 0.01 do
          (loop for long-period from 2 to 200 do
                (loop for short-period from 1 to (1- long-period) do
                      (let ((performance
                              (directional-sma-crossover-performance
                                short-period long-period ratio sequence)))
                        (when (or (null optimal-performance)
                                (< optimal-performance performance))
                          (format t "New optimal: (DIRECTIONAL-SMA-CROSSOVER ~A ~A ~A) => ~A ~A B&H => ~A.~%"
                                  short-period long-period ratio
                                  performance
                                  (cond ((< performance b&h-performance) "<")
                                        ((= performance b&h-performance) "=")
                                        ((> performance b&h-performance) ">"))
                                  b&h-performance)
                          (setf optimal-short-period short-period
                                optimal-long-period long-period
                                optimal-ratio ratio
                                optimal-performance performance))))))
    (list optimal-short-period optimal-long-period optimal-ratio
          optimal-performance)))

(defun fibs (low high)
  (let* ((phi (/ (1+ (sqrt 5)) 2))
         (inverse-phi (/ phi))
         (range (- high low))
         (r/2 (/ range 2))
         (mid (+ low r/2))
         (mid+z (+ low (* inverse-phi range)))
         (z (- mid+z mid)))
    (format t "Range: ~$ ~$~%Low: ~$ ~$ ~$~%Mid: ~$ ~$ ~$~%High: ~$ ~$ ~$"
            range r/2
            (- low z) low (+ low z)
            (- mid z) mid mid+z
            (- high z) high (+ high z))))

(defun fibonacci (table &key (key 'adjusted-closing-price))
  (fibs (reduce #'min (funcall key table))
        (reduce #'max (funcall key table))))

(defparameter ^dji (load-table '^dji))
(defparameter ^gspc (load-table '^gspc))
(defparameter ^ixic (load-table '^ixic))
