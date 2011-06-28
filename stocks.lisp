(load "utilities")
(load "time")
(load "statistics")
(load "csv")
(unless (find-package 'stocks)
  (defpackage :stocks
    (:use :common-lisp
          #+cmu :extensions #+sbcl :sb-ext
          :utilities :time :statistics)
    (:export :*data-directory*
             :*^dji-components*
             :*ticker-descriptions*
             :*initial-money*
             :stock-description
             :ticker-filename
             :components-filename
             :retrieve-stock-data
             :retrieve-stocks-data
             :retrieve-index-components
             :buy-and-hold
             :trend-following
             :report-results
             :table-record
             :universal-time
             :opening-price
             :high-price
             :low-price
             :closing-price
             :trading-volume
             :adjusted-closing-price
             :table
             :ticker-symbol
             :records
             :elt-record
             :stock-description
             :load-table
             :difference
             :ratio
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
             :shares-buyable)))
(in-package :stocks)

;; This is the directory where all of the stock trading data should be stored.
(defparameter *data-directory* "stock-data")
;; This is the initial quantity of money for the various analyzer methods.
(defparameter *initial-money* 100000.00)

(defparameter *www-user-agent-identifier*
  "Mozilla/4.0 (compatible; MSIE 5.5; Windows NT 5.0)")
(defun wget (&rest arguments)
  "A simple wrapper for GNU wget."
  (#+cmu extensions:run-program
   #+sbcl sb-ext:run-program
   "/usr/local/bin/wget"
    (append (list "-q" "-U" *www-user-agent-identifier*) arguments)
    :output t)
  (values))

(load "stock-ticker-descriptions")

(defun ticker-filename (ticker-symbol)
  (concatenate 'string
               *data-directory* "/"
               (string-downcase ticker-symbol) "-table.csv"))

(defun components-filename (index-symbol)
  (concatenate 'string
               *data-directory* "/"
               (string-downcase index-symbol) "-components.csv"))

(defclass table-record ()
  ((universal-time
     :accessor universal-time
     :initarg :universal-time
     :type integer)
   (opening-price
     :accessor opening-price
     :initarg :opening-price
     :type float)
   (high-price
     :accessor high-price
     :initarg :high-price
     :type float)
   (low-price
     :accessor low-price
     :initarg :low-price
     :type float)
   (closing-price
     :accessor closing-price
     :initarg :closing-price
     :type float)
   (trading-volume
     :accessor trading-volume
     :initarg :trading-volume
     :type (integer 0 *))
   (adjusted-closing-price
     :accessor adjusted-closing-price
     :initarg :adjusted-closing-price
     :type float)))

(defmethod print-object ((table-record table-record) stream)
  (format stream "(stocks:table-record :universal-time ~A :opening-price ~A :high-price ~A :low-price ~A :closing-price ~A :trading-volume ~A :adjusted-closing-price ~A)"
          (universal-time table-record)
          (opening-price table-record)
          (high-price table-record)
          (low-price table-record)
          (closing-price table-record)
          (trading-volume table-record)
          (adjusted-closing-price table-record)))

(defun table-record (&rest rest)
  (apply #'make-instance 'table-record rest))

(defmethod decoded-time-list ((table-record table-record))
  (decoded-time-list (universal-time table-record)))

(defclass table ()
  ((ticker-symbol
     :accessor ticker-symbol
     :initform ""
     :initarg :ticker-symbol
     :type string)
   (records
     :accessor records
     :initform nil
     :initarg :records
     :type list)))

(defmethod universal-time ((table table))
  (mapcar #'universal-time (records table)))

(defmethod opening-price ((table table))
  (mapcar #'opening-price (records table)))

(defmethod high-price ((table table))
  (mapcar #'high-price (records table)))

(defmethod low-price ((table table))
  (mapcar #'low-price (records table)))

(defmethod closing-price ((table table))
  (mapcar #'closing-price (records table)))

(defmethod trading-volume ((table table))
  (mapcar #'trading-volume (records table)))

(defmethod adjusted-closing-price ((table table))
  (mapcar #'adjusted-closing-price (records table)))

(defmethod elt-record ((table table) position &optional (key #'identity))
  (with-slots (records) table
    ;; We allow for NIL as a position indicating the very end of the list.
    (when (null position)
      (setf position (1- (length records))))
    (assert (nonnegative-integer? position))
    (funcall key (elt records position))))

(defmethod print-object ((table table) stream)
  (format stream
          "~&#<stocks:table ~A, ~A records~A ... through ... ~A>~%"
          (ticker-symbol table)
          (length (records table))
          (elt-record table 0)
          (elt-record table nil)))

(defmethod stock-description ((table table))
  (stock-description (ticker-symbol table)))

(defun parse-yahoo-finance-stock-csv-record (record)
  "This reads in a line from a Yahoo! Finance CSV file about a stock and returns
  an equivalent stock table record."
  (make-instance 'table-record
                 :universal-time (dd-month-yyyy-to-universal-time
                                   (first record))
                 :opening-price (second record)
                 :high-price (third record)
                 :low-price (fourth record)
                 :closing-price (fifth record)
                 :trading-volume (sixth record)
                 :adjusted-closing-price (seventh record)))

(defun load-table-from-yahoo (ticker-symbol)
  (make-instance 'table
                 :ticker-symbol ticker-symbol
                 :records
                 (sort (mapcar #'parse-yahoo-finance-stock-csv-record
                               (rest (csv:parse-file
                                       (ticker-filename ticker-symbol))))
                       #'< :key #'universal-time)))

(defun load-table (ticker-symbol)
  (load-table-from-yahoo ticker-symbol))

(defmethod difference
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This reports the DIFFERENCE between the value of the KEY at the START
  position and at the END position."
  (- (elt-record table end key)
     (elt-record table start key)))

#+sbcl (sb-ext:without-package-locks
         (defgeneric ratio (table &key key start end)))
#+cmu (ext:without-package-locks
        (defgeneric ratio (table &key key start end)))
#+clisp (ext:without-package-lock ()
          (defgeneric ratio (table &key key start end)))
(defmethod ratio
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This reports the RATIO between the value of the KEY at the START position
  and at the END position."
  (/ (elt-record table end key)
     (elt-record table start key)))

(defmethod percent-change
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  "This reports the PERCENT DIFFERENCE between the value of the KEY at the
  START position and at the END position."
  (* 100.0 (ratio table :key key :start start :end end)))

(defmethod spread-difference
  ((table table) &key (key #'adjusted-closing-price) (position 0) (spread -1))
  "This reports the DIFFERENCE between the value of the KEY at the POSITION and
  at the POSITION + SPREAD."
  (difference table :key key :start (+ position spread) :end position))

(defmethod spread-ratio
  ((table table) &key (key #'adjusted-closing-price) (position 0) (spread -1))
  "This reports the RATIO between the value of the KEY at the POSITION and at
  the POSITION + SPREAD."
  (ratio table :key key :start (+ position spread) :end position))

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

(defmethod minimum?  ((table table)
                      &key
                      (elt nil)
                      (key #'adjusted-closing-price)
                      (start 0) (end nil))
  "This is the MINIMUM of the stock table's records."
  (minimum? (records table) :elt elt :key key :start start :end end))

(defmethod maximum?  ((table table)
                      &key
                      (elt nil)
                      (key #'adjusted-closing-price)
                      (start 0) (end nil))
  "This is the MAXIMUM of the stock table's records."
  (maximum? (records table) :elt elt :key key :start start :end end))

(defmethod sample-z-score
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  (let ((sdev (sample-standard-deviation
                table :key key :start start :end end)))
    (if (zerop sdev)
      0.0
      (/ (- (elt-record table start key)
            (arithmetic-mean table :key key :start start :end end))
         sdev))))

(defmethod unbiased-sample-z-score
  ((table table) &key (key #'adjusted-closing-price) (start 0) (end nil))
  (let ((sdev (unbiased-sample-standard-deviation
                table :key key :start start :end end)))
    (if (zerop sdev)
      0.0
      (/ (- (elt-record table start key)
            (arithmetic-mean table :key key :start start :end end))
         sdev))))

#|
(defmethod plot
  ((table table) slots
    &key (start 0) (end nil) (datastyle "lines"))
  "This calls out to GNUPlot to render the data."
  (unless (listp slots)
    (setf slots (list slots)))
  (with-open-file (gp "stock.gp" :direction :output)
    (format gp "~&set title \"~A (~A) from ~D ~A ~D until ~D ~A ~D\"~%"
            (description table)
            (string-upcase (ticker-symbol table))
            (first (date (nth start (records table))))
            (second (date (nth start (records table))))
            (third (date (nth start (records table))))
            (first (date (nth end (records table))))
            (second (date (nth end (records table))))
            (third (date (nth end (records table)))))
    (format gp "~&set xlabel \"Day\"~%")
    (format gp "~&set ylabel \"Money\"~%")
    (format gp "~&set data style ~A~%" datastyle)
    (format gp "~&plot \\~%")
    (dotimes (i (length slots))
      (format gp "~&     'stock.data' using 1:~D title \"~A\""
              (+ 2 i)
              (nth i slots))
      (when (< i (1- (length slots)))
        (format gp ",\\")))
    (format gp "~&pause -1~%"))
  (with-open-file (data "stock.data" :direction :output)
    (format t "~D~%" (length (subseq (records table) end start)))
    (let ((start-time (universal-time (nth start (records table)))))
      (dolist (record (subseq (records table) end start))
        (format data "~&~D"
                (/ (- (universal-time record) start-time)
                   60 60 24))
        (dolist (slot slots)
          (format data "  ~F  " (funcall slot record)))
        (format data " ~%"))))
  (extensions:run-program "gnuplot" (list "stocks.gp") :output t))
|#

(defmethod shares-buyable ((record table-record) money)
  (floor (/ money (adjusted-closing-price record))))

(defmethod buy-and-hold ((table table) initial-money
                         &key (start 0) (end nil))
  (* initial-money (ratio table :start start :end end)))

(defun retrieve-stock-data (ticker-symbol
                             &key
                             (filename (ticker-filename ticker-symbol))
                             ;; "d": daily, "w": weekly, "m": monthly.
                             (frequency "d")
                             (start-month (time-month))
                             (start-date (time-date))
                             (start-year (- (time-year) 5))
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
  (setf ticker-symbol (string-upcase ticker-symbol)
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
                     "?s=" (string-upcase ticker-symbol)
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
               (retrieve-stock-data ticker))
           *ticker-descriptions*))

(defun retrieve-index-components
  (index-symbol &key (filename (components-filename index-symbol)))
  "This function retrieves the components included in the specified index from
  Yahoo! Finance's backend servers."
  (wget "-O" filename
        (concatenate 'simple-string
                     "http://finance.yahoo.com/d/quotes.csv"
                     "?s=@" (string-upcase index-symbol)
                     "&f=sl1d1t1c1ohgv"
                     "&e=.csv")))

(defun index-components (index-symbol
                          &key (filename (components-filename index-symbol)))
  "This function returns the components included in the specified index from
  the file retrieved from Yahoo! Finance's backend servers."
  (retrieve-index-components index-symbol :filename filename)
  (mapcar (compose #'first #'parse-yahoo-finance-stock-csv-record)
          (read-lines filename)))

#|
(defmethod perfect-trader
  ((table table)
   &key
   (start nil)
   (end 0)
   (initial-money *initial-money*))
  
  )
|#

#|
(defmethod trend-following
  ((table table)
   &key
   (start 0)
   (end (- (length (records table)) 3))
   (initial-money *initial-money*)
   (bank-%pa 1.03))
  "This method analyzes a stock using the simple trend-following method of
  buying the stock if yesterday was an up-turn and selling if yesterday was
  a down-turn."
  (assert (<= start (- (length (records table)) 3)))
  (let ((money initial-money)
        (back-2 (elt-record table (- start 2) #'adjusted-closing-price))
        (back-1 (elt-record table (- start 1) #'adjusted-closing-price)))
    (dolist (adj-close (reverse (mapcar 'adjusted-closing-price
                                        (subseq (records table) end start))))
      (flet ((bank (money)
                   ;; roughly 220 trading days per annum.
                   (* money (1+ (/ (1- bank-%pa) 220)))))
        (if (>= back-1 back-2)
          (setf money ;; buy the stock
                (+ (* adj-close (floor (/ money back-1)))
                   (bank (- money (* back-1 (floor (/ money back-1)))))))
          (setf money (bank money)))
        (setf back-2 back-1
              back-1 adj-close)))
    money))
|#

#|
(defmethod report-results ((table table)
                           &key
                           (start nil)
                           (end 0)
                           (initial-money *initial-money*)
                           (bank-%pa 1.00))
  (when (or (null start)
            (> start
               (- (length (records table)) 3)))
    (setf start
          (- (length (records table)) 3)))
  (format t "~&Trading results for ~A (~A).~%"
         (description table)
         (ticker-symbol table))
  (format t "~&Initial:")
  (describe (nth start (records table)))
  (format t "~&Final:")
  (describe (nth end (records table)))
  (let ((b&h (buy-and-hold table initial-money :start start :end end))
        (tf (trend-following table
                             :start start
                             :end end
                             :initial-money initial-money
                             :bank-%pa bank-%pa))
        (years (/ (- (universal-time (nth end (records table)))
                     (universal-time (nth start (records table))))
                  60 60 24 365.25)))
    (format t "~&Years covered: ~,5F.~%" years)
    (format t "mean: $~$, sdev: $~$, high: $~$, low: $~$."
            (mean table 'adjusted-closing-price
                  :start end :end start)
            (standard-deviation table 'adjusted-closing-price
                                :start end :end start)
            (high table 'adjusted-closing-price
                  :start end :end start)
            (low table 'adjusted-closing-price
                 :start end :end start))
    (format t "~&Buy-and-hold: $~$.~%" b&h)
    (format t "~&Trend-following: $~$.~%" tf)
    (format t "~&TF/B&H: ~F.~%" (/ tf b&h))))
|#

(defclass stock-trader ()
  ((table
     :accessor table
     :initarg :table
     :type 'table
     :documentation
     "This is the working table for the agent.  Typically we will share
     data between the traders, and this shouldn't be modified therefore.")
   (bank-interest-rate
     :accessor bank-interest-rate
     :initform 1.0
     :initarg :bank-interest-rate
     :type float
     :documentation
     "This is the interest rate applied to the money in the bank, per annum.
     This is a float multiplier, not a percentage.  Therefore, 2%pa is 1.02.")
   (decision-function?
     :accessor decision-function?
     :initform (constantly nil) ; Perhaps the most intelligent trader?
     :initarg :decision-function?
     :type function
     :documentation
     "This is the decision function for the stock trader.  It returns T if it
     recommends buying the stock, and NIL otherwise.")))

(defmethod decision? ((stock-trader stock-trader) (records list))
  "This wrapper calls the DECISION-FUNCTION? member and returns its result."
  (funcall (decision-function? stock-trader) records))
