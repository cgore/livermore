(load "utilities/utilities")
(load "statistics")
(load "stocks")
(load "time")
(load "xcs")
(load "xcs-analyzer")
(in-package "XCS")
(use-package '("COMMON-LISP"
               "UTILITIES"
               "STOCKS"
               "TIME"))
(export '(stocks-xcs-analyzer
           stocks-experiment
           table
           initial-index
           current-index
           previous-action
           current-action
           money
           previous-money
           correct-actions
           actions
           initialize
           money-from-stock
           elt-record
           current-record
           elt-previous-record
           previous-record
           should-have-bought-stock?
           buy-stock?
           *correct-actions-history*
           execute-action
           get-situation
           correct-action?
           positive-action?
           negative-action?
           get-reward
           end-of-problem?
           load-*table*
           start-stocks-xcs-experiment
           experiment-stats
           castats
           all-stats))
(load "stocks-xcs-parameters.lisp")

(defclass stocks-xcs-analyzer (analyzer)
  ((table
     :accessor table
     :initarg :table)
   (initial-index
     :accessor initial-index
     :initform *stock-starting-index*
     :initarg :initial-index
     :type integer)
   (current-index
     :accessor current-index
     :initform *stock-starting-index*
     :initarg :current-index
     :type integer)
   (previous-action
     :accessor previous-action
     :initform nil
     :initarg :previous-action)
   (money
     :accessor money
     :initform *initial-money*
     :initarg :money
     :type float)
   (previous-money
     :accessor previous-money
     :initform *initial-money*
     :initarg :previous-money
     :type float)
   (negative-actions
     :accessor negative-actions
     :initform 0
     :initarg :negative-actions
     :type integer)
   (positive-actions
     :accessor positive-actions
     :initform 0
     :initarg :positive-actions
     :type integer)))

(defmethod elt-record ((stocks-xcs-analyzer stocks-xcs-analyzer) (n integer))
  (assert (not (minusp n)))
  (elt-record (table stocks-xcs-analyzer) n))

(defmethod current-record ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (elt-record stocks-xcs-analyzer (current-index stocks-xcs-analyzer)))

(defmethod elt-previous-record ((stocks-xcs-analyzer stocks-xcs-analyzer)
                                (n integer))
  (assert (not (minusp n)))
  (elt-record stocks-xcs-analyzer (- (current-index stocks-xcs-analyzer) n)))

(defmethod previous-record ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (elt-previous-record stocks-xcs-analyzer 1))

(defmethod money-from-stock ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (with-slots (money) stocks-xcs-analyzer
    (let* ((shares (shares-buyable (previous-record stocks-xcs-analyzer) money))
           (prev-adj-close (adjusted-closing-price (previous-record
                                                     stocks-xcs-analyzer)))
           (adj-close (adjusted-closing-price (current-record
                                                stocks-xcs-analyzer))))
      (+ (* shares adj-close)
         (- money (* shares prev-adj-close))))))

(defmethod should-have-bought-stock? ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (> (adjusted-closing-price (current-record stocks-xcs-analyzer))
     (adjusted-closing-price (previous-record stocks-xcs-analyzer))))

(defmethod buy-stock? ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (with-slots (current-action previous-action) stocks-xcs-analyzer
    (or (equal current-action :stock)
        (and (equal current-action :hold)
             (equal previous-action :stock)))))

(defvar *correct-actions-history* nil)

(defmethod correct-action? ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (or (and (buy-stock? stocks-xcs-analyzer)
           (should-have-bought-stock? stocks-xcs-analyzer))
      (and (not (buy-stock? stocks-xcs-analyzer))
           (not (should-have-bought-stock? stocks-xcs-analyzer)))))

(defmethod positive-action? ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (> (money stocks-xcs-analyzer)
     (previous-money stocks-xcs-analyzer)))

(defmethod negative-action? ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (< (money stocks-xcs-analyzer)
     (previous-money stocks-xcs-analyzer)))

;; Version exists in xcs-analyzer.lisp.
(defmethod execute-action ((stocks-xcs-analyzer stocks-xcs-analyzer) action)
  (with-slots (table 
                current-index initial-index
                previous-action current-action
                actions correct-actions
                positive-actions negative-actions
                previous-money money) stocks-xcs-analyzer
    (incf current-index)
    (setf current-action action
          previous-action current-action
          previous-money money)
    (when (buy-stock? stocks-xcs-analyzer)
      (setf money (money-from-stock stocks-xcs-analyzer)))
    (incf actions)
    (when (correct-action? stocks-xcs-analyzer)
      (incf correct-actions))
    (when (positive-action? stocks-xcs-analyzer)
      (incf positive-actions))
    (when (negative-action? stocks-xcs-analyzer)
      (incf negative-actions))
    (push (if (plusp (actions stocks-xcs-analyzer))
            (/ correct-actions actions)
            0)
          *correct-actions-history*)))

(defmethod get-situation ((stocks-xcs-analyzer stocks-xcs-analyzer)) 
  (with-slots (table current-index) stocks-xcs-analyzer
    (let ((yesterday (1- current-index)))
      (flet ((z+? (f days)
                  (> 0.1 (unbiased-sample-z-score table :key f
                           :start (- yesterday days) :end yesterday)))
             (z-? (f days)
                  (< -0.1 (unbiased-sample-z-score table :key f
                            :start (- yesterday days) :end yesterday))))
          (vector (maximum? table :start (1- yesterday) :end yesterday)
                  (maximum? table :end yesterday)
                  (minimum? table :end yesterday)
                  (z+? 'adjusted-closing-price 5)
                  (z+? 'adjusted-closing-price 30)
                  (z-? 'adjusted-closing-price 5)
                  (z-? 'adjusted-closing-price 30)
                  (maximum? table :key #'opening-price
                            :start (1- yesterday) :end yesterday)
                  (maximum? table :key #'trading-volume
                            :start (1- yesterday) :end yesterday)
                  (maximum? table :key #'trading-volume :end yesterday)
                  (minimum? table :key #'trading-volume :end yesterday)
                  (z+? 'trading-volume 5)
                  (z-? 'trading-volume 5))))))

;; Version exists in xcs-analyzer.lisp.
(defmethod get-reward ((stocks-xcs-analyzer stocks-xcs-analyzer))
  (with-slots (money previous-money
                current-index
                table
                previous-action current-action
                actions correct-actions
                positive-actions negative-actions) stocks-xcs-analyzer
    (let ((time (opening-time (current-record stocks-xcs-analyzer))))
      (when (zerop (mod current-index *stocks-xcs-report-days*))
        (format *stocks-xcs-output*
          "~&~A: ~A [~D]: $~8,2F, ~A action [~(~5A~)], ~5,2F% correct, change: ~5,2F%.~%"
          (string-upcase (ticker-symbol table))
          (dd-mon-yyyy-string time)
          current-index
          money
          (if (correct-action? stocks-xcs-analyzer) "  correct" "incorrect")
          (current-action stocks-xcs-analyzer)
          (* 100.0 (/ correct-actions actions))
          (* 100.0 (/ (- money previous-money)
                      previous-money))))
      (case *reward-method*
        (:money money)
        (:delta-money (- money previous-money))
        (:weighted-delta-money (/ (- money previous-money)
                                  previous-money))
        (:diff-money 
          (- money (if (buy-stock? stocks-xcs-analyzer)
                     previous-money
                     (money-from-stock stocks-xcs-analyzer))))
        (:weighted-diff-money 
          (- money (if (buy-stock? stocks-xcs-analyzer)
                     previous-money
                     (* 0.5 (money-from-stock stocks-xcs-analyzer)))))
        (:correctness (if (correct-action? stocks-xcs-analyzer) 1000.0 0.0))))))

(defmethod end-of-problem? ((stocks-xcs-analyzer stocks-xcs-analyzer))
 (oddp (current-index stocks-xcs-analyzer)))

(defclass stocks-experiment (experiment) ())

(defmethod records ((stocks-experiment stocks-experiment))
  (records (table (environment stocks-experiment))))

(defmethod terminate? ((stocks-experiment stocks-experiment))
  (with-slots (current-index money) (environment stocks-experiment)
    (or (= current-index
           (1- (length (records stocks-experiment))))
        (not (plusp money)))))

(defun load-*table* (stock-ticker)
  (defparameter *table* (load-table stock-ticker)))
(load-*table* *initial-stock-ticker*)

(defun start-stocks-xcs-experiment ()
  (defparameter *analyzer*
    (make-instance 'stocks-xcs-analyzer :table *table*))
  (defparameter *xcs*
    (make-instance 'xcs :learning-parameters *learning-parameters*))
  (defparameter *experiment*
    (make-instance 'stocks-experiment
                   :environment *analyzer*
                   :reinforcement-program *analyzer*
                   :xcs *xcs*))
  (setf *correct-actions-history* nil)
  (start *experiment*)
  (format t "~&~34~ RESULTS ~34~~%")
  (let* ((start (- (length (records *table*))
                   *stock-starting-index*))
         (b&h (buy-and-hold *table* :start start))
         (tf (trend-following *table* :start start))
         (start-utime (opening-time (nth start (records *table*))))
         (end-utime (closing-time (nth (current-index *analyzer*)
                                     (records *table*)))))
    (format t "~&~A (~A).~%"
            (description *table*)
            (string-upcase (ticker-symbol *table*)))
    (format t "~&Initial (~A): $~$.~%Final (~A): $~$.~%"
            (dd-mon-yyyy-string start-utime)
            *initial-money*
            (dd-mon-yyyy-string end-utime)
            (money *analyzer*))
    (format t "~&~D correct actions out of ~D, ~,5F% accuracy.~%"
            (correct-actions *analyzer*)
            (actions *analyzer*)
            (* 100.0 (/ (correct-actions *analyzer*)
                        (actions *analyzer*))))
    (format t "~&Buy-and-hold: $~$ (~,5F).~%"
            b&h (/ (money *analyzer*) b&h))
    (format t "~&Trend-following: $~$ (~,5F).~%"
            tf (/ (money *analyzer*) tf))
    (format t "~&~77~~%")
    (with-open-file (ca.data "correct-actions.data"
                             :direction :output
                             :if-exists :overwrite
                             :if-does-not-exist :create)
      (dolist (li (reverse *correct-actions-history*))
        (format ca.data "~&~F~%" li)))
    `((:ticker-symbol ,(ticker-symbol *table*))
      (:start-utime ,start-utime)
      (:initial-money ,*initial-money*)
      (:end-utime ,end-utime)
      (:final-money ,(money *analyzer*))
      (:correct-actions ,(correct-actions *analyzer*))
      (:actions ,(actions *analyzer*))
      (:correct-actions/actions ,(/ (correct-actions *analyzer*)
                                    (actions *analyzer*)))
;      (:buy-and-hold ,b&h)
;      (:money/buy-and-hold ,(/ (money *analyzer*) b&h))
;      (:trend-following ,tf)
;      (:money/trend-following ,(/ (money *analyzer*) tf))
      )))

#|
(defun experiment-stats (&key (runs 30)
                              (console-output t)
                              (file-output nil)
                              (data-output nil))
  (setf *stats* nil)
  (dotimes (run runs)
    (push (start-stocks-experiment) *stats*))
  (format console-output "~2%~32~ STATISTICS ~32~~%")
  (format console-output
          "~&Experiment for ~A (~A) on ~A.~%"
          (description *table*)
          (ticker-symbol *table*)
          (date-time-string (get-month) (get-date) (get-year)
                            (get-hour) (get-minute) (get-second)))
  (format data-output "~&~,,15$ ~,,15$ ~,,15$~%"
          (stat:mean *stats* :key #'(lambda (x)
                                      (second (assoc :buy-and-hold x))))
          (stat:mean *stats* :key #'(lambda (x)
                                      (second (assoc :trend-following x))))
          (stat:mean *stats* :key #'(lambda (x)
                                      (second (assoc :final-money x)))))
  (stat:format-report console-output "Money" "$~$"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :final-money x))))
  (stat:format-report console-output "Correct actions" "~D"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :correct-actions x))))
  (stat:format-report console-output "Correct actions/actions" "~,5F"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :correct-actions/actions x))))
  (stat:format-report console-output "Buy-and-hold" "$~$"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :buy-and-hold x))))
  (stat:format-report console-output "Money/B&H" "~,5F"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :money/buy-and-hold x))))
  (stat:format-report console-output "Trend-following" "$~$"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :trend-following x))))
  (stat:format-report console-output "Money/TF" "~,5F"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :money/trend-following x))))
  (format file-output
          "~%Experiment for ~A (~A) on ~A.~%"
          (description *table*)
          (ticker-symbol *table*)
          (date-time-string (get-month) (get-date) (get-year)
                            (get-hour) (get-minute) (get-second)))
  (stat:format-report file-output "Money" "$~$"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :final-money x))))
  (stat:format-report file-output "Correct actions" "~D"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :correct-actions x))))
  (stat:format-report file-output "Correct actions/actions" "~,5F"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :correct-actions/actions x))))
  (stat:format-report file-output "Buy-and-hold" "$~$"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :buy-and-hold x))))
  (stat:format-report file-output "Money/B&H" "~,5F"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :money/buy-and-hold x))))
  (stat:format-report file-output "Trend-following" "$~$"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :trend-following x))))
  (stat:format-report file-output "$Money/TF" "~,5F"
                      *stats*
                      :key #'(lambda (x)
                               (second (assoc :money/trend-following x))))
  (format t "~&~72~~%"))

(defun castats ()
  (with-open-file (castats.data "castats.data"
                                :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create)
    (format castats.data
            "~&# GA-threshold final-money correct-actios/actions~%"))
  (do ((ga-th 1 (1+ ga-th))
       results)
    ((> ga-th 100)
     nil)
    (setf (GA-threshold *learning-parameters*)
          ga-th)
    (ignore-errors
      (setf results (start-experiment))
      (with-open-file (castats.data "castats.data"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
        (format castats.data "~&~F  ~F  ~F~%"
                (GA-threshold *learning-parameters*)
                (second (assoc :final-money results))
                (second (assoc :correct-actions/actions results)))))))

(defun all-stats (&key (runs 10)
                       (stocks (cons "^dji" *^DJI-components*)))
  (with-open-file (file-output "stocks-xcs.log"
                               :direction :output
                               :if-exists :append
                               :if-does-not-exist :create)
    (with-open-file (data-output "stocks-xcs.data"
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create)
      (dolist (stock stocks)
        (load-*table* stock)
        (experiment-stats :runs runs
                          :file-output file-output
                          :data-output data-output)))))
|#
