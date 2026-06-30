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


(defpackage :livermore/stocks-xcs
  (:use :common-lisp
        :livermore/statistics
        :livermore/stocks
        :livermore/stocks-xcs-parameters
        :livermore/time
        :livermore/xcs
        :livermore/xcs-analyzer)
  (:export :*analyzer*
           :*correct-actions-history*
           :*experiment*
           :*xcs*
           :actions
           :all-stats
           :buy-stock?
           :castats
           :correct-action?
           :correct-actions
           :current-action
           :current-index
           :current-record
           :elt-previous-record
           :elt-record
           :end-of-problem?
           :execute-action
           :experiment-stats
           :get-reward
           :get-situation
           :initial-index
           :initialize
           :load-*table*
           :money
           :money-from-stock
           :negative-action?
           :positive-action?
           :previous-action
           :previous-money
           :previous-record
           :should-have-bought-stock?
           :start-stocks-xcs-experiment
           :stocks-experiment
           :stocks-xcs-analyzer
           :table))
(in-package :livermore/stocks-xcs)

(defparameter *analyzer* nil)
(defparameter *correct-actions-history* nil)
(defparameter *experiment* nil)
(defparameter *xcs* nil)

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

(defmethod elt-record ((stocks-xcs-analyzer stocks-xcs-analyzer) (n integer) &optional (key #'identity))
  (declare (ignore key))
  (assert (not (minusp n)))
  (elt-record (table stocks-xcs-analyzer) n key))

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
  (setf *analyzer* (make-instance 'stocks-xcs-analyzer :table *table*))
  (setf *xcs* (make-instance 'xcs :learning-parameters *learning-parameters*))
  (setf *experiment* (make-instance 'stocks-experiment
                                    :environment *analyzer*
                                    :reinforcement-program *analyzer*
                                    :xcs *xcs*))
  (setf *correct-actions-history* nil)
  (start *experiment*)
  (format t "~&~34~ RESULTS ~34~~%")
  (let* ((start (- (length (records *table*))
                   *stock-starting-index*))
         (b&h (buy-and-hold *table* :start start))
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
                                    (actions *analyzer*))))))
