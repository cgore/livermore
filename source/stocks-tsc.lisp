;;;; Copyright (c) 2005 -- 2014, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
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

(load "utilities/utilities")
(load "statistics")
(load "tmscs")
(load "stocks")
(load "time")
(in-package "XCS")
(use-package '("STATISTICS" "STOCKS" "TIME"))
(export '(stocks-tsc-analyzer
           table
           history
           action-history
           money-history
           stats-history
           number-of-situations
           initial-history-depth
           stocks-tsc-experiment
           get-situation
           classify
           correct-action
           correct-action?
           incorrect-action?
           end-of-problem?
           terminate?
           start-stocks-tsc-experiment
           start-stocks-tsc-experiment-stats))

(defclass stocks-tsc-analyzer (environment reinforcement-program)
  ((table
     :accessor table
     :initarg :table)
   (history
     :accessor history
     :initform nil
     :initarg :history
     :type list)
   (action-history
     :accessor action-history
     :initform nil
     :initarg :action-history
     :type list)
   (money-history
     :accessor money-history
     :initform (list *initial-money*)
     :initarg :money-history
     :type list)
   (stats-history
     :accessor stats-history
     :initform nil
     :initarg :stats-history
     :type list)
   (number-of-situations
     :accessor number-of-situations
     :initform 0
     :initarg :number-of-situations
     :type (integer 0 *))
   (number-of-actions
     :accessor number-of-actions
     :initform 0
     :initarg :number-of-actions
     :type (integer 0 *))
   (number-of-correct-actions
     :accessor number-of-correct-actions
     :initform 0
     :initarg :number-of-correct-actions
     :type (integer 0 *))
   (number-of-up-steps
     :accessor number-of-up-steps
     :initform 0
     :initarg :number-of-up-steps
     :type (integer 0 *))
   (initial-history-depth
     :accessor initial-history-depth
     :initform 200
     :initarg :initial-history-depth
     :type (integer 0 *))))

(defclass stocks-tsc-experiment (experiment)
  ())

(defun %-of (a b)
  (* 100.0 (/ a b)))

(defmethod single-step-output ((experiment stocks-tsc-experiment))
  (with-slots (environment reinforcement-program xcs) experiment
    (with-slots (history
                  stats-history
                  number-of-correct-actions
                  number-of-actions
                  number-of-up-steps) environment
      (with-slots (population match-set action-set) xcs
        (let* ((b&h (buy-and-hold (table environment) *initial-money*
                                  :end (1+ (number-of-situations environment))))
               (stats-history-entry 
                 (list (correct-action environment)
                       (current-action environment)
                       number-of-correct-actions
                       number-of-up-steps
                       number-of-actions
                       (%-of number-of-correct-actions number-of-actions)
                       (%-of number-of-up-steps number-of-actions)
                       (correct-action? environment)
                       (current-money environment)
                       b&h
                       (/ (current-money environment) b&h))))
          (push stats-history-entry stats-history)
          (when *single-step-output?*
            (format t "~&[~5A] ~5A: ~D[~D]/~D = ~,3F[~,3F]%, "
                    (correct-action environment)
                    (current-action environment)
                    number-of-correct-actions
                    number-of-up-steps
                    number-of-actions
                    (%-of number-of-correct-actions number-of-actions)
                    (%-of number-of-up-steps number-of-actions))
            (format t "~A choice [$~8,2F $~8,2F ~,5F].~%"
                    (if (correct-action? environment) "  correct" "incorrect")
                    (current-money environment)
                    b&h
                    (/ (current-money environment) b&h))
            (when *stat-report*
              (stat-report t population 
                           :key 'prediction 
                           :pre-string "~&  P p :: ")
              (stat-report t population
                           :key 'prediction-error
                           :pre-string "~&  P perr :: ")
              (stat-report t population 
                           :key 'fitness 
                           :pre-string "~&  P F :: ")
              (stat-report t match-set 
                           :key 'prediction 
                           :pre-string "~&  M p :: ")
              (stat-report t match-set
                           :key 'prediction-error
                           :pre-string "~&  M perr :: ")
              (stat-report t match-set 
                           :key 'fitness 
                           :pre-string "~&  M F :: ")
              (stat-report t action-set
                           :key 'prediction
                           :pre-string "~&  A p :: ")
              (stat-report t action-set
                           :key 'prediction-error
                           :pre-string "~&  A perr :: ")
              (stat-report t action-set
                           :key 'fitness
                           :pre-string "~&  A F :: "))))))))

(defun situation-function (time-step)
  (elt-record *table* time-step))

(defmethod get-situation ((analyzer stocks-tsc-analyzer))
  (with-slots (number-of-situations history initial-history-depth) analyzer
    (push (situation-function (incf number-of-situations)) history)))

(defmethod current-situation ((analyzer stocks-tsc-analyzer))
  (situation-function (number-of-situations analyzer)))

(defmethod next-situation ((analyzer stocks-tsc-analyzer))
  (situation-function (1+ (number-of-situations analyzer))))

(defmethod classify.3 ((analyzer stocks-tsc-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (let (($   (adjusted-closing-price (current-situation analyzer)))
          ($+1 (adjusted-closing-price (next-situation    analyzer))))
      (cond ((< $ $+1) :stock)
            ((> $ $+1) :bank)
            (t :hold)))))

(defmethod money-ratio ((analyzer stocks-tsc-analyzer))
  (/ (adjusted-closing-price (next-situation analyzer))
     (adjusted-closing-price (current-situation analyzer))))

(defmethod classify.money-ratio ((analyzer stocks-tsc-analyzer) (ratio float))
  (< ratio (money-ratio analyzer)))

(defmethod classify.money-ratio.opt ((analyzer stocks-tsc-analyzer))
  (classify.money-ratio analyzer 0.975))

(defmethod classify.money-ratio.pess ((analyzer stocks-tsc-analyzer))
  (classify.money-ratio analyzer 1.025))

(defmethod going-up? ((analyzer stocks-tsc-analyzer))
  (< 1 (money-ratio analyzer)))

(defmethod going-down? ((analyzer stocks-tsc-analyzer))
  (< (money-ratio analyzer) 1))

(defmethod classify.going-up? ((analyzer stocks-tsc-analyzer))
  "We classify the next point as either up or down from our current point."
  (if (going-up? analyzer) :stock :bank))

(defmethod classify ((analyzer stocks-tsc-analyzer))
  (funcall *classification-method* analyzer))

(defmethod current-action ((analyzer stocks-tsc-analyzer))
  (first (first (action-history analyzer))))

(defmethod previous-action ((analyzer stocks-tsc-analyzer))
  (first (second (action-history analyzer))))

(defmethod current-money ((analyzer stocks-tsc-analyzer))
  (first (money-history analyzer)))

(defmethod correct-action ((stocks-tsc-analyzer stocks-tsc-analyzer))
  (classify stocks-tsc-analyzer))

(defmethod correct-action? ((analyzer stocks-tsc-analyzer))
  (or (equalp (current-action analyzer)
              (correct-action analyzer))
      (and (equalp (current-action analyzer)
                   :hold)
           (equalp (previous-action analyzer)
                   (correct-action analyzer)))))

(defmethod incorrect-action? ((analyzer stocks-tsc-analyzer))
  (not (correct-action? analyzer)))

(defmethod get-reward.a1 ((analyzer stocks-tsc-analyzer))
  (if (correct-action? analyzer) 1000.0 0.0))

(defmethod get-reward.a2 ((analyzer stocks-tsc-analyzer))
  (if (correct-action? analyzer) 1000.0 -200.0))

(defmethod get-reward.b ((analyzer stocks-tsc-analyzer))
  (if (< 1.005 (money-ratio analyzer))
    1000.0 0.0))

(defmethod get-reward.c ((analyzer stocks-tsc-analyzer))
  (let ((m 1000.0)
        (e 2.0)
        (s 1.015))
    (* m (expt (- (money-ratio analyzer) s) e))))

(defmethod get-reward.d ((analyzer stocks-tsc-analyzer)
                         correct-up correct-down incorrect-up incorrect-down)
  ;; We actually do not going up instead of going down for the case of
  ;; no change in the price to be handled correctly.
  (dolist (reward (list correct-up correct-down incorrect-up incorrect-down))
    (assert (numberp reward)))
  (cond ((and (correct-action? analyzer) (going-up? analyzer))
         correct-up)
        ((and (correct-action? analyzer) (not (going-up? analyzer)))
         correct-down)
        ((and (incorrect-action? analyzer) (going-up? analyzer))
         incorrect-up)
        ((and (incorrect-action? analyzer) (going-up? analyzer))
         incorrect-down)
        (t 0.0)))

(defmethod get-reward.d-opt ((analyzer stocks-tsc-analyzer))
  (get-reward.d analyzer 1000.0 750.0 10.0 200.0))

(defmethod get-reward.d-pess ((analyzer stocks-tsc-analyzer))
  (get-reward.d analyzer 750.0 1000.0 200.0 10.0))

(defmethod get-reward ((analyzer stocks-tsc-analyzer))
  (funcall *reward-method* analyzer))

(defmethod end-of-problem? ((stocks-tsc-analyzer stocks-tsc-analyzer))
  ;; Does this really make any sense in TMSCS?
  nil) ; T previously

(defmethod terminate? ((experiment stocks-tsc-experiment))
  (<= *stock-termination-actions* (number-of-actions (environment experiment))))

(defmethod initialize ((analyzer stocks-tsc-analyzer))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod money-from-stock ((analyzer stocks-tsc-analyzer))
  (buy-and-hold (table analyzer)
                (current-money analyzer)
                :start (number-of-situations analyzer)
                :end (1+ (number-of-situations analyzer))))

(defmethod money-from-bank ((analyzer stocks-tsc-analyzer)
                            &key (interest-rate 1.04))
  ;; This is an approximation, based upon 220 trading days a year, and it is
  ;; not very accurate.  However, it ends up rounding down, so it is acceptable
  ;; for our puproses.
  (* (1+ (/ (1- interest-rate) 220))
     (first (money-history analyzer))))

(defmethod new-money ((analyzer stocks-tsc-analyzer))
  (if (equalp :stock (current-action analyzer))
    (money-from-stock analyzer)
    (money-from-bank analyzer)))

(defmethod execute-action ((analyzer stocks-tsc-analyzer) action)
  (with-slots (action-history
                money-history
                number-of-actions
                number-of-correct-actions
                number-of-up-steps) analyzer
    (incf number-of-actions)
    (when (correct-action? analyzer)
      (incf number-of-correct-actions))
    (when (going-up? analyzer)
      (incf number-of-up-steps))
    (push (list action
                (correct-action? analyzer)
                number-of-correct-actions
                number-of-actions)
          action-history)
    (push (new-money analyzer) money-history)))

(defun simple-slope (list &key (key #'identity) (start 0) (end nil))
  "This function returns the simple slope of the line."
  (assert (listp list))
  (when (null end)
    (setf end (1- (length list))))
  (assert (not (= start end)))
  (/ (- (funcall key (nth start list))
        (funcall key (nth end list)))
     (- start end)))

(defclass stocks-tsc-predicate (tms-predicate) ())
(defmethod print-object ((predicate stocks-tsc-predicate) stream)
  (format stream "~A -- ~A [~A,~A]"
          (initial predicate) (final predicate)
          (lower predicate) (upper predicate)))

(defclass stocks-tsc-classifier (tms-classifier) ())

(load "stocks-tsc-parameters")

(defun load-*table* (stock-ticker)
  (defparameter *table* (load-table stock-ticker)))
(load-*table* *stock-ticker*)

(defun stocks-tsc-experiment ()
  (let* ((analyzer (make-instance 'stocks-tsc-analyzer :table *table*))
         (parameters *stocks-tsc-learning-parameters*)
         (stocks-tsc (make-instance 'tmscs
                                    :predicate-type 'stocks-tsc-predicate
                                    :classifier-type 'stocks-tsc-classifier
                                    :learning-parameters parameters))
         (experiment (make-instance 'stocks-tsc-experiment
                                    :environment analyzer
                                    :reinforcement-program analyzer
                                    :xcs stocks-tsc)))
    (format t "~&Starting stock-tsc experiment, stock=~A, reward method=~A.~%"
            *stock-ticker* *reward-method*)
    (describe parameters)
    (initialize stocks-tsc)
    (start experiment)
    (list experiment parameters (stats-history analyzer))))

;; This is a list of the current reward methods that we are interested in investigating.
(defparameter *reward-methods* 
  '(get-reward.a1
    get-reward.a2
    get-reward.b
    get-reward.c
    get-reward.d-opt
    get-reward.d-pess))

(defun stocks-tsc-reward-stats (&optional (reward-method (second *reward-methods*)) (n 30))
  (setf *reward-method* reward-method)
  (with-open-file (output "stocks-tsc-reward-stats"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
      (format output
              "~3%Starting statistical run for reward method ~A, ~A.~%~
              Parameters:~%~A~3%"
              reward-method (date-time-string) *stocks-tsc-learning-parameters*))
    (dotimes (i n)
      (let ((run-result (stocks-tsc-experiment)))
        (with-open-file (output "stocks-tsc-reward-stats"
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (format output
                  "Run ~A completed, ~A.~%~
                  ~3%Experiment history:~%~A~2%"
                  i (date-time-string) (third run-result))))))

(defun stocks-tsc-parameter-stats (parameter-name value &optional (n 30))
  (let ((filename (string-concatenate "stocks-tsc-parameter-stats-" 
                                      parameter-name)))
    (with-open-file (output filename 
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (setf (slot-value *stocks-tsc-learning-parameters* parameter-name)
            value)
      (format output
              "~3%Starting statistical run for ~A = ~A.~%~
              Parameters:~%~A~3%"
              parameter-name value *stocks-tsc-learning-parameters*)
      (dotimes (i n)
        (let ((run-result (stocks-tsc-experiment)))
          (format output
                  "Run ~A completed~%~
                  ~3%Experiment history:~%~A~2%"
                  i (third run-result)))))))

(defparameter *parameter-methods* 
  '((ga-threshold (25 30 35 40 45 50))
    (crossover-probability (0.5 0.6 0.7 0.8 0.9 1.0))
    (mutation-probability (0.01 0.02 0.03 0.04 0.05))
    (exploration-probability (0.1 0.2 0.3 0.4 0.5))
    (maximum-environment-condition-length (1 2 5 10 20))
    (maximum-temporal-mutation (1 2 5 8 10))
    (maximum-position-mutation (1 2 5 8 10))))

(defun stocks-tsc-parameter-stats-experiment (method-nth value-nth)
  (let ((method (first (nth method-nth *parameter-methods*)))
        (value (nth value-nth (nth method-nth *parameter-methods*))))
    (stocks-tsc-parameter-stats method value)))
