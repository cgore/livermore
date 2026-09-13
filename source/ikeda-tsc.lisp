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

(defpackage :livermore/ikeda-tsc
  (:use :common-lisp
        :livermore/ikeda-tsc-parameters
        :livermore/statistics
        :livermore/tmscs
        :livermore/xcs
        :sigma/behave
        :sigma/control
        :sigma/numeric
        :sigma/probability
        :sigma/random
        :sigma/sequence)
  (:export :*ikeda-action-history*
           :*ikeda-hist*
           :*ikeda-tmscs*
           :*ikeda-tmscs-analyzer*
           :*ikeda-tmscs-experiment*
           :classify
           :correct-action
           :end-of-problem?
           :get-situation
           :history
           :ikeda-step
           :ikeda-tmscs-analyzer
           :ikeda-tmscs-experiment
           :ikeda-x
           :start-ikeda-tmscs-experiment
           :terminate?))
(in-package :livermore/ikeda-tsc)
(defstruct ihs heading steps multiplier value)
(defparameter *ikeda-hist* nil)
(defparameter *ikeda-action-history* nil)
(defparameter *ikeda-tmscs-analyzer* nil)
(defparameter *ikeda-tmscs* nil)
(defparameter *ikeda-tmscs-experiment* nil)

(defclass ikeda-tmscs-analyzer (environment reinforcement-program)
  ((history
     :accessor history
     :initform nil
     :initarg :history
     :type list)
   (number-of-situations
     :accessor number-of-situations
     :initform 0
     :initarg :number-of-situations
     :type (integer 0 *))
   (current-action
     :accessor current-action
     :initarg :current-action)
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
   (initial-history-depth
     :accessor initial-history-depth
     :initform 50
     :initarg :initial-history-depth
     :type (integer 0 *)))
  (:documentation
   "An environment that presents a piecewise linear series and asks
   whether the next point is uptrending or downtrending."))

(defclass ikeda-tmscs-experiment (experiment)
  ()
  (:documentation "A TMSCS experiment that uses an ikeda-tmscs-analyzer."))

(defmethod single-step-output ((experiment ikeda-tmscs-experiment))
  "This prints the last action and, when *STAT-REPORT* is true, statistics
  for the population, match set, and action set."
  (with-slots (environment reinforcement-program xcs) experiment
    (with-slots (current-action
                  history
                  number-of-correct-actions
                  number-of-actions) environment
      (with-slots (population match-set action-set) xcs
        (format t "~&[~12A] ~12A: ~D/~D = ~,3F%, ~A choice ~D.~%"
                (correct-action environment)
                current-action
                number-of-correct-actions
                number-of-actions
                (* 100.0 (/ number-of-correct-actions number-of-actions))
                (if (correct-action? environment)
                  "  correct"
                  "incorrect")
                (first *ikeda-hist*))
        (when *stat-report*
          (stat-report t population :key 'prediction :pre-string "~&  P p :: ")
          (stat-report t population :key 'prediction-error
                       :pre-string "~&  P perr :: ")
          (stat-report t population :key 'fitness :pre-string "~&  P F :: ")
          (stat-report t match-set :key 'prediction :pre-string "~&  M p :: ")
          (stat-report t match-set :key 'prediction-error
                       :pre-string "~&  M perr :: ")
          (stat-report t match-set :key 'fitness :pre-string "~&  M F :: ")
          (stat-report t action-set :key 'prediction :pre-string "~&  A p :: ")
          (stat-report t action-set :key 'prediction-error
                       :pre-string "~&  A perr :: ")
          (stat-report t action-set :key 'fitness
                       :pre-string "~&  A F :: "))))))

(defun situation-function[3] (time-step)
  "This returns the value at TIME-STEP of a piecewise linear series whose
  slope is perturbed by *IKEDA-PERTURBATION*."
  (labels ((perturb (percentage)
                    (random-in-range (1+ (- percentage)) (1+ percentage)))
           (new-situation ()
             (push (cond
                     ;; A completely empty history.
                     ((null *ikeda-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *ikeda-hist*)))
                      (let ((f (first *ikeda-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (perturb *ikeda-perturbation*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *ikeda-hist*))))
                      (let* ((f (first *ikeda-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (perturb *ikeda-perturbation*)
                                               h (/ m 5)))))))
                   *ikeda-hist*)))
    (while (<= (length *ikeda-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *ikeda-hist*))))

(defun situation-function (time-step)
  "This returns the value at TIME-STEP of a piecewise linear series whose
  heading may flip with probability *IKEDA-FLIP*."
  (labels ((flip? (probability)
                  (if (probability? probability) -1 1))
           (new-situation ()
             (push (cond
                     ;; A completely empty history.
                     ((null *ikeda-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *ikeda-hist*)))
                      (let ((f (first *ikeda-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (flip? *ikeda-flip*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *ikeda-hist*))))
                      (let* ((f (first *ikeda-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (flip? *ikeda-flip*)
                                               h (/ m 5)))))))
                   *ikeda-hist*)))
    (while (<= (length *ikeda-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *ikeda-hist*))))

(defun ikeda-step (x y &optional (u 0.9))
  "One iteration of the Ikeda map with parameter U, typically 0.9."
  (let ((tn (- 0.4 (/ 6.0 (+ 1.0 (* x x) (* y y))))))
    (values (+ 1.0 (* u (- (* x (cos tn)) (* y (sin tn)))))
            (* u (+ (* x (sin tn)) (* y (cos tn)))))))

(defun ikeda-x (n &optional (x0 0.1) (y0 0.1) (u 0.9))
  "The x-coordinate of the Ikeda map after N steps from (X0, Y0)."
  (let ((x x0) (y y0))
    (dotimes (i n x)
      (multiple-value-setq (x y) (ikeda-step x y u)))))

(defmethod get-situation ((analyzer ikeda-tmscs-analyzer))
  "This pushes the next series value onto the history and returns the series."
  (with-slots (number-of-situations history) analyzer
    (push (situation-function (incf number-of-situations)) history)
    history))

(defmethod classify ((analyzer ikeda-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (if (< (situation-function     number-of-situations)
           (situation-function (1+ number-of-situations)))
      :uptrending
      :downtrending)))

(defmethod correct-action ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  "This is :UPTRENDING or :DOWNTRENDING for the next point."
  (classify ikeda-tmscs-analyzer))

(defmethod correct-action? ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  "This predicate is true when the last action matches the next-point class."
  (equalp (current-action ikeda-tmscs-analyzer)
          (correct-action ikeda-tmscs-analyzer)))

(defmethod get-reward ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  "This returns 1000.0 for a correct action and 0.0 otherwise."
  (if (correct-action? ikeda-tmscs-analyzer)
    1000.0
    0.0))

(defmethod end-of-problem? ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  "This predicate is always true.  Each trial is a single step."
  ;; Does this really make any sense in TMSCS?
  t)

(defmethod terminate? ((experiment ikeda-tmscs-experiment))
  "This predicate is true after NUMBER-OF-TRIALS actions."
  (>= (number-of-actions (environment experiment))
      (number-of-trials experiment)))

(defmethod initialize-instance :after ((analyzer ikeda-tmscs-analyzer) &rest initargs &key &allow-other-keys)
  "This fills HISTORY to INITIAL-HISTORY-DEPTH."
  (declare (ignore initargs))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod execute-action ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer) action)
  "This records ACTION, updates the correctness counts, and appends a
  record to *IKEDA-ACTION-HISTORY*."
  (with-slots (current-action
                number-of-actions
                number-of-correct-actions) ikeda-tmscs-analyzer
    (setf current-action action)
    (incf number-of-actions)
    (when (correct-action? ikeda-tmscs-analyzer)
      (incf number-of-correct-actions))
    (push (list current-action
                (correct-action? ikeda-tmscs-analyzer)
                number-of-correct-actions
                number-of-actions)
          *ikeda-action-history*)))

(defclass ikeda-tmscs-predicate (tms-predicate) ())
(defmethod print-object ((predicate ikeda-tmscs-predicate) stream)
  (format stream "~A -- ~A [~A,~A]"
          (initial predicate) (final predicate)
          (lower predicate) (upper predicate)))

(defclass ikeda-tmscs-classifier (tms-classifier) ())

(defun start-ikeda-tmscs-experiment
    (&optional (number-of-trials 10000) (run t))
  "This builds an Ikeda TMSCS experiment and starts it."
  (setf *ikeda-hist* nil
        *ikeda-action-history* nil)
  (setf *ikeda-tmscs-analyzer*
        (make-instance 'ikeda-tmscs-analyzer))
  (setf *ikeda-tmscs*
        (make-instance 'tmscs
                       :predicate-type 'ikeda-tmscs-predicate
                       :classifier-type 'ikeda-tmscs-classifier
                       :learning-parameters *ikeda-tmscs-learning-parameters*))
  (setf *ikeda-tmscs-experiment*
        (make-instance 'ikeda-tmscs-experiment
                       :environment *ikeda-tmscs-analyzer*
                       :reinforcement-program *ikeda-tmscs-analyzer*
                       :xcs *ikeda-tmscs*
                       :number-of-trials number-of-trials))
  (if run
    (start *ikeda-tmscs-experiment*)
    *ikeda-tmscs-experiment*))

(behavior 'ikeda-map
  (multiple-value-bind (x y) (ikeda-step 0.1 0.1)
    (should-be-a 'float x y))
  (should-be-a 'float (ikeda-x 10))
  (should= 0.1 (ikeda-x 0)))

(behavior 'ikeda-tmscs-experiment
  (let ((*standard-output* (make-broadcast-stream)))
    (start-ikeda-tmscs-experiment 6 t))
  (should= 6 (number-of-actions *ikeda-tmscs-analyzer*))
  (should-be-true (plusp (length (population *ikeda-tmscs*)))))
