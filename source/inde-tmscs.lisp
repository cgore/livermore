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

(defpackage :livermore/inde-tmscs
  (:use :common-lisp
        :livermore/inde-tmscs-parameters
        :livermore/statistics
        :livermore/tmscs
        :livermore/xcs
        :sigma/behave
        :sigma/control
        :sigma/numeric
        :sigma/probability
        :sigma/random
        :sigma/sequence)
  (:export :*inde-action-history*
           :*inde-hist*
           :*inde-tmscs*
           :*inde-tmscs-analyzer*
           :*inde-tmscs-experiment*
           :classify
           :correct-action
           :end-of-problem?
           :get-situation
           :history
           :ihs
           :initial-history-depth
           :inde-tmscs-analyzer
           :inde-tmscs-experiment
           :situation-function
           :start-inde-tmscs-experiment
           :terminate?))
(in-package :livermore/inde-tmscs)
(defstruct ihs heading steps multiplier value)
(defparameter *inde-hist* nil)
(defparameter *inde-action-history* nil)
(defparameter *inde-tmscs-analyzer* nil)
(defparameter *inde-tmscs* nil)
(defparameter *inde-tmscs-experiment* nil)

(defclass inde-tmscs-analyzer (environment reinforcement-program)
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

(defclass inde-tmscs-experiment (experiment)
  ()
  (:documentation "A TMSCS experiment that uses an inde-tmscs-analyzer."))

(defmethod single-step-output ((experiment inde-tmscs-experiment))
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
                (first *inde-hist*))
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
  slope is perturbed by *INDE-PERTURBATION*."
  (labels ((perturb (percentage)
                    (random-in-range (1+ (- percentage)) (1+ percentage)))
           (new-situation ()
             (push (cond
                     ;; A completely empty history.
                     ((null *inde-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *inde-hist*)))
                      (let ((f (first *inde-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (perturb *inde-perturbation*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *inde-hist*))))
                      (let* ((f (first *inde-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (perturb *inde-perturbation*)
                                               h (/ m 5)))))))
                   *inde-hist*)))
    (while (<= (length *inde-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *inde-hist*))))

(defun situation-function (time-step)
  "This returns the value at TIME-STEP of a piecewise linear series whose
  heading may flip with probability *INDE-FLIP*."
  (labels ((flip? (probability)
                  (if (probability? probability) -1 1))
           (new-situation ()
             (push (cond
                     ;; A completely empty history.
                     ((null *inde-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *inde-hist*)))
                      (let ((f (first *inde-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (flip? *inde-flip*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *inde-hist*))))
                      (let* ((f (first *inde-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (flip? *inde-flip*)
                                               h (/ m 5)))))))
                   *inde-hist*)))
    (while (<= (length *inde-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *inde-hist*))))

(defmethod get-situation ((analyzer inde-tmscs-analyzer))
  "This pushes the next series value onto the history and returns the series."
  (with-slots (number-of-situations history) analyzer
    (push (situation-function (incf number-of-situations)) history)
    history))

(defmethod classify ((analyzer inde-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (if (< (situation-function     number-of-situations)
           (situation-function (1+ number-of-situations)))
      :uptrending
      :downtrending)))

(defmethod correct-action ((inde-tmscs-analyzer inde-tmscs-analyzer))
  "This is :UPTRENDING or :DOWNTRENDING for the next point."
  (classify inde-tmscs-analyzer))

(defmethod correct-action? ((inde-tmscs-analyzer inde-tmscs-analyzer))
  "This predicate is true when the last action matches the next-point class."
  (equalp (current-action inde-tmscs-analyzer)
          (correct-action inde-tmscs-analyzer)))

(defmethod get-reward ((inde-tmscs-analyzer inde-tmscs-analyzer))
  "This returns 1000.0 for a correct action and 0.0 otherwise."
  (if (correct-action? inde-tmscs-analyzer)
    1000.0
    0.0))

(defmethod end-of-problem? ((inde-tmscs-analyzer inde-tmscs-analyzer))
  "This predicate is always true.  Each trial is a single step."
  ;; Does this really make any sense in TMSCS?
  t)

(defmethod terminate? ((experiment inde-tmscs-experiment))
  "This predicate is true after NUMBER-OF-TRIALS actions."
  (>= (number-of-actions (environment experiment))
      (number-of-trials experiment)))

(defmethod initialize-instance :after ((analyzer inde-tmscs-analyzer) &rest initargs &key &allow-other-keys)
  "This fills HISTORY to INITIAL-HISTORY-DEPTH."
  (declare (ignore initargs))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod execute-action ((inde-tmscs-analyzer inde-tmscs-analyzer) action)
  "This records ACTION, updates the correctness counts, and appends a
  record to *INDE-ACTION-HISTORY*."
  (with-slots (current-action
                number-of-actions
                number-of-correct-actions) inde-tmscs-analyzer
    (setf current-action action)
    (incf number-of-actions)
    (when (correct-action? inde-tmscs-analyzer)
      (incf number-of-correct-actions))
    (push (list current-action
                (correct-action? inde-tmscs-analyzer)
                number-of-correct-actions
                number-of-actions)
          *inde-action-history*)))

(defclass inde-tmscs-predicate (tms-predicate) ())
(defmethod print-object ((predicate inde-tmscs-predicate) stream)
  (format stream "~A -- ~A [~A,~A]"
          (initial predicate) (final predicate)
          (lower predicate) (upper predicate)))

(defclass inde-tmscs-classifier (tms-classifier) ())

(defun start-inde-tmscs-experiment
    (&optional (number-of-trials 10000) (run t))
  "This builds an independent TMSCS experiment and starts it.
  This is thesis section 4.2, increasing/decreasing method 4."
  (setf *inde-hist* nil
        *inde-action-history* nil)
  (setf *inde-tmscs-analyzer*
        (make-instance 'inde-tmscs-analyzer))
  (setf *inde-tmscs*
        (make-instance 'tmscs
                       :predicate-type 'inde-tmscs-predicate
                       :classifier-type 'inde-tmscs-classifier
                       :learning-parameters *inde-tmscs-learning-parameters*))
  (setf *inde-tmscs-experiment*
        (make-instance 'inde-tmscs-experiment
                       :environment *inde-tmscs-analyzer*
                       :reinforcement-program *inde-tmscs-analyzer*
                       :xcs *inde-tmscs*
                       :number-of-trials number-of-trials))
  (if run
    (start *inde-tmscs-experiment*)
    *inde-tmscs-experiment*))

(behavior 'inde-situation-function
  (let ((*inde-hist* nil)
        (*inde-flip* 0.0))
    (should-be-a 'number (situation-function 0))
    (should-be-a 'number (situation-function 20))
    (should= 21 (length *inde-hist*))
    (should-be-true (every #'ihs-p *inde-hist*))))

(behavior 'inde-tmscs-analyzer
  (let ((*inde-hist* nil))
    (let ((a (make-instance 'inde-tmscs-analyzer :initial-history-depth 10)))
      (should= 10 (length (history a)))
      (should-be-true (member (classify a) '(:uptrending :downtrending)))
      (should-be-true (end-of-problem? a)))))

(behavior 'inde-tmscs-experiment
  "Thesis section 4.2: a short increasing/decreasing TMSCS run."
  (let ((*standard-output* (make-broadcast-stream))
        (*inde-flip* 0.1))
    (start-inde-tmscs-experiment 8 t))
  (should= 8 (number-of-actions *inde-tmscs-analyzer*))
  (should-be-true (plusp (length (population *inde-tmscs*))))
  (should-be-true (<= (number-of-correct-actions *inde-tmscs-analyzer*)
                      (number-of-actions *inde-tmscs-analyzer*)))
  (should= 8 (length *inde-action-history*)))
