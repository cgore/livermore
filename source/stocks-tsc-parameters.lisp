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

(defpackage :livermore/stocks-tsc-parameters
  (:use :common-lisp
        :livermore/learning-parameters
        :livermore/stocks
        :livermore/tmscs
        :sigma/behave)
  (:export :*classification-method*
           :*reward-method*
           :*single-step-output?*
           :*stat-report*
           :*stock-starting-index*
           :*stock-termination-actions*
           :*stock-ticker*
           :*stocks-tsc-initial-money*
           :*stocks-tsc-learning-parameters*
           :*stocks-tsc-learning-parameters-long*
           :*valid-actions*
           :*valid-actions-2*
           :*valid-actions-3*))
(in-package :livermore/stocks-tsc-parameters)

(defparameter *single-step-output?* t)
(defparameter *stat-report* nil)
(defparameter *stock-starting-index* 100)
(defparameter *stock-termination-actions* 1500)
(defparameter *reward-method* :a2)
(defparameter *classification-method* :going-up)
(defparameter *valid-actions-3* '(:stock :bank :hold))
(defparameter *valid-actions-2* '(:stock :bank))
(defparameter *valid-actions* *valid-actions-2*)
(defparameter *stocks-tsc-initial-money* 1000000.00)
(defparameter *stocks-tsc-learning-parameters*
  (make-instance 'tmscs-learning-parameters
                 :maximum-environment-condition-length 10
                 :valid-operations (list #'simple-slope)
                 :valid-fields (list #'closing-price
                                     #'opening-price
                                     #'trading-volume)
                 :minimum-number-of-actions (length *valid-actions*)
                 :maximum-total-numerosity 1000
                 :learning-rate 0.2
                 :discount-factor 0.71
                 :GA-threshold 25
                 :equal-error-threshold 20.0
                 :multiplier-parameter 0.1
                 :crossover-probability 0.9
                 :mutation-probability 0.04
                 :exploration-probability 0.2
                 :fitness-fraction-threshold 0.1
                 :covering-probability 0.33
                 :initial-prediction 10.0
                 :initial-prediction-error 0.0
                 :initial-fitness 0.01
                 :minimum-number-of-actions (length *valid-actions*)
                 :GA-subsumption? t
                 :action-set-subsumption? nil
                 :possible-actions *valid-actions*))

;;; First try for a ~4000-day window (1990-2006), not the 1500-day thesis run.
;;; Larger working memory, slower GA/prediction updates, less exploration.
;;; Leave action-set-subsumption off; it can hang XCSR-style loops.
(defparameter *stocks-tsc-learning-parameters-long*
  (make-instance 'tmscs-learning-parameters
                 :maximum-environment-condition-length 15
                 :visible-time-range '(0 100)
                 :valid-operations (list #'simple-slope)
                 :valid-fields (list #'closing-price
                                     #'opening-price
                                     #'trading-volume)
                 :minimum-number-of-actions (length *valid-actions*)
                 :maximum-total-numerosity 5000
                 :learning-rate 0.1
                 :discount-factor 0.71
                 :GA-threshold 75
                 :deletion-threshold 50
                 :equal-error-threshold 20.0
                 :multiplier-parameter 0.1
                 :crossover-probability 0.8
                 :mutation-probability 0.03
                 :exploration-probability 0.1
                 :fitness-fraction-threshold 0.1
                 :covering-probability 0.33
                 :initial-prediction 10.0
                 :initial-prediction-error 0.0
                 :initial-fitness 0.01
                 :minimum-subsumption-experience 40
                 :GA-subsumption? t
                 :action-set-subsumption? nil
                 :possible-actions *valid-actions*))

(defparameter *stock-ticker* "^dji")

(behavior 'stocks-tsc-learning-parameters
  (should-be-a 'tmscs-learning-parameters *stocks-tsc-learning-parameters*)
  (should-be-a 'tmscs-learning-parameters *stocks-tsc-learning-parameters-long*)
  (should= 5000 (maximum-total-numerosity *stocks-tsc-learning-parameters-long*))
  (should= 75 (GA-threshold *stocks-tsc-learning-parameters-long*))
  (should= 0.1 (exploration-probability *stocks-tsc-learning-parameters-long*))
  (should-be-false (action-set-subsumption? *stocks-tsc-learning-parameters-long*))
  (should-eq :a2 *reward-method*)
  (should= 100 *stock-starting-index*)
  (should= 1500 *stock-termination-actions*)
  (should-equal '(:stock :bank) *valid-actions*)
  (should= 1000000.00 *stocks-tsc-initial-money*))
