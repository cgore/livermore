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


(defpackage :livermore/learning-parameters
  (:use :common-lisp)
  (:export :learning-parameters
           :maximum-total-numerosity
           :learning-rate
           :multiplier-parameter
           :equal-error-threshold
           :power-parameter
           :discount-factor
           :ga-threshold
           :crossover-probability
           :mutation-probability
           :deletion-threshold
           :fitness-fraction-threshold
           :minimum-subsumption-experience
           :covering-probability
           :initial-prediction
           :initial-prediction-error
           :initial-fitness
           :exploration-probability
           :minimum-number-of-actions
           :ga-subsumption?
           :action-set-subsumption?
           :possible-actions))
(in-package :livermore/learning-parameters)

(defclass learning-parameters ()
  ((maximum-total-numerosity
    :accessor maximum-total-numerosity
    :initform 100
    :initarg :maximum-total-numerosity
    :type integer
    :documentation
    "This is `N' in Butz and Wilson's paper, usually in the hundreds.")
   (learning-rate
    :accessor learning-rate
    :initform 0.1
    :initarg :learning-rate
    :type float
    :documentation
    "This is `beta' in Butz and Wilson's paper, usually in [0.1, 0.2].")
   (multiplier-parameter
    :accessor multiplier-parameter
    :initform 0.1
    :initarg :multiplier-parameter
    :type float
    :documentation
    "This is `alpha' in Butz and Wilson's paper, usually 0.1.")
   (equal-error-threshold
    :accessor equal-error-threshold
    :initform 10.0
    :initarg :equal-error-threshold
    :type float
    :documentation
    "This is `epsilon_0' in Butz and Wilson's paper, usually 1% of rho,
       the reward (which is typically set to 1000/0 in the simplest cases.)")
   (power-parameter
    :accessor power-parameter
    :initform 5
    :initarg :power-parameter
    :documentation
    "This is `nu' in Butz and Wilson's paper, usually 5.")
   (discount-factor
    :accessor discount-factor
    :initform 0.71
    :initarg :discount-factor
    :type float
    :documentation
    "This is `gamma' in Butz and Wilson's paper, usually 0.71.")
   (GA-threshold
    :accessor GA-threshold
    :initform 50
    :initarg :GA-threshold
    :documentation
    "This is `theta_GA' in Butz and Wilson's paper, usually in [25, 50].")
   (crossover-probability
    :accessor crossover-probability
    :initform 0.5
    :initarg :crossover-probability
    :type float
    :documentation
    "This is `chi' in Butz and Wilson's paper, usually in [0.5, 1.0].")
   (mutation-probability
    :accessor mutation-probability
    :initform 0.025
    :initarg :mutation-probability
    :type float
    :documentation
    "This is `mu' in Butz and Wilson's paper, usually in [0.01, 0.05].")
   (deletion-threshold
    :accessor deletion-threshold
    :initform 20
    :initarg :deletion-threshold
    :documentation
    "This is `theta_del' in Butz and Wilson's paper, usually 20.")
   (fitness-fraction-threshold
    :accessor fitness-fraction-threshold
    :initform 0.1
    :initarg :fitness-fraction-threshold
    :type float
    :documentation
    "This is used in the DELETION-VOTE method, and is usually 0.1.")
   (minimum-subsumption-experience
    :accessor minimum-subsumption-experience
    :initform 20
    :initarg :minimum-subsumption-experience
    :type integer
    :documentation
    "This is `theta_sub' in Butz and Wilson's paper, usually >= 20.")
   (covering-probability
    :accessor covering-probability
    :initform 0.33
    :initarg :covering-probability
    :type float
    :documentation
    "This is `P_#' in Butz and Wilson's paper, usually around 0.33.")
   (initial-prediction
    :accessor initial-prediction
    :initform 0.001
    :initarg :initial-prediction
    :type float
    :documentation
    "This is `p_I' in Butz and Wilson's paper, usually only slightly more
       than zero.")
   (initial-prediction-error
    :accessor initial-prediction-error
    :initform 0.001
    :initarg :initial-prediction-error
    :type float
    :documentation
    "This is `epsilon_I' in Butz and Wilson's paper, usually only slightly
       more than zero.")
   (initial-fitness
    :accessor initial-fitness
    :initform 0.001
    :initarg :initial-fitness
    :type float
    :documentation
    "This is `F_I' in Butz and Wilson's paper, usually only slightly more
       than zero.")
   (exploration-probability
    :accessor exploration-probability
    :initform 0.5
    :initarg :exploration-probability
    :type float
    :documentation
    "This is `P_explr' in Butz and Wilson's paper, usually around 0.5.")
   (minimum-number-of-actions 
    :accessor minimum-number-of-actions
    :initarg :minimum-number-of-actions
    :type (integer 0 *)
    :documentation
    "This is the theta_mna in Butz and Wilson's paper, usually equal to the
       number of possible actions in order for covering to take place.")
   (maximum-number-of-steps
    :accessor maximum-number-of-steps
    :initform 50
    :initarg maximum-number-of-steps
    :type integer
    :documentation
    "This is the maximum number of steps that a multistep problem can spend
        in one trial.")
   (GA-subsumption?
    :accessor GA-subsumption?
    :initform t
    :initarg :GA-subsumption?
    :type boolean
    :documentation
    "This is `doGASubsumption' in Butz and Wilson's paper, a boolean
       parameter to specify if subsumption should be performed during the
       genetic algorithm's run or not.")
   (action-set-subsumption?
    :accessor action-set-subsumption?
    :initform t
    :initarg :action-set-subsumption?
    :type boolean
    :documentation
    "This is a boolean parameter that specifies if action sets are to be
       tested for for subsuming classifiers.  It is called
       doActionSetSubsumption in Butz and Wilson's paper.")
   (possible-actions
    :accessor possible-actions
    :initform '(nil t)
    :initarg :possible-actions
    :type list
    :documentation
    "This is a list of all the possible actions available to the XCS."))
  (:documentation "These are all of the learning parameters used by the XCS."))
