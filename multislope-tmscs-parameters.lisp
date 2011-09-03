;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore,
;;;; All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com

(in-package "XCS")

(defparameter *stat-report* nil)
(defparameter *multislope-categories* 5)
(defparameter *multislope-tmscs-learning-parameters*
  (make-instance 'tmscs-learning-parameters
                 :valid-operations (list #'simple-slope)
                 :minimum-number-of-actions 2
                 :maximum-total-numerosity 2000 ; N = 400
                 :learning-rate 0.2 ; beta = 0.2
                 :discount-factor 0.71 ; gamma = 0.71
                 :GA-threshold 25 ; theta = 25
                 :equal-error-threshold 100.0 ; epsilon_0 = 10.0
                 :multiplier-parameter 0.1 ; alpha = 0.1
                 :crossover-probability 0.8 ; chi = 0.8
                 :mutation-probability 0.1 ; mu = 0.04
                 :exploration-probability 0.35
                 :fitness-fraction-threshold 0.1 ; delta = 0.1
                 ; phi = 0.5, covering multiplier
                 :covering-probability 0.33 ; P_# = 0.33
                 :initial-prediction 10.0 ; p_I = 10.0
                 :initial-prediction-error 0.0 ; epsilon_I = 0.0
                 :initial-fitness 0.01 ; F_I = 0.01
                 :minimum-number-of-actions 2
                 :GA-subsumption? nil
                 :action-set-subsumption? nil
                 :possible-actions
                 (if (= 3 *multislope-categories*)
                   '(:uptrending :steady :downtrending)
                   '(:strong-up :weak-up :steady :weak-down :strong-down))))
(defparameter *multislope-perturbation* 1.0)
(defparameter *multislope-flip* 0.0)
