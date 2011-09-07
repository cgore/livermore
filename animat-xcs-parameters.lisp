;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore, all rights reserved.
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Phone: +1 (573) 452-3216
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com

(in-package "XCS")
;; see http://www.boston.quik.com/sw/pd/imp-notes.html for some corrections.
(defparameter *animat-learning-parameters*
  (make-instance 'learning-parameters
                 :maximum-total-numerosity 400 ; N = 400
                 :learning-rate 0.2 ; beta = 0.2
                 :discount-factor 0.71 ; gamma = 0.71
                 :GA-threshold 25 ; theta = 25
                 :equal-error-threshold 10.0 ; epsilon_0 = 10.0
                 :multiplier-parameter 0.1 ; alpha = 0.1
                 :crossover-probability 0.8 ; chi = 0.8
                 :mutation-probability 0.01 ; mu = 0.04
                 :exploration-probability 0.5
                 :fitness-fraction-threshold 0.1 ; delta = 0.1
                 ; phi = 0.5, covering multiplier
                 :covering-probability 0.5 ; P_# = 0.33
                 :initial-prediction 10.0 ; p_I = 10.0
                 :initial-prediction-error 0.0 ; epsilon_I = 0.0
                 :initial-fitness 0.01 ; F_I = 0.01
                 :minimum-number-of-actions 8
                 :possible-actions '(0 1 2 3 4 5 6 7)
                 :GA-subsumption? t 
                 :action-set-subsumption? t))
