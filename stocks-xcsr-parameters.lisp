(in-package "XCS")
(defparameter *threshold-learning-parameters*
  (make-instance 'xcsr-learning-parameters
                 :maximum-total-numerosity 800 ; N = 800
                 :learning-rate 0.2 ; beta = 0.2
                 :discount-factor 0.71 ; gamma = 0.71
                 :GA-threshold 12 ; theta = 25
                 :equal-error-threshold 10.0 ; epsilon_0 = 10.0
                 :multiplier-parameter 0.1 ; alpha = 0.1
                 :crossover-probability 0.8 ; chi = 0.8
                 :mutation-probability 0.04 ; mu = 0.04
                 :exploration-probability 0.5
                 :fitness-fraction-threshold 0.1 ; delta = 0.1
                 ; phi = 0.5, covering multiplier
                 :covering-probability 0.33 ; P_# = 0.33
                 :initial-prediction 10.0 ; p_I = 10.0
                 :initial-prediction-error 0.0 ; epsilon_I = 0.0
                 :initial-fitness 0.01 ; F_I = 0.01
                 :minimum-number-of-actions 2
                 :possible-actions '(nil t)
                 :problem-lower-limit 0.0
                 :problem-upper-limit 1.0
                 :covering-maximum 0.1
                 :mutation-maximum 0.1
                 :GA-subsumption? nil 
                 :action-set-subsumption? t))
