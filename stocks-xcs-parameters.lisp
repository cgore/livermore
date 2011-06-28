(in-package "XCS")
;;; Use this function to reload the parameters.
(defun rlp-stocks ()
  (load "stocks-xcs-parameters.lisp"))
(export 'rlp-stocks)

(defparameter *stock-starting-index* 100)
(defparameter *reward-method* :correctness)
(defparameter *valid-actions* '(:stock :bank :hold))
(defparameter *learning-parameters*
  (make-instance 'learning-parameters
                 :maximum-total-numerosity 1000 ; N
                 :equal-error-threshold 0.01 ; epsilon_0
                 :GA-threshold 50 ; theta_GA
                 :crossover-probability 0.5 ; chi
                 :mutation-probability 0.05 ; mu
                 :minimum-number-of-actions (length *valid-actions*) ; theta_mna
                 :possible-actions *valid-actions*
                 :exploration-probability 0.25))
(defparameter *initial-stock-ticker* "^dji")
(defparameter *stocks-xcs-output* t)
(defparameter *stocks-xcs-report-days* 1)
