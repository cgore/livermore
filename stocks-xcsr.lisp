(load "utilities/utilities")
(load "xcs")
(load "xcs-analyzer")
(load "threshold")
(in-package "XCS")
(use-package '("COMMON-LISP" "UTILITIES" "THRESHOLD"))
(export '(stocks-xcsr-analyzer
           current-situation
           random-situation
           get-situation
           correct-action
           get-reward
           end-of-problem?
           terminate?
           start-stocks-xcsr-experiment))
(load "stocks-xcsr-parameters.lisp")

(defclass stocks-xcsr-analyzer (analyzer)
  ((problem-range-lower
     :accessor problem-range-lower
     :initform 0.0
     :initarg :problem-range-lower
     :type float)
   (problem-range-upper
     :accessor problem-range-upper
     :initform 1.0
     :initarg :problem-range-upper
     :type float)
   (current-situation
     :accessor current-situation
     :initarg :current-situation
     :type list)))

(defclass stocks-xcsr-experiment (experiment)
  ())

(defmethod get-situation ((threshold-analyzer threshold-analyzer))
  (incf (number-of-situations threshold-analyzer))
  (setf (current-situation threshold-analyzer)
        (random-situation threshold-analyzer)))

(defmethod correct-action ((threshold-analyzer threshold-analyzer))
  (threshold-indicator (thresholds threshold-analyzer)
                       (current-situation threshold-analyzer)))

(defun start-stocks-xcsr-experiment
  (&key (problem-length 6) (problem-range-lower 0.0) (problem-range-upper 1.0))
  (defparameter *stocks-xcsr-analyzer*
    (make-instance 'stocks-xcsr-analyzer
                   :thresholds
                     (let ((result nil))
                       (dotimes (i problem-length result)
                         (push (random-in-range problem-range-lower
                                                problem-range-upper)
                               result)))))
  (defparameter *stocks-xcsr*
    (make-instance 'xcsr
                   :learning-parameters *stocks-xcsr-learning-parameters*))
  (defparameter *stocks-xcsr-experiment*
    (make-instance 'stocks-xcsr-experiment
                   :environment *stocks-xcsr-analyzer*
                   :reinforcement-program *stocks-xcsr-analyzer*
                   :xcs *stocks-xcsr*))
  (start *stocks-xcsr-experiment*))
