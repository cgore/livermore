(load "utilities")
(load "xcsr")
(load "xcs-analyzer")
(load "threshold")
(in-package "XCS")
(use-package '("COMMON-LISP" "UTILITIES" "THRESHOLD"))
(export '(threshold-analyzer
           current-situation
           random-situation
           get-situation
           correct-action
           get-reward
           end-of-problem?
           terminate?
           start-threshold-experiment))
(load "threshold-xcsr-parameters.lisp")

(defclass threshold-analyzer (analyzer)
   ((thresholds
     :accessor thresholds
     :initform nil
     :initarg :thresholds
     :type list)
   (problem-range-lower
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

(defclass threshold-experiment (experiment)
  ())

(defmethod problem-length ((threshold-analyzer threshold-analyzer))
  (length (thresholds threshold-analyzer)))

(defmethod random-situation ((threshold-analyzer threshold-analyzer))
  (let ((result nil))
    (dotimes (i (problem-length threshold-analyzer) result)
      (push (random-in-range (problem-range-lower threshold-analyzer)
                             (problem-range-upper threshold-analyzer))
            result))))

(defmethod get-situation ((threshold-analyzer threshold-analyzer))
  (incf (number-of-situations threshold-analyzer))
  (setf (current-situation threshold-analyzer)
        (random-situation threshold-analyzer)))

(defmethod correct-action ((threshold-analyzer threshold-analyzer))
  (threshold-indicator (thresholds threshold-analyzer)
                       (current-situation threshold-analyzer)))

(defun start-threshold-experiment
  (&key (problem-length 6) (problem-range-lower 0.0) (problem-range-upper 1.0))
  (defparameter *threshold-analyzer*
    (make-instance 'threshold-analyzer
                   :thresholds
                     (let ((result nil))
                       (dotimes (i problem-length result)
                         (push (random-in-range problem-range-lower
                                                problem-range-upper)
                               result)))))
  (defparameter *threshold-xcsr*
    (make-instance 'xcsr
                   :learning-parameters *threshold-learning-parameters*))
  (defparameter *threshold-experiment*
    (make-instance 'threshold-experiment
                   :environment *threshold-analyzer*
                   :reinforcement-program *threshold-analyzer*
                   :xcs *threshold-xcsr*))
  (start *threshold-experiment*))
