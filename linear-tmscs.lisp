(load "utilities/utilities")
(load "statistics")
(load "tmscs")
(in-package "XCS")
(use-package "STATISTICS")
(export '(linear-tmscs-analyzer
           history
           action-history
           initial-history-depth
           linear-tmscs-experiment
           get-situation
           classify
           correct-action
           end-of-problem?
           terminate?
           start-linear-tmscs-experiment
           *linear-tmscs-analyzer*
           *linear-tmscs-learning-parameters*
           *linear-tmscs*
           *linear-tmscs-experiment*))

(defclass linear-tmscs-analyzer (environment reinforcement-program)
  ((history
     :accessor history
     :initform nil
     :initarg :history
     :type list)
   (number-of-situations
     :accessor number-of-situations
     :initform 0
     :initarg :number-of-situations
     :type natural-number)
   (current-action
     :accessor current-action
     :initarg :current-action)
   (number-of-actions
     :accessor number-of-actions
     :initform 0
     :initarg :number-of-actions
     :type natural-number)
   (number-of-correct-actions
     :accessor number-of-correct-actions
     :initform 0
     :initarg :number-of-correct-actions
     :type natural-number)
   (initial-history-depth
     :accessor initial-history-depth
     :initform 200
     :initarg :initial-history-depth
     :type natural-number)))

(defclass linear-tmscs-experiment (experiment)
  ())

(defun situation-function (time-step)
  (sin (* time-step pi 1/25)))

(defmethod get-situation ((analyzer linear-tmscs-analyzer))
  (with-slots (number-of-situations history initial-history-depth) analyzer
    (push (situation-function (incf number-of-situations)) history)))

(defmethod classify ((analyzer linear-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (< (situation-function     number-of-situations)
       (situation-function (1+ number-of-situations)))))

(defmethod correct-action ((linear-tmscs-analyzer linear-tmscs-analyzer))
  (classify linear-tmscs-analyzer))

(defmethod correct-action? ((linear-tmscs-analyzer linear-tmscs-analyzer))
  (equalp (current-action linear-tmscs-analyzer)
          (correct-action linear-tmscs-analyzer)))

(defmethod get-reward ((linear-tmscs-analyzer linear-tmscs-analyzer))
  (if (correct-action? linear-tmscs-analyzer) 1000.0 0.0))

(defmethod end-of-problem? ((linear-tmscs-analyzer linear-tmscs-analyzer))
  ;; Does this really make any sense in TMSCS?
  t)

(defmethod terminate? ((linear-tmscs-analyzer linear-tmscs-analyzer))
  (<= 10000 (number-of-actions linear-tmscs-analyzer)))

(defmethod initialize ((analyzer linear-tmscs-analyzer))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod execute-action ((linear-tmscs-analyzer linear-tmscs-analyzer) action)
  (with-slots (current-action
                history
                number-of-actions
                starting-index
                ending-index
                number-of-correct-actions) linear-tmscs-analyzer
    (setf current-action action)
    (incf number-of-actions)
    (when (correct-action? linear-tmscs-analyzer)
      (incf number-of-correct-actions))
    (format t "~&[~3A] ~3A: ~D/~D = ~,3F%, ~A choice (~2D ~,5F).~%"
            (correct-action linear-tmscs-analyzer)
            action
            number-of-correct-actions
            number-of-actions
            (* 100.0 (/ number-of-correct-actions number-of-actions))
            (if (correct-action? linear-tmscs-analyzer)
              "  correct"
              "incorrect")
            (mod number-of-actions 25)
            (first history))))

(defun simple-slope (sequence &key (key #'identity) (start 0) (end nil))
  (assert (sequence? sequence))
  (when (null end)
    (setf end (1- (length sequence))))
  (/ (- (funcall key (elt sequence start))
        (funcall key (elt sequence end)))
     (- start end)))

(defun start-linear-tmscs-experiment ()
  (defparameter *linear-tmscs-analyzer*
    (make-instance 'linear-tmscs-analyzer))
  (defparameter *linear-tmscs-learning-parameters*
    (make-instance 'tmscs-learning-parameters
                   :valid-operations (list #'simple-slope)
                   :minimum-number-of-actions 2
                   :possible-actions '(nil t)
                   :learning-rate 0.3
                   :equal-error-threshold 100
                   :discount-factor 0.1
                   :mutation-probability 0.1
                   :exploration-probability 0.1))
  (defparameter *linear-tmscs*
    (make-instance 'tmscs
                   :learning-parameters *linear-tmscs-learning-parameters*))
  (defparameter *linear-tmscs-experiment*
    (make-instance 'linear-tmscs-experiment
                   :environment *linear-tmscs-analyzer*
                   :reinforcement-program *linear-tmscs-analyzer*
                   :xcs *linear-tmscs*))
  (initialize *linear-tmscs-analyzer*)
  (start *linear-tmscs-experiment*))
