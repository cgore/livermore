(load "utilities")
(load "statistics")
(load "tmscs")
(in-package "XCS")
(use-package "STATISTICS")
(export '(sawtooth-tmscs-analyzer
           history
           action-history
           initial-history-depth
           sawtooth-tmscs-experiment
           get-situation
           classify
           correct-action
           end-of-problem?
           terminate?
           start-sawtooth-tmscs-experiment
           *sawtooth-tmscs-analyzer*
           *sawtooth-tmscs-learning-parameters*
           *sawtooth-tmscs*
           *sawtooth-tmscs-experiment*))

(defclass sawtooth-tmscs-analyzer (environment reinforcement-program)
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
     :initform 200
     :initarg :initial-history-depth
     :type (integer 0 *))))

(defclass sawtooth-tmscs-experiment (experiment)
  ())

(defmethod single-step-output ((experiment sawtooth-tmscs-experiment))
  (with-slots (environment reinforcement-program xcs) experiment
    (with-slots (current-action
                  history
                  number-of-correct-actions
                  number-of-actions) environment
      (with-slots (population match-set action-set) xcs
        (format t "~&[~3A] ~3A: ~D/~D = ~,3F%, ~A choice (~2D ~,5F).~%"
                (correct-action environment)
                current-action
                number-of-correct-actions
                number-of-actions
                (* 100.0 (/ number-of-correct-actions number-of-actions))
                (if (correct-action? environment)
                  "  correct"
                  "incorrect")
                (mod number-of-actions 25)
                (first history))
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

(defun situation-function (time-step)
  (sin (* time-step pi 1/25)))

(defmethod get-situation ((analyzer sawtooth-tmscs-analyzer))
  (with-slots (number-of-situations history initial-history-depth) analyzer
    (push (situation-function (incf number-of-situations)) history)))

(defmethod classify ((analyzer sawtooth-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (< (situation-function     number-of-situations)
       (situation-function (1+ number-of-situations)))))

(defmethod correct-action ((sawtooth-tmscs-analyzer sawtooth-tmscs-analyzer))
  (classify sawtooth-tmscs-analyzer))

(defmethod correct-action? ((sawtooth-tmscs-analyzer sawtooth-tmscs-analyzer))
  (equalp (current-action sawtooth-tmscs-analyzer)
          (correct-action sawtooth-tmscs-analyzer)))

(defmethod get-reward ((sawtooth-tmscs-analyzer sawtooth-tmscs-analyzer))
  (if (correct-action? sawtooth-tmscs-analyzer)
    1000.0
    0.0))

(defmethod end-of-problem? ((sawtooth-tmscs-analyzer sawtooth-tmscs-analyzer))
  ;; Does this really make any sense in TMSCS?
  t)

(defmethod terminate? ((sawtooth-tmscs-analyzer sawtooth-tmscs-analyzer))
  (<= 10000 (number-of-actions sawtooth-tmscs-analyzer)))

(defmethod initialize ((analyzer sawtooth-tmscs-analyzer))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod execute-action ((sawtooth-tmscs-analyzer sawtooth-tmscs-analyzer) action)
  (with-slots (current-action
                number-of-actions
                number-of-correct-actions) sawtooth-tmscs-analyzer
    (setf current-action action)
    (incf number-of-actions)
    (when (correct-action? sawtooth-tmscs-analyzer)
      (incf number-of-correct-actions))))

(defun simple-slope (sequence &key (key #'identity) (start 0) (end nil))
  (assert (sequence? sequence))
  (when (null end)
    (setf end (1- (length sequence))))
  (assert (not (= start end)))
  (/ (- (funcall key (elt sequence start))
        (funcall key (elt sequence end)))
     (- start end)))

(defclass sawtooth-tmscs-predicate (tms-predicate) ())
(defmethod print-object ((predicate sawtooth-tmscs-predicate) stream)
  (format stream "~A -- ~A [~A,~A]"
          (initial predicate) (final predicate)
          (lower predicate) (upper predicate)))

(defclass sawtooth-tmscs-classifier (tms-classifier) ())

(load "sawtooth-tmscs-parameters")

(defun start-sawtooth-tmscs-experiment ()
  (defparameter *sawtooth-tmscs-analyzer*
    (make-instance 'sawtooth-tmscs-analyzer))
  (defparameter *sawtooth-tmscs*
    (make-instance 'tmscs
                   :predicate-type 'sawtooth-tmscs-predicate
                   :classifier-type 'sawtooth-tmscs-classifier
                   :learning-parameters *sawtooth-tmscs-learning-parameters*))
  (defparameter *sawtooth-tmscs-experiment*
    (make-instance 'sawtooth-tmscs-experiment
                   :environment *sawtooth-tmscs-analyzer*
                   :reinforcement-program *sawtooth-tmscs-analyzer*
                   :xcs *sawtooth-tmscs*))
  (initialize *sawtooth-tmscs-analyzer*)
  (start *sawtooth-tmscs-experiment*))
