(load "utilities")
(load "statistics")
(load "tmscs")
(in-package "XCS")
(use-package "STATISTICS")
(export '(multislope-tmscs-analyzer
           history
           action-history
           initial-history-depth
           multislope-tmscs-experiment
           get-situation
           classify
           correct-action
           end-of-problem?
           terminate?
           start-multislope-tmscs-experiment
           *multislope-tmscs-analyzer*
           *multislope-tmscs-learning-parameters*
           *multislope-tmscs*
           *multislope-tmscs-experiment*))
(defstruct ihs heading steps multiplier value)
(defparameter *multislope-hist* nil)
(defparameter *multislope-action-history* nil)

(defclass multislope-tmscs-analyzer (environment reinforcement-program)
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

(defclass multislope-tmscs-experiment (experiment)
  ())

(defmethod single-step-output ((experiment multislope-tmscs-experiment))
  (with-slots (environment reinforcement-program xcs) experiment
    (with-slots (current-action
                  history
                  number-of-correct-actions
                  number-of-actions) environment
      (with-slots (population match-set action-set) xcs
        (format t "~&[~12A] ~12A: ~D/~D = ~,3F%, ~A choice ~D.~%"
                (correct-action environment)
                current-action
                number-of-correct-actions
                number-of-actions
                (* 100.0 (/ number-of-correct-actions number-of-actions))
                (if (correct-action? environment)
                  "  correct"
                  "incorrect")
                (first *multislope-hist*))
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

(defun situation-function[3] (time-step)
  (labels ((perturb (percentage)
                    (random-in-range (1+ (- percentage)) (1+ percentage)))
           (new-situation ()
             (push (cond
                     ;; A completely empty history.
                     ((null *multislope-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *multislope-hist*)))
                      (let ((f (first *multislope-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (perturb *multislope-perturbation*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *multislope-hist*))))
                      (let* ((f (first *multislope-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (perturb *multislope-perturbation*)
                                               h (/ m 5)))))))
                   *multislope-hist*)))
    (while (<= (length *multislope-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *multislope-hist*))))

(defun situation-function (time-step)
  (labels ((flip? (probability)
                  (if (probability? probability) -1 1))
           (new-situation ()
             (push (cond
                     ;; A completely empty history.
                     ((null *multislope-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *multislope-hist*)))
                      (let ((f (first *multislope-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (flip? *multislope-flip*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *multislope-hist*))))
                      (let* ((f (first *multislope-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (flip? *multislope-flip*)
                                               h (/ m 5)))))))
                   *multislope-hist*)))
    (while (<= (length *multislope-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *multislope-hist*))))

(defmethod get-situation ((analyzer multislope-tmscs-analyzer))
  (with-slots (number-of-situations history initial-history-depth) analyzer
    (push (situation-function (incf number-of-situations)) history)))

(defmethod classify-3 ((analyzer multislope-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (let ((x   (situation-function     number-of-situations))
          (x+1 (situation-function (1+ number-of-situations))))
      (cond ((< x (* 1.50 x+1)) :uptrending)
            ((> x (* 0.50 x+1)) :downtrending)
            (t :steady)))))

(defmethod classify-5 ((analyzer multislope-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (let* ((x   (situation-function     number-of-situations))
           (x+1 (situation-function (1+ number-of-situations)))
           (slope (- x+1 x)))
      (cond ((<=    slope -3) :strong-down)
            ((<= -3 slope -1) :weak-down)
            ((<= -1 slope  1) :steady)
            ((<=  1 slope  3) :weak-up)
            ((<=  3 slope)    :strong-up)))))

(defmethod classify ((analyzer multislope-tmscs-analyzer))
  (if (= 3 *multislope-categories*)     
    (classify-3 analyzer)
    (classify-5 analyzer)))

(defmethod correct-action ((multislope-tmscs-analyzer multislope-tmscs-analyzer))
  (classify multislope-tmscs-analyzer))

(defmethod correct-action? ((multislope-tmscs-analyzer multislope-tmscs-analyzer))
  (equalp (current-action multislope-tmscs-analyzer)
          (correct-action multislope-tmscs-analyzer)))

(defmethod get-reward ((multislope-tmscs-analyzer multislope-tmscs-analyzer))
  (if (correct-action? multislope-tmscs-analyzer)
    1000.0
    0.0))

(defmethod end-of-problem? ((multislope-tmscs-analyzer multislope-tmscs-analyzer))
  ;; Does this really make any sense in TMSCS?
  t)

(defmethod terminate? ((multislope-tmscs-analyzer multislope-tmscs-analyzer))
  (<= 10000 (number-of-actions multislope-tmscs-analyzer)))

(defmethod initialize ((analyzer multislope-tmscs-analyzer))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod execute-action ((multislope-tmscs-analyzer multislope-tmscs-analyzer) action)
  (with-slots (current-action
                number-of-actions
                number-of-correct-actions) multislope-tmscs-analyzer
    (setf current-action action)
    (incf number-of-actions)
    (when (correct-action? multislope-tmscs-analyzer)
      (incf number-of-correct-actions))
    (push (list current-action
                (correct-action? multislope-tmscs-analyzer)
                number-of-correct-actions
                number-of-actions)
          *multislope-action-history*)))

(defun simple-slope (list &key (key #'identity) (start 0) (end nil))
  (assert (listp list))
  (when (null end)
    (setf end (1- (length list))))
  (assert (not (= start end)))
  (/ (- (funcall key (nth start list))
        (funcall key (nth end list)))
     (- start end)))

(defclass multislope-tmscs-predicate (tms-predicate) ())
(defmethod print-object ((predicate multislope-tmscs-predicate) stream)
  (format stream "~A -- ~A [~A,~A]"
          (initial predicate) (final predicate)
          (lower predicate) (upper predicate)))

(defclass multislope-tmscs-classifier (tms-classifier) ())

(load "multislope-tmscs-parameters")

(defun start-multislope-tmscs-experiment ()
  (defparameter *multislope-hist* nil)
  (defparameter *multislope-action-history* nil)
  (defparameter *multislope-tmscs-analyzer*
    (make-instance 'multislope-tmscs-analyzer))
  (defparameter *multislope-tmscs*
    (make-instance 'tmscs
                   :predicate-type 'multislope-tmscs-predicate
                   :classifier-type 'multislope-tmscs-classifier
                   :learning-parameters *multislope-tmscs-learning-parameters*))
  (defparameter *multislope-tmscs-experiment*
    (make-instance 'multislope-tmscs-experiment
                   :environment *multislope-tmscs-analyzer*
                   :reinforcement-program *multislope-tmscs-analyzer*
                   :xcs *multislope-tmscs*))
  (initialize *multislope-tmscs-analyzer*)
  (start *multislope-tmscs-experiment*))
