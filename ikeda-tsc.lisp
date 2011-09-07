;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore, all rights reserved.
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Phone: +1 (573) 452-3216
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com

(load "utilities/utilities")
(load "statistics")
(load "tmscs")
(in-package "XCS")
(use-package "STATISTICS")
(export '(ikeda-tmscs-analyzer
           history
           action-history
           initial-history-depth
           ikeda-tmscs-experiment
           get-situation
           classify
           correct-action
           end-of-problem?
           terminate?
           start-ikeda-tmscs-experiment
           *ikeda-tmscs-analyzer*
           *ikeda-tmscs-learning-parameters*
           *ikeda-tmscs*
           *ikeda-tmscs-experiment*))
(defstruct ihs heading steps multiplier value)
(defparameter *ikeda-hist* nil)
(defparameter *ikeda-action-history* nil)

(defclass ikeda-tmscs-analyzer (environment reinforcement-program)
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

(defclass ikeda-tmscs-experiment (experiment)
  ())

(defmethod single-step-output ((experiment ikeda-tmscs-experiment))
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
                (first *ikeda-hist*))
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
                     ((null *ikeda-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *ikeda-hist*)))
                      (let ((f (first *ikeda-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (perturb *ikeda-perturbation*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *ikeda-hist*))))
                      (let* ((f (first *ikeda-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (perturb *ikeda-perturbation*)
                                               h (/ m 5)))))))
                   *ikeda-hist*)))
    (while (<= (length *ikeda-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *ikeda-hist*))))

(defun situation-function (time-step)
  (labels ((flip? (probability)
                  (if (probability? probability) -1 1))
           (new-situation ()
             (push (cond
                     ;; A completely empty history.
                     ((null *ikeda-hist*)
                      (make-ihs :heading (random-element '(-1 1))
                                :steps (random-in-range 50 100)
                                :multiplier (random-in-range 1 10)
                                :value (random-in-range -10.0 10.0)))
                     ;; Continue the previous trend line.
                     ((plusp (ihs-steps (first *ikeda-hist*)))
                      (let ((f (first *ikeda-hist*)))
                        (make-ihs :heading (ihs-heading f)
                                  :steps (1- (ihs-steps f))
                                  :multiplier (ihs-multiplier f)
                                  :value (+ (ihs-value f)
                                            (* (flip? *ikeda-flip*)
                                               (ihs-heading f)
                                               (/ (ihs-multiplier f) 5))))))
                     ;; A new trend line.
                     ((not (plusp (ihs-steps (first *ikeda-hist*))))
                      (let* ((f (first *ikeda-hist*))
                             (h (- (ihs-heading f)))
                             (m (random-in-range 1 10)))
                        (make-ihs :heading h
                                  :steps (random-in-range 50 100)
                                  :multiplier m
                                  :value (+ (ihs-value f)
                                            (* (flip? *ikeda-flip*)
                                               h (/ m 5)))))))
                   *ikeda-hist*)))
    (while (<= (length *ikeda-hist*) time-step)
           (new-situation))
    (ihs-value (nth-from-end time-step *ikeda-hist*))))

(defun ikeda-function (initial length)
  (assert (numberp initial))
  (assert (typep length '(integer 0 *)))
  (let ((data (list initial))) ; This data is stored in reverse order.
    (lambda (n)
      (while (< (1+ (length data)) n)
        (push (make-array 
                (1+ (* 0.9 (car data)
                       (exp (- (* 

(defun ikeda (n z0)
  (assert (numberp n))
  (assert (numberp z0))
  (if (= n 0)

(defmethod get-situation ((analyzer ikeda-tmscs-analyzer))
  (with-slots (number-of-situations history initial-history-depth) analyzer
    (push (situation-function (incf number-of-situations)) history)))

(defmethod classify ((analyzer ikeda-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (if (< (situation-function     number-of-situations)
           (situation-function (1+ number-of-situations)))
      :uptrending
      :downtrending)))

(defmethod correct-action ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  (classify ikeda-tmscs-analyzer))

(defmethod correct-action? ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  (equalp (current-action ikeda-tmscs-analyzer)
          (correct-action ikeda-tmscs-analyzer)))

(defmethod get-reward ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  (if (correct-action? ikeda-tmscs-analyzer)
    1000.0
    0.0))

(defmethod end-of-problem? ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  ;; Does this really make any sense in TMSCS?
  t)

(defmethod terminate? ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer))
  (<= 10000 (number-of-actions ikeda-tmscs-analyzer)))

(defmethod initialize ((analyzer ikeda-tmscs-analyzer))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod execute-action ((ikeda-tmscs-analyzer ikeda-tmscs-analyzer) action)
  (with-slots (current-action
                number-of-actions
                number-of-correct-actions) ikeda-tmscs-analyzer
    (setf current-action action)
    (incf number-of-actions)
    (when (correct-action? ikeda-tmscs-analyzer)
      (incf number-of-correct-actions))
    (push (list current-action
                (correct-action? ikeda-tmscs-analyzer)
                number-of-correct-actions
                number-of-actions)
          *ikeda-action-history*)))

(defun simple-slope (list &key (key #'identity) (start 0) (end nil))
  (assert (listp list))
  (when (null end)
    (setf end (1- (length list))))
  (assert (not (= start end)))
  (/ (- (funcall key (nth start list))
        (funcall key (nth end list)))
     (- start end)))

(defclass ikeda-tmscs-predicate (tms-predicate) ())
(defmethod print-object ((predicate ikeda-tmscs-predicate) stream)
  (format stream "~A -- ~A [~A,~A]"
          (initial predicate) (final predicate)
          (lower predicate) (upper predicate)))

(defclass ikeda-tmscs-classifier (tms-classifier) ())

(load "ikeda-tmscs-parameters")

(defun start-ikeda-tmscs-experiment ()
  (defparameter *ikeda-hist* nil)
  (defparameter *ikeda-action-history* nil)
  (defparameter *ikeda-tmscs-analyzer*
    (make-instance 'ikeda-tmscs-analyzer))
  (defparameter *ikeda-tmscs*
    (make-instance 'tmscs
                   :predicate-type 'ikeda-tmscs-predicate
                   :classifier-type 'ikeda-tmscs-classifier
                   :learning-parameters *ikeda-tmscs-learning-parameters*))
  (defparameter *ikeda-tmscs-experiment*
    (make-instance 'ikeda-tmscs-experiment
                   :environment *ikeda-tmscs-analyzer*
                   :reinforcement-program *ikeda-tmscs-analyzer*
                   :xcs *ikeda-tmscs*))
  (initialize *ikeda-tmscs-analyzer*)
  (start *ikeda-tmscs-experiment*))
