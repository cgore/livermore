;;;; Copyright (c) 2005 -- 2026, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(defpackage :livermore/linear-tmscs
  (:use :common-lisp
        :livermore/linear-tmscs-parameters
        :livermore/statistics
        :livermore/tmscs
        :livermore/xcs
        :sigma/behave
        :sigma/control
        :sigma/numeric)
  (:export :*linear-tmscs*
           :*linear-tmscs-analyzer*
           :*linear-tmscs-experiment*
           :classify
           :correct-action
           :end-of-problem?
           :get-situation
           :history
           :initial-history-depth
           :linear-tmscs-analyzer
           :linear-tmscs-experiment
           :start-linear-tmscs-experiment
           :terminate?))
(in-package :livermore/linear-tmscs)

(defparameter *linear-tmscs-analyzer* nil)
(defparameter *linear-tmscs* nil)
(defparameter *linear-tmscs-experiment* nil)

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
     :initform 50
     :initarg :initial-history-depth
     :type (integer 0 *)
     :documentation "How many history points to generate before learning."))
  (:documentation
   "An environment that presents a sine wave and asks whether the next
   point is up or down."))

(defclass linear-tmscs-experiment (experiment)
  ()
  (:documentation "A TMSCS experiment that uses a linear-tmscs-analyzer."))

(defun situation-function (time-step)
  "This is a sine wave with period 50."
  (sin (* time-step pi 1/25)))

(defmethod get-situation ((analyzer linear-tmscs-analyzer))
  "This pushes the next sine value onto the history and returns the series."
  (with-slots (number-of-situations history) analyzer
    (push (situation-function (incf number-of-situations)) history)
    history))

(defmethod classify ((analyzer linear-tmscs-analyzer))
  "We classify the next point as either up or down from our current point."
  (with-slots (number-of-situations) analyzer
    (< (situation-function     number-of-situations)
       (situation-function (1+ number-of-situations)))))

(defmethod correct-action ((linear-tmscs-analyzer linear-tmscs-analyzer))
  "This is the up-or-down class of the next point."
  (classify linear-tmscs-analyzer))

(defmethod correct-action? ((linear-tmscs-analyzer linear-tmscs-analyzer))
  "This predicate is true when the last action matches the next-point class."
  (equalp (current-action linear-tmscs-analyzer)
          (correct-action linear-tmscs-analyzer)))

(defmethod get-reward ((linear-tmscs-analyzer linear-tmscs-analyzer))
  "This returns 1000.0 for a correct action and 0.0 otherwise."
  (if (correct-action? linear-tmscs-analyzer) 1000.0 0.0))

(defmethod end-of-problem? ((linear-tmscs-analyzer linear-tmscs-analyzer))
  "This predicate is always true.  Each trial is a single step."
  ;; Does this really make any sense in TMSCS?
  t)

(defmethod terminate? ((experiment linear-tmscs-experiment))
  "This predicate is true after NUMBER-OF-TRIALS actions."
  (>= (number-of-actions (environment experiment))
      (number-of-trials experiment)))

(defmethod initialize-instance :after ((analyzer linear-tmscs-analyzer) &rest initargs &key &allow-other-keys)
  "This fills HISTORY to INITIAL-HISTORY-DEPTH."
  (declare (ignore initargs))
  (with-slots (history initial-history-depth) analyzer
    (while (< (length history) initial-history-depth)
      (get-situation analyzer))))

(defmethod execute-action ((linear-tmscs-analyzer linear-tmscs-analyzer) action)
  "This records ACTION, updates the correctness counts, and prints a short
  running report."
  (with-slots (current-action
                history
                number-of-actions
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

(defun start-linear-tmscs-experiment
    (&optional (number-of-trials 10000) (run t))
  "This builds a linear TMSCS experiment and starts it."
  (setf *linear-tmscs-analyzer*
        (make-instance 'linear-tmscs-analyzer))
  (setf *linear-tmscs*
        (make-instance 'tmscs
                       :learning-parameters *linear-tmscs-learning-parameters*))
  (setf *linear-tmscs-experiment*
        (make-instance 'linear-tmscs-experiment
                       :environment *linear-tmscs-analyzer*
                       :reinforcement-program *linear-tmscs-analyzer*
                       :xcs *linear-tmscs*
                       :number-of-trials number-of-trials))
  (if run
    (start *linear-tmscs-experiment*)
    *linear-tmscs-experiment*))

(behavior 'linear-situation-function
  (should= 0 (situation-function 0))
  (should-be-true (> (situation-function 6) 0))
  (should-be-true (< (situation-function 30) 0)))

(behavior 'linear-tmscs-analyzer
  (let ((a (make-instance 'linear-tmscs-analyzer :initial-history-depth 10)))
    (should= 10 (length (history a)))
    (should-be-true (end-of-problem? a))
    (should-be-true (member (classify a) '(t nil)))))

(behavior 'linear-tmscs-experiment
  (let ((*standard-output* (make-broadcast-stream)))
    (start-linear-tmscs-experiment 6 t))
  (should= 6 (number-of-actions *linear-tmscs-analyzer*))
  (should-be-true (plusp (length (population *linear-tmscs*)))))
