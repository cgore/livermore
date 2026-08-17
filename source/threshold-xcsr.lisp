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

(load "utilities/utilities")
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
     :type list
     :documentation "The threshold vector the agent must meet or exceed.")
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
     :type list
     :documentation "The last random point presented as a situation."))
  (:documentation
   "An environment whose correct action is whether a random point meets a
   fixed threshold vector."))

(defclass threshold-experiment (experiment)
  ()
  (:documentation "An XCSR experiment that uses a threshold-analyzer."))

(defmethod problem-length ((threshold-analyzer threshold-analyzer))
  "This is the number of coordinates in the threshold."
  (length (thresholds threshold-analyzer)))

(defmethod random-situation ((threshold-analyzer threshold-analyzer))
  "This returns a random point in the problem range."
  (let ((result nil))
    (dotimes (i (problem-length threshold-analyzer) result)
      (push (random-in-range (problem-range-lower threshold-analyzer)
                             (problem-range-upper threshold-analyzer))
            result))))

(defmethod get-situation ((threshold-analyzer threshold-analyzer))
  "This stores and returns a new random situation."
  (incf (number-of-situations threshold-analyzer))
  (setf (current-situation threshold-analyzer)
        (random-situation threshold-analyzer)))

(defmethod correct-action ((threshold-analyzer threshold-analyzer))
  "This is true when the current situation meets the threshold."
  (threshold-indicator (thresholds threshold-analyzer)
                       (current-situation threshold-analyzer)))

(defun start-threshold-experiment
  (&key (problem-length 6) (problem-range-lower 0.0) (problem-range-upper 1.0))
  "This builds a threshold XCSR experiment of PROBLEM-LENGTH and starts it."
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
