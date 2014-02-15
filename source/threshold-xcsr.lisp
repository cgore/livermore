;;;; Copyright (c) 2005 -- 2014, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
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
