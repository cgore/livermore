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
(load "xcs")
(load "xcs-analyzer")
(load "multiplexer")
(in-package "XCS")
(use-package '("COMMON-LISP" "UTILITIES" "MULTIPLEXER"))
(export '(multiplexer-analyzer
           current-situation
           address-width
           multiplexer-experiment
           random-situation
           get-situation
           get-reward
           end-of-problem?
           terminate?
           start-multiplexer-experiment))
(load "multiplexer-xcs-parameters.lisp")

(defclass multiplexer-analyzer (analyzer)
  ((current-situation
     :accessor current-situation
     :initarg :current-situation
     :type bit-vector)
   (address-width
     :accessor address-width
     :initform 2
     :initarg :address-width
     :type (integer 0 *))))

(defclass multiplexer-experiment (experiment)
  ())

(defmethod random-situation ((multiplexer-analyzer multiplexer-analyzer))
  (random-bit-vector
    (multiplexer-length
      (address-width multiplexer-analyzer))))

(defmethod get-situation ((multiplexer-analyzer multiplexer-analyzer))
  (incf (number-of-situations multiplexer-analyzer))
  (truth-vector (setf (current-situation multiplexer-analyzer)
                      (random-situation multiplexer-analyzer))))

(defmethod correct-action ((multiplexer-analyzer multiplexer-analyzer))
  (multiplexer (address-width multiplexer-analyzer)
               (current-situation multiplexer-analyzer)))

(defmethod get-reward ((multiplexer-analyzer multiplexer-analyzer))
  "This really weird reward scheme is used in Wilson's 1995 paper on XCS in
  order to demonstrate the power of niching.  Since the evolution is only
  applied to the match set [M] in his paper (only the action set [A] for us)
  this stil works."
  (with-slots (current-situation address-width) multiplexer-analyzer
    (+ (* 200 (binary-decoder (subseq current-situation 0 address-width)))
       (if (= 1 (correct-action multiplexer-analyzer)) 100 0)
       (if (correct-action? multiplexer-analyzer) 300 0))))

(defmethod end-of-problem? ((multiplexer-analyzer multiplexer-analyzer))
  t)

(defmethod terminate? ((multiplexer-experiment multiplexer-experiment))
  (<= 10000 (actions (environment multiplexer-experiment))))

(defun start-multiplexer-experiment (&optional (address-width 2))
  (defparameter *multiplexer-analyzer*
    (make-instance 'multiplexer-analyzer
                   :address-width address-width))
  (defparameter *multiplexer-xcs*
    (make-instance 'xcs
                   :learning-parameters *multiplexer-learning-parameters*))
  (defparameter *multiplexer-experiment*
    (make-instance 'multiplexer-experiment
                   :environment *multiplexer-analyzer*
                   :reinforcement-program *multiplexer-analyzer*
                   :xcs *multiplexer-xcs*))
  (start *multiplexer-experiment*))
