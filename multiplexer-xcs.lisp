;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore,
;;;; All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com

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
