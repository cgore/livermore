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

(defpackage :livermore/monk-xcs
  (:use :common-lisp
        :livermore/monk-xcs-parameters
        :livermore/xcs
        :livermore/xcs-set-predicate
        :livermore/xcs-ternary-predicate
        :sigma/behave
        :sigma/probability
        :sigma/random)
  (:export :*monk-analyzer*
           :*monk-experiment*
           :*monk-xcs*
           :actions
           :correct-action
           :correct-action?
           :correct-actions
           :current-action
           :current-situation
           :end-of-problem?
           :execute-action
           :get-reward
           :get-situation
           :monk-1?
           :monk-2?
           :monk-3-no-noise?
           :monk-3?
           :monk-analyzer
           :monk-analyzer-ternary
           :monk-attributes?
           :monk-problem
           :monk?
           :random-monk-attributes
           :random-situation
           :start-monk))
(in-package :livermore/monk-xcs)

(defparameter *monk-analyzer* nil)
(defparameter *monk-experiment* nil)
(defparameter *monk-xcs* nil)

(defun monk-attributes? (attributes)
  "This predicate is true when ATTRIBUTES is a six-element Monk feature
  vector with the legal ranges (3, 3, 2, 3, 4, 2)."
  (and (listp attributes)
       (= 6 (length attributes))
       (member (first  attributes) '(1 2 3))
       (member (second attributes) '(1 2 3))
       (member (third  attributes) '(1 2))
       (member (fourth attributes) '(1 2 3))
       (member (fifth  attributes) '(1 2 3 4))
       (member (sixth  attributes) '(1 2))))

(defun monk-attributes-to-truth-values (attributes)
  "This basically translates the monk attribute set into a binary encoding."
  (assert (monk-attributes? attributes))
  (flet ((to-bits (attribute bits)
           "WARNING: This function is a cheap hack."
           (if (= bits 1)
             (cond ((= attribute 1) '(nil))
                   ((= attribute 2) '(t)))
             (cond ((= attribute 1) '(nil nil))
                   ((= attribute 2) '(nil t))
                   ((= attribute 3) '(t nil))
                   ((= attribute 4) '(t t))))))
    (loop with result = nil
          for attribute in attributes
          and bits in '(2 2 1 2 2 1)
          do (setf result (append result (to-bits attribute bits)))
          finally (return result))))

(defun random-monk-attributes ()
  "This returns a random legal six-element Monk feature vector."
  (list (random-element '(1 2 3))
        (random-element '(1 2 3))
        (random-element '(1 2))
        (random-element '(1 2 3))
        (random-element '(1 2 3 4))
        (random-element '(1 2))))

(defun monk-1? (attributes)
  "This is basically the Monk's first problem."
  (assert (monk-attributes? attributes))
  (or (= (first attributes) (second attributes))
      (= 1 (fifth attributes))))

(defun monk-2? (attributes)
  "This is basically the Monk's second problem."
  (assert (monk-attributes? attributes))
  (= 2 (count 1 attributes)))

(defun monk-3-no-noise? (attributes)
  "This is basically the Monk's third problem, just without any noise added."
  (assert (monk-attributes? attributes))
  (or (and (= 3 (fifth  attributes))
           (= 1 (fourth attributes)))
      (and (not (= 4 (fifth  attributes)))
           (not (= 3 (second attributes))))))

(defun monk-3? (attributes)
  "This is the Monk's third problem, with 5% class noise."
  (if (probability? 0.05)
    (not (monk-3-no-noise? attributes))
    (monk-3-no-noise? attributes)))

(defclass monk-analyzer (environment reinforcement-program)
  ((current-situation
     :accessor current-situation
     :initarg :current-situation)
   (current-action
     :accessor current-action
     :initarg :current-action)
   (actions
     :accessor actions
     :initform 0
     :initarg :actions
     :type integer)
   (correct-actions
     :accessor correct-actions
     :initform 0
     :initarg :correct-actions
     :type integer)
   (monk-problem
     :accessor monk-problem
     :initform #'monk-1?
     :initarg :monk-problem
     :documentation "The Monk target function, such as #'MONK-1?."))
  (:documentation
   "An environment for the Monk's problems.  Situations are six-element
   attribute lists."))

(defclass monk-analyzer-ternary (monk-analyzer)
  ((current-attributes
    :accessor current-attributes
    :initarg :current-attributes
    :documentation "The six-element Monk vector before ternary encoding."))
  (:documentation
   "A Monk analyzer that presents situations as a binary encoding."))

(defmethod monk? ((analyzer monk-analyzer) attributes)
  "This method evaluates calls the current monk problem on the attributes."
  (funcall (monk-problem analyzer) attributes))

(defmethod random-situation ((analyzer monk-analyzer))
  "This returns a random Monk attribute list."
  (random-monk-attributes))

(defmethod random-situation ((analyzer monk-analyzer-ternary))
  "This returns a random Monk attribute list encoded as a truth vector."
  (setf (current-attributes analyzer) (random-monk-attributes))
  (coerce (monk-attributes-to-truth-values (current-attributes analyzer))
          'vector))

(defmethod get-situation ((analyzer monk-analyzer))
  "This sets and returns a new random situation."
  (setf (current-situation analyzer)
        (random-situation analyzer)))

(defmethod correct-action ((analyzer monk-analyzer))
  "This is the Boolean label of the current situation under MONK-PROBLEM."
  (monk? analyzer (current-situation analyzer)))

(defmethod correct-action ((analyzer monk-analyzer-ternary))
  "This is the Boolean label of the stored Monk attributes."
  (monk? analyzer (current-attributes analyzer)))

(defmethod correct-action? ((analyzer monk-analyzer))
  "This method predicate returns true only if the analyzed chose the correct
   action for its current action."
  (equal (current-action analyzer)
         (correct-action analyzer)))

(defmethod execute-action ((analyzer monk-analyzer) action)
  "This records ACTION and updates the correctness counts."
  (setf (current-action analyzer) action)
  (incf (actions analyzer))
  (when (correct-action? analyzer)
    (incf (correct-actions analyzer))))

(defmethod get-reward ((analyzer monk-analyzer))
  "This reward method is rather simplistic, but will probably do."
  (if (correct-action? analyzer) 100 -200))

(defmethod end-of-problem? ((analyzer monk-analyzer))
  "This predicate is always true.  Each Monk trial is a single step."
  t)

(defun start-monk (&optional (number-of-trials 10000) (run t))
  "This builds a ternary-predicate Monk-1 experiment and starts it."
  (setf *monk-analyzer* (make-instance 'monk-analyzer-ternary))
  (setf *monk-xcs*
        (make-instance 'xcs
                       :learning-parameters *monk-learning-parameters*
                       :predicate-type 'ternary-predicate))
  (setf *monk-experiment*
        (make-instance 'experiment
                       :environment *monk-analyzer*
                       :reinforcement-program *monk-analyzer*
                       :xcs *monk-xcs*
                       :number-of-trials number-of-trials))
  (if run
    (start *monk-experiment*)
    *monk-experiment*))

(behavior 'monk-attributes
  (should-be-true (monk-attributes? '(1 1 1 1 1 1)))
  (should-be-true (monk-attributes? '(3 3 2 3 4 2)))
  (should-be-false (monk-attributes? '(1 1 1 1 1)))
  (should-be-false (monk-attributes? '(1 1 1 1 5 1)))
  (dotimes (i 20)
    (should-be-true (monk-attributes? (random-monk-attributes)))))

(behavior 'monk-problems
  (spec "monk-1 is true when a1 = a2 or a5 = 1"
    (should-be-true (monk-1? '(1 1 1 1 2 1)))
    (should-be-true (monk-1? '(1 2 1 1 1 1)))
    (should-be-false (monk-1? '(1 2 1 1 2 1))))
  (spec "monk-2 is true when exactly two attributes are 1"
    (should-be-true (monk-2? '(1 1 2 2 2 2)))
    (should-be-false (monk-2? '(1 2 2 2 2 2)))
    (should-be-false (monk-2? '(1 1 1 2 2 2))))
  (spec "monk-3 without noise"
    (should-be-true (monk-3-no-noise? '(1 1 1 1 3 1)))
    (should-be-true (monk-3-no-noise? '(2 1 1 2 2 1)))
    (should-be-false (monk-3-no-noise? '(1 3 1 2 4 1))))
  (spec "monk-3 with noise still returns a boolean"
    (dotimes (i 8)
      (should-be-true (member (monk-3? '(1 1 1 1 3 1)) '(t nil))))))

(behavior 'monk-ternary-encoding
  (should-equal '(nil nil nil nil nil nil nil nil nil nil)
                (monk-attributes-to-truth-values '(1 1 1 1 1 1)))
  (let ((bits (monk-attributes-to-truth-values '(3 3 2 3 4 2))))
    (should= 10 (length bits))
    (should-equal '(t nil t nil t t nil t t t) bits)))

(behavior 'monk-xcs-experiment
  (let ((experiment (start-monk 8 nil)))
    (should-be-a 'experiment experiment)
    (should-be-a 'monk-analyzer-ternary *monk-analyzer*)
    (let ((sit (get-situation *monk-analyzer*)))
      (should= 10 (length sit))
      (should-be-true (every #'ternary-value? sit))
      (should-be-true (member (correct-action *monk-analyzer*) '(t nil))))))
