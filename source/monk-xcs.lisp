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
(in-package "XCS")
(use-package '("COMMON-LISP" "UTILITIES" "MULTIPLEXER"))
(export '(monk-attributes?
           random-monk-attributes
           monk-1?
           monk-2?
           monk-3-no-noise?
           monk-3?
           monk-analyzer
           current-situation
           current-action
           actions
           correct-actions
           monk-problem
           monk?
           initialize
           random-situation
           get-situation
           correct-action
           correct-action?
           execute-action
           get-reward
           end-of-problem?
           start-monk))
(load "monk-xcs-parameters.lisp")

(defun monk-attributes? (attributes)
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
          finally return result)))

(defun random-monk-attributes ()
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
  (or (and (= 3 (fifth  attribute))
           (= 1 (fourth attribute)))
      (and (not (= 4 (fifth  attribute)))
           (not (= 3 (second attribute))))))

(defun monk-3? (attributes)
  "This is basically the Monk's third problem."
  (if-probability 0.05
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
     :initarg :monk-problem)))

(defclass monk-analyzer-ternary (monk-analyzer))

(defmethod monk? ((analyzer monk-analyzer) attributes)
  "This method evaluates calls the current monk problem on the attributes."
  (funcall (monk-problem analyzer) attributes))

(defmethod initialize ((analyzer monk-analyzer))
  nil)

(defmethod random-situation ((analyzer monk-analyzer))
  (random-monk-attributes))

(defmethod random-situation ((analyzer monk-analyzer-ternary))
  (monk-attributes-to-truth-values (random-monk-attributes)))

(defmethod get-situation ((analyzer monk-analyzer))
  (setf (current-situation analyzer)
        (random-situation analyzer)))

(defmethod correct-action ((analyzer monk-analyzer))
  (monk? analyzer (current-situation analyzer)))

(defmethod correct-action? ((analyzer monk-analyzer))
  "This method predicate returns true only if the analyzed chose the correct
   action for its current action."
  (= (current-action analyzer)
     (correct-action analyzer)))

(defmethod execute-action ((analyzer monk-analyzer) action)
  )

(defmethod get-reward ((analyzer monk-analyzer))
  "This reward method is rather simplistic, but will probably do."
  (if (correct-action? analyzer) 100 -200))

(defmethod end-of-problem? ((analyzer monk-analyzer))
  (< 1000 (actions analyzer)))

(defun start-monk (&optional (address-width 2))
  (defparameter *monk-analyzer* (make-instance 'monk-analyzer))
  (defparameter *monk-analyzer-ternary* (make-instance 'monk-analyzer-ternary))
  (defparameter *monk-xcs*
    (make-instance 'xcs
                   :learning-parameters *monk-learning-parameters*
                   :predicate-type 'set-predicate))
  (defparameter *monk-xcs-ternary*
    (make-instance 'xcs
                   :learning-parameters *monk-learning-parameters*
                   :predicate-type 'ternary-predicate))
  (defparameter *monk-experiment*
    (make-instance 'experiment
                   :environment *monk-analyzer*
                   :reinforcement-program *monk-analyzer*
                   :xcs *monk-xcs*))
  (start *monk-experiment*))
