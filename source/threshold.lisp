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

(defpackage :livermore/threshold
  (:use :common-lisp
        :sigma/behave
        :sigma/sequence)
  (:export :threshold-indicator))
(in-package :livermore/threshold)

(defgeneric threshold-indicator (threshold instance)
  (:documentation
   "True iff INSTANCE meets or exceeds THRESHOLD.

A number meets a number when INSTANCE >= THRESHOLD. A list or vector meets
another of the same length when every coordinate satisfies that same
comparison."))

(defmethod threshold-indicator ((threshold list) (instance list))
  "This predicate is true when every element of INSTANCE is at least the
  corresponding element of THRESHOLD."
  (assert (= (length threshold)
             (length instance)))
  (every #'>= instance threshold))

(defmethod threshold-indicator ((threshold vector) (instance vector))
  "This predicate applies the list rule to two vectors of the same length."
  (threshold-indicator (vector-to-list threshold) (vector-to-list instance)))

(defmethod threshold-indicator ((threshold number) (instance number))
  "This predicate is true when INSTANCE is at least THRESHOLD."
  (>= instance threshold))

(behavior 'threshold-indicator
  (spec "a number meets a threshold at or above it"
    (should-be-true (threshold-indicator 5 6))
    (should-be-true (threshold-indicator 5 5))
    (should-be-false (threshold-indicator 5 4)))
  (spec "a list meets a threshold when every coordinate is at or above it"
    (should-be-true (threshold-indicator '(1 2) '(2 3)))
    (should-be-true (threshold-indicator '(1 2) '(2 2)))
    (should-be-false (threshold-indicator '(1 2) '(0 3))))
  (spec "vectors use the same rule as lists"
    (should-be-true (threshold-indicator #(1 2) #(2 3)))
    (should-be-true (threshold-indicator #(1 2) #(2 2)))
    (should-be-false (threshold-indicator #(1 2) #(2 1)))))
