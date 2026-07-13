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

(defpackage :livermore/xcs-predicate
  (:use :common-lisp
        :sigma/control
        :livermore/learning-parameters)
  (:export :cover
           :covering-score
           :covering?
           :duplicate
           :identical?
           :match?
           :more-general?
           :mutate))
(in-package :livermore/xcs-predicate)


(defclass xcs-predicate ()
  ()
  (:documentation "Base class for all XCS predicates.  You will need to make a subclass for your
specific implementation."))


;;; DEFGENERICs for everything we need.

(defgeneric cover (predicate situation learning-parameters)
  (:documentation "Generate a covering predicate/condition that matches the given situation.
This is the core operation used by GENERATE-COVERING-CLASSIFIER."))

(defgeneric covering-score (predicate learning-parameters)
  (:documentation "A score for how covering/general this predicate is (used in GA niching)."))

(defgeneric covering? (predicate)
  (:documentation "True if this predicate is fully covering."))

(defgeneric identical? (a b)
  (:documentation "Returns true if A and B are identical."))

(defgeneric match? (predicate situation)
  (:documentation "Returns true if PREDICATE matches the SITUATION."))

(defgeneric more-general? (general specific)
  (:documentation "Returns true if GENERAL is strictly more general than SPECIFIC."))

(defgeneric mutate (predicate situation learning-parameters)
  (:documentation "Mutate the predicate according to the learning parameters."))


;;; Default DEFMETHODs for everything, you'll often need to specialize these.

(defmethod cover ((predicate-type symbol) situation (lp learning-parameters))
  "Convenience dispatch: instantiate the predicate type then cover."
  (cover (make-instance predicate-type) situation lp))

(defmethod covering-score ((p xcs-predicate) (lp learning-parameters))
  "Default score; specialize for the specific predicates."
  0)

(defmethod covering? ((p xcs-predicate))
  "Default: most predicates are not fully covering unless specialized."
  nil)

(defmethod duplicate ((p xcs-predicate))
  "Default uses CLOS machinery; override for complex state."
  (let ((copy (make-instance (type-of p))))
    (when (slot-exists-p p 'value)  ; common simple case
      (setf (slot-value copy 'value) (slot-value p 'value)))
    copy))

(defmethod identical? ((a xcs-predicate) (b xcs-predicate))
  "Default fallback, specialize as needed for efficiency."
  (and (eql (type-of a)
            (type-of b))
       (equalp a b)))

(defmethod match? ((p xcs-predicate) situation)
  "Default: error out. Subclasses must provide a proper implementation."
  (error "MATCH? must be specialized for predicate type ~A on situation ~A"
         (type-of p) situation))

(defmethod more-general? ((general xcs-predicate) (specific xcs-predicate))
  "Default: false unless specialized."
  nil)

(defmethod mutate ((p xcs-predicate) situation (lp learning-parameters))
  "Default: error out. Subclasses must provide a proper implementation."
  (error "MUTATE must be specialized for predicate type ~A on situation ~A"
         (type-of p) situation))
