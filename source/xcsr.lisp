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

(unless (find-package 'xcs) (load "xcs"))
(in-package "XCS")

(defclass range-predicate ()
  ((lower
     :accessor lower
     :initform nil
     :initarg :lower
     :type number
     :documentation "This is the lower bound of the range, inclusive.")
   (upper
     :accessor upper
     :initform nil
     :initarg :upper
     :type number
     :documentation "This is the upper bound of the range, inclusive."))
  (:documentation
    "This class represents a single range predicate as used by XCSR."))

(defclass xcsr-learning-parameters (learning-parameters)
  ((problem-range
     :accessor problem-range
     :initform (list 0.0 1.0)
     :initarg :problem-range
     :type list
     :documentation
     "This is the range that the input will take on.")
   (covering-maximum
     :accessor covering-maximum
     :initform 0.1
     :initarg :covering-maximum
     :type positive-float
     :documentation
       "This is how large of a fraction of the range can be added to both the
       lower and the upper bounds combined in the covering method.")
   (mutation-maximum
     :accessor mutation-maximum
     :initform 0.10 ; 10% change at maximum per allele.
     :initarg :mutation-maximum
     :type positive-float
     :documentation
       "This is how large of a fraction of the range may be added or subtracted
       from the lower and the upper bounds each in the mutation method.")
   (initial-spread-limit
     :accessor initial-spread-limit
     :initform 0.5
     :initarg :initial-first-spread-limit
     :type positive-float
     :documentation
       "This is s_0 in Wilson's XCSR article.  It is the maximim initial spread
       when a new predicate is created through covering."))
  (:documentation "These are the learning parameters for a ranged XCS."))

(defclass xcsr (xcs)
  ((predicate-type
     :initform 'range-predicate)
   (learning-parameters
     :type xcsr-learning-parameters)))

(defmethod print-object ((range-predicate range-predicate) stream)
  (format stream "[~A,~A]" (lower range-predicate) (upper range-predicate)))

(defmethod duplicate ((range-predicate range-predicate))
  "This method returns a newly-created duplicate of the range-predicate."
  (make-instance (type-of range-predicate)
    :lower (duplicate (lower range-predicate))
    :upper (duplicate (upper range-predicate))))

(defmethod spread ((range-predicate range-predicate))
  "This method returns the spread covered by the range predicate."
  (with-slots (lower upper) range-predicate
    (when (> lower upper)
      (swap lower upper))
    (/ (- upper lower) 2)))

(defmethod center ((range-predicate range-predicate))
  "This method returns the center value of the range predicate."
  (with-slots (lower upper) range-predicate
    (when (> lower upper)
      (swap lower upper))
    (+ lower (spread range-predicate))))

(defmethod identical? ((r range-predicate) (s range-predicate))
  "This method returns true iff the two range predicates are functionally 
  identical for all possible situations."
  (when (> (lower r) (upper r))
    (swap (lower r) (upper r)))
  (when (> (lower s) (upper s))
    (swap (lower s) (upper s)))
  (and (= (lower r) (lower s))
       (= (upper r) (upper s))))

(defmethod match? ((range-predicate range-predicate) (situation number))
  "This method returns true iff the range predicate matches the situation."
  (with-slots (lower upper) range-predicate
    (when (> lower upper)
      (swap lower upper))
    (<= lower situation upper)))

(defmethod match? ((r range-predicate)
                   (s range-predicate))
  "This returns true if R matches (contains) S."
  (when (> (lower r) (upper r))
    (swap (lower r) (upper r)))
  (when (> (lower s) (upper s))
    (swap (lower s) (upper s)))
  (<= (lower r) (lower s) (upper s) (upper r)))

(defmethod more-general? ((general range-predicate)
                          (specific range-predicate))
  "This returns true if GENERAL is strictly more general than SPECIFIC."
  (and (match? general specific)
       (or (< (lower general) (lower specific))
           (< (upper specific) (upper general)))))

(defmethod cover ((range-predicate range-predicate)
                  (situation sequence)
                  (parameters learning-parameters))
  "This method generates a range predicate that covers the specified situation
  element, which must be a float."
  (with-slots (problem-range initial-spread-limit) parameters
    (map (type-of situation)
         #'(lambda (situation-element)
             (when (< situation-element (first problem-range))
               (setf (first problem-range) situation-element))
             (when (> (second problem-range) situation-element)
               (setf (second problem-range) situation-element))
             (let ((spread (random-in-range 0.0 initial-spread-limit)))
               (make-instance 'range-predicate
                              :lower (- situation-element spread)
                              :upper (+ situation-element spread))))
         situation)))

(defmethod mutate ((range range-predicate)
                   situation
                   (parameters xcsr-learning-parameters))
  "Mutation operates by adding a small random fractional amount to both the
  lower and the upper values of the range.  In Wilson's original paper on XCSR
  this was fixed at 10% of the total, here it is MUTATION-MAXIMUM."
  (with-slots (mutation-probability mutation-maximum problem-range) parameters
    (when (probability? mutation-probability)
      (with-slots (lower upper) range
        (let ((max-change (* mutation-maximum (spread range))))
          (setf lower (random-in-ranges problem-range
                                        (list (- lower max-change)
                                              (+ lower max-change)))
                upper (random-in-ranges problem-range
                                        (list (- upper max-change)
                                              (+ upper max-change)))))))))

(defmethod covering-score ((range-predicate range-predicate)
                           (parameters xcsr-learning-parameters))
  (with-slots (lower upper) range-predicate
    (abs (/ (- lower upper)
            (apply #'- (problem-range parameters))))))

;; TODO: In Wilson's XCSR paper, he allows crossover to occur in the middle of an
;; allele, since he represented the environment condition as a list of the form
;; (center_0 spread_0 ... center_N spread_N).  We currently do not allow this, but
;; we may eant to.  An investigation into which is a better approach would help.
;(defmethod crossover ((p classifier) (q classifier) (xcsr xcsr)))
