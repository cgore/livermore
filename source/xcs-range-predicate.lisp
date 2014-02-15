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

(in-package "XCS")

(defclass range-predicate ()
  ((lower
     :accessor lower
     :initform 0.0
     :initarg :lower
     :type float
     :documentation "This is the lower bound of the range, inclusive.")
   (upper
     :accessor upper
     :initform 1.0  ;;; TODO: This is not handled properly
     :initarg :upper
     :type float
     :documentation "This is the upper bound of the range, inclusive.")
   (covering-maximum
     :accessor covering-maximum
     :allocation :class
     :initform 0.20
     :initarg :covering-maximum
     :type positive-float
     :documentation
       "This is how large of a fraction of the range can be added to both the
       lower and the upper bounds combined in the covering method.")
   (mutation-maximum
     :accessor mutation-maximum
     :allocation :class
     :initform 0.20 ; 20% change at maximum.
     :initarg :mutation-maximum
     :type positive-float
     :documentation
       "This is how large of a fraction of the range may be added or subtracted
       from the lower and the upper bounds combined in the mutation method.")))

(defclass xcsr (xcs)
  ((predicate-type
     :initform 'range-predicate)))

(defmethod print-object ((range-predicate range-predicate) stream)
  (format stream "#<~A -- ~A>"
          (lower range-predicate)
          (upper range-predicate)))

(defmethod duplicate ((range range-predicate))
  (make-instance 'range-predicate
    :lower (lower range)
    :upper (upper range)
    :covering-maximum (covering-maximum range)
    :mutation-maximum (mutation-maximum range)))

(defmethod spread ((range range-predicate))
  (/ (- (upper range) (lower range)) 2))

(defmethod center ((range range-predicate))
  (+ (lower range) (spread range)))

(defmethod identical? ((range-1 range-predicate)
                       (range-2 range-predicate))
  (and (= (lower range-1)
          (lower range-2))
       (= (upper range-1)
          (upper range-2))))

(defmethod match? ((range range-predicate)
                   (situation float))
  (<= (lower range)
      situation
      (upper range)))

(defmethod match? ((range-1 range-predicate)
                   (range-2 range-predicate))
  "This returns true if RANGE-1 contains RANGE-2."
  (<= (lower range-1)
      (lower range-2)
      (upper range-2)
      (upper range-1)))

(defmethod more-general? ((general range-predicate)
                          (specific range-predicate))
  "This returns true if GENERAL is strictly more general than SPECIFIC."
  (and (match? general specific)
       (or (< (lower general) (lower specific))
           (< (upper specific) (upper general)))))

(defmethod cover ((range range-predicate)
                  (situation-element float)
                  (covering-probability float))
  "This method generates a range predicate that covers the specified situation
  element, which must be a float."
  (let ((result (duplicate range)))
    (flet ((covering-amount ()
             (* (random (/ (covering-maximum result) 2))
                (spread range))))
      (setf (lower result)
            (- situation-element (covering-amount)))
      (setf (upper result)
            (+ situation-element (covering-amount))))
    result))

(defmethod mutate ((range range-predicate)
                   situation
                   (mutation-probability float))
  "Mutation operates by adding a small random fractional amount to both the 
  lower and the upper values of the range.  In Wilson's original paper on XCSR
  this was fixed at 10% total, here it is MUTATION-MAXIMUM."
  (when (probability? mutation-probability)
    (with-slots (lower upper) range
      (let ((max-change (* (/ (mutation-maximum range) 2)
                           (spread range))))
        (incf lower (random-in-range (- max-change) max-change))
        (incf upper (random-in-range (- max-change) max-change))))))
