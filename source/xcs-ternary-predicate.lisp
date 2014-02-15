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
(export '(ternary-value?
           ternary-value
           ternary-predicate
           value
           covering?
           ?
           covering-score))

(defun ternary-value? (x)
  (member x '(nil t :#)))

(deftype ternary-value ()
  '(satisfies ternary-value?))

(defclass ternary-predicate ()
  ((value
     :accessor value
     :initform :#
     :initarg :value
     :type ternary-value))
  (:documentation "A ternary predicate is either T for true, or NIL for false,
     or :# which matches either T or NIL"))

(defmethod covering? ((p ternary-predicate))
  (equal (value p) :#))

(defmethod ? ((p ternary-predicate))
  (? (value p)))

(defun ternary-predicate (value)
  "This is a simple constructor for the TERNARY-PREDICATE class."
  (make-instance 'ternary-predicate :value value))

(defmethod print-object ((tern ternary-predicate) stream)
  (format stream "~A" (value tern)))

(defmethod duplicate ((tern ternary-predicate))
  (make-instance 'ternary-predicate :value (value tern)))

(defmethod identical? ((x ternary-predicate) y)
  "This method returns true if X is exactly equal to Y."
  (equal (value x) y))

(defmethod identical? ((x ternary-predicate)
                       (y ternary-predicate))
  "This method returns true if the two predicates are exactly equal."
  (equal (value x) (value y)))

(defmethod match? ((x ternary-predicate) y)
  "This returns true when X matches the situation Y."
  (or (equal (value x) :#)
      (and (not (equal y :#))
           (equal (? x) (? y)))))

(defmethod match? ((x ternary-predicate)
                   (y ternary-predicate))
  "This returns true when X matches all the situations matched by Y."
  (or (equal (value x) :#)
      (and (not (equal (value y) :#))
           (equal (? x) (? y)))))

(defmethod more-general? ((general ternary-predicate)
                          (specific ternary-predicate))
  "This returns true when GENERAL is strictly more general than SPECIFIC."
  (and (equal (value general) :#)
       (not (equal (value specific) :#))))

(defmethod cover ((ternary-predicate ternary-predicate)
                  (situation sequence)
                  (learning-parameters learning-parameters))
  "This method generates a ternary predicate that covers the specified
  situation element, which must be a ternary value."
  (with-slots (covering-probability) learning-parameters
    (map (type-of situation)
         #'(lambda (situation-element)
             (assert (typep situation-element 'ternary-value))
             (make-instance 'ternary-predicate
                            :value (if (probability? covering-probability)
                                     :#
                                     situation-element)))
         situation)))

(defmethod mutate ((tern ternary-predicate)
                   situation
                   (learning-parameters learning-parameters))
  (with-slots (mutation-probability) learning-parameters
    (when (probability? mutation-probability)
      (with-slots (value) tern
        (setf value (if (equal value :#) situation :#))))))

(defmethod covering-score ((ternary-predicate ternary-predicate)
                           (learning-parameters learning-parameters))
  (if (covering? ternary-predicate) 1 0))
