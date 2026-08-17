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


(defpackage :livermore/multiplexer
  (:use :common-lisp
        :sigma/behave)
  (:export :random-bit
           :random-bit-vector
           :binary-decoder
           :multiplexer-length
           :multiplexer
           :truth-vector))
(in-package :livermore/multiplexer)


(defun random-bit ()
  "This returns a random bit, either 0 or 1."
  (random 2))

(defun random-bit-vector (length)
  "This returns a randomly filled bit vector of the specified length."
  (assert (and (integerp length)
               (plusp length)))
  (let ((vector (make-array length :element-type 'bit)))
    (dotimes (i (length vector) vector)
      (setf (bit vector i) (random-bit)))))

(defun binary-decoder (seq)
  "This decodes a binary sequence, consisting of either NIL or 0 for falsehood,
  and anything else for truth, into the positive integer that it represents."
  (let ((result 0)
        (seq (reverse seq)))
    (dotimes (index (length seq) result)
      (when (let ((e (elt seq index)))
              (and e (not (and (integerp e) (zerop e)))))
        (incf result (expt 2 index))))))

(defmethod multiplexer-length ((address-width integer))
  "This returns the bit-vector length of a multiplexer whose address is
  ADDRESS-WIDTH bits: ADDRESS-WIDTH plus 2^ADDRESS-WIDTH data bits."
  (assert (plusp address-width))
  (+ address-width (expt 2 address-width)))

(defmethod multiplexer ((address-width integer) (bits bit-vector))
  "A multiplexer takes in a bit vector consisting of a binary address part and
  a data part, where the length of the data part is (EXPT 2 ADDRESS-WIDTH).
  For example, with BITS set to #*110001 we have a 6-multiplexer, where the
  address part is #*11 and the data part is #*0001 which would return 1."
  (assert (and (plusp address-width)
               (= (length bits)
                  (multiplexer-length address-width))))
  (elt (subseq bits address-width)
       (binary-decoder (subseq bits 0 address-width))))

(defmethod truth-vector ((bvec bit-vector))
  "This returns a general vector of T and NIL corresponding to the 1 and 0
  bits of BVEC."
  (let ((tvec (make-array (length bvec))))
    (dotimes (i (length tvec) tvec)
      (setf (aref tvec i)
            (if (zerop (bit bvec i)) nil t)))))

(behavior 'random-bit
  (dotimes (i 20)
    (should-be-true (member (random-bit) '(0 1)))))

(behavior 'random-bit-vector
  (let ((v (random-bit-vector 8)))
    (should-be-a 'simple-bit-vector v)
    (should= 8 (length v))
    (should-be-true (every (lambda (b) (or (= b 0) (= b 1))) v))))

(behavior 'binary-decoder
  (should= 0 (binary-decoder '()))
  (should= 0 (binary-decoder '(nil)))
  (should= 0 (binary-decoder '(0)))
  (should= 1 (binary-decoder '(t)))
  (should= 1 (binary-decoder '(1)))
  (should= 2 (binary-decoder '(t nil)))
  (should= 3 (binary-decoder '(t t)))
  (should= 3 (binary-decoder #*11))
  (spec "treats any non-zero, non-NIL element as a 1-bit"
    (should= 1 (binary-decoder '(5)))))

(behavior 'multiplexer-length
  (should= 3 (multiplexer-length 1))
  (should= 6 (multiplexer-length 2))
  (should= 11 (multiplexer-length 3)))

(behavior 'multiplexer
  (spec "a 3-multiplexer: address 0 selects the first data bit, address 1 the second"
    (should= 0 (multiplexer 1 #*000))
    (should= 0 (multiplexer 1 #*001))
    (should= 1 (multiplexer 1 #*010))
    (should= 1 (multiplexer 1 #*011))
    (should= 0 (multiplexer 1 #*100))
    (should= 1 (multiplexer 1 #*101))
    (should= 0 (multiplexer 1 #*110))
    (should= 1 (multiplexer 1 #*111)))
  (spec "the 6-multiplexer docstring example: address 3 selects the last data bit"
    (should= 1 (multiplexer 2 #*110001)))
  (spec "address 0 selects the first data bit"
    (should= 1 (multiplexer 2 #*001000))
    (should= 0 (multiplexer 2 #*000001))))

(behavior 'truth-vector
  (should-equalp #(nil) (truth-vector #*0))
  (should-equalp #(t) (truth-vector #*1))
  (should-equalp #(t nil t) (truth-vector #*101)))
