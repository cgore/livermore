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
(unless (find-package 'multiplexer)
  (defpackage "MULTIPLEXER"
    (:use "COMMON-LISP" "UTILITIES")))
(in-package "MULTIPLEXER")
(export '( random-bit
           random-bit-vector
           binary-decoder
           multiplexer-length
           multiplexer
           truth-vector))

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
  (elt (reverse (subseq bits address-width))
       (binary-decoder (subseq bits 0 address-width))))

(defmethod truth-vector ((bvec bit-vector))
  (let ((tvec (make-array (length bvec))))
    (dotimes (i (length tvec) tvec)
      (setf (aref tvec i)
            (if (zerop (bit bvec i)) nil t)))))
