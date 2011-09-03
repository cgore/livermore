;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore,
;;;; All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com

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
