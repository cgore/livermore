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

(require :asdf)
(asdf:load-system "cgore-utilities")

;;;; This is a set of functions for evaluating evolutionary algorithms.
;;;; It is an implementation of the functions presented in:
;;;;
;;;; @ARTICLE{Whitley96EvaluatingEvolutionaryAlgorithms,
;;;;     author = {D. Whitley and K. Mathias and S. Rana and J. Dzubera},
;;;;     title = {Evaluating Evolutionary Algorithms},
;;;;     journal = {Artificial Intelligence},
;;;;     year = {1996},
;;;;     volume = {85},
;;;;     pages = {245--276}
;;;; }
;;;;
;;;; http://citeseerx.ist.psu.edu/citeseerx/viewdoc/summary?doi=10.1.1.53.134
;;;;
;;;; http://www.cs.jhu.edu/~sheppard/cs.605.754/papers/paper10b.ps.gz

(defpackage :cgore-whitley
  (:nicknames :whitley)
  (:use :common-lisp :cgore-utilities :cgore-random)
  (:export :f1 :f2 :f3 :f4 :f5
           :f6  :rastrigin
           :f7  :schwefel
           :f8  :griewangk
           :f9  :sine-envelope-sine-wave
           :f10 :stretched-v-sine-wave))
(in-package :cgore-whitley)

(defun f1 (x)
  "This function, F1, is a unimodal function, the sum of the squares.  It is
  originally from De Jong's dissertation, the classical test suite for
  evolutionary systems."
  (assert (listp x))
  (assert (= 3 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -5.12 xi 5.11)))
          x)
  (sum x :key (lambda (xi) (expt 2 xi))))

(defun f2 (x)
  "This function, F2, is a nonlinear function over two variables.  It is
  originally from De Jong's dissertation, the classical test suite for
  evolutionary systems."
  (assert (listp x))
  (assert (= 2 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -2.048 xi 2.047)))
          x)
  (+ (* 100.0 (expt 2 (- (expt 2 (first x))
                         (second x))))
     (expt 2 (- 1 (first x)))))

(defun f3 (x)
  "This function, F3, is a discontinuous function, the sum of the floors.  It is
  originally from De Jong's dissertation, the classical test suite for
  evolutionary systems."
  (assert (listp x))
  (assert (= 5 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -5.12 xi 5.11)))
          x)
  (sum x :key (lambda (xi) (floor xi))))

(defun f4 (x)
  "This function, F4, is a noisy function.  It is originally from De Jong's
  dissertation, the classical test suite for evolutionary systems."
  (assert (listp x))
  (assert (= 30 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -1.28 xi 1.27)))
          x)
  (let ((i (integer-range 1 (length x))))
    (+ (sum (mapcar (lambda (ii xi)
                      (* ii (expt 4 xi)))
                    i x))
       (gauss 0 1))))

(let ((f5a (make-array '(2 25)
                       :initial-contents
                       '((-32 -16   0  16  32
                          -32 -16   0  16  32
                          -32 -16   0  16  32
                          -32 -16   0  16  32
                          -32 -16   0  16  32)

                         (-32 -32 -32 -32 -32
                          -16 -16 -16 -16 -16
                            0   0   0   0   0
                           16  16  16  16  16
                           32  32  32  32  32)))))
  (defun f5 (x)
    "This function, F5, is a multimodal function with several local optima.  It
    is originally from De Jong's dissertation, the classical test suite for
    evolutionary systems."
    (assert (listp x))
    (assert (= 2 (length x)))
    (mapcar (lambda (xi)
              (assert (floatp xi))
              (assert (<= -65.536 xi 65.535)))
            x)
    (/ 1 (+ 0.002
            (sum (loop for j from 1 to 25 collect
                       (/ 1 (+ j
                               (sum (loop for i from 1 to 2 collect
                                          (expt 6.0
                                                (- (nth (1- i) x)
                                                   (aref f5a
                                                         (1- i)
                                                         (1- j))))))))))))))

(defun f6 (x)
  "This function, F6, is the Rastrigin function."
  (assert (listp x))
  (assert (<= 1 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -5.12 xi 5.11)))
          x)
  (+ (* (length x) 10.0)
     (sum x :key (lambda (xi)
                   (- (* xi xi)
                      (* 10 (cos (* 2 pi xi))))))))

(function-alias 'f6 'rastrigin)

(defun f7 (x)
  "This function, F7, is the Schwefel function."
  (assert (listp x))
  (assert (<= 1 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= 512 xi 511)))
          x)
  (sum x :key (lambda (xi)
                (- (* xi (sin (sqrt (abs xi))))))))

(function-alias 'f7 'schwefel)

(defun f8 (x)
  "This function, F8, is the Griewangk function."
  (assert (listp x))
  (assert (<= 1 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -5.12 xi 5.11)))
          x)
  (let ((i (loop for i from 1 to (length x) collect i)))
    (- (1+ (sum x :key (lambda (xi)
                         (/ (* xi xi) 4000.0))))
       (product (mapcar (lambda (xi ii)
                          (cos (/ xi (sqrt ii))))
                        x i)))))

(function-alias 'f8 'griewangk)

(defun f9 (x)
  "This function, F9, is the sine envelope sine wave function."
  (assert (listp x))
  (assert (= 2 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -100.0 xi 100.0)))
          x)
  (let* ((x1 (first x))
         (x2 (second x))
         (xsq (+ (expt 2 x1) (expt 2 x2))))
    (+ 0.5 (/ (- (expt 2 (sin (sqrt xsq)))
                 0.5)
              (expt 2 (+ 1.0 (* 0.001 xsq)))))))

(function-alias 'f9 'sine-envelope-sine-wave)

(defun f10 (x)
  "This function, F10, is the stretched V sine wave function."
  (assert (listp x))
  (assert (= 2 (length x)))
  (mapcar (lambda (xi)
            (assert (floatp xi))
            (assert (<= -100.0 xi 100.0)))
          x)
  (let* ((x1 (first x))
         (x2 (second x))
         (xsq (+ (expt 2 x1) (expt 2 x2))))
    (* (expt 0.25 xsq)
       (1+ (expt 2 (sin (* 50 (expt 0.1 xsq))))))))

(function-alias 'f10 'stretched-v-sine-wave)
