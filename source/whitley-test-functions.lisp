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

(defpackage :livermore/whitley-test-functions
  (:use :common-lisp
        :sigma/behave
        :sigma/control
        :sigma/numeric
        :sigma/random)
  (:export :f1 :f2 :f3 :f4 :f5
           :f6  :rastrigin
           :f7  :schwefel
           :f8  :griewangk
           :f9  :sine-envelope-sine-wave
           :f10 :stretched-v-sine-wave))
(in-package :livermore/whitley-test-functions)

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
  (sum x :key (lambda (xi) (expt xi 2))))

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
  (+ (* 100.0 (expt (- (second x) (expt (first x) 2)) 2))
     (expt (- 1 (first x)) 2)))

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
                      (* ii (expt xi 4)))
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
                                          (expt (- (nth (1- i) x)
                                                   (aref f5a
                                                         (1- i)
                                                         (1- j)))
                                                6)))))))))))

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
            (assert (<= -512 xi 511)))
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
         (xsq (+ (expt x1 2) (expt x2 2))))
    (+ 0.5 (/ (- (expt (sin (sqrt xsq)) 2)
                 0.5)
              (expt (+ 1.0 (* 0.001 xsq)) 2)))))

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
         (xsq (+ (expt x1 2) (expt x2 2))))
    (* (expt xsq 0.25)
       (1+ (expt (sin (* 50 (expt xsq 0.1))) 2)))))

(function-alias 'f10 'stretched-v-sine-wave)

(behavior 'f1
  (spec "the origin is a global minimum of 0"
    (should= 0.0 (f1 '(0.0 0.0 0.0))))
  (should= 1.0 (f1 '(1.0 0.0 0.0)))
  (should= 14.0 (f1 '(1.0 2.0 3.0)))
  (should= (f1 '(-1.0 2.0 -3.0)) (f1 '(1.0 2.0 3.0))))

(behavior 'f2
  (spec "Rosenbrock: 100 (x2 - x1^2)^2 + (1 - x1)^2, minimum 0 at (1, 1)"
    (should= 0.0 (f2 '(1.0 1.0)))
    (should= 1.0 (f2 '(0.0 0.0)))
    (should= 100.0 (f2 '(1.0 2.0)))
    (should= 4.0 (f2 '(-1.0 1.0)))))

(behavior 'f3
  (should= 0 (f3 '(0.0 0.0 0.0 0.0 0.0)))
  (should= 5 (f3 '(1.9 1.1 1.0 1.5 1.2))))

(behavior 'f4
  (spec "sum i xi^4 plus unit Gaussian noise"
    (let ((zeros (make-list 30 :initial-element 0.0))
          (ones (make-list 30 :initial-element 1.0)))
      (should-be-a 'float (f4 zeros))
      (should-be-a 'float (f4 ones))
      (should-be-true (< (abs (f4 zeros)) 8.0))
      (should-be-true (< (abs (- (f4 ones) 465.0)) 8.0)))))

(behavior 'f5
  (spec "Shekel foxholes: 1 / (0.002 + sum_j 1/(j + sum_i (xi - aij)^6))"
    (let* ((a (make-array
               '(2 25)
               :initial-contents
               '((-32 -16 0 16 32 -32 -16 0 16 32 -32 -16 0 16 32
                  -32 -16 0 16 32 -32 -16 0 16 32)
                 (-32 -32 -32 -32 -32 -16 -16 -16 -16 -16
                    0   0   0   0   0  16  16  16  16  16
                   32  32  32  32  32))))
           (expected (lambda (x)
                       (/ 1.0
                          (+ 0.002
                             (loop for j from 1 to 25
                                   sum (/ 1.0
                                          (+ j
                                             (expt (- (first x)
                                                      (aref a 0 (1- j)))
                                                   6)
                                             (expt (- (second x)
                                                      (aref a 1 (1- j)))
                                                   6)))))))))
      (should= (funcall expected '(-32.0 -32.0)) (f5 '(-32.0 -32.0)))
      (should= (funcall expected '(0.0 0.0)) (f5 '(0.0 0.0)))
      (should= (funcall expected '(16.0 32.0)) (f5 '(16.0 32.0)))))
  (spec "the first foxhole is a local minimum near 1/1.002"
    (should-be-true (< (abs (- (f5 '(-32.0 -32.0)) (/ 1.0 1.002))) 1.0e-5))
    (should-be-true (< (f5 '(-32.0 -32.0)) (f5 '(0.0 0.0))))))

(behavior 'rastrigin
  (spec "the origin is a global minimum of 0"
    (should= 0.0 (f6 '(0.0)))
    (should= 0.0 (rastrigin '(0.0 0.0 0.0))))
  (should-eq (fdefinition 'f6) (fdefinition 'rastrigin)))

(behavior 'schwefel
  (should= 0.0 (f7 '(0.0)))
  (should-eq (fdefinition 'f7) (fdefinition 'schwefel)))

(behavior 'griewangk
  (spec "the origin is a global minimum of 0"
    (should= 0.0 (f8 '(0.0)))
    (should= 0.0 (griewangk '(0.0 0.0))))
  (should-eq (fdefinition 'f8) (fdefinition 'griewangk)))

(behavior 'sine-envelope-sine-wave
  (spec "the origin is a global minimum of 0"
    (should= 0.0 (f9 '(0.0 0.0))))
  (spec "0.5 + (sin^2(sqrt(r^2)) - 0.5) / (1 + 0.001 r^2)^2"
    (let* ((xsq (+ (* 3.0 3.0) (* 4.0 4.0)))
           (expected (+ 0.5 (/ (- (expt (sin (sqrt xsq)) 2) 0.5)
                               (expt (+ 1.0 (* 0.001 xsq)) 2)))))
      (should= expected (f9 '(3.0 4.0)))
      (should= expected (sine-envelope-sine-wave '(3.0 4.0)))))
  (should-eq (fdefinition 'f9) (fdefinition 'sine-envelope-sine-wave)))

(behavior 'stretched-v-sine-wave
  (spec "the origin is a global minimum of 0"
    (should= 0.0 (f10 '(0.0 0.0))))
  (spec "(r^2)^0.25 * (1 + sin^2(50 (r^2)^0.1))"
    (let* ((xsq (+ (* 3.0 3.0) (* 4.0 4.0)))
           (expected (* (expt xsq 0.25)
                        (1+ (expt (sin (* 50 (expt xsq 0.1))) 2)))))
      (should= expected (f10 '(3.0 4.0)))
      (should= expected (stretched-v-sine-wave '(3.0 4.0)))))
  (should-eq (fdefinition 'f10) (fdefinition 'stretched-v-sine-wave)))
