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

(defpackage :livermore/stocks-xcsr
  (:use :common-lisp
        :livermore/stocks
        :livermore/stocks-xcsr-parameters
        :livermore/xcs
        :livermore/xcs-analyzer
        :livermore/xcsr
        :sigma/behave)
  (:export :*stocks-xcsr*
           :*stocks-xcsr-analyzer*
           :*stocks-xcsr-experiment*
           :correct-action
           :current-situation
           :end-of-problem?
           :get-situation
           :start-stocks-xcsr-experiment
           :stocks-xcsr-analyzer
           :stocks-xcsr-experiment
           :window))
(in-package :livermore/stocks-xcsr)

(defparameter *stocks-xcsr-analyzer* nil)
(defparameter *stocks-xcsr* nil)
(defparameter *stocks-xcsr-experiment* nil)

(defclass stocks-xcsr-analyzer (analyzer)
  ((table
    :accessor table
    :initarg :table
    :documentation "The price table this analyzer walks.")
   (window
    :accessor window
    :initform 6
    :initarg :window
    :type (integer 1 *)
    :documentation "How many recent daily returns form the situation.")
   (current-index
    :accessor current-index
    :initform 10
    :initarg :current-index
    :type integer)))

(defclass stocks-xcsr-experiment (experiment)
  ())

(defmethod current-record ((analyzer stocks-xcsr-analyzer))
  (elt-record (table analyzer) (current-index analyzer)))

(defmethod next-record ((analyzer stocks-xcsr-analyzer))
  (elt-record (table analyzer) (1+ (current-index analyzer))))

(defmethod get-situation ((analyzer stocks-xcsr-analyzer))
  "A vector of the last WINDOW daily returns ending at CURRENT-INDEX."
  (with-slots (table window current-index current-situation
               number-of-situations) analyzer
    (when (plusp number-of-situations)
      (incf current-index))
    (incf number-of-situations)
    (setf current-situation
          (coerce
           (loop for i from (- current-index window) below current-index
                 collect (/ (adjusted-closing-price (elt-record table (1+ i)))
                            (adjusted-closing-price (elt-record table i))))
           'vector))))

(defmethod correct-action ((analyzer stocks-xcsr-analyzer))
  "This is :STOCK when tomorrow's adjusted close is higher than today's."
  (if (> (adjusted-closing-price (next-record analyzer))
         (adjusted-closing-price (current-record analyzer)))
    :stock
    :bank))

(defmethod end-of-problem? ((analyzer stocks-xcsr-analyzer))
  t)

(defmethod terminate? ((experiment stocks-xcsr-experiment))
  (let ((analyzer (environment experiment)))
    (or (>= (actions analyzer) (number-of-trials experiment))
        (>= (current-index analyzer)
            (- (length (records (table analyzer))) 2)))))

(defun start-stocks-xcsr-experiment
    (&key (table nil)
          (ticker "^dji")
          (number-of-trials 10000)
          (run t))
  "This builds an XCSR experiment on TABLE (or TICKER) and starts it."
  (let ((price-table (or table (load-table ticker))))
    (setf *stocks-xcsr-analyzer*
          (make-instance 'stocks-xcsr-analyzer
                         :table price-table
                         :current-index 10))
    (setf *stocks-xcsr*
          (make-instance 'xcsr
                         :predicate-type 'range-predicate
                         :learning-parameters *stocks-xcsr-learning-parameters*))
    (setf *stocks-xcsr-experiment*
          (make-instance 'stocks-xcsr-experiment
                         :environment *stocks-xcsr-analyzer*
                         :reinforcement-program *stocks-xcsr-analyzer*
                         :xcs *stocks-xcsr*
                         :number-of-trials number-of-trials))
    (if run
      (start *stocks-xcsr-experiment*)
      *stocks-xcsr-experiment*)))

(behavior 'stocks-xcsr-analyzer
  (let* ((d0 (encode-universal-time 0 0 0 2 1 1995 0))
         (day (* 24 60 60))
         (table (make-instance 'table
                               :ticker-symbol "TEST"
                               :records
                               (loop for i from 0 below 20
                                     collect (livermore/stocks::%test-record
                                               (+ d0 (* i day))
                                               (+ 100.0 i)))))
         (analyzer (make-instance 'stocks-xcsr-analyzer
                                  :table table
                                  :window 4
                                  :current-index 8)))
    (let ((sit (get-situation analyzer)))
      (should= 4 (length sit))
      (should-be-true (every #'numberp (coerce sit 'list))))
    (should-eq :stock (correct-action analyzer))
    (should-be-true (end-of-problem? analyzer))))

(behavior 'stocks-xcsr-experiment
  (let* ((d0 (encode-universal-time 0 0 0 2 1 1995 0))
         (day (* 24 60 60))
         (table (make-instance 'table
                               :ticker-symbol "TEST"
                               :records
                               (loop for i from 0 below 30
                                     collect (livermore/stocks::%test-record
                                               (+ d0 (* i day))
                                               (+ 100.0 (sin i)))))))
    (let ((*standard-output* (make-broadcast-stream)))
      (start-stocks-xcsr-experiment :table table :number-of-trials 5 :run nil))
    (should-be-a 'xcsr *stocks-xcsr*)
    (let ((sit (get-situation *stocks-xcsr-analyzer*)))
      (should-be-a 'vector sit)
      (should= 6 (length sit)))))
