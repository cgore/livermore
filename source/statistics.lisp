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

(unless (find-package 'utilities) (load "utilities/utilities"))
(unless (find-package 'statistics)
  (defpackage "STATISTICS"
    (:nicknames "STAT")
    (:use "COMMON-LISP" "UTILITIES")
    (:documentation "This contains several useful statistical functions.")))
(in-package "STATISTICS")
(export '(arithmetic-mean
           sample-variance
           unbiased-sample-variance
           sample-standard-deviation
           unbiased-sample-standard-deviation
           stat-report))

(defgeneric arithmetic-mean (sequence &key key start end))

(defmethod arithmetic-mean
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The arithmetic mean is what most people mean when they use the word
  ``mean''  This is also called the ``sample mean,'' and is what is usually
  meant by the word ``average'' in conventional English usage.  It is usually
  written as x with an overbar."
  (let ((subsequence (subseq sequence start end)))
    (unless (empty-sequence? subsequence)
      (/ (sum subsequence :key key)
         (length subsequence)))))

(defgeneric sample-variance (sequence &key key start end))

(defmethod sample-variance
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The sample variance is also the second central moment and is usually written
  as m_2 or s^2_N."
  (let* ((subsequence (subseq sequence start end))
         (xbar (arithmetic-mean subsequence :key key)))
    (unless (or (null xbar)
                (empty-sequence? subsequence))
      (/ (sum subsequence
              :key (compose #'(lambda (xi)
                                (expt (- xi xbar) 2))
                            key))
         (length subsequence)))))

(defgeneric unbiased-sample-variance (sequence &key key start end))

(defmethod unbiased-sample-variance
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The sample variance is also the second central moment as adjusted to be an
  unbiased estimator and is usually written as s^2_{N-1}."
  (let ((subsequence (subseq sequence start end)))
    (unless (>= 1 (length subsequence))
      (let ((xbar (arithmetic-mean subsequence :key key)))
        (/ (sum subsequence :key (compose #'(lambda (xi)
                                              (expt (- xi xbar) 2))
                                          key))
           (1- (length subsequence)))))))

(defgeneric sample-standard-deviation (sequence &key key start end))

(defmethod sample-standard-deviation
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The sample standard deviation is the square root of the sample variance
  and is normally notated as s_N."
  (let ((variance (sample-variance sequence :key key :start start :end end)))
    (unless (null variance)
      (sqrt variance))))

(defgeneric unbiased-sample-standard-deviation (sequence &key key start end))

(defmethod unbiased-sample-standard-deviation
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The unbiased sample standard deviation is the square root of the unbiased
  sample variance and is normally notated as s_{N-1}."
  (let ((variance
          (unbiased-sample-variance sequence :key key :start start :end end)))
    (unless (null variance)
      (sqrt variance))))

(defgeneric stat-report (destination data
                          &key key start end
                            pre-string format-string post-string))

(defmethod stat-report
  (destination (data sequence)
    &key (key #'identity) (start 0) (end nil) 
         (pre-string "~&") (format-string "~A") (post-string "~%"))
  "This prints out some basic statistics about a sequence of data."
  (format destination
          (concatenate 'string
                       pre-string
                       "Mean: " format-string
                       ", sdev: " format-string
                       ", min: " format-string
                       ", max: " format-string
                       post-string)
          (arithmetic-mean data :key key :start start :end end)
          (unbiased-sample-standard-deviation data
                        :key key :start start :end end)
          (minimum data :key key :start start :end end)
          (maximum data :key key :start start :end end)))
