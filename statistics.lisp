(load "utilities")
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

(defmethod arithmetic-mean 
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The arithmetic mean is what most people mean when they use the
  word ``mean.''  This is also called the ``sample mean,'' and is
  what is usually meant by the word ``average'' in conventional
  English usage.  It is usually notated as x with an overbar."
  (let ((subsequence (subseq sequence start end)))
    (unless (empty-sequence? subsequence)
      (/ (sum subsequence :key key)
         (length subsequence)))))

(defmethod sample-variance
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The sample variance is also the second central moment and is usually
  written as m_2 or s^2_N."
  (let* ((subsequence (subseq sequence start end))
         (xbar (arithmetic-mean subsequence :key key)))
    (unless (or (null xbar)
                (empty-sequence? subsequence))
      (/ (sum subsequence
              :key (compose #'(lambda (xi)
                                (expt (- xi xbar) 2))
                            key))
         (length subsequence)))))

(defmethod unbiased-sample-variance
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The sample variance is also the second central moment as adjusted to be
  an unbiased estimator and is usually written as s^2_{N-1}."
  (let ((subsequence (subseq sequence start end)))
    (unless (>= 1 (length subsequence))
      (let ((xbar (arithmetic-mean subsequence :key key)))
        (/ (sum subsequence :key (compose #'(lambda (xi)
                                              (expt (- xi xbar) 2))
                                          key))
           (1- (length subsequence)))))))

(defmethod sample-standard-deviation
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The sample standard deviation is the square root of the sample variance
  and is normally notated as s_N."
  (let ((variance (sample-variance sequence :key key :start start :end end)))
    (unless (null variance)
      (sqrt variance))))

(defmethod unbiased-sample-standard-deviation
  ((sequence sequence) &key (key #'identity) (start 0) (end nil))
  "The unbiased sample standard deviation is the square root of the unbiased
  sample variance and is normally notated as s_{N-1}."
  (let ((variance
          (unbiased-sample-variance sequence :key key :start start :end end)))
    (unless (null variance)
      (sqrt variance))))

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
