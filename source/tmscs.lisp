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


(defpackage :livermore/tmscs
  (:use :common-lisp
        :sigma/behave
        :sigma/control
        :sigma/numeric
        :sigma/probability
        :sigma/random
        :sigma/sequence
        :sigma/string
        :sigma/time-series
        :livermore/learning-parameters
        :livermore/xcs
        :livermore/xcsr)
  (:export :covering-score
           :duplicate
           :field
           :field-path
           :final
           :identical?
           :initial
           :match?
           :maximum-environment-condition-length
           :maximum-position-mutation
           :maximum-temporal-mutation
           :more-general?
           :operate
           :operation
           :path
           :simple-slope
           :tms-classifier
           :tms-predicate
           :tmscs
           :tmscs-learning-parameters
           :valid-fields
           :valid-operations
           :visible-time-range))
(in-package :livermore/tmscs)


(defclass tms-classifier (classifier)
  ()
  (:documentation
   "A classifier whose condition is a list of time-multiseries predicates."))

(defmethod match? ((tms-classifier tms-classifier) situation)
  "This predicate returns true only if the classifier matches the situation."
  (every (rcurry 'match? situation)
         (environment-condition tms-classifier)))

(defclass tms-predicate (range-predicate)
  ((initial
     :accessor initial
     :initform nil
     :initarg :initial
     :type list
     :documentation
     "This is the initial point in the path this predicate takes through the
     time multiseries.  It is a list consisting of the temporal position first
     and then all of the array positions.")
   (final
     :accessor final
     :initform nil
     :initarg :final
     :type list
     :documentation
     "This is the final point in the path this predicate takes through the time 
     multiseries.  It is a list consisting of the temporal position first and
     then all of the array positions.")
   (field
     :accessor field
     :initform #'identity
     :initarg :field
     :type function
     :documentation
     "This is an optional field specifier to be called on the entries in the
     time multiseries.  This allows for a large but discrete selection of data
     attributes to be stored in any particular cell of the time multiseries,
     such as having each cell be a structure or a class with accessable slots.")
   (operation
     :accessor operation
     :initform #'identity
     :initarg :operation
     :type function
     :documentation
     "This is the operation which this predicate applies to the path of data it
     pulls from the time multiseries.  Statistical operations would probably be
     good choices for most problems, and there may be problem-specific ones."))
  (:documentation
    "This is a time multiseries predicate.  It is a single rule consisting of a
    path through the data starting at the INITIAL point and ending at the FINAL
    point using an optional FIELD on the data entries and applying some specific
    OPERATION to make the classification."))

(defclass tmscs-learning-parameters (xcsr-learning-parameters)
  ((maximum-environment-condition-length
     :accessor maximum-environment-condition-length
     :initform 1
     :initarg :maximum-environment-condition-length
     :type (integer 0 *)
     :documentation "This is how many predicates we may have at the maximum.")
   (maximum-temporal-mutation
     :accessor maximum-temporal-mutation
     :initform 5
     :initarg :maximum-temporal-mutation
     :type (integer 0 *)
     :documentation
     "This is the most the temporal element of the position may be randomly
     perturbed during the mutation process.")
   (maximum-position-mutation
     :accessor maximum-position-mutation
     :initform 5
     :initarg :maximum-position-mutation
     :type (integer 0 *) ; Allowing an array or list may be nice.
     :documentation
     "This is the most any dimensional element of a position may be randomly
     perturbed during the mutation process.")
   (valid-operations
     :accessor valid-operations
     :initarg :valid-operations
     :type list
     :documentation "This is a list of all the valid operations as closures.")
   (valid-fields
     :accessor valid-fields
     :initform (list #'identity)
     :initarg :valid-fields
     :type list
     :documentation
     "This is a list of all the fields the classifier system will be allowed to
     use for the rules in the population.  This doesn't need to be all of the
     fields, but it usually can be.")
   (visible-time-range
     :accessor visible-time-range
     :initform '(0 100)
     :initarg :visible-time-range
     :type list
     :documentation
     "This is the range in time that is visible to the classifiers.  None of
     the classifiers are allowed to look beyond this window.  This also is
     generally how much of a history should be generated before the classifier
     system is allowed to start."))
  (:documentation "These are the learning parameters for a TMSCS problem."))

(defun simple-slope (sequence &key (key #'identity) (start 0) (end nil))
  "The slope of SEQUENCE from START to END, using KEY on each element.
  This is the operator used by the thesis time-series experiments."
  (assert (plusp (length sequence)))
  (when (null end)
    (setf end (1- (length sequence))))
  (assert (/= start end))
  (/ (- (funcall key (elt sequence start))
        (funcall key (elt sequence end)))
     (- start end)))

(defmethod print-object ((tms-predicate tms-predicate) stream)
  (format stream (string-concatenate "(livermore/tmscs:tms-predicate"
                                     " :operation ~A"
                                     " :field ~A"
                                     " :initial ~A"
                                     " :final ~A"
                                     " :lower ~A"
                                     " :upper ~A)")
          (operation tms-predicate)
          (field tms-predicate)
          (initial tms-predicate)
          (final tms-predicate)
          (lower tms-predicate)
          (upper tms-predicate)))

(defun tms-predicate (&rest rest)
  "This is a simple constructor for the TMS-PREDICATE class."
  (apply #'make-instance 'tms-predicate rest))

(defmethod duplicate ((tms-predicate tms-predicate))
  "This makes a deep duplicate of the TMS-predicate."
  (make-instance (type-of tms-predicate)
                 :operation (duplicate (operation tms-predicate))
                 :field (duplicate (field tms-predicate))
                 :initial (duplicate (initial tms-predicate))
                 :final (duplicate (final tms-predicate))
                 :lower (duplicate (lower tms-predicate))
                 :upper (duplicate (upper tms-predicate))))

(defmethod identical? ((x tms-predicate) (y tms-predicate))
  "This predicate returns true only if the two time multiseries predicates are
  functionally identical to each other."
  (and (equalp (operation x) (operation y))
       (equalp (field x) (field y))
       (equalp (initial x) (initial y))
       (equalp (final x) (final y))
       (= (lower x) (lower y))
       (= (upper x) (upper y))))

(defmethod path ((tms-predicate tms-predicate))
  "This method returns the path to take through the time multiseries according
  to the predicate."
  (raster-line (initial tms-predicate) (final tms-predicate)))

(defmethod field-path ((tms-predicate tms-predicate) situation)
  "This method returns the sequence of data along the path in the time
  multiseries as specified by the predicate."
  (assert (or (time-multiseries? situation)
              (time-series? situation)))
  (with-slots (field) tms-predicate
    (mapcar field (tms-values situation (path tms-predicate)))))

(defmethod operate ((tms-predicate tms-predicate) situation)
  "This method evaluates the predicate on the situation."
  (assert (or (time-multiseries? situation)
              (time-series? situation)))
  (funcall (operation tms-predicate) (field-path tms-predicate situation)))

(defmethod match? ((tms-predicate tms-predicate) (situation list))
  "This predicate returns true only if the predicate matches the situation."
  (assert (or (time-multiseries? situation)
              (time-series? situation)))
  (<= (lower tms-predicate)
      (operate tms-predicate situation)
      (upper tms-predicate)))

(defmethod match? ((general tms-predicate)
                   (specific tms-predicate))
  "This predicate returns true only if the general predicate completey matches
  the specific predicate in all situations."
  (and (equal (operation general) (operation specific))
       (equal (field general) (field specific))
       (<= (lower general) (lower specific) (upper specific) (upper general))
       (member (initial specific) (path general) :test #'equalp)
       (member (final specific) (path general) :test #'equalp)))

(defmethod more-general? ((general tms-predicate)
                          (specific tms-predicate))
  "This predicate returns true only if the general predicate matches the
  specific predicate and if it is more general than it as well."
  (and (match? general specific)
       (or (< (lower general) (lower specific))
           (< (upper specific) (upper general))
           (member (initial specific)
                   (remove (initial general) (path general) :test #'equalp)
                   :test #'equalp)
           (member (final specific)
                   (remove (final general) (path general) :test #'equalp)
                   :test #'equalp))))

(defclass tmscs (xcsr)
  ((predicate-type
     :initform 'tms-predicate)
   (classifier-type
     :initform 'tms-classifier)
   (learning-parameters
     :type tmscs-learning-parameters))
  (:documentation
   "TMSCS is XCSR specialized for time-multiseries predicates."))

(defmethod generate-covering-classifier ((tmscs tmscs))
  "This creates a classifier which matches the current situation."
  (with-slots (learning-parameters
                situation
                match-set
                predicate-type
                classifier-type) tmscs
    (with-slots (possible-actions
                  initial-prediction
                  initial-prediction-error
                  initial-fitness
                  maximum-environment-condition-length) learning-parameters
      (let ((eclen (random-in-range 1 maximum-environment-condition-length))
            (environment-condition nil)
            (action (random-element (set-difference possible-actions
                                                    (actions-in match-set)))))
        (dotimes (i eclen)
          (push (cover predicate-type situation learning-parameters)
                environment-condition))
        (make-instance classifier-type
                       :environment-condition environment-condition
                       :action action
                       :prediction initial-prediction
                       :prediction-error initial-prediction-error
                       :fitness initial-fitness
                       :experience 0
                       :time-stamp (number-of-situations tmscs)
                       :action-set-size 1
                       :numerosity 1)))))

(defmethod cover ((tms-predicate tms-predicate)
                  situation
                  (parameters tmscs-learning-parameters))
  "This method returns a newly-generated time multiseries predicate that covers
  the situation in some mostly random way."
  (assert (or (time-multiseries? situation)
              (time-series? situation)))
  (with-slots (initial-spread-limit
                valid-fields
                valid-operations
                visible-time-range) parameters
    (flet ((random-point ()
              (let ((dims (tms-dimensions situation)))
                (cons (random-in-range
                        (first visible-time-range)
                        (min (second visible-time-range)
                             (1- (first dims))))
                      (mapcar #'random (rest dims))))))
      (let* ((initial (random-point))
             (final (do ((final (random-point)
                                (random-point)))
                      ((not (equalp final initial))
                       (when (> (first initial)
                                (first final))
                         (swap initial final))
                       final)))
             (field (random-element valid-fields))
             (operation (random-element valid-operations))
             (spread (random-in-range 0.0 initial-spread-limit))
             (result (make-instance (type-of tms-predicate)
                                    :initial initial
                                    :final final
                                    :field field
                                    :operation operation))
             (value (operate result situation)))
        (with-slots (lower upper initial final) result
          (setf (lower result) (- value spread)
                (upper result) (+ value spread))
          (when (> (first initial) (first final))
            (swap initial final)))
        result))))

(defmethod mutate ((tms-classifier tms-classifier)
                   situation
                   (parameters tmscs-learning-parameters))
  "This method mutates a single TMS classifier."
  (with-slots (mutation-probability possible-actions) parameters
    (with-slots (environment-condition action) tms-classifier
      (mapcar (rcurry 'mutate situation parameters) environment-condition))))

(defmethod mutate ((tms-predicate tms-predicate)
                   situation
                   (learning-parameters tmscs-learning-parameters))
  "This mutates a single TMS predicate.  The initial and final points may be
  moved slightly, but only along the same line as the rest of the data.  The
  lower and the upper values of the range may be altered slightly, but only
  within the limits of MAX-CHANGE, and insuring that the current situation
  maintains its current classification under the rule."
  (with-slots (mutation-probability
                maximum-temporal-mutation
                maximum-position-mutation
                mutation-maximum
                valid-operations
                visible-time-range
                problem-range
                valid-fields) learning-parameters
    (with-slots (lower upper initial final field operation) tms-predicate
      (when (probability? mutation-probability)
        (let ((new-path (raster-line initial final
                                     :from-start (random-element '(-1 0 1) )
                                     :from-end (random-element '(-1 0 1))))
              (max-change (* mutation-maximum (spread tms-predicate)))
              (value (operate tms-predicate situation)))
          (mapcar #'(lambda (position)
                      "The first dimension of a time multiseries is the temporal
                      one, and since this is represented as a list it must be
                      a nonnegative integer."
                      (when (minusp (first position))
                        (setf (first position) 0)))
                  new-path)
          (assert (< 1 (length new-path)))
          (setf initial (first new-path)
                final (the-last new-path)
                lower (random-in-ranges problem-range
                                        (list (- lower max-change)
                                              (min (+ lower max-change)
                                                   value)))
                upper (random-in-ranges problem-range
                                        (list (max value
                                                   (- upper max-change))
                                              (+ upper max-change))))
          (assert (not (equalp initial final))))))))

(defmethod covering-score ((tms-predicate tms-predicate)
                           (parameters tmscs-learning-parameters))
  "This is the path length times the width of the range as a fraction of
  the problem range."
  (with-slots (lower upper) tms-predicate
    (* (length (path tms-predicate))
       (abs (/ (- lower upper)
               (apply #'- (problem-range parameters)))))))

(defmethod crossover ((p tms-classifier) (q tms-classifier) (tmscs tmscs))
  "This applies one-point crossover to the environment conditions of P and
  Q, then averages their prediction, error, and fitness.  It may also swap
  a prefix of slots inside the predicate at the cut."
  (flet ((midpoint (x y)
                   (/ (+ x y) 2)))
    (when (probability? (crossover-probability (learning-parameters tmscs)))
      (with-slots ((ecp environment-condition)) p
        (with-slots ((ecq environment-condition)) q
          (let ((new-prediction (midpoint (prediction p) (prediction q)))
                (new-prediction-error (midpoint (prediction-error p)
                                                (prediction-error q)))
                (new-fitness (midpoint (fitness p) (fitness q)))
                ;; This is the predicate in which crossover is to occur.
                (crossover-pos (random (min (length ecp) (length ecq)))))
            ;; Simple one-point crossover.
            (swap (nthcdr crossover-pos ecp)
                  (nthcdr crossover-pos ecq))
            ;; Now we must split the crossover predicate, but only if we don't
            ;; end up producing single-element paths in the swap.
            (unless (< (length (remove-duplicates
                                 (list (initial (nth crossover-pos ecp))
                                       (final (nth crossover-pos ecp))
                                       (initial (nth crossover-pos ecq))
                                       (final (nth crossover-pos ecq)))))
                       4)
              (let* ((items '(lower upper initial final field operation))
                     (crossover-subpos (random (length items)))
                     (crossover-items (subseq items 0 crossover-subpos)))
                (mapcar #'(lambda (item)
                            (swap (slot-value (nth crossover-pos ecp) item)
                                  (slot-value (nth crossover-pos ecq) item)))
                        crossover-items)))
            (mapcar #'(lambda (predicate)
                        (swap-unless #'< (lower predicate) (upper predicate)))
                    (append ecp ecq))
            (setf (prediction p) new-prediction
                  (prediction q) new-prediction
                  (prediction-error p) new-prediction-error
                  (prediction-error q) new-prediction-error
                  (fitness p) new-fitness
                  (fitness q) new-fitness)))))))

(behavior 'simple-slope
  (should= 1 (simple-slope '(0 1 2 3)))
  (should= 1 (simple-slope #(0 1 2 3)))
  (should= -1 (simple-slope '(3 2 1 0)))
  (should= 10 (simple-slope '(0 10) :end 1))
  (should= 10 (simple-slope '((0) (10)) :key #'first)))

(behavior 'tms-predicate
  (let* ((series '(0.0 1.0 2.0 3.0 4.0))
         (p (tms-predicate :initial '(0) :final '(4)
                           :field #'identity
                           :operation #'simple-slope
                           :lower 0.5 :upper 1.5)))
    (spec "path follows the raster line"
      (should-equal '((0) (1) (2) (3) (4)) (path p)))
    (spec "operate is the slope of the series along the path"
      (should= 1 (operate p series)))
    (spec "match? a series whose slope is in range"
      (should-be-true (match? p series))
      (should-be-false (match? p '(0.0 0.0 0.0 0.0 0.0))))
    (spec "identical? and duplicate"
      (let ((copy (duplicate p)))
        (should-be-true (identical? p copy))
        (should-not-eq p copy)
        (should-be-false (identical? p (tms-predicate :initial '(0) :final '(3)
                                                     :lower 0.5 :upper 1.5))))))
  (spec "a wider range is more general when the path contains the specific path"
    (let ((general (tms-predicate :initial '(0) :final '(4)
                                  :lower 0.0 :upper 2.0))
          (specific (tms-predicate :initial '(1) :final '(3)
                                   :lower 0.5 :upper 1.5)))
      (should-be-true (match? general specific))
      (should-be-true (more-general? general specific))
      (should-be-false (more-general? specific general))
      (should-be-false (more-general? general general)))))

(behavior 'tms-classifier
  (let* ((series '(0.0 1.0 2.0 3.0 4.0))
         (p (tms-predicate :initial '(0) :final '(4)
                           :operation #'simple-slope
                           :lower 0.5 :upper 1.5))
         (c (make-instance 'tms-classifier
                           :environment-condition (list p)
                           :action :uptrending)))
    (should-be-true (match? c series))
    (should-be-false (match? c '(5.0 4.0 3.0 2.0 1.0)))))

(behavior 'tmscs-learning-parameters
  (let ((lp (make-instance 'tmscs-learning-parameters
                           :minimum-number-of-actions 2
                           :valid-operations (list #'simple-slope))))
    (should-be-a 'tmscs-learning-parameters lp)
    (should-be-a 'xcsr-learning-parameters lp)
    (should= 1 (maximum-environment-condition-length lp))
    (should= 5 (maximum-temporal-mutation lp))
    (should= 5 (maximum-position-mutation lp))
    (should-equal '(0 100) (visible-time-range lp))
    (should-eq #'identity (first (valid-fields lp))))
  (spec "visible-time-range is an initarg"
    (should-equal '(0 50)
                  (visible-time-range
                    (make-instance 'tmscs-learning-parameters
                                   :minimum-number-of-actions 2
                                   :valid-operations (list #'simple-slope)
                                   :visible-time-range '(0 50))))))

(behavior 'cover-tms-predicate
  (let* ((series '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))
         (lp (make-instance 'tmscs-learning-parameters
                            :minimum-number-of-actions 2
                            :valid-operations (list #'simple-slope)
                            :valid-fields (list #'identity)
                            :visible-time-range '(0 9)
                            :initial-spread-limit 0.5))
         (p (cover (make-instance 'tms-predicate) series lp)))
    (should-be-a 'tms-predicate p)
    (should-be-true (match? p series))
    (should-be-true (< (first (initial p)) (first (final p))))))
