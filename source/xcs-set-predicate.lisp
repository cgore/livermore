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


(defpackage :livermore/xcs-set-predicate
  (:use :common-lisp
        :sigma/behave
        :sigma/control
        :sigma/probability
        :sigma/random
        :sigma/sequence
        :livermore/learning-parameters
        :livermore/xcs-predicate)
  (:export :cover
           :covering-score
           :covering?
           :duplicate
           :equivalent?
           :identical?
           :match?
           :more-general?
           :mutate
           :non-members
           :possible-members
           :print-object
           :set-predicate
           :universal?))
(in-package :livermore/xcs-set-predicate)


(defclass set-predicate (xcs-predicate)
  ((members
    :accessor members
    :initform nil
    :initarg :members
    :type list
    :documentation "The values this predicate matches when it is not covering.")
   (possible-members
    :accessor possible-members
    :initform nil
    :initarg :possible-members
    :type list
    :documentation
    "The universe of values this predicate may mention.  Share this list
    among predicates of the same problem.")
   (covering?
    :accessor covering?
    :initform nil
    :initarg :covering?
    :type boolean
    :documentation "True if this predicate matches every possible member."))
  (:documentation
   "A set predicate consists of a list of members of the set, or can cover all
    conditions if the COVERING? member is T.  The POSSIBLE-MEMBERS list should
    be shared among the set-predicates of the same problem, but the MEMBERS
    list should obviously not be."))

(defmethod print-object ((set-predicate set-predicate) stream)
  (if (covering? set-predicate)
    (format stream ":#")
    (format stream "~A" (members set-predicate))))

(defmethod non-members ((set-predicate set-predicate))
  "This method returns a list of all of the possible members from the state
   space which are not members of this predicate."
  (if (covering? set-predicate)
    nil
    (set-difference (possible-members set-predicate)
                    (members set-predicate))))

(defmethod universal? ((set-predicate set-predicate))
  "This method returns true only if all of the possible members are covered."
  (or (covering? set-predicate)
      (zerop (length (non-members set-predicate)))))

(defmethod duplicate ((set-predicate set-predicate))
  "This method returns a newly-instantiated copy of the set-predicate."
  (make-instance 'set-predicate
                 :members (duplicate (members set-predicate))
                 ;; The list of possible members should be shared.
                 :possible-members (possible-members set-predicate)
                 :covering? (duplicate (covering? set-predicate))))

(defmethod equivalent? ((x set-predicate) (y set-predicate))
  "This method returns true only if the two set predicates are functionally
   equivalent to each other; that is, they cover the same parts of the space."
  (or (and (universal? x)
           (universal? y))
      (set-equal (members x)
                 (members y))))

(defmethod identical? ((x set-predicate) (y set-predicate))
  "This method returns true only if the two set predicates are identical to each
   other; that is, if the cover the same parts of the space, and cover them in
   the same method; it is more stringent than the EQUIVALENT? predicate."
  (and (equal (covering? x)
              (covering? y))
       (set-equal (members x)
                  (members y))))

(defmethod match? ((set-predicate set-predicate) condition)
  "This predicate is true when SET-PREDICATE covers CONDITION, either
  because it is fully covering or because CONDITION is one of its members."
  (or (covering? set-predicate)
      (member condition (members set-predicate))))

(defmethod match? ((x set-predicate) (y set-predicate))
  "This predicate is true when X matches every situation that Y matches."
  (or (covering? x)
      (and (not (covering? y))
           (subsetp (members y) (members x)))))

(defmethod more-general? ((general set-predicate)
                          (specific set-predicate))
  "True iff GENERAL matches everything SPECIFIC matches and they are not
   functionally equivalent.  Identical sets and two universal predicates
   (covering or an explicit listing of every possible member) are not
   strictly more general than each other."
  (and (match? general specific)
       (not (equivalent? general specific))))

(defmethod cover ((set-predicate set-predicate)
                  (situations list)
                  (covering-probability float))
  "This returns a set predicate whose members are SITUATIONS.  With
  probability COVERING-PROBABILITY the new predicate is fully covering."
  (make-instance 'set-predicate
                 :members (duplicate situations)
                 :covering? (probability? covering-probability)))

(defmethod cover ((set-predicate set-predicate)
                  situation
                  (covering-probability float))
  "This covers a single SITUATION as a one-element member list."
  (cover set-predicate (list situation) covering-probability))

(defmethod mutate ((set-predicate set-predicate)
                   situation
                   (mutation-probability float))
  "This mutates SET-PREDICATE with probability MUTATION-PROBABILITY.  A
  covering predicate becomes a singleton of SITUATION.  Otherwise the
  predicate may become covering, gain a member, or lose a member other
  than SITUATION."
  (when (probability? mutation-probability)
    (with-slots (members covering? possible-members) set-predicate
      ;; The list of possible members should include at least two possibilities.
      (assert (< 1 (length possible-members)))
      (flet ((insert-new-member ()
               "This function inserts a new randomly chosen possible member."
               (let ((potential-members (remove situation
                                                (non-members set-predicate))))
                 ;; There must be somebody left to insert.
                 (assert (plusp (length potential-members)))
                 (pushnew (random-element potential-members)
                          members
                          :test 'identical?)))
             (delete-existing-member ()
               "This function removes one of the members."
               (let ((deletion-candidates (remove situation members)))
                 ;; There must be somebody left to delete.
                 (assert (plusp (length deletion-candidates)))
                 (deletef (random-element deletion-candidates)
                          members
                          :test 'identical?))))
        (cond
          ;; If the COVERING? flag is set to T then the only logical mutation is
          ;; to toggle it to NIL.
          (covering?
            (progn (setf covering? nil)
                   (pushnew situation
                            members
                            :test 'identical?)))

          ;; This probability of toggling the COVERING? flag to T should be
          ;; related to the number of non-members, but I suspect that I will
          ;; want to make the exact value of the multiplier a parameter
          ;; eventually, instead of a hard-coded 2.0.
          ((probability? (* 2.0 (/ (length (non-members set-predicate)))))
           (setf covering? t))

          ;; Special case when we only have one member: prefer to add rather than delete
          ((equalp 1 (length members))
           (insert-new-member))

          ;; Normal mutation: 50% chance to delete (if possible), else insert.
          ;; The probability of adding a new member and removing an existing
          ;; member should be equal, but we don't want to have the list of
          ;; members become empty.
          ((and (probability? 0.5)
                (> (length members) 1))
           (delete-existing-member))

          ;; The only remaining possibility.
          (t (pushnew (random-element
                       (remove situation (non-members set-predicate)))
                      members
                      :test 'identical?)))))))

(defmethod covering-score ((set-predicate set-predicate)
                           (learning-parameters learning-parameters))
  "This calculates the covering score, which is the ratio of the number of
   members covered by this set predicate to the total number of possible
   members in the entire input space.  If the covering flag is set to T, then
   this is the same as covering the entire universal set, so the covering score
   is then 1.  This should never be less than 0 and never more than 1."
  (if (covering? set-predicate)
    1
    (/ (length (members set-predicate))
       (length (possible-members set-predicate)))))

(behavior 'set-predicate
  (let* ((space '(a b c))
         (covering (make-instance 'set-predicate
                                  :possible-members space
                                  :covering? t))
         (ab (make-instance 'set-predicate
                            :members '(a b)
                            :possible-members space))
         (a (make-instance 'set-predicate
                           :members '(a)
                           :possible-members space))
         (full (make-instance 'set-predicate
                              :members '(a b c)
                              :possible-members space)))
    (spec "match?"
      (should-be-true (match? covering 'z))
      (should-be-true (match? ab 'a))
      (should-be-true (match? ab 'b))
      (should-be-false (match? ab 'c))
      (should-be-true (match? covering a))
      (should-be-true (match? ab a))
      (should-be-false (match? a ab)))
    (spec "more-general? is strict"
      (should-be-true (more-general? covering a))
      (should-be-true (more-general? ab a))
      (should-be-true (more-general? covering ab))
      (should-be-false (more-general? a ab))
      (should-be-false (more-general? a covering))
      (should-be-false (more-general? a a))
      (should-be-false (more-general? ab ab))
      (should-be-false (more-general? covering covering))
      (should-be-false (more-general? covering full))
      (should-be-false (more-general? full covering)))
    (spec "universal? and covering-score"
      (should-be-true (universal? covering))
      (should-be-true (universal? full))
      (should-be-false (universal? a))
      (should-be-null (non-members covering))
      (should-equal '(c) (non-members ab))
      (let ((lp (make-instance 'learning-parameters :minimum-number-of-actions 1)))
        (should= 1 (covering-score covering lp))
        (should= 1 (covering-score full lp))
        (should= 1/3 (covering-score a lp))))
    (spec "identical? vs equivalent?"
      (should-be-true (identical? a (make-instance 'set-predicate
                                                   :members '(a)
                                                   :possible-members space)))
      (should-be-false (identical? covering full))
      (should-be-true (equivalent? covering full)))
    (spec "duplicate shares the possible-members list"
      (let ((copy (duplicate ab)))
        (should-be-true (identical? ab copy))
        (should-not-eq ab copy)
        (should-eq (possible-members ab) (possible-members copy))))))
