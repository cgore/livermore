(in-package "XCS")

(defclass set-predicate ()
  ((members
     :accessor members
     :initform nil
     :initarg :members
     :type list)
   (possible-members
     :accessor possible-members
     :initform nil
     :initarg :possible-members
     :type list)
   (covering?
     :accessor covering?
     :initform nil
     :initarg :covering?
     :type boolean))
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
  (or (covering? set-predicate)
      (member condition (members set-predicate))))

(defmethod match? ((x set-predicate) (y set-predicate))
  (or (covering? x)
      (and (not (covering? y))
           (subsetp (members y) (members x)))))

(defmethod more-general? ((general set-predicate)
                          (specific set-predicate))
  (and (not (covering? specific))
       (or (covering? general)
           (subsetp (members specific) (members general)))))

(defmethod cover ((set-predicate set-predicate)
                  (situations list)
                  (covering-probability float))
  (make-instance 'set-predicate
                 :members (duplicate situations)
                 :covering? (probability? covering-probability)))

(defmethod cover ((set-predicate set-predicate)
                  situation
                  (covering-probability float))
  (cover set-predicate (list situation) covering-probability))

(defmethod mutate ((set-predicate set-predicate)
                   situation
                   (mutation-probability float))
  (when (probability? mutation-probability)
    (with-slots (members covering?) set-predicate
      ;; The list of possible members should include at least two possibilities.
      (assert (< 1 (length possible-members)))
      (flet ((insert-new-member ()
               "This function inserts a new randomly chosen possible member."
               (let ((potential-members (remove situation
                                                (non-members set-predicate))))
                 ;; There must be somebody left to insert.
                 (assert (plusp (length potential-members)))
                 (pushnew (random-element potential-members))))
             (delete-existing-member ()
               "This function removes one of the members."
               (let ((deletion-candidates (remove situation members)))
                 ;; There must be somebody left to delete.
                 (assert (plusp (length deletion-candidates)))
                 (deletef (random-element deletion-candidates) members
                          :test 'identical?))))
        (cond
          ;; If the COVERING? flag is set to T then the only logical mutation is
          ;; to toggle it to NIL.
          (covering?
            (progn (setf covering? nil)
                   (pushnew situation members)))
          ;; This probability of toggling the COVERING? flag to T should be
          ;; related to the number of non-members, but I suspect that I will
          ;; want to make the exact value of the multiplier a parameter
          ;; eventually, instead of a hard-coded 2.0.
          ((probability? (* 2.0 (/ (length (non-members set-predicate)))))
           (setf covering? t))
          ((equalp 1 (length members))
           ;; The probability of adding a new member and removing an
           ;; existing member should be equal, but we don't want to have the
           ;; list of members become empty.
           ((and (not( equalp 1 (length members)))
                 (probability? 0.5))
            (deletef (random-element (remove situation members)) members
                     :test 'identical?))
           ;; The only remaining possibility.
           (t (pushnew (random-element
                         (remove situation (non-members set-predicate)))
                       members))))))))

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
