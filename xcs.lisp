;;;; Christopher Mark Gore
;;;; chris-gore@earthlink.net, cmgb75@umr.edu
;;;; http://www.cgore.com

(load "utilities")
(unless (find-package "XCS")
  (defpackage "XCS"
    (:use "COMMON-LISP"
          #+cmu "EXTENSIONS" 
          "UTILITIES")
    (:export :ternary-value
             :match?
             :more-general?
             :cover
             :mutate
             :range-predicate
             :lower
             :upper
             :spread
             :center
             :classifier
             :environment-condition
             :action
             :prediction
             :prediction-error
             :fitness
             :experience
             :time-stamp
             :action-set-size
             :numerosity
             :accuracy
             :learning-parameters
             :maximum-total-numerosity
             :learning-rate
             :multiplier-parameter
             :equal-error-threshold
             :power-parameter
             :discount-factor
             :ga-threshold
             :crossover-probability
             :mutation-probability
             :deletion-threshold
             :fitness-fraction-threshold
             :minimum-subsumption-experience
             :covering-probability
             :initial-prediction
             :initial-prediction-error
             :initial-fitness
             :exploration-probability
             :minimum-number-of-actions
             :ga-subsumption?
             :action-set-subsumption?
             :possible-actions
             :prediction-array-element
             :action
             :fitness-sum
             :xcs
             :learning-parameters
             :population
             :match-set
             :action-set
             :previous-action-set
             :prediction-array
             :reward
             :previous-reward
             :situation
             :previous-situation
             :action
             :number-of-situations
             :experiment
             :single-step-output
             :environment
             :reinforcement-program
             :terminate?
             :system-prediction
             :actions-in
             :start
             :run
             :get-situation
             :generate-match-set
             :generate-covering-classifier
             :generate-prediction-array
             :select-action
             :generate-action-set
             :update-set
             :update-fitness
             :run-ga?
             :run-ga
             :select-offspring
             :crossover
             :insert-into-population
             :delete-from-population
             :sum-of-numerosities
             :deletion-vote
             :most-general-subsumers
             :do-action-set-subsumption
             :could-subsume?
             :subsume?)
    (:documentation
      "This is an implementation of Wilson's XCS system, originally based upon
      the article ``An Algorithmic Description of XCS'' by Martin V. Butz and
      Stewart W. Wilson, as found in LNAI volume 1996, pp. 253 -- 272.")))
(in-package "XCS")

(defclass classifier ()
  ((environment-condition
     :accessor environment-condition
     :initarg :environment-condition
     :documentation
       "This is `C' in Butz and Wilson's paper.  It is the conditions which
       are matched by this classifier.")
   (action
     :accessor action
     :initarg :action
     :documentation
       "This is `A' in Butz and Wison's paper.  It is the action that this
       classifier recommends taking if its environment condition matches the
       current situation.")
   (prediction
     :accessor prediction
     :initarg :prediction
     :documentation
       "This is `p' in Butz and Wilson's paper.  It is the predicated payoff
       of this rule in the current environment.")
   (prediction-error
     :accessor prediction-error
     :initarg :prediction-error
     :documentation
       "This is `epsilon' in Butz and Wilson's paper.  It is the error that
       has been demonstrated in the prediction.")
   (fitness
     :accessor fitness
     :initarg :fitness
     :type float
     :documentation
       "This is `F' in Butz and Wilson's paper.  It is the fitness used by the
       genetic algorithm.")
   (experience
     :accessor experience
     :initform 0
     :initarg :experience
     :type (integer 0 *)
     :documentation
       "This is `exp' in Butz and Wilson's paper.")
   (time-stamp
     :accessor time-stamp
     :initform 0
     :initarg :time-stamp
     :type (integer 0 *)
     :documentation
       "This is `ts' in Butz and Wilson's paper.")
   (action-set-size
     :accessor action-set-size
     :initform 0
     :initarg :action-set-size
     :documentation
       "This is `as' in Butz and Wilson's paper.")
   (numerosity
     :accessor numerosity
     :initform 1
     :initarg :numerosity
     :type positive-integer
     :documentation
       "This is `n' in Butz and Wilson's paper.")
   (accuracy
     :accessor accuracy
     :initform 0.0
     :initarg :accuracy
     :documentation
       "This is `kappa' in Butz and Wilson's paper."))
  (:documentation "This is a single XCS classifier, a 'rule'."))

(defmethod print-object ((classifier classifier) stream)
  (with-slots (environment-condition
                action
                prediction
                prediction-error
                fitness) classifier
    (format stream "#<XCS --> ~A (p: ~A, perr: ~A, F: ~A) on ~A>~%"
            action prediction prediction-error fitness environment-condition)))

(defclass learning-parameters ()
  ((maximum-total-numerosity
     :accessor maximum-total-numerosity
     :initform 100
     :initarg :maximum-total-numerosity
     :type integer
     :documentation
       "This is `N' in Butz and Wilson's paper, usually in the hundreds.")
   (learning-rate
     :accessor learning-rate
     :initform 0.1
     :initarg :learning-rate
     :type float
     :documentation
       "This is `beta' in Butz and Wilson's paper, usually in [0.1, 0.2].")
   (multiplier-parameter
     :accessor multiplier-parameter
     :initform 0.1
     :initarg :multiplier-parameter
     :type float
     :documentation
       "This is `alpha' in Butz and Wilson's paper, usually 0.1.")
   (equal-error-threshold
     :accessor equal-error-threshold
     :initform 10.0
     :initarg :equal-error-threshold
     :type float
     :documentation
       "This is `epsilon_0' in Butz and Wilson's paper, usually 1% of rho,
       the reward (which is typically set to 1000/0 in the simplest cases.)")
   (power-parameter
     :accessor power-parameter
     :initform 5
     :initarg :power-parameter
     :documentation
       "This is `nu' in Butz and Wilson's paper, usually 5.")
   (discount-factor
     :accessor discount-factor
     :initform 0.71
     :initarg :discount-factor
     :type float
     :documentation
       "This is `gamma' in Butz and Wilson's paper, usually 0.71.")
   (GA-threshold
     :accessor GA-threshold
     :initform 50
     :initarg :GA-threshold
     :documentation
       "This is `theta_GA' in Butz and Wilson's paper, usually in [25, 50].")
   (crossover-probability
     :accessor crossover-probability
     :initform 0.5
     :initarg :crossover-probability
     :type float
     :documentation
       "This is `chi' in Butz and Wilson's paper, usually in [0.5, 1.0].")
   (mutation-probability
     :accessor mutation-probability
     :initform 0.025
     :initarg :mutation-probability
     :type float
     :documentation
       "This is `mu' in Butz and Wilson's paper, usually in [0.01, 0.05].")
   (deletion-threshold
     :accessor deletion-threshold
     :initform 20
     :initarg :deletion-threshold
     :documentation
       "This is `theta_del' in Butz and Wilson's paper, usually 20.")
   (fitness-fraction-threshold
     :accessor fitness-fraction-threshold
     :initform 0.1
     :initarg :fitness-fraction-threshold
     :type float
     :documentation
       "This is used in the DELETION-VOTE method, and is usually 0.1.")
   (minimum-subsumption-experience
     :accessor minimum-subsumption-experience
     :initform 20
     :initarg :minimum-subsumption-experience
     :type integer
     :documentation
       "This is `theta_sub' in Butz and Wilson's paper, usually >= 20.")
   (covering-probability
     :accessor covering-probability
     :initform 0.33
     :initarg :covering-probability
     :type float
     :documentation
       "This is `P_#' in Butz and Wilson's paper, usually around 0.33.")
   (initial-prediction
     :accessor initial-prediction
     :initform 0.001
     :initarg :initial-prediction
     :type float
     :documentation
       "This is `p_I' in Butz and Wilson's paper, usually only slightly more
       than zero.")
   (initial-prediction-error
     :accessor initial-prediction-error
     :initform 0.001
     :initarg :initial-prediction-error
     :type float
     :documentation
       "This is `epsilon_I' in Butz and Wilson's paper, usually only slightly
       more than zero.")
   (initial-fitness
     :accessor initial-fitness
     :initform 0.001
     :initarg :initial-fitness
     :type float
     :documentation
       "This is `F_I' in Butz and Wilson's paper, usually only slightly more
       than zero.")
   (exploration-probability
     :accessor exploration-probability
     :initform 0.5
     :initarg :exploration-probability
     :type float
     :documentation
       "This is `P_explr' in Butz and Wilson's paper, usually around 0.5.")
   (minimum-number-of-actions 
     :accessor minimum-number-of-actions
     :initarg :minimum-number-of-actions
     :type (integer 0 *)
     :documentation
       "This is the theta_mna in Butz and Wilson's paper, usually equal to the
       number of possible actions in order for covering to take place.")
   (maximum-number-of-steps
      :accessor maximum-number-of-steps
      :initform 50
      :initarg maximum-number-of-steps
      :type integer
      :documentation
        "This is the maximum number of steps that a multistep problem can spend
        in one trial.")
   (GA-subsumption?
     :accessor GA-subsumption?
     :initform t
     :initarg :GA-subsumption?
     :type boolean
     :documentation
       "This is `doGASubsumption' in Butz and Wilson's paper, a boolean
       parameter to specify if subsumption should be performed during the
       genetic algorithm's run or not.")
   (action-set-subsumption?
     :accessor action-set-subsumption?
     :initform t
     :initarg :action-set-subsumption?
     :type boolean
     :documentation
       "This is a boolean parameter that specifies if action sets are to be
       tested for for subsuming classifiers.  It is called
       doActionSetSubsumption in Butz and Wilson's paper.")
   (possible-actions
     :accessor possible-actions
     :initform '(nil t)
     :initarg :possible-actions
     :type list
     :documentation
       "This is a list of all the possible actions available to the XCS."))
   (:documentation "These are all of the learning parameters used by the XCS."))

(defclass xcs ()
  ((learning-parameters
     :accessor learning-parameters
     :initarg :learning-parameters
     :type learning-parameters)
   (population
     :accessor population
     :initform nil
     :initarg :population
     :type list
     :documentation
       "This is `[P]' in Butz and Wilson's paper.")
   (match-set
     :accessor match-set
     :initform nil
     :initarg :match-set
     :type list
     :documentation
       "This is `[M]' in Butz and Wilson's paper.")
   (action-set
     :accessor action-set
     :initform nil
     :initarg :action-set
     :type list
     :documentation
       "This is `[A]' in Butz and Wilson's paper.")
   (previous-action-set
     :accessor previous-action-set
     :initform nil
     :initarg :previous-action-set
     :type list
     :documentation
       "This is `[A]_-1' in Butz and Wilson's paper.")
   (prediction-array
     :accessor prediction-array
     :initform nil
     :initarg :prediction-array
     :type list
     :documentation
       "This is `[PA]' in Butz and Wilson's paper.")
   (reward
     :accessor reward
     :initform 0.0
     :initarg :reward
     :type float
     :documentation
       "This is `rho' in Butz and Wilson's paper.")
   (previous-reward
     :accessor previous-reward
     :initform 0.0
     :initarg :previous-reward
     :type float
     :documentation
       "This is `rho_-1' in Butz and Wilson's paper.")
   (payoff
     :accessor payoff
     :initform 0.0
     :initarg :payoff
     :type float
     :documentation
       "This is the number `P' in Butz and Wilson's paper.")
   (predicate-type
     :accessor predicate-type
     :initform 'ternary-predicate
     :initarg :predicate-type
     :documentation
       "This chooses the type of predicates used.  We normally would use
       'TERNARY-PREDICATE here, or any other symbol which names the class of
       the predicates to be used.  To allow multi-typed XCS, we will need to
       allow lists here too.")
   (classifier-type
     :accessor classifier-type
     :initform 'classifier
     :initarg :classifier-type
     :type symbol
     :documentation "This is the type of classifier used.")
   (situation
     :accessor situation
     :initform nil
     :initarg :situation
     :documentation
       "This is `sigma' in Butz and Wilson's paper.")
   (previous-situation
     :accessor previous-situation
     :initform nil
     :initarg :previous-situation
     :documentation
       "This is `sigma_-1' in Butz and Wilson's paper.")
   (action
     :accessor action
     :initform nil
     :initarg :action
     :documentation
       "This is `act' in Butz and Wilson's paper.")
   (number-of-situations
     :accessor number-of-situations
     :initform 0
     :initarg :number-of-situations
     :type integer)
   (explore?
     :accessor explore?
     :initform nil
     :initarg :explore?
     :type boolean)
   (exploration-length
     :accessor exploration-length
     :initform 1000
     :initarg :exploration-length
     :type (integer 0 *))
   (exploitation-length
     :accessor exploitation-length
     :initform 1
     :initarg :exploitation-length
     :type (integer 0 *))
   (number-of-exploration-loops
     :accessor number-of-exploration-loops
     :initform 1
     :initarg :number-of-exploration-loops
     :type (integer 0 *))
   (number-of-exploitation-loops
     :accessor number-of-exploitation-loops
     :initform 1
     :initarg :number-of-exploitation-loops
     :type (integer 0 *))
   ))

(defclass environment ()
  ((multistep?
     :accessor multistep?
     :initform nil
     :initarg multistep?
     :type boolean)))

(defmethod single-step? ((env environment))
  (not (multistep? env)))

(defmethod initialize ((env environment)) nil)
(defmethod reset ((env environment)) nil)
(defmethod get-situation ((env environment)) nil)
(defmethod execute-action ((env environment) action) nil)

(defclass reinforcement-program () ())
(defmethod initialize ((rp reinforcement-program)) nil)
(defmethod get-reward ((rp reinforcement-program)) nil)
(defmethod end-of-problem? ((rp reinforcement-program)) nil)

(defclass experiment ()
  ((environment
     :accessor environment
     :initarg :environment)
   (reinforcement-program
     :accessor reinforcement-program
     :initarg :reinforcement-program)
   (xcs
     :accessor xcs
     :initarg :xcs)
   (number-of-trials
      :accessor number-of-trials
      :initform 10000
      :initarg :number-of-trials
      :type integer))
  (:documentation "This class holds an instance of a single experiment, which
                  contains an XCS, reinforcement program, and environment."))
(defgeneric single-step-output (experiment))
(defgeneric terminate? (experiment))

(load "xcs-ternary-predicate")

(defmethod could-subsume? ((classifier classifier)
                           (learning-parameters learning-parameters))
  "This is the ``COULD SUBSUME'' function in Butz and Wilson's paper.  We say
  that a classifier could subsume if it has sufficient accuracy and experience."
  (and (> (experience classifier)
          (minimum-subsumption-experience learning-parameters))
       (< (prediction-error classifier)
          (equal-error-threshold learning-parameters))))

(defmethod subsume? ((general classifier)
                     (specific classifier)
                     (learning-parameters learning-parameters))
  "This is the ``DOES SUBSUME'' function in Butz and Wilson's paper.  It is
  the function deciding overall if a classifier should subsume another."
  (and (equal (action general) (action specific))
       (could-subsume? general learning-parameters)
       (more-general? general specific)))

(defmethod duplicate ((classifier classifier))
  (make-instance (type-of classifier)
    :environment-condition (duplicate (environment-condition classifier))
    :action (action classifier)
    :prediction (prediction classifier)
    :prediction-error (prediction-error classifier)
    :fitness (fitness classifier)
    :experience (experience classifier)
    :time-stamp (time-stamp classifier)
    :action-set-size (action-set-size classifier)
    :numerosity (numerosity classifier)
    :accuracy (accuracy classifier)))

(defmethod identical? ((situation-1 sequence)
                       (situation-2 sequence))
  (and (= (length situation-1)
          (length situation-2))
       (every 'identical? situation-1 situation-2)))

(defmethod identical? ((classifier classifier)
                       (situation sequence))
  (identical? (environment-condition classifier) situation))

(defmethod identical? ((classifier-1 classifier)
                       (classifier-2 classifier))
  (and (identical? (environment-condition classifier-1)
                   (environment-condition classifier-2))
       (equalp (action classifier-1)
               (action classifier-2))))

(defmethod actions-in ((classifiers sequence))
  "This returns a list of all the actions present in CLASSIFIERS."
  (remove-duplicates (mapcar 'action classifiers)))

(defmethod initialize ((xcs xcs)) nil)

(defmethod deletion-vote ((classifier classifier)
                          (learning-parameters learning-parameters)
                          (average-fitness float))
  "This is the ``DELETION VOTE'' function in Butz and Wilson's paper.
  It serves as a method to rank this particular classifier for deletion."
  (with-slots (action-set-size numerosity experience fitness) classifier
    (* action-set-size
       numerosity
       (if (and (> experience (deletion-threshold learning-parameters))
                (< (/ fitness numerosity)
                   (* (fitness-fraction-threshold learning-parameters)
                      average-fitness)))
         (/ average-fitness (/ fitness numerosity))
         1))))

(defmethod delete-from-population ((xcs xcs))
  "This is the ``DELETE FROM POPULATION'' function in Butz and Wilson's paper.
  It decides which members of the population are suitable for deletion, allowing
  for niching, and then removes low-fitness individuals."
  (with-slots (population learning-parameters) xcs
    (let ((sum-of-numerosities (sum population :key 'numerosity)))
      (when (>= sum-of-numerosities
                (maximum-total-numerosity learning-parameters))
        (let* ((average-fitness (/ (sum population :key 'fitness)
                                   sum-of-numerosities))
               (choice-point
                 (* (random 1.0)
                    (sum (mapcar #'(lambda (classifier)
                                     (deletion-vote classifier
                                                    learning-parameters
                                                    average-fitness))
                                 population))))
               (sum-of-votes 0))
          (dolist (classifier population)
            (incf sum-of-votes (deletion-vote classifier
                                              learning-parameters
                                              average-fitness))
            (when (> sum-of-votes choice-point)
              (if (> (numerosity classifier) 1)
                (decf (numerosity classifier))
                (deletef classifier population :test 'identical?))
              (return-from delete-from-population))))))))

(defmethod cover ((predicate-type symbol)
                  situation
                  (learning-parameters learning-parameters))
  "This method generates a predicate of the specified type that covers the
  given element of the situation."
  (cover (make-instance predicate-type) situation learning-parameters))

(defmethod generate-covering-classifier ((xcs xcs))
  "This is the ``GENERATE COVERING CLASSIFIER'' function in Butz and Wilson's
  paper.  It creates a classifier which matches the current situation."
  (with-slots (learning-parameters
                situation
                match-set
                predicate-type
                classifier-type) xcs
    (with-slots (possible-actions
                  initial-prediction
                  initial-prediction-error
                  initial-fitness) learning-parameters
      (make-instance classifier-type
        :environment-condition 
        (cover predicate-type situation learning-parameters)
        :action
        (random-element (set-difference possible-actions
                                        (actions-in match-set)))
        :prediction initial-prediction
        :prediction-error initial-prediction-error
        :fitness initial-fitness
        :experience 0
        :time-stamp (number-of-situations xcs)
        :action-set-size 1
        :numerosity 1))))

(defmethod match? ((situation-1 sequence)
                   (situation-2 sequence))
  "This determines if SITUATION-1 and SITUATION-2, both sequences, match."
  (every 'match? situation-1 situation-2))

(defmethod match? ((classifier classifier)
                   (situation sequence))
  "This is the ``DOES MATCH'' function in Butz and Wilson's paper.  It
  determines if CLASSIFIER matches the specified SITUATION."
  (match? (environment-condition classifier) situation))

(defmethod match? ((classifier-1 classifier)
                   (classifier-2 classifier))
  "This determines if CLASSIFIER-1 and CLASSIFIER-2 match."
  (match? (environment-condition classifier-1)
          (environment-condition classifier-2)))

(defmethod generate-match-set ((xcs xcs))
  "This is the ``GENERATE MATCH SET'' function in Butz and Wilson's paper.
  The match set [M] contains all of the classifiers in the population [P] which
  match the current situation.  After filling the match set with all
  pre-existing matching classifiers, it repeatedly generates new covering
  classifiers until the minimum number of actions is satisfied."
  (with-slots (population match-set situation learning-parameters) xcs
    (flet ((exit-condition ()
             (>= (length (actions-in match-set))
                 (minimum-number-of-actions learning-parameters))))
      (do-until (exit-condition)
        (setf match-set (remove-if-not (rcurry #'match? situation) population))
        (unless (exit-condition)
          (push (generate-covering-classifier xcs) population)
          (delete-from-population xcs)
          (setf match-set nil))))))

(defmethod system-prediction ((xcs xcs) action)
  "The system prediction is the way we rank a particular action."
  (let* ((classifiers (remove-if-not #'(lambda (classifier)
                                         (equal action (action classifier)))
                                     (match-set xcs)))
         (sum-of-F (sum classifiers :key 'fitness))
         (sum-of-p*F (sum classifiers
                          :key #'(lambda (classifier)
                                   (* (prediction classifier)
                                      (fitness classifier))))))
    (if (zerop sum-of-F)
      sum-of-p*F
      (/ sum-of-p*F sum-of-F))))

(defmethod select-action-random ((xcs xcs))
  (setf (action xcs)
        (random-element (actions-in (match-set xcs)))))

(defmethod select-action-best ((xcs xcs))
  (setf (action xcs)
        (best (actions-in (match-set xcs))
              #'>
              :key #'(lambda (action)
                       (system-prediction xcs action)))))

;;; TODO: Do I want to change this?
(defmethod select-action-learning ((xcs xcs))
  "This is the ``SELECT ACTION'' function in Butz and Wilson's paper.  In this
  version, there is a random possibility of pure exploration, that is, picking
  a random action out of the prediction array, versus picking the 'best' action
  in the prediction array.  Other action selection methods may be valid: see
  the paper for more information."
  (with-slots (learning-parameters match-set) xcs
    (setf (action xcs)
          (if (probability? (exploration-probability learning-parameters))
            (select-action-random xcs)
            (select-action-best xcs)))))

(defmethod generate-action-set ((xcs xcs))
  "This is the ``GENERATE ACTION SET'' function in Butz and Wilson's paper.
  It forms the action set [A] out of the match set [M], all of the classifiers
  that match the selected action."
  (with-slots (match-set action-set previous-action-set action) xcs
    (setf previous-action-set action-set
          action-set (remove-if-not #'(lambda (classifier)
                                        (equal action (action classifier)))
                                    match-set))))

(defmethod most-general-subsumers ((xcs xcs))
  "This function returns the most general classifiers that are capable of
  subsumption and is used in the DO-ACTION-SET-SUBSUMPTION function."
  (with-slots (learning-parameters action-set) xcs
    (flet ((subsumer? (classifier)
                      (could-subsume? classifier learning-parameters))
           (key (classifier)
                (reduce #'+ (environment-condition classifier)
                        :key (rcurry 'covering-score learning-parameters))))
      (let ((best (best (remove-if-not #'subsumer? action-set) #'> :key #'key)))
        (flet ((selected (classifier)
                         (more-general? best classifier)))
          (when best (remove-if-not #'selected action-set)))))))

(defmethod do-action-set-subsumption ((xcs xcs))
  "This is the function ``DO ACTION SET SUBSUMPTION'' function in Butz and
  Wilson's paper.  The function chooses the subsumer from the most general
  classifiers capable of subsumption and then subsumes all possible classifiers
  in to the subsumer."
  (let ((subsumer (random-element (most-general-subsumers xcs))))
    (when subsumer
      (dolist (classifier (action-set xcs))
        (when (more-general? subsumer classifier)
          (incf (numerosity subsumer) (numerosity classifier))
          (deletef classifier (action-set xcs) :test 'identical?)
          (deletef classifier (population xcs) :test 'identical?))))))

(defmethod update-fitness ((xcs xcs))
  "This is the ``UPDATE FITNESS'' function in Butz and Wilson's paper.
  The fitness of all of the classifiers in the action set are updated in a
  normalized manner."
  (with-slots (learning-parameters action-set) xcs
    (with-slots (equal-error-threshold
                  learning-rate
                  multiplier-parameter
                  power-parameter) learning-parameters
      (dolist (classifier action-set)
        (setf (accuracy classifier)
          (if (< (prediction-error classifier) equal-error-threshold)
            1
            (* multiplier-parameter
               (expt (/ (prediction-error classifier) equal-error-threshold)
                     (- power-parameter))))))
      (let ((accuracy-sum (sum action-set
                               :key #'(lambda (classifier)
                                        (* (accuracy classifier)
                                           (numerosity classifier))))))
        (dolist (classifier action-set)
          (with-slots (fitness accuracy numerosity) classifier
            (incf fitness
                  (* learning-rate (- (* accuracy numerosity (/ accuracy-sum))
                                      fitness)))))))))

(defmethod update-set ((xcs xcs) &optional (action-set nil))
  "This is the ``UPDATE SET'' function in Butz and Wilson's paper.
  It updates the parameters for classifiers in the action set."
  (with-slots (learning-parameters population payoff) xcs
    (with-slots (learning-rate action-set-subsumption?) learning-parameters
      (when (eql action-set nil)
	(setf action-set (action-set xcs)))
      (let ((sum-of-numerosities (sum action-set :key 'numerosity)))
        (dolist (classifier action-set)
          (with-slots (experience prediction 
                        action-set-size prediction-error) classifier
            (incf experience)
            (if (< experience (/ learning-rate))
              (progn (incf prediction (/ (- payoff prediction) experience))
                     (incf prediction-error (/ (- (abs (- payoff prediction))
                                                  prediction-error)
                                               experience))
                     (incf action-set-size (/ (- sum-of-numerosities
                                                 action-set-size)
                                              experience)))
              (progn (incf prediction (* learning-rate
                                         (- payoff prediction)))
                     (incf prediction-error (* learning-rate
                                               (- (abs (- payoff prediction))
                                                  prediction-error)))
                     (incf action-set-size (* learning-rate
                                              (- sum-of-numerosities
                                                 action-set-size))))))))
      (update-fitness xcs)
      (when action-set-subsumption?
        (do-action-set-subsumption xcs)))))

(defmethod two-point-crossover ((p sequence) (q sequence))
  "This is basic random two-point crossover.  The two points are allowed to
  degenerate to the exact same point in this version."
  (let* ((minimum-length (min (length p) (length q)))
         (crossover-points (sort (list (random minimum-length)
                                       (random minimum-length))
                                 #'<)))
    (loop for i from (first crossover-points) to (second crossover-points)
          do (swap (elt p i) (elt q i)))))

(defmethod crossover ((p classifier) (q classifier) (xcs xcs))
  "This is the ``APPLY CROSSOVER'' function in Butz and Wilson's paper.  It
  applies the chosen method of crossover, in this case two-point crossover."
  (flet ((midpoint (x y)
           (/ (+ x y) 2)))
    (when (probability? (crossover-probability (learning-parameters xcs)))
      (two-point-crossover (environment-condition p)
                           (environment-condition q))
      (let ((new-prediction (midpoint (prediction p) (prediction q)))
            (new-prediction-error (midpoint (prediction-error p)
                                            (prediction-error q)))
            (new-fitness (midpoint (fitness p) (fitness q))))
        (setf (prediction p) new-prediction
              (prediction q) new-prediction
              (prediction-error p) new-prediction-error
              (prediction-error q) new-prediction-error
              (fitness p) new-fitness
              (fitness q) new-fitness)))))

(defmethod select-offspring ((xcs xcs))
  "This is the ``SELECT OFFSPRING'' function in Butz and Wilson's paper.
  It uses a roulette method of selection."
  (with-slots (action-set) xcs
    (let ((choice-point (* (random 1.0) (sum action-set :key 'fitness)))
          (fitness-sum 0.0))
      (dolist (classifier action-set)
        (incf fitness-sum (fitness classifier))
        (when (> fitness-sum choice-point)
          (return-from select-offspring classifier))))))

(defmethod mutate ((classifier classifier)
                   situation
                   (learning-parameters learning-parameters))
  "This is the ``APPLY MUTATION'' function in Butz and Wilson's paper."
  (with-slots (mutation-probability possible-actions) learning-parameters
    (with-slots (environment-condition) classifier
      (dotimes (i (min (length environment-condition)
                       (length situation)))
        (mutate (elt environment-condition i)
                (elt situation i)
                learning-parameters))
      (when (probability? mutation-probability))
        (setf (action classifier)
              (random-element possible-actions)))))

(defmethod insert-into-population ((classifier classifier) (xcs xcs))
  "This is the ``INSERT IN POPULATION'' function in Butz and Wilson's paper.
  It is basically just a modified PUSH to take into account the numerosity."
  (with-slots (population) xcs
    (let ((individual (find-if #'(lambda (individual)
                                   (identical? classifier individual))
                               population)))
      (if individual
        (incf (numerosity individual)
              (numerosity classifier))
        (push classifier population)))))

(defmethod more-general? ((general sequence) (specific sequence))
  (and (match? general specific)
       (some 'more-general? general specific)))

(defmethod more-general? ((general classifier) (specific classifier))
  "This is the \"IS MORE GENERAL\" function in Butz and Wilson's paper.  It
  returns true iff GENERAL is more general than SPECIFIC."
  (more-general? (environment-condition general)
                 (environment-condition specific)))

(defmethod run-ga? ((xcs xcs))
  "This predicate decides if we should run the GA or not."
  (with-slots (learning-parameters action-set) xcs
    (and (plusp (length action-set))
         (< (GA-threshold learning-parameters)
            (- (number-of-situations xcs)
               (/ (sum action-set :key #'(lambda (classifier)
                                           (* (time-stamp classifier)
                                              (numerosity classifier))))
                  (sum action-set :key 'numerosity)))))))

(defmethod run-ga ((xcs xcs) situation &optional (action-set nil))
  "This is the ``RUN GA'' function in Butz and Wilson's paper.  It runs a
  genetic algorithm not on the full population [P], but instead only on the
  action set [A] in order to induce niching."
  (with-slots (learning-parameters population) xcs
    (with-slots (GA-subsumption?) learning-parameters
      (when (run-ga? xcs)
	(when (eql action-set nil)
	  (setf action-set (action-set xcs)))
        (dolist (classifier action-set)
          (setf (time-stamp classifier)
                (number-of-situations xcs)))
        (let* ((parent-1 (select-offspring xcs))
               (parent-2 (select-offspring xcs))
               (child-1 (duplicate parent-1))
               (child-2 (duplicate parent-2)))
          (assert (and child-1 child-2))
          (setf (numerosity child-1) 1
                (numerosity child-2) 1
                (experience child-1) 0
                (experience child-2) 0)
          (crossover child-1 child-2 xcs)
          (dolist (child (list child-1 child-2))
            (multf (fitness child) 0.1)
            (mutate child situation learning-parameters)
            (cond ((and GA-subsumption?
                        (subsume? parent-1 child learning-parameters))
                   (incf (numerosity parent-1)))
                  ((and GA-subsumption?
                        (subsume? parent-2 child learning-parameters))
                   (incf (numerosity parent-2)))
                  (t (insert-into-population child xcs)))
	    (delete-from-population xcs)))))))

(defmethod get-situation ((experiment experiment))
  (with-slots (xcs environment) experiment
    (with-slots (situation previous-situation) xcs
      (setf previous-situation situation
            situation (get-situation environment))
      (incf (number-of-situations xcs)))))

(defmethod get-reward ((experiment experiment))
  (with-slots (reward previous-reward) (xcs experiment)
    (setf previous-reward reward
          reward (get-reward (reinforcement-program experiment)))))

;;;; New stuff from Butz's xcs.c starts here.

(defmethod multi-step-explore ((experiment experiment))
  (with-slots (xcs environment reinforcement-program) experiment
    (with-slots (learning-parameters
                  match-set
                  action previous-action-set action-set
                  previous-reward reward payoff
                  previous-situation situation) xcs
      (with-slots (maximum-number-of-steps discount-factor) learning-parameters
        (setf previous-action-set nil
	      action-set nil)
      (dotimes (step (maximum-number-of-steps learning-parameters))
	(get-situation experiment)
	(generate-match-set xcs)
	(select-action-learning xcs)
	(generate-action-set xcs)
	(execute-action environment action)
	(get-reward experiment)
	(when previous-action-set
	    (setf payoff
                  (+ previous-reward
                     (* discount-factor
                        (apply #'max
                               (mapcar #'(lambda (action)
                                           (system-prediction xcs action))
                                       (actions-in match-set))))))
	    (update-set xcs previous-action-set)
	    (run-ga xcs previous-situation previous-action-set))
	(when (end-of-problem? reinforcement-program)
	      (setf payoff reward)
	      (update-set xcs)
	      (run-ga xcs situation)
	      (setf previous-action-set nil)
	      (return-from multi-step-explore)))))))

(defmethod multi-step-exploit ((experiment experiment))
  (with-slots (xcs environment reinforcement-program) experiment
    (with-slots (learning-parameters) xcs
      (dotimes (step (maximum-number-of-steps learning-parameters))
	(get-situation experiment)
	(generate-match-set xcs)
	(select-action-best xcs)
	(execute-action environment (action xcs))
	(get-reward experiment)
	(when (end-of-problem? reinforcement-program)
	  (return-from multi-step-exploit))))))

(defmethod multi-step-experiment ((experiment experiment))
  (with-slots (xcs number-of-trials environment) experiment
    (with-slots (explore?) xcs
      (dotimes (trial number-of-trials)
	(reset environment)
	(if explore?
          (multi-step-explore experiment)
          (multi-step-exploit experiment))))))

(defmethod single-step-explore ((experiment experiment))
  (with-slots (xcs environment) experiment
    (with-slots (action reward payoff situation match-set action-set) xcs
      (generate-match-set xcs)
      ; generate prediction array
      (select-action-learning xcs)
      (execute-action environment action)
      (get-reward experiment)
      (generate-action-set xcs)
      (setf payoff reward)
      (update-set xcs)
      (run-ga xcs situation)
      (setf match-set nil
            action-set nil))))

(defmethod single-step-exploit ((experiment experiment))
  (with-slots (xcs environment) experiment
    (generate-match-set xcs)
    ; generate prediction array
    (select-action-best xcs)
    (execute-action environment (action xcs))
    (get-reward experiment)
    (setf (match-set xcs) nil)))

(defmethod single-step-experiment ((experiment experiment))
  (with-slots (xcs environment number-of-trials) experiment
    (with-slots (explore? situation learning-parameters) xcs
      (with-slots (exploration-probability) learning-parameters
        (do-until (terminate? experiment)
          (setf explore? (probability? exploration-probability))
          (get-situation experiment)
          (if explore?
            (single-step-explore experiment)
            (single-step-exploit experiment))
          (single-step-output experiment))))))

#|
(defmethod single-step-experiment ((experiment experiment))
  (with-slots (xcs environment number-of-trials) experiment
    (with-slots (explore? situation) xcs
      (setf explore? nil)
      (do-until (terminate? experiment)
        (get-situation experiment)
        (single-step-explore experiment)
        (single-step-output experiment)))))
|#

(defmethod start ((experiment experiment))
  "This starts an XCS experiment."
  ;; CLisp apparently doesn't initialize the *RANDOM-STATE* nicely for us like
  ;; CMUCL, so we initialize here ourselves.
  (setf *random-state* (make-random-state t))
  (with-slots (environment reinforcement-program xcs) experiment
    (initialize environment)
    (initialize reinforcement-program)
    (initialize xcs)
    (if (multistep? environment)
      (multi-step-experiment experiment)
      (single-step-experiment experiment))))

(defmethod start-experiments ((experiment experiment)
			      (number-of-experiments integer))
  (dotimes (experiment-number number-of-experiments)
    (start experiment)))

(defmethod main ((experiment experiment) &optional (number-of-experiments 1))
  (with-slots (environment) experiment
    (start-experiments environment number-of-experiments)))
