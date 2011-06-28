(load "utilities")
(load "xcs")
(in-package "XCS")
(use-package '("COMMON-LISP" "UTILITIES"))
(export '(analyzer
           current-situation
           number-of-situations
           current-action
           actions
           correct-actions
           action-history
           odd-action-history
           even-action-history
           initialize ; Define per-problem
           correct-action ; Define per-problem
           correct-action?
           get-reward
           execute-action))

(defclass analyzer (environment reinforcement-program)
   ((current-situation
     :accessor current-situation
     :initarg :current-situation)
   (number-of-situations
     :accessor number-of-situations
     :initform 0
     :initarg :number-of-situations
     :type (integer 0 *))
   (current-action
     :accessor current-action
     :initarg :current-action)
   (actions
     :accessor actions
     :initform 0
     :initarg :actions
     :type (integer 0 *))
   (correct-actions
     :accessor correct-actions
     :initform 0
     :initarg :correct-actions
     :type (integer 0 *))
   (action-history
     :accessor action-history
     :initform nil
     :initarg :action-history
     :type list)
   (odd-action-history
     :accessor odd-action-history
     :initform nil
     :initarg :odd-action-history
     :type list)
   (even-action-history
     :accessor even-action-history
     :initform nil
     :initarg :even-action-history
     :type list)))

;(defmethod initialize ((analyzer analyzer))
;  "Define this method for any individual problem that needs initialization."
;  nil)

;(defmethod correct-action ((analyzer analyzer))
;  "Define this method for each individual problem."
;  nil)

(defmethod correct-action? ((analyzer analyzer))
  "This predicate returns true if the classifier picked the correct action."
  (equalp (current-action analyzer)
          (correct-action analyzer)))

(defmethod get-reward ((analyzer analyzer))
  "This is a very simple reward scheme that should do for simple problems."
  (if (correct-action? analyzer) 1000.0 0.0))

(defmethod execute-action ((analyzer analyzer) action)
  (with-slots (current-situation
                actions
                correct-actions
                current-action
                action-history
                odd-action-history
                even-action-history) analyzer
    (setf current-action action)
    (incf actions)
    (when (correct-action? analyzer)
      (incf correct-actions))
    (if (oddp actions)
      (push (correct-action? analyzer) odd-action-history)
      (push (correct-action? analyzer) even-action-history))
    (push (correct-action? analyzer) action-history)    
    (let ((ca50 (count t action-history :end 50))
          (ca50len (min 50 (length action-history)))
          (cao50 (count t odd-action-history :end 50))
          (cao50len (min 50 (length odd-action-history)))
          (cae50 (count t even-action-history :end 50))
          (cae50len (min 50 (length even-action-history))))
      (format t "~&~D/~D = ~,3F% (50: ~D/~D=~D% 50odd: ~D/~D=~D% 50even: ~D/~D=~D%): ~A --> ~A, ~A choice."
              correct-actions
              actions
              (* 100 (float (/ correct-actions
                               (if (zerop actions) 1 actions))))
              ca50
              ca50len
              (floor (* 100 (/ ca50 (if (zerop ca50len) 1 ca50len))))
              cao50
              cao50len
              (floor (* 100 (/ cao50 (if (zerop cao50len) 1 cao50len))))
              cae50
              cae50len
              (floor (* 100 (/ cae50 (if (zerop cae50len) 1 cae50len))))
              current-situation
              action
              (if (correct-action? analyzer) "correct" "incorrect")))))
