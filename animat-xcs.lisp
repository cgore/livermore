(load "utilities/utilities")
(load "xcs")
(in-package "XCS")
(use-package '("COMMON-LISP" "UTILITIES"))
(export '(animat-analyzer
           current-situation
           initialize
           get-situation
           execute-action
           get-reward
           end-of-problem?
           start-animat-experiment))

(load "animat-xcs-parameters.lisp")

(defclass animat-analyzer (environment reinforcement-program)
  ((file-name
     :accessor file-name
     :initarg :file-name)
   (current-situation
     :accessor current-situation
     :initarg :current-situation
     :type vector)
   (x-location
     :accessor x-location
     :initarg :x-location
     :initform 0)
   (y-location
     :accessor y-location
     :initarg :y-location
     :initform 0)
   (world-width
     :accessor world-width
     :initarg :world-width
     :initform 5)
   (world-height
     :accessor world-height
     :initarg :world-height
     :initform 5)
   (world
     :accessor world
     :initarg :world)
   (number-of-situations
     :accessor number-of-situations
     :initform 0
     :initarg :number-of-situations
     :type integer)
   (current-step-count
     :accessor current-step-count
     :initarg :current-step-count
     :initform 0
     :type integer)
   (current-action
     :accessor current-action
     :initarg :current-action)
   (number-of-problems
      :accessor number-of-problems
      :initarg :number-of-problems
      :initform 0)
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

(defclass animat-experiment (experiment)
  ())

(defmethod initialize ((analyzer animat-analyzer))
  "Reads the file given by file-name into the world array"
  (with-slots (world-width 
	       world-height 
	       world file-name
	       multistep?) analyzer
    (setf multistep? t)
    (with-open-file (stream file-name
		      :direction :input
		      :element-type 'character)
      (setf world-width (read stream))
      (setf world-height (read stream))
      
      (setf world (make-array (list world-width world-height)))
      (loop for i from 0 to (1- world-height) do
	    (loop for j from 0 to (1- world-width) do
	      (setf (aref world j i)
		    (case (read-char stream)
		      (#\* :*) ; Empty
		      (#\O :O) ; Rock 1
		      (#\F :F) ; Food 1
		      (#\Q :Q) ; Rock 2
		      (#\G :G) ; Food 2
		      (t :B))))
	    (read-line stream)))))

(defmethod reset ((analyzer animat-analyzer))
  "Re-initialize the animat at a random open location in the world"
  (with-slots (world x-location y-location
	       world-width world-height
	       current-step-count
	       number-of-problems) analyzer
    (when (> number-of-problems 0)
      (update-histories analyzer))
    (setf current-step-count 0)
    (incf number-of-problems)
    (loop do
      (let ((x (random world-width))
	    (y (random world-height)))
	(when (eql (aref world x y) :*)
	  (setf x-location x
		y-location y)
	  (return-from reset))))))

(defun sensor-code (symb)
  "Return the 3-bit sensor code corresponding to the given symbol"
  (case symb
    (:F (list t t nil))
    (:G (list t t t))
    (:O (list nil t nil))
    (:Q (list nil t t))
    (:* (list nil nil nil))))

(defmethod multistep? ((analyzer animat-analyzer))
  t)
		      
(defmethod get-situation ((analyzer animat-analyzer))
  "Get the current-situation which is represented by a 24-element
   truth list, which contains the 3-element sensor codes for the 8
   locations nearest the current location. The codes are arranged in
   counter-clockwise order starting from directly north. Boundaries
   wrap around to the other side."
  (with-slots (current-situation 
	       world 
	       x-location y-location
	       world-width world-height) analyzer
    (setf current-situation nil)
    (dolist (delta
	     (list '(0 -1) '(1 -1) '(1 0)
		   '(1 1) '(0 1) '(-1 1)
		   '(-1 0) '(-1 -1)))
      (let ((new-x (+ x-location (first delta)))
	    (new-y (+ y-location (second delta))))
	(snap-index new-x world-width)
	(snap-index new-y world-height)
	(nconcf current-situation
		(sensor-code (aref world new-x new-y)))))
    current-situation))
	
(defmethod execute-action ((analyzer animat-analyzer) (action integer))
  "Moves the animat in the given direction. Edges wrap around to the
   other side, and trying to move into a rock is simply a null
   operation."
  (with-slots (world 
	       x-location y-location
	       world-width world-height
	       current-action current-step-count) analyzer
    (setf current-action action)
    (incf current-step-count)
    (let ((deltas (list '(0 -1) '(1 -1) '(1 0)
		   '(1 1) '(0 1) '(-1 1)
		   '(-1 0) '(-1 -1))))
      (let* ((current-delta (elt deltas action))
	     (new-x (+ x-location (first current-delta)))
	     (new-y (+ y-location (second current-delta))))
	(snap-index new-x world-width)
	(snap-index new-y world-height)
	(case (aref world new-x new-y)
	  (:O nil) ; Rock, don't move
	  (:Q nil) ; Rock, don't move
	  (t
	   (setf x-location new-x)
	   (setf y-location new-y)))))))

(defmethod update-histories ((analyzer animat-analyzer))
  (with-slots (number-of-problems action-history
	       odd-action-history even-action-history
	       current-step-count) analyzer
    (if (oddp number-of-problems)
	(push current-step-count odd-action-history)
        (push current-step-count even-action-history))
    (push current-step-count action-history)

    (when (>= number-of-problems 2)
      (let*((steps50len (min 50 (length action-history)))
	    (steps50 (sum action-history :end steps50len))
	    (stepso50len (min 50 (length odd-action-history)))
	    (stepso50 (sum odd-action-history :end stepso50len))
	    (stepse50len (min 50 (length even-action-history)))
	    (stepse50 (sum even-action-history :end stepse50len)))
	(format t "~%Last 50: Ave.: ~F, Odd: ~F, Even: ~F"
		(/ steps50 steps50len)
		(/ stepso50 stepso50len)
		(/ stepse50 stepse50len))))))

(defmethod get-reward ((analyzer animat-analyzer))
  "Get reward based on the current location of the animat. It gets
   a reward of 1000 for being on food and a reward of 0 for all else."
  (with-slots (world 
	       x-location y-location
	       current-step-count
	       number-of-problems) analyzer
    (let ((current-square (aref world x-location y-location)))
      (if (or (eql current-square :F)
	      (eql current-square :G))
	  1000
	  0))))

(defmethod end-of-problem? ((analyzer animat-analyzer))
  "The problem ends when the animat is located on food."
    (with-slots (world x-location y-location) analyzer
    (let ((current-square (aref world x-location y-location)))
      (if (or (eql current-square :F)
	      (eql current-square :G))
	  t
	  nil))))

(defun start-animat-experiment (file-name)
  (defparameter *animat-analyzer*
    (make-instance 'animat-analyzer
                   :file-name file-name))
  (defparameter *animat-xcs*
    (make-instance 'xcs
                   :learning-parameters *animat-learning-parameters*))
  (defparameter *animat-experiment*
    (make-instance 'animat-experiment
                   :environment *animat-analyzer*
                   :reinforcement-program *animat-analyzer*
                   :xcs *animat-xcs*))
  (start *animat-experiment*))
