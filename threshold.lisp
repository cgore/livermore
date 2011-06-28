(load "utilities")
(unless (find-package "THRESHOLD")
  (defpackage "THRESHOLD"
    (:use "COMMON-LISP" "UTILITIES")))
(in-package "THRESHOLD")
(export '(threshold-indicator))

(defmethod threshold-indicator ((threshold list) (instance list))
  (assert (= (length threshold)
             (length instance)))
  (mapcar #'(lambda (threshold instance)
              (when (< instance threshold)
                (return-from threshold-indicator nil)))
          threshold instance)
  t)

(defmethod threshold-indicator ((threshold vector) (instance vector))
  (threshold-indicator (vector-to-list threshold) (vector-to-list instance)))

(defmethod threshold-indicator ((threshold number) (instance number))
  (> instance threshold))
