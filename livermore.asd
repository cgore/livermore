(defpackage :livermore/system
  (:use :common-lisp
        :asdf)
  (:export :version-string
           :version-list
           :version-major
           :version-minor
           :version-revision))

(in-package :livermore/system)

(defparameter version-major 1)
(defparameter version-minor 0)
(defparameter version-revision 0)

(defun version-list ()
  (list version-major version-minor version-revision))

(defun version-string ()
  (format nil "~{~A.~A.~A~}" (version-list)))

(defsystem "livermore"
  :description "Livermore is an evolutionary rules-based AI library in Common Lisp."
  :version #.(version-string)
  :author "Christopher Mark Gore <cgore@cgore.com>"
  :license "BSD-3-Clause"
  :homepage "https://github.com/cgore/livermore"
  :source-control (:git "https://github.com/cgore/livermore.git")
  :bug-tracker "https://github.com/cgore/livermore/issues"
  :components ((:module "source"
                :components (
                             ;; === Foundational utilities (no internal deps) ===
                             (:file "time")
                             (:file "statistics")
                             (:file "csv")
                             (:file "stocks")

                             ;; === Core XCS Framework ===
                             (:file "xcs"
                              :depends-on ("time" "statistics" "stocks"))

                             (:file "xcsr"
                              :depends-on ("xcs"))

                             (:file "xcs-analyzer"
                              :depends-on ("xcs" "xcsr"))

                             (:file "xcs-range-predicate"
                              :depends-on ("xcs"))
                             (:file "xcs-set-predicate"
                              :depends-on ("xcs"))
                             (:file "xcs-ternary-predicate"
                              :depends-on ("xcs"))

                             ;; === Core Experiment Frameworks ===
                             (:file "tmscs"
                              :depends-on ("xcsr"))

                             ;; === Specific Implementations ===
                             (:file "animat-xcs"
                              :depends-on ("xcs"))
                             (:file "monk-xcs"
                              :depends-on ("xcs"))
                             (:file "multiplexer"
                              :depends-on ("xcs"))
                             (:file "multiplexer-xcs"
                              :depends-on ("xcs" "multiplexer"))

                             (:file "threshold"
                              :depends-on ("xcs"))
                             (:file "threshold-xcsr"
                              :depends-on ("xcsr" "threshold"))

                             (:file "stocks-xcs"
                              :depends-on ("xcs" "stocks"))
                             (:file "stocks-xcsr"
                              :depends-on ("xcsr" "stocks-xcs"))
                             (:file "stocks-tsc"
                              :depends-on ("tmscs" "stocks"))

                             ;; === Parameter / Configuration Files (load last) ===
                             (:file "animat-xcs-parameters"
                              :depends-on ("animat-xcs"))
                             (:file "ikeda-tsc-parameters"
                              :depends-on ("tmscs"))           ; assuming ikeda-tsc exists
                             (:file "inde-tmscs-parameters"
                              :depends-on ("tmscs"))
                             (:file "linear-tmscs-parameters"
                              :depends-on ("tmscs"))
                             (:file "multiplexer-xcs-parameters"
                              :depends-on ("multiplexer-xcs"))
                             (:file "multislope-tmscs-parameters"
                              :depends-on ("tmscs"))
                             (:file "sawtooth-tmscs-parameters"
                              :depends-on ("tmscs"))
                             (:file "stocks-tsc-parameters"
                              :depends-on ("stocks-tsc"))
                             (:file "stocks-xcs-parameters"
                              :depends-on ("stocks-xcs"))
                             (:file "stocks-xcsr-parameters"
                              :depends-on ("stocks-xcsr"))
                             (:file "threshold-xcsr-parameters"
                              :depends-on ("threshold-xcsr"))

                             ;; === Supporting files ===
                             (:file "thesis-stats"
                              :depends-on ("xcs" "stocks"))
                             (:file "trade-chart"
                              :depends-on ("stocks"))
                             (:file "stock-ticker-descriptions")
                             (:file "whitley-test-functions"))
                ))))
