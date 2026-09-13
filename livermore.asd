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
(defparameter version-minor 1)
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
  :depends-on ("sigma")
  :homepage "https://github.com/cgore/livermore"
  :source-control (:git "https://github.com/cgore/livermore.git")
  :bug-tracker "https://github.com/cgore/livermore/issues"

  ;; Specs live in the sources as BEHAVIOR/SHOULD forms and run at load time.
  ;; TEST-OP reloads every source file so those top-level assertions run again.
  :in-order-to ((test-op (load-op "livermore")))
  :perform (test-op (operation system)
                    (declare (ignore operation))
                    (labels ((reload (component)
                               (typecase component
                                 (cl-source-file
                                  (load (component-pathname component)))
                                 (parent-component
                                  (map nil #'reload (component-children component))))))
                      (reload system)))

  :components ((:module "source"
                :components
                ((:file "csv")
                 (:file "learning-parameters")
                 (:file "multiplexer")
                 (:file "statistics")
                 (:file "stock-ticker-descriptions")
                 (:file "time")
                 (:file "threshold")
                 (:file "thesis-stats"              :depends-on ("statistics"))
                 (:file "whitley-test-functions")
                 (:file "xcs-predicate"             :depends-on ("learning-parameters"))
                 (:file "xcs-set-predicate"         :depends-on ("learning-parameters"
                                                                 "xcs-predicate"))
                 (:file "xcs-ternary-predicate"     :depends-on ("learning-parameters"
                                                                 "xcs-predicate"))
                 (:file "xcs"                       :depends-on ("learning-parameters"
                                                                 "xcs-predicate"
                                                                 "xcs-set-predicate"
                                                                 "xcs-ternary-predicate"))
                 (:file "xcs-analyzer"              :depends-on ("xcs"))
                 (:file "xcsr"                      :depends-on ("learning-parameters"
                                                                 "xcs"))
                 (:file "tmscs"                     :depends-on ("learning-parameters"
                                                                 "xcs"
                                                                 "xcsr"))
                 (:file "stocks"                    :depends-on ("csv"
                                                                 "statistics"
                                                                 "stock-ticker-descriptions"
                                                                 "time"))
                 (:file "animat-xcs-parameters"     :depends-on ("learning-parameters"
                                                                 "xcs"))
                 (:file "animat-xcs"                :depends-on ("xcs"
                                                                 "animat-xcs-parameters"))
                 (:file "stocks-xcs-parameters"     :depends-on ("learning-parameters"
                                                                 "xcs"))
                 (:file "stocks-xcs"                :depends-on ("statistics"
                                                                 "stocks"
                                                                 "stocks-xcs-parameters"
                                                                 "time"
                                                                 "xcs"
                                                                 "xcs-analyzer"))
                 (:file "multiplexer-xcs-parameters" :depends-on ("learning-parameters"
                                                                  "xcs"))
                 (:file "multiplexer-xcs"           :depends-on ("multiplexer"
                                                                 "multiplexer-xcs-parameters"
                                                                 "xcs"
                                                                 "xcs-analyzer"))
                 (:file "monk-xcs-parameters"       :depends-on ("learning-parameters"
                                                                 "xcs"))
                 (:file "monk-xcs"                  :depends-on ("monk-xcs-parameters"
                                                                 "xcs"
                                                                 "xcs-set-predicate"
                                                                 "xcs-ternary-predicate"))
                 (:file "linear-tmscs-parameters"   :depends-on ("tmscs"))
                 (:file "linear-tmscs"              :depends-on ("linear-tmscs-parameters"
                                                                 "statistics"
                                                                 "tmscs"
                                                                 "xcs"))
                 (:file "inde-tmscs-parameters"     :depends-on ("tmscs"))
                 (:file "inde-tmscs"                :depends-on ("inde-tmscs-parameters"
                                                                 "statistics"
                                                                 "tmscs"
                                                                 "xcs"))
                 (:file "sawtooth-tmscs-parameters" :depends-on ("tmscs"))
                 (:file "sawtooth-tmscs"            :depends-on ("sawtooth-tmscs-parameters"
                                                                 "statistics"
                                                                 "tmscs"
                                                                 "xcs"))
                 (:file "multislope-tmscs-parameters" :depends-on ("tmscs"))
                 (:file "multislope-tmscs"          :depends-on ("multislope-tmscs-parameters"
                                                                 "statistics"
                                                                 "tmscs"
                                                                 "xcs"))
                 (:file "ikeda-tsc-parameters"      :depends-on ("tmscs"))
                 (:file "ikeda-tsc"                 :depends-on ("ikeda-tsc-parameters"
                                                                 "statistics"
                                                                 "tmscs"
                                                                 "xcs"))
                 (:file "threshold-xcsr-parameters" :depends-on ("xcsr"))
                 (:file "threshold-xcsr"            :depends-on ("threshold"
                                                                 "threshold-xcsr-parameters"
                                                                 "xcs"
                                                                 "xcs-analyzer"
                                                                 "xcsr"))
                 (:file "stocks-xcsr-parameters"    :depends-on ("xcsr"))
                 (:file "stocks-xcsr"               :depends-on ("stocks"
                                                                 "stocks-xcsr-parameters"
                                                                 "xcs"
                                                                 "xcs-analyzer"
                                                                 "xcsr"))
                 (:file "stocks-tsc-parameters"     :depends-on ("stocks"
                                                                 "tmscs"))
                 (:file "stocks-tsc"                :depends-on ("statistics"
                                                                 "stocks"
                                                                 "stocks-tsc-parameters"
                                                                 "time"
                                                                 "tmscs"
                                                                 "xcs"))
                 (:file "livermore"                 :depends-on ("animat-xcs"
                                                                 "csv"
                                                                 "ikeda-tsc"
                                                                 "inde-tmscs"
                                                                 "learning-parameters"
                                                                 "linear-tmscs"
                                                                 "monk-xcs"
                                                                 "multiplexer"
                                                                 "multiplexer-xcs"
                                                                 "multislope-tmscs"
                                                                 "sawtooth-tmscs"
                                                                 "statistics"
                                                                 "stock-ticker-descriptions"
                                                                 "stocks"
                                                                 "stocks-tsc"
                                                                 "stocks-xcs"
                                                                 "stocks-xcsr"
                                                                 "thesis-stats"
                                                                 "threshold"
                                                                 "threshold-xcsr"
                                                                 "time"
                                                                 "tmscs"
                                                                 "whitley-test-functions"
                                                                 "xcs"
                                                                 "xcs-analyzer"
                                                                 "xcsr"))))))

(defsystem "livermore/trade-chart"
  :description "McCLIM candlestick/bar chart for Livermore stock tables."
  :version #.(version-string)
  :author "Christopher Mark Gore <cgore@cgore.com>"
  :license "BSD-3-Clause"
  :depends-on ("livermore" "mcclim")
  :components ((:module "source"
                :components ((:file "trade-chart")))))
