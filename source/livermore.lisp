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

;;; The Livermore umbrella package.  It re-exports every public symbol from
;;; the sub-packages, so that (use-package :livermore) gives you the whole
;;; library at once.

(uiop:define-package :livermore
  (:use :common-lisp
        :sigma/behave
        :livermore/system)
  (:import-from :livermore/animat-xcs :start-animat-experiment)
  (:import-from :livermore/multiplexer-xcs :start-multiplexer-experiment)
  (:import-from :livermore/monk-xcs :start-monk)
  (:import-from :livermore/linear-tmscs :start-linear-tmscs-experiment)
  (:import-from :livermore/inde-tmscs :start-inde-tmscs-experiment)
  (:import-from :livermore/sawtooth-tmscs :start-sawtooth-tmscs-experiment)
  (:import-from :livermore/multislope-tmscs :start-multislope-tmscs-experiment)
  (:import-from :livermore/ikeda-tsc :start-ikeda-tmscs-experiment)
  (:import-from :livermore/threshold-xcsr :start-threshold-experiment)
  (:import-from :livermore/stocks-xcs :start-stocks-xcs-experiment)
  (:import-from :livermore/stocks-xcsr :start-stocks-xcsr-experiment)
  (:import-from :livermore/stocks-tsc :start-stocks-tsc-experiment)
  (:export :start-animat-experiment
           :start-multiplexer-experiment
           :start-monk
           :start-linear-tmscs-experiment
           :start-inde-tmscs-experiment
           :start-sawtooth-tmscs-experiment
           :start-multislope-tmscs-experiment
           :start-ikeda-tmscs-experiment
           :start-threshold-experiment
           :start-stocks-xcs-experiment
           :start-stocks-xcsr-experiment
           :start-stocks-tsc-experiment)
  (:reexport :livermore/system
             :livermore/csv
             :livermore/learning-parameters
             :livermore/multiplexer
             :livermore/statistics
             :livermore/stock-ticker-descriptions
             :livermore/time
             :livermore/threshold
             :livermore/thesis-stats
             :livermore/whitley-test-functions
             :livermore/xcs
             :livermore/xcs-analyzer
             :livermore/xcsr
             :livermore/tmscs
             :livermore/stocks))
(in-package :livermore)

(defun umbrella-exports-p (name)
  "True when NAME is an external symbol of the LIVERMORE package."
  (eq :external (nth-value 1 (find-symbol name (find-package :livermore)))))

(behavior 'version-string
  (should-equal '(1 1 0) (version-list))
  (should-string= "1.1.0" (version-string)))

(behavior 'use-all-symbols
  (should-be-true (umbrella-exports-p "XCS"))
  (should-be-true (umbrella-exports-p "XCSR"))
  (should-be-true (umbrella-exports-p "TMSCS"))
  (should-be-true (umbrella-exports-p "SIMPLE-SLOPE"))
  (should-be-true (umbrella-exports-p "LOAD-TABLE"))
  (should-be-true (umbrella-exports-p "START-INDE-TMSCS-EXPERIMENT"))
  (should-be-true (umbrella-exports-p "START-STOCKS-TSC-EXPERIMENT"))
  (should-be-true (umbrella-exports-p "START-MULTIPLEXER-EXPERIMENT"))
  (should-be-true (umbrella-exports-p "START-ANIMAT-EXPERIMENT"))
  (should-be-true (umbrella-exports-p "MULTIPLEXER"))
  (should-be-true (umbrella-exports-p "THRESHOLD-INDICATOR"))
  (should-be-true (umbrella-exports-p "STOCK-DESCRIPTION"))
  (should-be-true (umbrella-exports-p "ARITHMETIC-MEAN"))
  (should-be-true (umbrella-exports-p "RANGE-PREDICATE"))
  (should-be-true (umbrella-exports-p "CLASSIFIER"))
  (should-be-false (umbrella-exports-p "SHOULD=")))
