;;;; Copyright (c) 2005 -- 2014, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
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

(in-package "XCS")

(defparameter *stat-report* nil)
(defparameter *linear-tmscs-learning-parameters*
  (make-instance 'tmscs-learning-parameters
                 :valid-operations (list #'simple-slope)
                 :minimum-number-of-actions 2
                 :maximum-total-numerosity 2000 ; N = 400
                 :learning-rate 0.2 ; beta = 0.2
                 :discount-factor 0.71 ; gamma = 0.71
                 :GA-threshold 25 ; theta = 25
                 :equal-error-threshold 100.0 ; epsilon_0 = 10.0
                 :multiplier-parameter 0.1 ; alpha = 0.1
                 :crossover-probability 0.8 ; chi = 0.8
                 :mutation-probability 0.1 ; mu = 0.04
                 :exploration-probability 0.3
                 :fitness-fraction-threshold 0.1 ; delta = 0.1
                 ; phi = 0.5, covering multiplier
                 :covering-probability 0.33 ; P_# = 0.33
                 :initial-prediction 10.0 ; p_I = 10.0
                 :initial-prediction-error 0.0 ; epsilon_I = 0.0
                 :initial-fitness 0.01 ; F_I = 0.01
                 :minimum-number-of-actions 2
                 :GA-subsumption? nil
                 :action-set-subsumption? nil
                 :possible-actions '(nil t)))
