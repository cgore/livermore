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
  :depends-on ("md5"
               "clsql" ; https://github.com/sharplispers/clsql
               "clsql-postgresql-socket"
               "sigma")
  :homepage "https://github.com/cgore/livermore"
  :source-control (:git "https://github.com/cgore/livermore.git")
  :bug-tracker "https://github.com/cgore/livermore/issues"
  :components ((:module "source"
                :components ((:file "csv")
                             (:file "learning-parameters")
                             (:file "multiplexer")
                             (:file "statistics")
                             (:file "stock-ticker-descriptions")
                             (:file "stocks" :depends-on ("csv" "statistics" "stock-ticker-descriptions" "time"))
                             (:file "threshold")
                             (:file "thesis-stats" :depends-on ("statistics"))
                             (:file "time")
                             (:file "xcs-range-predicate"   :depends-on ("learning-parameters"))
                             (:file "xcs-set-predicate"     :depends-on ("learning-parameters"))
                             (:file "xcs-ternary-predicate" :depends-on ("learning-parameters"))
                             (:file "whitley-test-functions")
                             ;;(:file "xcs" :depends-on ("learning-parameters" "xcs-ternary-predicate"))
                             ))))
