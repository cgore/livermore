;;;; Christopher Mark Gore.
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281.
;;;; WWW: <http://www.cgore.com>.
;;;; E-mail: <chris-gore@earthlink.net>.
;;;;
;;;; Last edited Monday, April 17, AD 2006; xb.
;;;;
;;;; Copyright (C) 2005, 2006.
;;;; All rights reserved.
;;;;
;;;; The compilation of software is distributed under the following terms:
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.

(load "utilities.lisp")
(in-package "UTILITIES")

(defun raster-line.unit-test ()
  (assert (equalp '((1 1) (2 2))
                  (raster-line '(1 1) '(2 2))))
  (assert (equalp '((1 1) (2 2) (3 3) (4 4))
                  (raster-line '(1 1) '(4 4))))
  (assert (equalp '((1 1) (2 2) (3 3) (4 4) (5 5))
                  (raster-line '(1 1) '(4 4) :from-end 1)))
  (assert (equalp '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8))
                  (raster-line '(1 1) '(4 4) :from-end 4)))
  (assert (equalp '((1 1) (1 2) (2 3) (2 4))
                  (raster-line '(1 1) '(2 4))))
  (assert (equalp '((2 2) (1 1))
                  (raster-line '(2 2) '(1 1))))
  (assert (equalp '((4 4) (3 3) (2 2) (1 1))
                  (raster-line '(4 4) '(1 1))))
  (assert (equalp '((1 1))
                  (raster-line '(1 1) '(1 1))))
  (assert (equalp '((-1 -1) (0 0) (1 1))
                  (raster-line '(-1 -1) '(1 1))))
  (assert (equalp '((-3 -3) (-2 -2) (-1 -1) (0 0))
                  (raster-line '(-3 -3) '(0 0))))
  (assert (equalp '((-2 2) (-1 1) (0 0) (1 -1) (2 -2))
                  (raster-line '(-2 2) '(2 -2))))
  (assert (equalp nil
                  (raster-line '(1 1) '(4 4) :from-end -4)))
  (assert (equalp nil
                  (raster-line '(1 1) '(4 4) :from-end -10)))
  (assert (equalp '((0 0) (1 1) (2 2) (3 3) (4 4))
                  (raster-line '(1 1) '(4 4) :from-start -1)))
  (assert (equalp '((2 2) (3 3) (4 4))
                  (raster-line '(1 1) '(4 4) :from-start 1)))
  (assert (equalp '((-2 -2) (-1 -1) (0 0) (1 1) (2 2) (3 3) (4 4))
                  (raster-line '(1 1) '(4 4) :from-start -3)))
  (assert (equalp '((3 3) (4 4))
                  (raster-line '(1 1) '(4 4) :from-start 2)))
  (assert (equalp nil
                  (raster-line '(1 1) '(4 4) :from-start 10)))
  (assert (equalp nil
                  (raster-line '(1 1) '(4 4) :from-start 2 :from-end -2))))

(defun unit-test ()
  (raster-line.unit-test))
