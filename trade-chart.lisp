;;;; Copyright (C) 2005 -- 2008, all rights reserved.
;;;; Christopher Mark Gore.
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281.
;;;; WWW: <http://www.cgore.com>.
;;;; E-mail: <chris-gore@earthlink.net>.
;;;; $Date: 2008-07-31 22:13:24 -0500 (Thu, 31 Jul 2008) $
;;;; $HeadURL: file:///var/svn/trading/trunk/trade-chart.lisp $
;;;; $Revision: 539 $

(require 'asdf)
(require 'clx)
(require 'clim)

(unless (find-package 'utilities) (load "utilities"))
(unless (find-package 'stocks) (load "stocks"))
(unless (find-package 'trade-chart)
  (defpackage :trade-chart
    (:use :clim-lisp ; N.B.: use this instead of common-lisp for McCLIM.
          :clim
          :clim-extensions
          :utilities
          :stocks)
    (:export :make-trade-chart :trade-chart :demo)))
(in-package :trade-chart)

(asdf:oos 'asdf:load-op 'mcclim)

(define-application-frame trade-chart ()
  ((x0 :accessor x0 :initarg :x0)
   (y0 :accessor y0 :initarg :y0)
   (x1 :accessor x1 :initarg :x1)
   (y1 :accessor y1 :initarg :y1)
   (up-ink :accessor up-ink :initarg :up-ink :initform +blue+)
   (down-ink :accessor down-ink :initarg :down-ink :initform +red+)
   (table :accessor table :initarg :data)
   (chart-style :accessor chart-style :initarg :chart-style :initform 'closing-line-chart))
  (:panes (chart :application :display-function 'display-chart)
          (interactor :interactor))
  (:layouts (default (vertically (:width 1017 :height 709)
                                 (:fill chart)
                                 (96 interactor)))))

(define-trade-chart-command
  (com-quit :name t) ()
  (frame-exit *application-frame*))

(defun rescale ()
  (setf (x0 *application-frame*)
        (apply #'min (opening-time (table *application-frame*)))
        (y0 *application-frame*)
        (apply #'min (adjusted-low-price (table *application-frame*)))
        (x1 *application-frame*)
        (apply #'max (closing-time (table *application-frame*)))
        (y1 *application-frame*)
        (apply #'max (adjusted-high-price (table *application-frame*)))))

(define-trade-chart-command
  (com-rescale :name t) ()
  (rescale))

(define-trade-chart-command
  (com-closing-line :name t) ()
  (setf (chart-style *application-frame*) 'closing-line-chart))

(define-trade-chart-command
  (com-bar :name t) ()
  (setf (chart-style *application-frame*) 'bar-chart))

(define-trade-chart-command
  (com-ohlc-bar :name t) ()
  (setf (chart-style *application-frame*) 'ohlc-bar-chart))

(define-trade-chart-command
  (com-naked-bar :name t) ()
  (setf (chart-style *application-frame*) 'naked-bar-chart))

(define-trade-chart-command
  (com-body-bar :name t) ()
  (setf (chart-style *application-frame*) 'body-bar-chart))

(define-trade-chart-command
  (com-candlestick :name t) ()
  (setf (chart-style *application-frame*) 'candlestick-chart))

(define-trade-chart-command
  (com-empty-candlestick :name t) ()
  (setf (chart-style *application-frame*) 'empty-candlestick-chart))

(define-trade-chart-command
  (com-bar-and-closing-line :name t) ()
  (setf (chart-style *application-frame*) 'bar-and-closing-line-chart))

(define-trade-chart-command
  (com-four-line :name t) ()
  (setf (chart-style *application-frame*) 'four-line-chart))

(define-trade-chart-command
  (com-start :name t) ((x 'integer))
  (when (< x (1- (length (records (table *application-frame*)))))
    (setf (records-start (table *application-frame*)) x)
    (rescale)))

(defun line-chart (frame pane key &key (up-ink (up-ink frame)) (down-ink (down-ink frame)))
  (let* ((w (bounding-rectangle-width pane))
         (h (bounding-rectangle-height pane))
         (xr (- (x1 frame) (x0 frame)))
         (yr (- (y1 frame) (y0 frame)))
         (xs (/ w xr))
         (ys (/ h yr))
         (x0 (+ w (* xs (x0 frame))))
         (y0 (+ h (* ys (y0 frame)))))
    (with-first-quadrant-coordinates (pane x0 y0)
    (draw-rectangle* pane (x0 frame) (y0 frame) (x1 frame) (y1 frame))
    (with-scaling (pane xs ys)
      (mapcar #'(lambda (r s p q)
                  (draw-line* pane
                              (closing-time r) p
                              (closing-time s) q
                              :ink (if (> p q) down-ink up-ink)))
              (records (table *application-frame*))
              (rest (records (table *application-frame*)))
              (mapcar key (records (table *application-frame*)))
              (mapcar key (rest (records (table *application-frame*)))))))))

(defun closing-line-chart (frame pane)
  (line-chart frame pane #'adjusted-closing-price))

(defun naked-bar-chart (frame
                         pane
                         &key
                         (start 'adjusted-low-price)
                         (end 'adjusted-high-price)
                         (up-ink (up-ink frame))
                         (down-ink (down-ink frame))
                         (line-thickness 1))
  (let* ((w (bounding-rectangle-width pane))
         (h (bounding-rectangle-height pane))
         (yr (- (y1 frame) (y0 frame)))
         (ys (/ h yr)))
    (with-first-quadrant-coordinates (pane 0 (+ h (* ys (y0 frame))))
    (with-scaling (pane 1 ys)
      (mapcar #'(lambda (datum)
                  (let ((x (- (closing-time datum) (opening-time datum))))
                    (draw-line* pane
                                x (funcall start datum)
                                x (funcall end datum)
                                :ink (if (< (adjusted-closing-price datum)
                                            (adjusted-opening-price datum))
                                       down-ink
                                       up-ink)
                                :line-thickness line-thickness)))
              (records (table frame)))))))

(defun body-bar-chart (frame
                        pane
                        &key
                        (up-ink (up-ink frame))
                        (down-ink (down-ink frame))
                        (line-thickness 1))
  (naked-bar-chart frame pane
                   :start 'adjusted-opening-price
                   :end 'adjusted-closing-price
                   :up-ink up-ink
                   :down-ink down-ink
                   :line-thickness line-thickness))

(defun bar-chart (frame pane &key (up-ink (up-ink frame)) (down-ink (down-ink frame)))
  (naked-bar-chart frame pane :up-ink up-ink :down-ink down-ink)
  (let* ((w (bounding-rectangle-width pane))
         (h (bounding-rectangle-height pane))
         (yr (- (y1 frame) (y0 frame)))
         (ys (/ h yr)))
    (with-first-quadrant-coordinates (pane 0 (+ h (* ys (y0 frame))))
    (with-scaling (pane 1 ys)
      (mapcar #'(lambda (datum)
                  (draw-line* pane
                              (opening-time datum)
                              (adjusted-closing-price datum)
                              (closing-time datum)
                              (adjusted-closing-price datum)
                              :ink (if (< (adjusted-closing-price datum)
                                          (adjusted-opening-price datum))
                                     down-ink
                                     up-ink)))
              (records (table frame)))))))

(defun ohlc-bar-chart (frame pane &key (up-ink (up-ink frame)) (down-ink (down-ink frame)))
  (bar-chart frame pane :up-ink up-ink :down-ink down-ink)
  (let* ((w (bounding-rectangle-width pane))
         (h (bounding-rectangle-height pane))
         (yr (- (y1 frame) (y0 frame)))
         (ys (/ h yr)))
    (with-first-quadrant-coordinates (pane 0 (+ h (* ys (y0 frame))))
    (with-scaling (pane 1 ys)
      (mapcar #'(lambda (datum)
                  (draw-line* pane
                              (opening-time datum)
                              (adjusted-opening-price datum)
                              (closing-time datum)
                              (adjusted-opening-price datum)
                              :ink (if (< (adjusted-closing-price datum)
                                          (adjusted-opening-price datum))
                                     down-ink
                                     up-ink)))
              (records (table frame)))))))

(defun bar-and-closing-line-chart (frame pane)
  (closing-line-chart frame pane)
  (naked-bar-chart frame pane))

(defun candlestick-chart (frame pane &key (up-ink (up-ink frame)) (down-ink (down-ink frame)))
  (naked-bar-chart frame pane :up-ink up-ink :down-ink down-ink)
  (body-bar-chart frame pane :up-ink up-ink :down-ink down-ink :line-thickness 3))

(defun empty-candlestick-chart (frame pane &key (up-ink (up-ink frame)) (down-ink (down-ink frame)))
  (naked-bar-chart frame pane :up-ink up-ink :down-ink down-ink)
  (let* ((w (bounding-rectangle-width pane))
         (h (bounding-rectangle-height pane))
         (s (/ w (x1 frame)))
         (x 0)
         (yr (- (y1 frame) (y0 frame)))
         (ys (/ h yr)))
    (with-first-quadrant-coordinates (pane 0 (+ h (* ys (y0 frame))))
    (with-scaling (pane 1 ys)
      (mapcar #'(lambda (datum)
                  (draw-rectangle* pane
                              (- x 1) (adjusted-opening-price datum)
                              (+ x 2) (adjusted-closing-price datum)
                              :ink +background-ink+
                              :filled t)
                  (draw-rectangle* pane
                              (- x 1) (adjusted-opening-price datum)
                              (+ x 2) (adjusted-closing-price datum)
                              :filled (< (adjusted-closing-price datum)
                                         (adjusted-opening-price datum))
                              :ink (if (< (adjusted-closing-price datum)
                                          (adjusted-opening-price datum))
                                     down-ink
                                     up-ink))
                  (incf x s))
              (records (table frame)))))))

(defun four-line-chart (frame pane)
  (line-chart frame pane #'adjusted-low-price :up-ink +black+ :down-ink +black+)
  (line-chart frame pane #'adjusted-high-price :up-ink +black+ :down-ink +black+)
  (line-chart frame pane #'adjusted-opening-price :up-ink +green+ :down-ink +green+)
  (line-chart frame pane #'adjusted-closing-price :up-ink +red+ :down-ink +red+))

(defun display-chart (frame pane)
  (draw-text* pane (canonical-ticker (ticker-symbol (table frame)))
	      0 0 :align-x :left :align-y :top)
  (draw-text* pane (stock-description (table frame))
	      0 12 :align-x :left :align-y :top)
  (funcall (chart-style frame) frame pane))

(defun make-trade-chart (table)
  (make-application-frame 'trade-chart
                          :x0 (apply #'min (opening-time table))
                          :y0 (apply #'min (adjusted-low-price table))
                          :x1 (apply #'max (closing-time table))
                          :y1 (apply #'max (adjusted-high-price table))
                          :data table
                          :pretty-name "Trade Chart $Revision: 539 $"))

(defun trade-chart (table)
  (run-frame-top-level (make-trade-chart table)))

(defun demo ()
  (trade-chart (load-table '^dji :preferred-records #'yearly-records)))
