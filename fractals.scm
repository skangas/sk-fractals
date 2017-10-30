#!/usr/bin/racket
#lang racket
(require racket/gui/base)
(require racket/draw)

(define (check-if-mandelbrot c)
  (define (checker x c iterate)
   (if (or (zero? iterate) (> (magnitude x) 4.0))
       (and (< (magnitude x) 2.0)
            (list x iterate))
       (checker (+ (* x x) c) c (- iterate 1))))
  (checker 0 c 200))
;; Copyright (C) 2017 Stefan Kangas

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define r-min -1.5)
(define r-max 0.5)

(define i-min 0-1.0i)
(define i-max 0+1.0i)

(define canvas-height 500)
(define canvas-width 500)

(define (point-to-value x y)
  (+ (+ r-min (* (/ (- r-max r-min) canvas-width) x))
     (+ i-min (* (/ (- i-max i-min) canvas-height) y))))

;; GUI

(define frame (new frame%
                   [label "Float"]
                   [width (+ canvas-width 100)]
                   [height (+ canvas-height 100)]))
(define main-panel (new vertical-panel%
                   [parent frame]
                   ))

(define (draw-mandelbrot dc width height)
  (define (draw-line x y)
    (if (> x width)
        #t
        (begin
          (let ((pt (check-if-mandelbrot (point-to-value x y))))
            (if pt
                (send dc set-pen "black" 1 'solid)
                (send dc set-pen "white" 1 'solid))
            (send dc draw-line x y (+ x 1) y))
          (draw-line (+ x 1) y))))
  (define (draw-lines y)
    (if (> y height)
        #t
        (begin
          (draw-line 0 y)
          (draw-lines (+ y 1)))))
  (draw-lines 0))

(new canvas%
     [parent main-panel]
     [min-width canvas-width]
     [min-height canvas-height]
     [style '(border)]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-background (make-color 255 255 255))
                (send dc clear)
                (send dc draw-line 0 0 0 1)
                (draw-mandelbrot dc canvas-width canvas-height)
)])

(define bottom-status (new horizontal-panel%
                           [parent main-panel]
                           [min-height 10]))

(new message% [parent bottom-status]
     [label "Stefan's Mandelbrot v0.0.1alpha-dev"]
     [auto-resize #t])


(define menu-bar (new menu-bar%
                      (parent frame)))
(new menu%
     (label "&File")
     (parent menu-bar))
(new menu%
     (label "&Edit")
     (parent menu-bar))
(new menu%
     (label "&Help")
     (parent menu-bar))

(send frame show #t)

;; TODO: Parallelism with futures
;; https://docs.racket-lang.org/guide/parallelism.html
