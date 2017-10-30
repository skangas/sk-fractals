#!/usr/bin/racket
#lang racket
(require racket/gui/base)
(require racket/draw)

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

(define max-iterations 200)

;; HSV color space operations

(provide
 (contract-out [make-color/hsv
                (-> real? (real-in 0 1) (real-in 0 1) (is-a?/c color%))]))

;; make a color object from HSV values
(define (make-color/hsv hue saturation value)
  (define chroma (* saturation value))
  (define hue* (/ (remainder* hue (* 2 pi)) (/ pi 3)))
  (define X (* chroma (- 1 (abs (- (remainder* hue* 2) 1)))))
  (define-values (r1 g1 b1)
    (cond [(and (<= 0 hue*) (< hue* 1)) (values chroma X 0)]
          [(and (<= 1 hue*) (< hue* 2)) (values X chroma 0)]
          [(and (<= 2 hue*) (< hue* 3)) (values 0 chroma X)]
          [(and (<= 3 hue*) (< hue* 4)) (values 0 X chroma)]
          [(and (<= 4 hue*) (< hue* 5)) (values X 0 chroma)]
          [(and (<= 5 hue*) (< hue* 6)) (values chroma 0 X)]))
  (define m (- value chroma))
  (apply make-color (map (λ (x) (exact-round (* 255 (+ x m))))
                         (list r1 g1 b1))))

;; general remainder
(define (remainder* n1 n2)
  (define num-divides (/ n1 n2))
  (- n1 (* (floor num-divides) n2)))

;; Mandelbrot
(define (check-if-mandelbrot c)
  (define (checker x c iterate)
   (if (or (zero? iterate) (> (magnitude x) 4.0))
       (and (> (magnitude x) 2.0)
            (list iterate x))
       (checker (+ (* x x) c) c (- iterate 1))))
  (checker 0 c max-iterations))

(define (point-to-value x y)
  (+ (+ r-min (* (/ (- r-max r-min) canvas-width) x))
     (+ i-min (* (/ (- i-max i-min) canvas-height) y))))

(define (smooth-color iter c)
  (let* ((zn (magnitude c))
         (hue (- (+ 1.0 iter) (/ (log (log zn)) (log 2.0)))))
    hue))

(define (calc-color iter c)
  (let ((hue (smooth-color iter c)))
    (make-color/hsv hue 0.6 1.0)))

(define (calc-color2 iter c)
  (define color-scheme
    (list '(66 30 15)
          '(25 7 26)
          '(9 1 47)
          '(4 4 73)
          '(0 7 100)
          '(12 44 138)
          '(24 82 177)
          '(57 125 209)
          '(134 181 229)
          '(211 236 248)
          '(241 233 191)
          '(248 201 95)
          '(255 170 0)
          '(204 128 0)
          '(153 87 0)
          '(106 52 3)))
  (let* ((hue (smooth-color iter c))
         (idx (modulo (exact-floor hue) 16)))
    (apply make-color (list-ref color-scheme idx))))

(define (draw-mandelbrot dc width height)
  (define (draw-line x y)
    (if (> x width)
        #t
        (begin
          (let* ((pt (check-if-mandelbrot (point-to-value x y)))
                 (color (and pt (calc-color2 (car pt) (cadr pt)))))
            (if (eq? pt #f)
                (send dc set-pen "black" 1 'solid)
                (send dc set-pen color 1 'solid))
            (send dc draw-line x y (+ x 1) y))
          (draw-line (+ x 1) y))))
  (define (draw-lines y)
    (if (> y height)
        #t
        (begin
          (draw-line 0 y)
          (draw-lines (+ y 1)))))
  (draw-lines 0))

;; GUI

(define frame (new frame%
                   [label "Float"]
                   [width (+ canvas-width 100)]
                   [height (+ canvas-height 100)]))
(define main-panel (new vertical-panel%
                   [parent frame]
                   ))

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
