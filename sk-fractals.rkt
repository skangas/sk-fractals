#!/usr/bin/racket
#lang racket

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

(require profile)
(require profile/render-text)
(require racket/gui/base)
(require racket/draw)
(require racket/pretty)

(provide main)

(define canvas-height 500)
(define canvas-width 500)

(define max-iterations 200)

(define REAL-MIN -2.0)
(define REAL-MAX 2.0)
(define REAL-DIFF (- REAL-MAX REAL-MIN))
(define IMAG-MIN 0-2.0i)
(define IMAG-MAX 0+2.0i)
(define IMAG-DIFF (- IMAG-MAX IMAG-MIN))
(define SK-FRACTALS-VERSION "0.0.2-dev")

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

;; Handle zoom

;; (define point -0.63+0.2i)
;; (define zoom 50.0)

(define point 0.0+0.0i)
(define zoom 1.0)

(define (r-min)
  (- point (/ (/ REAL-DIFF zoom) 2)))

(define (r-max)
  (+ point (/ (/ REAL-DIFF zoom) 2)))

(define (i-min)
  (- point (/ (/ IMAG-DIFF zoom) 2)))

(define (i-max)
  (+ point (/ (/ IMAG-DIFF zoom) 2)))

;; Mandelbrot
(define (point-to-value x y)
  (+ (+ (r-min) (* (/ (- (r-max) (r-min)) canvas-width) x))
     (+ (i-min) (* (/ (- (i-max) (i-min)) canvas-height) y))))

(define (check-if-mandelbrot x y)
  (define (checker x c iterate)
   (if (or (zero? iterate) (> (magnitude x) 4.0))
       (and (> (magnitude x) 2.0)
            (list iterate x))
       (checker (+ (* x x) c) c (- iterate 1))))
  (checker 0 (point-to-value x y) max-iterations))

(define (smooth-color iter c)
  (let* ((zn (magnitude c))
         (hue (- (+ 1.0 iter) (/ (log (log zn)) (log 2.0)))))
    hue))

(define (calc-color iter c)
  (let ((hue (smooth-color iter c)))
    (make-color/hsv hue 0.6 1.0)))

(define (calc-color2 iter c)
  (define color-scheme
    (list '(255 66 30 15)
          '(255 25 7 26)
          '(255 9 1 47)
          '(255 4 4 73)
          '(255 0 7 100)
          '(255 12 44 138)
          '(255 24 82 177)
          '(255 57 125 209)
          '(255 134 181 229)
          '(255 211 236 248)
          '(255 241 233 191)
          '(255 248 201 95)
          '(255 255 170 0)
          '(255 204 128 0)
          '(255 153 87 0)
          '(255 106 52 3)))
  (let* ((hue (smooth-color iter c))
         (idx (modulo (exact-floor hue) 16)))
    (list-ref color-scheme idx)))

(define (calculate-mandelbrot width height)
  (define (calc-line x y)
    (if (>= x width)
        '()
        (append
         (let ((pt (check-if-mandelbrot x y)))
           (if (eq? pt #f)
               '(255 0 0 0)
               (calc-color2 (car pt) (cadr pt))))
         (calc-line (+ x 1) y))))
  (define (calc-lines y)
    (if (>= y height)
        '()
        (append
         (calc-line 0 y)
         (calc-lines (+ y 1)))))
  (apply bytes (calc-lines 0)))

;; GUI

(define (main)
  (define frame (new frame%
                     [label "SK Fractals"]))
  (define main-panel (new horizontal-panel%
                          [parent frame]))
  (define left-panel (new vertical-panel%
                          [parent main-panel]))

  ;; Main canvas
  (define mandelbrot-pixels (calculate-mandelbrot canvas-height canvas-width))
  ;; (define mandelbrot-pixels (profile-thunk (lambda () (calculate-mandelbrot canvas-height canvas-width))))
  (define bitmap (make-bitmap canvas-width canvas-height))
  (send bitmap set-argb-pixels 0 0 canvas-width canvas-height mandelbrot-pixels #f #f)
  (new canvas%
       [parent left-panel]
       [min-width canvas-width]
       [min-height canvas-height]
       [style '(border)]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-background (make-color 255 255 255))
          (send dc clear)
          (send dc draw-bitmap bitmap 0 0))])

  ;; Right info
  (define right-panel (new vertical-panel%
                           [parent main-panel]
                           [min-height canvas-height]
                           [min-width 200]))
  (define location-box (new group-box-panel%
                          [parent right-panel]
                          [label "Location"]))
  (new message% [parent location-box]
       [label (string-append "Real min: " (number->string (r-min)))]
       [auto-resize #t])
  (new message% [parent location-box]
       [label (string-append "Real max: " (number->string (r-max)))]
       [auto-resize #t])
  (new message% [parent location-box]
       [label (string-append "Imaginary min: " (number->string (i-min)))]
       [auto-resize #t])
  (new message% [parent location-box]
       [label (string-append "Imaginary max: " (number->string (i-max)))]
       [auto-resize #t])
  (define info-box (new group-box-panel%
                          [parent right-panel]
                          [label "Calculation"]))
  (new message% [parent info-box]
       [label (string-append "Iterations: " (number->string max-iterations))]
       [auto-resize #t])
  (new message% [parent info-box]
       [label (string-append "Height: " (number->string canvas-height))]
       [auto-resize #t])
  (new message% [parent info-box]
       [label (string-append "Width: " (number->string canvas-width))]
       [auto-resize #t])

  ;; Bottom status
  (define bottom-status (new horizontal-panel%
                             [parent left-panel]
                             [min-height 10]))
  (new message% [parent bottom-status]
       [label (string-append "SK Fractals v" SK-FRACTALS-VERSION)]
       [auto-resize #t])

  ;; Menu bar
  (define menu-bar (new menu-bar%
                        (parent frame)))
  (define file-menu (new menu%
                         (label "&File")
                         (parent menu-bar)))
  (new menu-item%
       [label "&Settings"]
       [parent file-menu]
       [callback (lambda (_ b) b)])
  (new separator-menu-item%
       [parent file-menu])
  (new menu-item%
       [label "&Exit"]
       [parent file-menu]
       [callback (lambda (_ ev) (exit 0))])
  (define help-menu (new menu%
                         (label "&Help")
                         (parent menu-bar)))
  (new menu-item%
       [label "&About"]
       [parent help-menu]
       [callback (lambda (_ b) b)])

  (send frame show #t))

;; TODO: Parallelism with futures
;; https://docs.racket-lang.org/guide/parallelism.html
(main)