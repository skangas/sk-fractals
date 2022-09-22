#!/usr/bin/racket
#lang racket

;; Copyright (C) 2017-2022 Stefan Kangas

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
(require racket/unsafe/ops)
(require racket/format)
(require racket/flonum)
(require racket/fixnum)

(define canvas-height 800)
(define canvas-width 800)
(define max-iterations 500)

(define new-point 0)
(define point-r -0.09061328)
(define point-c 0.83210332)
(define zoom 10.0)

(define SK-FRACTALS-VERSION "0.0.2-dev")

(struct complex (r i))

;; HSV color space operations

;; (provide
;;  (contract-out [make-color/hsv
;;                 (-> real? (real-in 0 1) (real-in 0 1) (is-a?/c color%))]))

;; make a color object from HSV values
(define (make-color/hsv hue saturation value)
  (define chroma (fl* saturation value))
  (define hue* (fl/ (remainder* hue (fl* 2 pi)) (fl/ pi 3)))
  (define X (fl* chroma (fl- 1.0 (flabs (fl- (remainder* hue* 2.0) 1.0)))))
  (define-values (r1 g1 b1)
    (cond [(and (fl<= 0 hue*) (fl< hue* 1.0)) (values chroma X 0)]
          [(and (fl<= 1 hue*) (fl< hue* 2.0)) (values X chroma 0)]
          [(and (fl<= 2 hue*) (fl< hue* 3.0)) (values 0 chroma X)]
          [(and (fl<= 3 hue*) (fl< hue* 4.0)) (values 0 X chroma)]
          [(and (fl<= 4 hue*) (fl< hue* 5.0)) (values X 0 chroma)]
          [(and (fl<= 5 hue*) (fl< hue* 6.0)) (values chroma 0 X)]))
  (define m (fl- value chroma))
  (apply make-color (map (λ (x) (exact-round (fl* 255.0 (fl+ x m))))
                         (list r1 g1 b1))))

;; general remainder
(define (remainder* n1 n2)
  (define num-divides (fl/ n1 n2))
  (fl- n1 (fl* (flfloor num-divides) n2)))

(define (smooth-color iter c)
  ;; magnitude = sqrt(r^2 + i^2) [pythagora's theorem]
  (let* ((zn (flsqrt (fl+ (fl* (complex-r c) (complex-r c)) (fl* (complex-i c) (complex-i c)))))
         (hue (fl- (fl+ 1.0 (->fl iter)) (fl/ (fllog (fllog zn)) (fllog 2.0)))))
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
         (idx (fxmodulo (fl->fx (flfloor hue)) 16)))
    (list-ref color-scheme idx)))

;; Handle zoom

(define (r-min) (fl- point-r (fl/ 2.0 zoom)))
(define (r-max) (fl+ point-r (fl/ 2.0 zoom)))
(define (i-min) (fl- point-c (fl/ 2.0 zoom)))
(define (i-max) (fl+ point-c (fl/ 2.0 zoom)))

;; Mandelbrot
(define (point-to-complex x y)
  (complex (fl+ (r-min) (fl* (fl/ (fl- (r-max) (r-min)) (->fl canvas-width)) (->fl x)))
           (fl+ (i-min) (fl* (fl/ (fl- (i-max) (i-min)) (->fl canvas-height)) (->fl y)))))

(define (get-point-string p)
  (string-append (number->string (complex-r p)) "+"
                 (number->string (complex-i p)) "i"))

;; (a+bi)(c+di) = (ac−bd) + (ad+bc)i
;; (a+bi)(a+bi) = (a^2−b^2) + (2ab)i
(define (mandelbrot iterations x y)
  (let* ((c (point-to-complex x y)))
    (let next ((r 0.0) (i 0.0) (cr (complex-r c)) (ci (complex-i c)) (n iterations))
      (cond ((>= (fl+ (fl* r r) (fl* i i)) 4.0)
             (list n (complex r i)))
            ((zero? n) #t)
            (else
             (let ((r-new (fl+ (fl- (fl* r r) (fl* i i)) cr))
                   (i-new (fl+ (fl* 2.0 (fl* r i)) ci)))
               (next r-new i-new cr ci (fx- n 1))))))))

(define (calculate-mandelbrot width height)
  (foldr append '()
   (for*/list ((y height)
               (x width))
     (let ((pt (mandelbrot max-iterations x y)))
       (if (eq? pt #t)
           '(255 0 0 0)
           (calc-color2 (unsafe-car pt) (unsafe-car (unsafe-cdr pt))))))))

;; GUI

(define frame (new frame%
                   [label "SK Fractals"]))
(define main-panel (new vertical-panel%
                        [parent frame]))
(define middle-panel (new horizontal-panel%
                        [parent main-panel]))
(define left-panel (new vertical-panel%
                        [parent middle-panel]))
(define canvas '())

;; Bottom status
(define bottom-status #f)
;; (new message% [parent bottom-status]
;;      [label (string-append "SK Fractals v" SK-FRACTALS-VERSION)]
;;      [auto-resize #t])
(define status-msg #f)

(define mouse-point 0.0)
;; (define mouse-point2 0.0)
(define dragging #f)
(define drag-point 0.0)
(define drag-point2 0.0)
(define drag-point-x 0)
(define drag-point-y 0)
(define bitmap #f)

;; Pens
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define black-pen (make-object pen% "BLACK" 1 'solid))

(define (draw-square-on bmp x y z w)
  (define dc (new bitmap-dc% [bitmap bmp]))
  (send dc set-pen black-pen)
  (send dc set-brush no-brush)
  (send dc draw-rectangle x y (- z x) (- w y)))

;; XX

(define mandel-canvas%
  (class canvas%
         ;; (define point zoom)
         (define/override (on-event event)
           (cond ((eq? (send event get-event-type) 'motion)
                  (let ((x (send event get-x))
                        (y (send event get-y)))
                    (set! mouse-point (point-to-complex x y))
                    (and dragging
                         (begin (set! bitmap (get-mandelbrot))
                                (draw-square-on bitmap drag-point-x drag-point-y x y)
                                (send canvas refresh-now)))
                    (send status-msg set-label
                          (if dragging
                              (string-append "Dragging at: " (get-point-string mouse-point))
                              (string-append "Cursor location: " (get-point-string mouse-point))))))
                 ((eq? (send event get-event-type) 'left-down)
                  (let* ((x (send event get-x))
                         (y (send event get-y))
                         (p (point-to-complex x y)))
                    (set! dragging #t)
                    (set! drag-point p)
                    (set! drag-point-x x)
                    (set! drag-point-y y)
                    (send status-msg set-label "foo")))
                 ((eq? (send event get-event-type) 'left-up)
                  (let ((p (point-to-complex (send event get-x) (send event get-y))))
                    (set! dragging #f)
                    (set! drag-point2 p)
                    (send status-msg set-label "bar")
                    (set! drag-point #f)))))
         ;; (define/override (on-char event)
         ;;   (send status-msg set-label "Canvas keyboard"))
         (super-new)))

;; Calculate Mandelbrot
(define mandelbrot-pixels (apply bytes (foldr append '() (for*/list ((x canvas-width)
                                                                     (y canvas-height))
                                                           '(255 0 0 0)))))
;; (define mandelbrot-pixels (profile-thunk (lambda () (apply bytes (calculate-mandelbrot canvas-height canvas-width)))))
;; (define mandelbrot-pixels (apply bytes (calculate-mandelbrot canvas-height canvas-width)))
(define (get-mandelbrot)
  (define bitmap (make-bitmap canvas-width canvas-height))
  (send bitmap set-argb-pixels 0 0 canvas-width canvas-height mandelbrot-pixels #f #f)
  bitmap)

(define (main)
  (set! bitmap (get-mandelbrot))
  (set! canvas (new mandel-canvas%
        [parent left-panel]
        [min-width canvas-width]
        [min-height canvas-height]
        [style '(border)]
        [paint-callback
         (lambda (canvas dc)
           (send dc set-background (make-color 255 255 255))
           (send dc clear)
           (send dc draw-bitmap bitmap 0 0))]))

  (define (recalculate-canvas f e)
    (set! mandelbrot-pixels (apply bytes (calculate-mandelbrot canvas-height canvas-width)))
    (set! bitmap (get-mandelbrot))
    (send canvas refresh-now))

  ;; Right info
  (define right-panel (new vertical-panel%
                           [parent middle-panel]
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
  (new text-field%
       (label "Zoom")
       (parent info-box)
       (init-value (number->string zoom))
       (callback (lambda (f e)
                   (set! zoom (string->number (send f get-value))))))
  (define calculate-button (new button%
                      (parent info-box)
                      (label "Calculate")
                      (callback recalculate-canvas)))
  (define reset-button (new button%
                      (parent info-box)
                      (label "Reset")))

  ;; Create status bar
  (set! bottom-status (new horizontal-panel%
                           [parent main-panel]
                           [min-height 10]))
  (set! status-msg (new message% [parent bottom-status]
                        [label "..."]
        [auto-resize #t]))

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

  (send frame show #t)
  (recalculate-canvas null null))

(main)
