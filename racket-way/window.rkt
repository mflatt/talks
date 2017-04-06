#lang racket/base
(require slideshow
         racket/draw
         racket/class)

(provide window)

(define (make-window w h)
  (dc (lambda (dc x y)
        (define pen (send dc get-pen))
        (define brush (send dc get-brush))
        (send dc set-pen (make-pen #:color "black"))
        (send dc set-brush (make-brush #:color (make-color 100 100 100)))
        (send dc draw-rounded-rectangle x y w h 5)
        (define c (send dc get-clipping-region))
        (send dc set-clipping-rect x (+ y 10) w (- h 10))
        (send dc set-brush (make-brush #:color "white"))
        (send dc draw-rounded-rectangle x y w h 5)
        (send dc set-clipping-region c)
        (send dc set-pen (make-pen #:style 'transparent))
        (send dc set-brush (make-brush #:color "red"))
        (send dc draw-ellipse (+ x 3) (+ y 2) 6 6)
        (send dc set-brush (make-brush #:color "yellow"))
        (send dc draw-ellipse (+ x 10) (+ y 2) 6 6)
        (send dc set-brush (make-brush #:color "green"))
        (send dc draw-ellipse (+ x 17) (+ y 2) 6 6)
        (send dc set-brush brush)
        (send dc set-pen pen))
      w h))

(define window
  (cc-superimpose (make-window 70 60)
                  (scale (t "λ") 1.2)))
