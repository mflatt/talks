#lang racket/base
(require slideshow
         racket/draw
         racket/class)

(provide hat)

(define (make-hat w h)
  (dc (lambda (dc x y)
        (define pen (send dc get-pen))
        (define brush (send dc get-brush))
        (send dc set-pen (make-pen #:style 'transparent))
        (send dc set-brush (make-brush #:color "black"))
        (send dc draw-rectangle (+ x (/ w 4)) (+ y 10) (/ w 2) 20)
        (send dc draw-ellipse (+ x (/ w 4)) (+ y 25) (/ w 2) 10)
        (send dc draw-polygon (list (cons 0 15)
                                    (cons (/ w 2) 0)
                                    (cons w 15)
                                    (cons (/ w 2) 30))
              x y)
        (send dc set-brush brush)
        (send dc set-pen pen))
      w h))

(define hat
  (rb-superimpose
   (make-hat 70 50)
   (inset (rotate (colorize (scale (t "λ") 1.2) "gold")
                  (* pi 1/7))
          0 0 5 0)))




