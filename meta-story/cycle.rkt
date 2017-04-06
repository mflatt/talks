#lang slideshow
(require racket/draw
         racket/class)

(provide cycle)

(define arc-arrow
  (let ([p (new dc-path%)])
    (define dr 0.05)
    (define dc 0.1)
    (send p move-to 10 -39)
    (send p arc -50 -50 100 100 
          (- (* pi 1/2) dr)
          (- (* pi -1/6) dr (- dc) 0.05)
          #f)
    (send p rotate (- (* pi 2/3) dc))
    (send p line-to 10 -39)
    (send p rotate (+ (* pi -2/3) dc))
    (send p arc -30 -30 60 60
          (- (* pi -1/6) dr (- dc))
          (- (* pi 1/2) dr))
    (send p close)
    p))

(define no-pen (make-pen #:style 'transparent))
(define red-brush (make-brush #:color "red"))
(define green-brush (make-brush #:color "forestgreen"))
(define blue-brush (make-brush #:color "blue"))

(define cycle
 (dc (lambda (dc x y)
       (let ([t (send dc get-transformation)]
             [p (send dc get-pen)]
             [b (send dc get-brush)])
         (send dc set-pen no-pen)
         (send dc translate (+ x 100) (+ y 100))
         (send dc scale 2 2)
         (send dc set-brush green-brush)
         (send dc draw-path arc-arrow 0 0)
         (send dc rotate (* pi 2/3))
         (send dc set-brush blue-brush)
         (send dc draw-path arc-arrow 0 0)
         (send dc rotate (* pi 2/3))
         (send dc set-brush red-brush)
         (send dc draw-path arc-arrow 0 0)
         (send dc set-brush b)
         (send dc set-pen p)
         (send dc set-transformation t)))
     200 200))

