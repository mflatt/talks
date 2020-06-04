#lang slideshow
(require racket/draw
         racket/class
         "color.rkt")

(provide like)

(define like-path
  (let ([p (new dc-path%)])
    (send p move-to 0 35)
    (send p line-to 15 15)
    (send p line-to 15 0)
    (send p curve-to
          25 0
          25 0
          25 25)
    (send p arc
          40 25
          10 10
          (* 1/2 pi)
          (* -1/2 pi)
          #f)
    (send p arc
          40 35
          10 10
          (* 1/2 pi)
          (* -1/2 pi)
          #f)
    (send p arc
          37 45
          10 10
          (* 1/2 pi)
          (* -1/2 pi)
          #f)
    (send p arc
          35 55
          10 10
          (* 1/2 pi)
          (* -1/2 pi)
          #f)
    (send p line-to 5 65)
    (send p arc
          0 55
          10 10
          (* -1/2 pi)
          (- pi)
          #f)
    (send p close)
    p))

(define (like #:sleeve? [sleeve? #f]
              #:left? [left? #f])
  (define no-pen (make-pen #:style 'transparent))
  (define hand-brush (make-brush #:color person-color))
  (define sleeve-brush (make-brush #:color "darkblue"))
  (dc (lambda (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (send dc set-pen no-pen)
        (send dc set-brush hand-brush)

        (define old-t (send dc get-transformation))
        (send dc translate x y)
        (when left?
          (send dc scale -1 1)
          (send dc translate (if sleeve? -70 -50) 0))

        (send dc draw-path like-path (if sleeve? 20 0) 0)

        (when sleeve?
          (send dc set-brush sleeve-brush)
          (send dc draw-rounded-rectangle 0 30 20 35))
        
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-transformation old-t))
      (if sleeve? 70 50) 70))

(module+ main
  (slide (like)
         (like #:left? #t #:sleeve? #t)))


