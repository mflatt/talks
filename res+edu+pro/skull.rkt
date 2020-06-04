#lang slideshow
(require racket/draw
         racket/class)

(provide skull)

(define (skull)
  (define no-pen (make-pen #:style 'transparent))
  (define black-brush (make-brush #:color "black"))
  (define white-brush (make-brush #:color "white"))
  (dc (lambda (dc x y)
        (define old-p (send dc get-pen))
        (define old-b (send dc get-brush))
        (define old-t (send dc get-transformation))
        (send dc set-pen no-pen)
        (send dc set-brush black-brush)
        (send dc translate x y)

        (send dc draw-ellipse 5 15 20 20)
        (send dc draw-ellipse 15 5 20 20)

        (send dc draw-ellipse 95 15 20 20)
        (send dc draw-ellipse 85 5 20 20)

        (send dc draw-ellipse 5 85 20 20)
        (send dc draw-ellipse 15 95 20 20)

        (send dc draw-ellipse 85 95 20 20)
        (send dc draw-ellipse 95 85 20 20)

        (send dc draw-ellipse 30 25 60 60)
        (send dc draw-ellipse 40 65 40 40)

        (send dc draw-polygon
              '((15 . 25)
                (25 . 15)
                (105 . 95)
                (95 . 105)))

        (send dc draw-polygon
              '((105 . 25)
                (95 . 15)
                (15 . 95)
                (25 . 105)))

        (send dc set-brush white-brush)
        (send dc draw-ellipse 40 40 20 20)
        (send dc draw-ellipse 60 40 20 20)

        (send dc set-transformation old-t)
        (send dc set-pen old-p)
        (send dc set-brush old-b))
      120 120))

(module+ main
  (slide (skull)))

      
