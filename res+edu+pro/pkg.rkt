#lang slideshow
(require racket/draw
         racket/class
         "util.rkt"
         "logo.rkt")

(provide pkg-icon)

(define box-base
  (let ([p (new dc-path%)])
    (send p move-to 80 80)
    (send p line-to 50 100)
    (send p line-to 20 80)
    (send p line-to 20 40)
    (send p line-to 50 60)
    (send p line-to 80 40)
    (send p close)
    p))

(define box-inside
  (let ([p (new dc-path%)])
    (send p move-to 20 40)
    (send p line-to 50 60)
    (send p line-to 80 40)
    (send p line-to 50 25)
    (send p close)
    p))

(define box-arms
  (let ([p (new dc-path%)])
    (send p move-to 20 40)
    (send p line-to 5 55)
    (send p line-to 35 75)
    (send p line-to 50 60)
    (send p close)
    (send p move-to 80 40)
    (send p line-to 95 55)
    (send p line-to 65 75)
    (send p line-to 50 60)
    (send p close)
    (send p move-to 20 40)
    (send p line-to 5 25)
    (send p line-to 35 10)
    (send p line-to 50 25)
    (send p close)
    (send p move-to 80 40)
    (send p line-to 95 25)
    (send p line-to 65 10)
    (send p line-to 50 25)
    (send p close)
    p))

(define (pkg-icon #:logo-inside? [logo-inside? #f])
  (define base-color "peru")
  (define no-pen (make-pen #:style 'transparent))
  (define middle-brush (make-brush #:color base-color))
  (define bright-brush (make-brush #:color (scale-color base-color 1.2)))
  (define dim-brush (make-brush #:color (scale-color base-color 0.8)))
  (define bright-pen (make-pen #:color (scale-color base-color 1.2) #:width 1))
  (define draw-logo (and logo-inside? (make-pict-drawer (scale logo 1/8))))
  (dc (lambda (dc x y)
        (define old-p (send dc get-pen))
        (define old-b (send dc get-brush))
        (send dc set-pen no-pen)
        
        (send dc set-brush dim-brush)
        (send dc draw-path box-inside x y)
        (when logo-inside?
          (draw-logo dc (+ x 25) (+ y 35)))

        (send dc set-brush middle-brush)
        (send dc draw-path box-base x y)

        (send dc set-brush bright-brush)
        (send dc draw-path box-arms x y)

        (send dc set-pen bright-pen)
        (send dc draw-line (+ x 50) (+ y 61) (+ x 50) (+ y 99))
        
        (send dc set-pen old-p)
        (send dc set-brush old-b))
      100 100))

(module+ main
  (slide
   (scale (pkg-icon #:logo-inside? #t) 2)))
