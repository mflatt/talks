#lang slideshow
(require slideshow/face
         slideshow/code
         "lightbulb.rkt"
         "style.rkt"
         "logo.rkt")

(provide part-1-slide
         part-2-slide)

(define (with-icon i p #:top? [top? #f])
  (refocus (vc-append (/ gap-size 2) 
                      (if top? i p)
                      (if top? p i))
           p))

(define (part-1-slide)
  (slide
   #:name "Part 1 ----------"
   (para
    #:align 'center
    (titlet "Part 1:")
    (with-icon (scale (face 'unhappy) 1/4)
               (titlet "Motivation"))
    (titlet "and")
    (with-icon (inset (scale (bright-lightbulb) 1/2)
                      0 0 0 -10)
               #:top? #t
               (titlet "Approach")))))

(define (part-2-slide)
  (slide
   #:name "Part 2 ----------"
   (para
    #:align 'center
    (with-icon
     (scale racket-logo 1/8)
     (titlet "Part 2: Modules and Syntax Objects")))))

(module+ main
  (part-1-slide)
  (part-2-slide))
