#lang racket/base
(require slideshow/pict
         racket/class
         racket/draw
         racket/math)

(provide racket-logo
         make-racket-logo)
 
(define left-lambda-path
  (let ([p (new dc-path%)])
    (send p move-to 153 44)
    (send p line-to 161.5 60)
    (send p curve-to 202.5 49 230 42 245 61)
    (send p curve-to 280.06 105.41 287.5 141 296.5 186)
    (send p curve-to 301.12 209.08 299.11 223.38 293.96 244)
    (send p curve-to 281.34 294.54 259.18 331.61 233.5 375)
    (send p curve-to 198.21 434.63 164.68 505.6 125.5 564)
    (send p line-to 135 572)
    p))
 
(define left-logo-path
  (let ([p (new dc-path%)])
    (send p append left-lambda-path)
    (send p arc 0 0 630 630 (* 47/72 2 pi) (* 121/360 2 pi) #f)
    p))
 
(define bottom-lambda-path
  (let ([p (new dc-path%)])
    (send p move-to 135 572)
    (send p line-to 188.5 564)
    (send p curve-to 208.5 517 230.91 465.21 251 420)
    (send p curve-to 267 384 278.5 348 296.5 312)
    (send p curve-to 301.01 302.98 318 258 329 274)
    (send p curve-to 338.89 288.39 351 314 358 332)
    (send p curve-to 377.28 381.58 395.57 429.61 414 477)
    (send p curve-to 428 513 436.5 540 449.5 573)
    (send p line-to 465 580)
    (send p line-to 529 545)
    p))
 
(define bottom-logo-path
  (let ([p (new dc-path%)])
    (send p append bottom-lambda-path)
    (send p arc 0 0 630 630 (* 157/180 2 pi) (* 47/72 2 pi) #f)
    p))
 
(define right-lambda-path
  (let ([p (new dc-path%)])
    (send p move-to 153 44)
    (send p curve-to 192.21 30.69 233.21 14.23 275 20)
    (send p curve-to 328.6 27.4 350.23 103.08 364 151)
    (send p curve-to 378.75 202.32 400.5 244 418 294)
    (send p curve-to 446.56 375.6 494.5 456 530.5 537)
    (send p line-to 529 545)
    p))
 
(define right-logo-path
  (let ([p (new dc-path%)])
    (send p append right-lambda-path)
    (send p arc 0 0 630 630 (* 157/180 2 pi) (* 121/360 2 pi) #t)
    p))
 
(define lambda-path
  (let ([p (new dc-path%)])
    (send p append left-lambda-path)
    (send p append bottom-lambda-path)
    (let ([t (new dc-path%)])
        (send t append right-lambda-path)
        (send t reverse)
        (send p append t))
    (send p close)
    p))
 
(define (paint-racket dc use-pen?)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush "white" 'solid)
  (send dc draw-path lambda-path)
 
  (send dc set-brush "red" 'solid)
  (send dc draw-path left-logo-path)
  (send dc draw-path bottom-logo-path)
 
  (send dc set-brush "blue" 'solid)
  (send dc draw-path right-logo-path)

  (when use-pen?
    (send dc set-pen "black" 3 'solid))
  (send dc set-brush "white" 'transparent)
  (send dc draw-ellipse 0 0 630 630))

(define (make-racket-logo #:use-pen? [use-pen? #t])
  (dc (lambda (dc x y)
        (define t (send dc get-transformation))
        (define p (send dc get-pen))
        (define b (send dc get-brush))
        (send dc translate x y)
        (paint-racket dc use-pen?)
        (send dc set-transformation t)
        (send dc set-pen p)
        (send dc set-brush b))
      630 630))

(define racket-logo (make-racket-logo))

(module+ main
  (require slideshow)
  (slide racket-logo))


