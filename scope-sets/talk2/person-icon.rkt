#lang racket/base
(require slideshow
         racket/class
         racket/draw)

(provide person-icon)

(define body (make-object dc-path%))

(send body move-to 0 90)
(send body line-to 0 25)
(send body arc 0 0 50 50 pi (/ pi 2) #f)
(send body line-to 75 0)
(send body arc 50 0 50 50 (/ pi 2) 0 #f)
(send body line-to 100 90)
(send body arc 80 80 20 20 0 (* -1 pi) #f)
(send body line-to 77 30)
(send body line-to 75 30)
(send body line-to 78 100)
(send body line-to 75 189)
(send body arc 53 178 22 22 0 (* -1 pi) #f)
(send body line-to 53 100)
(send body line-to 47 100)
(send body line-to 47 189)
(send body arc 25 178 22 22 0 (* -1 pi) #f)
(send body line-to 22 100)
(send body line-to 25 30)
(send body line-to 23 30)
(send body line-to 20 90)
(send body arc 00 80 20 20 0 (* -1 pi) #f)
(send body close)
  
(define (person-icon [h 200])
  (scale
   (vc-append
    8
    (filled-ellipse 50 50)
    (dc (lambda (dc x y)
          (define p (send dc get-pen))
          (define b (send dc get-brush))
          (send dc set-pen "black" 0 'transparent)
          (send dc set-brush (send p get-color) 'solid)
          (send dc draw-path body x y)
          (send dc set-pen p)
          (send dc set-brush b))
        100 200))
   (/ h 200)))

(module+ main
  (slide (person-icon)))
 
