#lang slideshow
(require racket/draw
         racket/class
         "history.rkt")

(provide rhombus-logo)

(define path1
  (let ((p (new dc-path%)))
    (send p move-to 478.398 472.197)
    (send p line-to 498.398 472.197)
    (send p curve-to
          489.19000000000005
          369.176
          509.34400000000005
          314.935
          509.34400000000005
          255.986)
    (send p curve-to
          509.34400000000005
          2.207
          500.03100000000006
          2.8930000000000007
          266.2510000000001
          2.8930000000000007)
    (send p curve-to
          225.84500000000008
          2.8930000000000007
          196.6930000000001
          8.260000000000002
          189.6850000000001
          18.09)
    (send p curve-to 272.435 71.989 408.349 247.839 455.398 412.197)
    (send p close)
    p))

(define path2
  (let ((p (new dc-path%)))
    (send p move-to 220.003 164.337)
    (send p curve-to
          180.522
          121.80399999999999
          136.308
          68.02499999999999
          39.47999999999999
          5.62199999999999)
    (send p line-to 0 5)
    (send p curve-to 0.573 112.011 3.159 180.092 3.159 255.986)
    (send p curve-to 3.159 319.8 26.785 478.09 65.756 462.609)
    (send p curve-to 100.111 319.392 164.697 219.907 220.003 164.337)
    (send p close)
    p))

(define path3
  (let ((p (new dc-path%)))
    (send p move-to 266.638 221.727)
    (send p curve-to
          211.84599999999998
          280.778
          157.24599999999998
          384.149
          137.486
          479.52099999999996)
    (send p curve-to
          172.90499999999997
          498.37799999999993
          213.326
          509.08
          256.252
          509.08)
    (send p curve-to 300.384 509.08 341.87 497.774 377.992 477.917)
    (send p curve-to 357.171 381.712 317.868 293.604 266.638 221.727)
    (send p close)
    p))

(define shape
  '((0 . 0)
    (460 . 0)
    (520 . 425)
    (60 . 425)))

(define inner-shape
  (let ([n 12])
    (define (adj i dx dy)
      (define p (list-ref shape i))
      (cons (+ (car p) dx) (+ (cdr p) dy)))
    (list
     (adj 0 n n)
     (adj 1 (- n) n)
     (adj 2 (- n) (- n))
     (adj 3 n (- n)))))

(define rhombus-region
  (let ([r (new region%)])
    (send r set-polygon shape)
    r))

(define rhombus-inner-region
  (let ([r (new region%)])
    (send r set-polygon inner-shape)
    r))

(define no-pen (make-pen #:style 'transparent))
(define white-brush (make-brush #:color "white"))
(define blue-brush (make-brush #:color (make-color #x9F #x1D #x20)))
(define red-brush (make-brush #:color (make-color #x3E #x5B #xA9)))

(define rhombus-logo
  (dc (lambda (dc x y)
        (define old-r (send dc get-clipping-region))
        (define old-t (send dc get-transformation))
        (define old-p (send dc get-pen))
        (define old-b (send dc get-brush))
        (send dc translate x y)
        (send dc set-clipping-region rhombus-region)
        (send dc set-pen no-pen)
        (send dc set-brush white-brush)
        (send dc draw-rectangle 0 0 520 460)
        (send dc set-clipping-region rhombus-inner-region)
        (send dc set-brush blue-brush)
        (send dc draw-path path1 10 -20)
        (send dc set-brush red-brush)
        (send dc draw-path path2 10 -20)
        (send dc draw-path path3 10 -20)
        (send dc set-clipping-region old-r)
        (send dc set-transformation old-t)
        (send dc set-brush old-b)
        (send dc set-pen old-p))
      520 425))

(module+ main
  (slide
   rhombus-logo))
