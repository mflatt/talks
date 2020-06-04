#lang slideshow
(require racket/draw
         racket/class)

(provide wires-door)

(define wire1
  (let ([p (new dc-path%)])
    (send p move-to 0 70)
    (send p curve-to
          30 80
          75 75
          50 50)
    (send p curve-to
          25 25
          50 10
          70 30)
    (send p curve-to
          90 50
          85 60
          100 80)
    p))

(define wire2a
  (let ([p (new dc-path%)])
    (send p arc -30 -30 160 120 (- pi) 0)
    p))

(define wire2b
  (let ([p (new dc-path%)])
    (send p arc -10 10 160 120 0 pi)
    p))

(define wire3a
  (let ([p (new dc-path%)])
    (send p move-to 0 30)
    (send p curve-to
          20 30
          20 50
          30 20)
    (send p curve-to
          40 -10
          80 50
          70 70)
    (send p curve-to
          60 90
          80 50
          100 60)
    p))

(define wire3b
  (let ([p (new dc-path%)])
    (send p arc -50 50 100 100 (* 1/2 pi) 0 #f)
    p))

(define wire3c
  (let ([p (new dc-path%)])
    (send p arc 70 -60 40 90 (* 3/4 pi) 0)
    p))



(define (wires-door #:open-n [open-n 1])
  (define no-pen (make-pen #:style 'transparent))
  (define no-brush (make-brush #:style 'transparent))
  (define edge-pen (make-pen #:color "gray"))
  (define inside-brush (make-brush #:color "DimGray"))
  (define open-brush (make-brush #:color "darkgray"))
  (define bright-brush (make-brush #:color "lightgray"))
  (define red-pen (make-pen #:color "red" #:width 3))
  (define green-pen (make-pen #:color "green" #:width 3))
  (define blue-pen (make-pen #:color "blue" #:width 3))

  (dc (lambda (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))

        (send dc set-pen no-pen)

        (send dc set-brush inside-brush)
        (send dc draw-rectangle x y 100 100)

        (send dc set-brush no-brush)
        (define old-rgn (send dc get-clipping-region))
        (send dc set-clipping-rect x y 100 100)
        (send dc set-pen blue-pen)
        (send dc draw-path wire2a x y)
        (send dc draw-path wire2b x y)
        (send dc set-pen green-pen)
        (send dc draw-path wire3a x y)
        (send dc draw-path wire3c x y)
        (send dc set-pen red-pen)
        (send dc draw-path wire1 x y)
        (send dc set-pen green-pen)
        (send dc draw-path wire3b x y)
        (send dc set-pen no-pen)
        (send dc set-clipping-region old-rgn)

        (cond
          [(open-n . > . 0.80)
           (define n (* 5 (- open-n 0.8)))
           (send dc set-brush bright-brush)
           (send dc draw-polygon
                 `((0 . 100)
                   (100 . 100)
                   (120 . ,(+ 100 (* 20 n)))
                   (20 . ,(+ 100 (* 20 n))))
                 x y)]
          [else
           (define n (* (/ 1 0.8) open-n))
           (send dc set-pen edge-pen)
           (send dc set-brush open-brush)
           (send dc draw-polygon
                 `((0 . 100)
                   (100 . 100)
                   (,(+ 100 (* 20 n)) . ,(* 100 n))
                   (,(* 20 n) . ,(* 100 n)))
                 x y)])

        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      100 100))

(module+ main
  (require slideshow/play)
  (slide (wires-door))
  (play-n
   (lambda (n)
     (wires-door #:open-n (fast-middle n)))))
