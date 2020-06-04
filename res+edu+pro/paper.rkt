#lang slideshow
(require racket/draw
         racket/class)

(provide paper
         three-papers
         two-papers)

(define (shade s)
  (define (adj n) (min 255 (inexact->exact (floor (* n s)))))
  (make-color (adj 245) (adj 245) (adj 220)))

(define pen-path
  (let ([p (new dc-path%)])
    (send p move-to 0 0)
    (send p curve-to
          5 -2
          13 10
          25 20)
    (send p curve-to
          37 30
          38 37
          40 38)
    (send p line-to 38 40)
    (send p curve-to
          37 38
          30 37
          20 25)
    (send p curve-to
          10 13
          -2 5
          0 0)
    (send p rotate (* pi))
    (send p translate 40 40)
    (send p close)
    p))

(define sign-path
  (let ([p (new dc-path%)])
    (send p move-to 1 10)
    (send p curve-to
          8 8
          10 10
          10 2)
    (send p curve-to
          12 5
          14 5
          15 10)
    (send p line-to
          20 5)
    (send p line-to
          23 9)
    (send p line-to
          25 4)
    (send p curve-to
          24 8
          25 10
          29 10)
    p))

(define sign2-part-path
  (let ([p (new dc-path%)])
    (send p move-to 1 10)
    (send p curve-to
          8 8
          5 10
          5 4)
    (send p curve-to
          6 5
          7 8
          10 10)
    (send p curve-to
          12 13
          14 7
          15 5)
    p))

(define sign2-path
  (let ([p (new dc-path%)])
    (send p append sign2-part-path)
    (send p curve-to
          15 7
          16 10
          20 10)
    (send p curve-to
          20 5
          20 5
          25 5)
    p))

(define (paper #:badge [badge #f]
               #:contract? [contract? #f]
               #:pen? [pen? #f])
  (define line-space (cond
                       [badge (pict-height badge)]
                       [contract? 30]
                       [else 0]))

  (define no-pen (make-pen #:style 'transparent))
  (define title-pen (make-pen #:width 5 #:cap 'butt #:color (shade 0.5)))
  (define text-pen (make-pen #:width 1 #:cap 'butt #:color (shade 0.5)))
  (define bright-pen (make-pen #:width 1 #:color (shade 1.2)))
  (define no-brush (make-brush #:style 'transparent))
  (define paper-brush (make-brush #:color (shade 1.0)))
  (define corner-brush (make-brush #:color (shade 0.9)))
  (define edge-pen (make-pen #:width 1 #:cap 'butt #:color (shade 0.9)))
  (define pen-brush (and contract? (make-brush #:color "darkred")))
  (define sign-pen (and contract? (make-pen #:color "blue" #:width 2)))

  (define paper
    (dc (lambda (dc x y)
          (define old-pen (send dc get-pen))
          (define old-brush (send dc get-brush))
          (send dc set-pen no-pen)

          (send dc set-brush paper-brush)
          (send dc draw-polygon
                '((0 . 0)
                  (0 . 100)
                  (75 . 100)
                  (75 . 20)
                  (55 . 20)
                  (55 . 0))
                x y)

          (cond
            [contract?
             (define t (send dc get-transformation))
             (send dc translate (+ x 10) (+ y 15))
             (send dc scale 0.8 0.8)
             (send dc draw-text "CONTRACT" 0 0)
             (send dc set-transformation t)]
            [else
             (send dc set-pen title-pen)
             (send dc draw-line
                   (+ x 10) (+ y 15)
                   (+ x 55) (+ y 15))])

          (send dc set-pen no-pen)
          (send dc set-brush corner-brush)
          (send dc draw-polygon
                '((55 . 20)
                  (75 . 20)
                  (55 . 0))
                x y)

          (send dc set-pen edge-pen)
          (send dc draw-lines
                '((55 . 0)
                  (0 . 0)
                  (0 . 100)
                  (75 . 100)
                  (75 . 20))
                x y)

          (send dc set-pen text-pen)
          (for ([j (in-range 30 (- 95 line-space) 5)])
            (cond
              [contract?
               (send dc draw-line
                     (+ x 5) (+ y j)
                     (+ x 70) (+ y j))]
              [else
               (send dc draw-line
                     (+ x 5) (+ y j)
                     (+ x 35) (+ y j))
               (send dc draw-line
                     (+ x 40) (+ y j)
                     (+ x 70) (+ y j))]))
          (when contract?
            (send dc draw-line
                  (+ x 40) (+ y 80)
                  (+ x 70) (+ y 80))
            (send dc draw-line
                  (+ x 40) (+ y 90)
                  (+ x 70) (+ y 90))
            (send dc set-brush no-brush)
            (send dc set-pen sign-pen)
            (send dc draw-path sign-path (+ x 40) (+ y 70))
            (send dc draw-path (if pen? sign2-part-path sign2-path) (+ x 40) (+ y 80))
            (when pen?
              (send dc set-pen no-pen)
              (send dc set-brush pen-brush)
              (send dc draw-path pen-path (+ x 15) (+ y 45))))
          
          (send dc set-brush old-brush)
          (send dc set-pen old-pen))
        75
        100))
  (if badge
      (cb-superimpose paper
                      (inset badge 5))
      paper))

(define (three-papers)
  (let ([p (paper)])
    (panorama
     (hc-append
      -120
      (vc-append -30
                 p
                 (inset p 10 0 0 0))
      p))))

(define (two-papers)
  (let ([p (paper)])
    (panorama
     (hc-append
      -120
      (vc-append -30
                 (blank)
                 (inset p 10 0 0 0))
      p))))

(module+ main
  (slide
   (paper #:contract? #t)
   (paper)))

