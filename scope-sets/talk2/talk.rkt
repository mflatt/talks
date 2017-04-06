#lang slideshow
(require "code.rkt"
         "scope.rkt"
         "eqn.rkt"
         "cover-image.rkt"
         "experience.rkt"
         "modules.rkt"
         "terminology.rkt"
         racket/set
         slideshow/play
         racket/draw
         racket/class)

(define movie-bitmaps? #f)
(define pre-bitmap-count 42)
(define total-bitmap-count 56)

(define cover
  (let* ([rgb->color (lambda (l)
                       (define (s v) (inexact->exact (floor (* 255 v))))
                       (make-color (s (car l)) (s (cadr l)) (s (caddr l))))]
         [bl-b (make-brush #:color "black")]
         [l-b (make-brush #:color (rgb->color cover-letters-rgb))]
         [b-b (make-brush #:color (rgb->color cover-base-rgb))]
         [t-p (make-pen #:style 'transparent)]
         [blk-p (make-pen #:color (make-color 50 50 50) #:width 2)]
         [p2 (new dc-path%)])
    (send p2 append cover-image-path)
    (send p2 rotate (* pi -1/6))
    (dc (lambda (dc x y)
          (define p (send dc get-pen))
          (define b (send dc get-brush))
          (send dc set-pen t-p)
          (send dc set-brush bl-b)
          (send dc draw-ellipse (+ x 0) (+ y 1) (* 2 RR) (* 2 RR))
          (send dc set-brush l-b)
          (send dc draw-ellipse x y (* 2 RR) (* 2 RR))
          (define i 10)
          (send dc set-brush b-b)
          (send dc draw-ellipse (+ x i) (+ y i) (* 2 (- RR i)) (* 2 (- RR i)))
          (send dc set-brush bl-b)
          (send dc draw-path p2 (+ x 0 RR) (+ y 1 RR))
          (send dc set-brush l-b)
          (send dc draw-path p2 (+ x RR) (+ y RR))
          (send dc set-brush b)
          (send dc set-pen p))
        (* 2 RR) (* 2 RR))))

(define (work-sign l1 l2 l3 #:bars? [bars? #f])
  (let* ([c (let ([ht (lambda (s) (text s `(bold . swiss) 40))])
              (vc-append gap-size
                         (ht l1)
                         (ht l2)
                         (ht l3)))]
         [h (* 1.2 (pict-width c))]
         [r (frame (inset (colorize (filled-rectangle h h) "orange")
                          -10)
                   #:line-width 5)]
         [s (rotate r (/ pi 4))]
         [bar (lambda (w h) (frame (colorize (filled-rectangle w h) "white")
                              #:line-width 2))])
    (scale (refocus (cc-superimpose (if bars?
                                        (inset (hc-append (bar (* h 1/10) (* 1.5 h))
                                                          (vc-append (* 0.75 h)
                                                                     (ghost (bar h (* h 1/10)))
                                                                     (bar h (* h 1/10)))
                                                          (bar (* h 1/10) (* 1.5 h)))
                                               0 (* h 1/2) 0 0)
                                        (blank))
                                    s
                                    c)
                    s)
           1 1)))

(define bg-color (make-color 200 250 200))
(define road-color (make-color 100 100 100))

(slide
 (let* ([p (hc-append
            (let ([p (inset (work-sign "BINDING" "AS SETS OF" "SCOPES" #:bars? #t)
                            0 0 0 (* 2 RR))])
              (scale
               (refocus (vc-append (- RR)
                                   p
                                   (colorize 
                                    (vc-append
                                     10
                                     (text "Matthew Flatt" `(bold . swiss) 32)
                                     (text "University of Utah" `(bold . swiss) 22))
                                    "white"))
                        p)
               1.2))
            (inset (scale (scale cover 1 0.7) 2)
                   0 (* 3 RR) 0 0))])
   (refocus (cc-superimpose
             (colorize (filled-rectangle 1024 768) bg-color)
             (inset (rotate (colorize (filled-rectangle 2024 500) road-color)
                            (/ pi 8))
                    0 400 0 0)
             p)
            p)))

(slide
 (inset (colorize (filled-rectangle 1024 768) "black")
        (- margin)))

#;
(slide
 (let ([p (inset (scale (ct-superimpose
                         (colorize (filled-rectangle 20 600) "white")
                         (inset
                          (work-sign "NEW" "EXPANDER" "AHEAD")
                          0 20 0 0))
                        0.75)
                 0 50 300 0)])
   (refocus (cc-superimpose
             (colorize (filled-rectangle 1024 768) bg-color)
             (colorize
              (dc (lambda (dc dx dy)
                    (send dc draw-ellipse (+ dx 450) (- dy -300) 2048 1048))
                  1024
                  768)
              road-color)
             (colorize
              (dc (lambda (dc dx dy)
                    (send dc draw-ellipse (+ dx 750) (- dy 00) 2048 2048))
                  1024
                  768)
              bg-color)
             (let ([s (colorize (filled-rectangle 1024 370) "lightblue")])
               (inset s 0 0 0 (- 768 (pict-height s))))
             p)
            p)))

(define (bitmap-slide n)
  (define bm (read-bitmap (build-path "/tmp/slides/" (format "slide-~a~a.png" (if (n . < . 10) "0" "") n))))
  (slide (inset (dc (lambda (dc x y)
                      (send dc draw-bitmap bm x y))
                    (send bm get-width)
                    (send bm get-height))
                (- margin))))

(when movie-bitmaps?
  (for ([n (in-range (add1 pre-bitmap-count))])
    (bitmap-slide n)))

(scope-slides #:more-pedantic? #t)

(modint-slides)

(terminology-slides)

(experience-slides)

(reduction-slides)

(when movie-bitmaps?
  (for ([n (in-range pre-bitmap-count total-bitmap-count)])
    (bitmap-slide n)))
