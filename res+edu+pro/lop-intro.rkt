#lang slideshow
(require slideshow/play
         "person.rkt"
         "gear.rkt"
         "res+prod.rkt"
         "lop.rkt"
         "util.rkt")

(provide lop-intro-slides)

(define (lop-intro-slides)
  ;; First, close door
  (define the-prof (professor))
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (cv-post-review #:door-n (- 1 (fast-middle n))
                     #:professor (ghost the-prof))))

  (define the-gear (gear))
  (define g-gear (ghost the-gear))
  (define g2-gear (ghost the-gear))
  
  (define source (cv-post-review #:door-n 0
                                 #:badge-gear g-gear))
  
  (define target
    ((lop-slides #:lang-gear-maker? #t
                 #:gear g2-gear)
     #:just-main? #t
     #:main-logo (blank)))

  (define source-s (extract-scale source g-gear))
  (define target-s (extract-scale target g2-gear))

  (play-n
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let ([n (fast-middle n)])
       (slide-pict (cc-superimpose
                    (ghost source)
                    target)
                   (scale the-gear (+ (* (- 1 n) source-s)
                                      (* n target-s)))
                   g-gear
                   g2-gear
                   n)))))
                 
(module+ main
  (slide (cv-post-review #:door-n 1))
  (lop-intro-slides)
  (slide ((lop-slides #:lang-gear-maker? #t)
          #:just-main? #t
          #:main-logo (blank))))
