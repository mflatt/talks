#lang slideshow
(require slideshow/play
         "person.rkt"
         "gear.rkt"
         "paper.rkt"
         "res+edu.rkt"
         "res+prod.rkt"
         "util.rkt")

(provide res+prod-intro-slides)

(define (res+prod-intro-slides)

  (define prof (professor))
  (define g-prof (ghost prof))
  (define g2-prof (ghost prof))

  (define the-gear (gear))
  (define g-gear (ghost the-gear))
  (define g2-gear (ghost the-gear))
  
  (define the-paper (paper))
  (define g-paper (ghost the-paper))
  (define g-paper1 (ghost the-paper))
  (define g-paper2 (ghost the-paper))
  (define g-paper3 (ghost the-paper))
  
  (define class (classroom #:gear? #t #:gear-copy-n 1
                           #:professor g-prof
                           #:gear g-gear
                           #:gear-copy the-gear
                           #:paper g-paper))
  (define the-cv (cv 1
                     #:professor g2-prof
                     #:main-gear g2-gear
                     #:paper1 g-paper1
                     #:paper2 g-paper2
                     #:paper3 g-paper3))

  (define class-gear-s (extract-scale class g-gear))

  (play-n
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let* ([n (fast-middle n)]
            [gear-s (+ class-gear-s (* n (- 1 class-gear-s)))])
       (let* ([p (cc-superimpose (over-cellophane class (- 1 n))
                                 (ghost the-cv))]
              [p (slide-pict p prof g-prof g2-prof n)]
              [p (slide-pict p the-paper g-paper g-paper1 n)]
              [p (slide-pict p the-paper g-paper g-paper2 n)]
              [p (slide-pict p the-paper g-paper g-paper3 n)]
              [p (slide-pict p (scale the-gear gear-s) g-gear g2-gear n)])
         p)))))
                 
(module+ main
  (slide (classroom #:gear? #t #:gear-copy-n 1))
  (res+prod-intro-slides)
  (slide (cv 1)))
