#lang slideshow
(require slideshow/play
         "person.rkt"
         "department.rkt"
         "bar.rkt"
         "util.rkt")

(provide department-intro-slides)

(define (department-intro-slides)
  (define prof0 (professor))
  (define prof1 (launder prof0))
  (define prof2 (launder prof0))
  (define prof3 (launder prof0))

  (define g-prof0 (ghost prof0))
  (define g-prof1 (ghost prof1))
  (define g-prof2 (ghost prof2))
  (define g-prof3 (ghost prof3))

  (define department (make-department #:prof1 g-prof1
                                      #:prof2 g-prof2
                                      #:prof3 g-prof3))

  (define title (reality-title #:prof g-prof0))

  (define title-s (extract-scale title g-prof0))

  (play-n
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let* ([n (fast-middle n)]
            [s (+ title-s (* n (- 1 title-s)))]
            [dtop gap-size])
       (let* ([p (cc-superimpose
                  (ct-superimpose full-page
                                  (cellophane title (- 1 n)))
                  (inset (over-cellophane (inset department 0 (- dtop) 0 0) n) 0 dtop 0 0))]
              [p (slide-pict p (scale prof1 s) g-prof0 g-prof1 n)]
              [p (slide-pict p (scale prof2 s) g-prof0 g-prof2 n)]
              [p (slide-pict p (scale prof3 s) g-prof0 g-prof3 n)])
         p)))))
                 
(module+ main
  (bar-slides)
  (department-intro-slides)
  (department-slides))
