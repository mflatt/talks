#lang slideshow
(require slideshow/play
         "logo.rkt"
         "chat.rkt"
         "lop.rkt")

(provide history-intro-slides)

(define (history-intro-slides)
  (define make (lop-slides #:lang-gear-maker? #t))

  (define g-logo (ghost (scale logo lop-main-logo-scale)))
  (define g2-logo (ghost logo))

  (define lop (make #:collapsed? #t
                    #:main-logo g-logo))

  (play-n
   #:skip-last? #t
   (lambda (n)
     (let ([n (fast-middle n)])
       (define p (cc-superimpose
                  (cellophane lop (- 1 n))
                  g2-logo))
       (define lg (scale logo (+ lop-main-logo-scale (* n (- 1 lop-main-logo-scale)))))
       (slide-pict p lg g-logo g2-logo n))))
   
  
  (play-n
   (lambda (n)
     (refocus (hc-append
               gap-size
               logo
               (cellophane (back-in-1995) n))
              logo))))

(module+ main
  (history-intro-slides))
