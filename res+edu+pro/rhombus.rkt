#lang slideshow
(require slideshow/play
         racket/draw
         racket/class
         "history.rkt"
         "rhombus-logo.rkt"
         "lop.rkt"
         "data.rkt"
         "util.rkt")

(provide rhombus-slides)

(define (rhombus-slides)
  (define make (lop-slides #:lang-gear-maker? #t))
  
  (define title (ca 2020 "Project Rhombus"))
  (define name "Project Rhombus")

  (as-history
   (play-n
    #:title title
    #:name name
    #:skip-last? #t
    (lambda (n)
      (make #:main-logo (blank) #:n4 n)))

   (define scaled-logo (scale rhombus-logo 1/4))

   (define rescale 1/3)
   (define rescaled-logo  (scale rhombus-logo rescale))
   (define g-rescaled-logo (ghost rescaled-logo))

   (play-n
    #:title title
    #:name name
    (let ([p (make #:n4 1
                   #:rhombus-logo scaled-logo
                   #:main-logo g-rescaled-logo)])
      (define-values (l t) (lt-find p scaled-logo))
      (define-values (r b) (rb-find p scaled-logo))
      (define small-scale (/ (- r l) (pict-width rhombus-logo)))
      (lambda (n)
        (let* ([n (fast-middle n)]
               [p (slide-pict p
                              (scale rhombus-logo (+ small-scale
                                                     (* n (- rescale small-scale))))
                              scaled-logo
                              g-rescaled-logo
                              n)])
          p))))

   (define fib
     (tts
      " define fib(n):"
      "   match n"
      "    | 0: 0"
      "    | 1: 1"
      "    | n: fib(n-1) + fib(n-2)"))
   
   (define add-rhombus
     (let ([rh (make #:n4 1 #:main-logo rescaled-logo #:just-main? #t)])
       (lambda (p #:fib? [fib? #f])
         (refocus
          (rc-superimpose
           rh
           (inset (cc-superimpose
                   (blank (* 1/3 (pict-width rh)) (* 1/3 (pict-height rh)))
                   (vc-append p
                              ((if fib? values ghost)
                               (inset fib
                                      (* -4 gap-size)
                                      (* 5 gap-size)
                                      (* -10 gap-size)
                                      (* -5 gap-size)))))
                  0 0 (* 1/6 (pict-width rh)) 0))
          rh))))

   (slide
    #:title title
    #:name name
    (add-rhombus (blank)))   

   (play-n
    #:title title
    #:name name
    #:skip-last? #t
    (lambda (n) (add-rhombus (data #:swap-n (fast-middle n)))))
   (play-n
    #:title title
    #:name name
    #:skip-first? #t
    (lambda (n) (add-rhombus (data #:swap-n 1 #:swap2-n (fast-middle n)))))

   (slide
    #:title title
    #:name name
    (add-rhombus #:fib? #t (data #:swap-n 1 #:swap2-n 1))))
  
  (void))

(module+ main
  (rhombus-slides))
