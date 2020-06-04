#lang slideshow
(require slideshow/play
         "person.rkt"
         "venn.rkt"
         "logo.rkt"
         "util.rkt")

(provide overview-slides)

(define (overview-slides)
  (define prof (professor))
  
  (define plain-venn (make-venn #:icons? #f))
  (define venn-s 0.5)
  (define small-venn (scale plain-venn venn-s))

  (define big-prof (scale prof 2))

  (define (outline-slide #:title? [title? #f]
                         #:racket? [racket? title?]
                         #:venn? [venn? racket?]
                         #:prof? [prof? #t]
                         #:fade [fade-n 0])
    (slide
     #:name "Outline"
     (outline #:racket? racket?
              #:venn? venn?
              #:prof? prof?
              #:number? title?)))

  (define (outline #:racket? [racket? #t]
                   #:venn? [venn? #t]
                   #:prof? [prof? #t]
                   #:number? [number? #f])
    (define scaled-logo (scale logo 0.75))
    (define (add-number n p #:size [size 1])
      (define (rescale p)
        (if (= size 1)
            p
            (refocus (cc-superimpose (ghost p)
                                     (scale p size))
                     p)))
      (cond
        [number? (refocus (vc-append
                           (* 3 gap-size)
                           (rescale (background (inset (colorize (scale (t (format "~a" n)) 1.5)
                                                                 "white")
                                                       10 0)
                                                "darkgray"))
                           (cc-superimpose
                            p
                            (ghost scaled-logo)
                            (ghost small-venn)))
                          p)]
        [else p]))
    (refocus (hc-append
              (* 1/8 client-w)
              (add-number 1 ((if venn? values ghost) small-venn))
              (add-number 2 ((if prof? values ghost) big-prof))
              (add-number 3 ((if racket? values ghost) scaled-logo) #:size 1.5))
             big-prof))
  
  (outline-slide)
  (outline-slide #:venn? #t)
  (outline-slide #:racket? #t)
  (outline-slide #:title? #t)

  (play-n
   #:skip-first? #t
   #:skip-last? #t
   (lambda (n)
     (let ([n (fast-middle n)])
       (define p (cc-superimpose
                  (cellophane (outline #:venn? #f) (- 1 n))
                  (ghost plain-venn)))
       (define v (scale plain-venn (+ venn-s (* n (- 1 venn-s)))))
       (slide-pict p v small-venn plain-venn n)))))

(module+ main
  (overview-slides))
