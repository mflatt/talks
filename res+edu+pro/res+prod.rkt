#lang slideshow
(require slideshow/play
         "person.rkt"
         "paper.rkt"
         "gear.rkt"
         "like.rkt"
         "wires-door.rkt"
         "book.rkt")

(provide res+prod-slides
         cv
         cv-post-review)

(define product-gap-size (* 4 gap-size))

(define the-prof (delay (professor)))

(define (cv gear-size
            #:fade [fade 0]
            #:one-paper-fade [one-paper-fade 0]
            #:paper-badge [paper-badge #f]
            #:main-gear [main-gear (gear)]
            #:one-paper-n [one-paper-n 0]
            #:slide-paper-n [slide-paper-n 0]
            #:professor [prof (force the-prof)]
            #:paper1 [paper1 #f]
            #:paper2 [paper2 #f]
            #:paper3 [paper3 #f])
  (define bg-g (launder (cellophane (scale main-gear gear-size) (- 1 fade))))
  (define g (refocus (cc-superimpose
                      (ghost main-gear)
                      bg-g)
                     main-gear))
  (define g-paper-badge (and paper-badge (ghost paper-badge)))
  (define one-paper (or paper1
                        (paper #:badge g-paper-badge)))
  (define next-paper (or paper2
                         (if paper-badge (paper #:badge paper-badge) one-paper)))
  (define next2-paper (or paper3
                          next-paper))
  (define products
    (vc-append
     gap-size
     (hc-append gap-size
                (let* ([p (cellophane one-paper (- 1 one-paper-fade))]
                       [p (if paper-badge
                              (pin-over p g-paper-badge lt-find paper-badge)
                              p)])
                  (if (= slide-paper-n 0)
                      p
                      (let ([g-p (launder (ghost p))])
                        (refocus (lt-superimpose
                                  g-p
                                  (inset p (* slide-paper-n
                                              (* 2 (+ (pict-width p) gap-size)))
                                         0 0 0))
                                 g-p))))
                (cellophane next-paper (- 1 one-paper-n))
                (cellophane next2-paper (- 1 one-paper-n)))
     (ghost g)))
  (let* ([p (refocus (hc-append
                      product-gap-size
                      prof
                      products)
                     products)]
         [p (pin-under p
                       g lt-find
                       (cellophane g (- 1 one-paper-n)))])
    p))

(define max-fade 0.4)
(define max-scale 15)

(define the-main-gear (delay (gear)))
(define the-badge-gear (delay (gear)))

(define badge-scale 0.5)
(define the-gear-badge (delay (scale (force the-badge-gear) badge-scale)))

(define (large-gear-badge n #:badge-gear [badge-gear (force the-badge-gear)])
  (let* ([gear-badge (scale badge-gear badge-scale)]
         [g-gear-badge (ghost gear-badge)])
    (refocus (cc-superimpose
              g-gear-badge
              (scale gear-badge (+ 1 (* 10 n))))
             g-gear-badge)))

(define (reviewed #:reviewed-n [review-n 1]
                  #:rezoom-n [rezoom-n 0])
  (let* ([p (cv max-scale
                #:one-paper-n 1
                #:one-paper-fade rezoom-n
                #:slide-paper-n (fast-middle review-n)
                #:paper-badge (large-gear-badge (fast-middle rezoom-n))
                #:main-gear (force the-main-gear))])
    (define happy? (= review-n 1))
    (refocus (hc-append
              product-gap-size
              p
              ((if (positive? rezoom-n) ghost values)
               (let ([r (reviewer #:happy? happy?)])
                 (if happy?
                     (let ([lk (like #:left? #t)])
                       (refocus (hb-append gap-size
                                           lk
                                           (inset r 0 0 0 (* 1/2 (pict-height lk))))
                                r))
                     r))))
             p)))

(define (post-review adjust-gear
                     #:main-gear [main-gear (force the-main-gear)]
                     #:badge-gear [badge-gear (force the-badge-gear)]
                     #:professor [prof (force the-prof)])
  (cv max-scale
      #:one-paper-n 1
      #:one-paper-fade 1
      #:slide-paper-n 1
      #:paper-badge (adjust-gear (large-gear-badge 1 #:badge-gear badge-gear))
      #:professor prof
      #:main-gear main-gear))

(define ((open-door n #:stack [stack values]) g)
  (define d (stack (wires-door #:open-n (fast-middle n))))
  (refocus (pin-over (hc-append (* 2 gap-size)
                                (ghost d)
                                g)
                     d lt-find
                     d)
           g))

(define (cv-post-review #:door-n n
                        #:main-gear [main-gear (force the-main-gear)]
                        #:badge-gear [badge-gear (force the-badge-gear)]
                        #:professor [prof (force the-prof)])
  (post-review (open-door n)
               #:professor prof
               #:main-gear main-gear
               #:badge-gear badge-gear))

(define (res+prod-slides)
  (define name "Research + Production")

  (play-n
   #:name name
   (lambda (n)
     (cv (+ 1 (* n (- max-scale 1)))
         #:fade (* max-fade n))))

  (play-n
   #:name name
   (lambda (n)
     (cv max-scale
         #:fade max-fade
         #:one-paper-n n
         #:paper-badge (force the-gear-badge))))

  (play-n (lambda (n) (reviewed #:reviewed-n n))
          #:name name
          #:skip-last? #t)

  (play-n (lambda (n) (reviewed #:rezoom-n n))
          #:name name)

  (define one-paper (paper))

  (define ((open-door+paper one-paper) g)
    ((open-door 1 #:stack (lambda (d)
                            (refocus (vc-append gap-size
                                                one-paper
                                                d)
                                     d)))
     g))

  (play-n
   #:name name
   #:skip-first? #t
   (lambda (n)
     (cv-post-review #:door-n n)))

  (slide
   #:name name
   (post-review (open-door+paper one-paper)))

  (slide
   #:name name
   (post-review (open-door+paper (refocus (hc-append (* 1.5 gap-size)
                                                     one-paper
                                                     (hc-append
                                                      (/ gap-size 4)
                                                      (book (scale (t "PhD") 0.9))
                                                      (scale (person #:hair-style 'straight
                                                                     #:hat-style 'high-mortarboard)
                                                             0.8)))
                                          one-paper))))

  (void))

(module+ main
  (res+prod-slides))
