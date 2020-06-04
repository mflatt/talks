#lang slideshow
(require slideshow/play
         racket/runtime-path
         "history.rkt"
         "util.rkt")

(provide bootstrap-slides)

(define drracket-png "rocket/drracket.png")
(define drracket (scale (bitmap drracket-png) 0.5))

(define rocket-png "rocket/rocket.png")
(define launch-png "rocket/launch.png")
(define rocket (scale (bitmap rocket-png) 2))
(define launch (scale (bitmap launch-png) 0.5))

(define-runtime-path bootstrap-logo-png "logos/bootstrap.png")
(define bootstrap-logo (let ([p (bitmap bootstrap-logo-png)])
                         (cc-superimpose
                          p
                          (colorize
                           (linewidth
                            4
                            (ellipse (pict-width p)
                                     (pict-height p)))
                           "MediumForestGreen"))))

(define area-scale 0.65)

(define bootstrap-alg-png "logos/bootstrap-alg.png")
(define bootstrap-alg (scale (bitmap bootstrap-alg-png) area-scale))

(define bootstrap-phys-png "logos/bootstrap-phys.png")
(define bootstrap-phys (scale (bitmap bootstrap-phys-png) area-scale))

(define bootstrap-ds-png "logos/bootstrap-ds.png")
(define bootstrap-ds (scale (bitmap bootstrap-ds-png) area-scale))

(define (bootstrap-slides)

  (define big-bang-title (ca 2004 "Big Bang"))
  (define big-bang-name "Big Bang")
  
  (define (make-bootstrap-title year) (ca year "Bootstrap" #:who "Schanzer et al."))
  (define bootstrap-title (make-bootstrap-title 2006))
  (define bootstrap-name "Bootstrap")
  
  (define rocket-demo
    (unsmoothed
     (vc-append
      gap-size
      (scale drracket 0.75)
      (scale
       (apply hc-append
              (for/list ([i 4])
                (inset
                 (cb-superimpose launch (inset rocket 0 0 0 (+ gap-size (* (* i i) 10))))
                 (* -1 gap-size)
                 0)))
       0.75))))

  (as-history
   #:prod 0
   (slide
    #:title big-bang-title
    #:name big-bang-name
    (leftward rocket-demo)))

  (define last-one
    (as-history
     #:res 0
     #:prod 0

     (define (scalex p s) (if (zero? s) (blank 0) (scale p s)))

     (define (bootstrap #:n [n 1]
                        #:url? [url? #f]
                        #:n2 [n2 (if url? 1 0)])
       (define adj (* -1 gap-size))
       (define (skinny p)
         (inset p 0 0 (- (pict-width p)) 0))
       (define (add-url p)
         (if url?
             (lb-superimpose p
                             (inset (scale (tt "bootstrapworld.org") 1.5)
                                    (* -1/6 client-w) adj 0 adj))
             p))
       (leftward
        (add-url
         (hc-append
          (* gap-size 3 n2)
          (hc-append
           (* gap-size 3 n (- 1 n2))
           (scalex rocket-demo (* (- 1 (* 0.25 n)) (- 1 n2)))
           (scalex bootstrap-logo n))
          (skinny (inset (scalex (vc-append (* -1/2 gap-size)
                                            bootstrap-alg
                                            bootstrap-phys
                                            bootstrap-ds)
                                 n2)
                         0 adj))))))

     (play-n
      #:title bootstrap-title
      #:name bootstrap-name
      #:skip-first? #t
      (lambda (n) (bootstrap #:n (fast-middle n))))

     (define bootstrap-title2 (make-bootstrap-title 2016))
     
     (play-n
      #:title bootstrap-title2
      #:name bootstrap-name
      #:skip-first? #t
      (lambda (n) (bootstrap #:n2 (fast-middle n))))

     (slide
      #:title bootstrap-title2
      #:name bootstrap-name
      (bootstrap #:url? #t))
     
     (lambda ()
       (slide
        #:title bootstrap-title2
        #:name bootstrap-name
        (bootstrap #:url? #t)))))

  (as-history
   #:res 0
   (last-one)))

(module+ main
  (bootstrap-slides))

