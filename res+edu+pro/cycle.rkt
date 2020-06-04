#lang slideshow
(require slideshow/play
         slideshow/code
         "history.rkt"
         "contract+type.rkt"
         "in-file.rkt"
         "util.rkt"
         "color.rkt")

(provide cycle-slides)

(define (cycle-slides)
  (define title "Modules and Macros and Contracts and Types")
  (define mod+typ-title "Modules and Macros and Types")
  (define syntax-parse-title (ctl-superimpose
                              (ca 2008 (code syntax-parse) #:who "Culpepper")
                              (titlet "")))
  
  (define (make-mod ct)
    (in-file #:name "dummy"
             (cc-superimpose (blank 200 100)
                             (ct
                              (background (inset (colorize (scale (tt "#'") 1.2) (current-base-color))
                                                 (/ gap-size 2))
                                          (blend-color ct-color module-background))))))
  (define mod-0 (make-mod ghost))
  (define mod0 (make-mod values))
  (define ctc0 (contract-icon))
  (define typ0 (type-icon))

  (define mod- (cc-superimpose mod-0 (ghost ctc0) (ghost typ0)))
  (define mod (cc-superimpose mod0 (ghost ctc0) (ghost typ0)))
  (define ctc (cc-superimpose ctc0 (ghost mod)))
  (define typ (cc-superimpose typ0 (ghost mod)))
  
  (define top (ghost mod))
  (define bottom (ghost mod))
  
  (define implies (vc-append
                   (* 1 gap-size)
                   top
                   (colorize (inset (scale (t "⇒") 2) gap-size) "forestgreen")
                   bottom))

  (define-values (tx ty) (lt-find implies top))
  (define-values (bx by) (lt-find implies bottom))

  (define blx (- bx (* 3/4 (pict-width top))))
  (define brx (+ bx (* 3/4 (pict-width top))))
  (define bly (- by (* 1/2 (pict-height top))))
  (define bry bly)
  (define ty2 (- ty (* 1/5 (pict-height top))))
  
  (define (drift n from to curve)
    (+ (* n to)
       (* (- 1 n) from)
       (* (sin (* n pi)) curve)))

  (define (still #:top top
                 #:bottom bottom)
    (leftward
     (let* ([p implies]
            [p (pin-over p
                         tx ty
                         top)]
            [p (pin-over p
                         bx by
                         bottom)])
       p)))

  (define (trio #:top top
                #:title [title title]
                #:name [name title]
                #:bottom-then-top bottom-then-top
                #:then-bottom then-bottom
                #:skip-last? [skip-last? #f])
    (play-n
     #:title title
     #:name name
     #:skip-last? #t
     (lambda (n)
       (leftward
        (let ([n (fast-middle n)])
          (let* ([p implies]
                 [p (pin-over p
                              (- tx (* n 3/4 client-w)) ty
                              top)]
                 [p (pin-over p
                              (drift n bx tx 300)
                              (drift n by ty 0)
                              bottom-then-top)]
                 [p (pin-over p
                              (- bx (* (- 1 n) 3/4 client-w)) by
                              then-bottom)])
            p))))))

  (define (duo #:title [title title]
               #:top-then-bottom top-then-bottom
               #:bottom-then-top bottom-then-top
               #:skip-last? [skip-last? #t])
    (play-n
     #:title title
     #:skip-last? skip-last?
     (lambda (n)
       (leftward
        (let ([n (fast-middle n)])
          (let* ([p implies]
                 [p (pin-over p
                              (drift n tx bx -300)
                              (drift n ty by 0)
                              top-then-bottom)]
                 [p (pin-over p
                              (drift n bx tx 300)
                              (drift n by ty 0)
                              bottom-then-top)])
            p))))))

  (as-history
   #:res 0
   #:edu 0
   
   (duo #:title mod+typ-title
        #:top-then-bottom mod-
        #:bottom-then-top typ
        #:skip-last? #f))

  (as-history
   #:res 2
   #:edu 0
   
   (trio #:title syntax-parse-title
         #:name "Syntax Parse"
         #:top typ
         #:bottom-then-top mod
         #:then-bottom ctc
         #:skip-last? #t))

  (as-history
   #:edu 0

   (trio #:top mod
         #:bottom-then-top ctc
         #:then-bottom typ
         #:skip-last? #t)

   (trio #:top ctc
         #:bottom-then-top typ
         #:then-bottom mod
         #:skip-last? #t)

   (play-n
    #:title title
    #:skip-last? #t
    (lambda (n)
      (leftward
       (let ([n (fast-middle n)])
         (let* ([p implies]
                [p (pin-over p
                             tx
                             (drift n ty ty2 0)
                             typ)]
                [p (pin-over p
                             (drift n bx brx 0)
                             (drift n by bry 0)
                             mod)]
                [p (pin-over p
                             (drift n (* -1 client-w) blx 0)
                             (drift n by bly 0)
                             ctc)])
           p)))))

   (define (rot n p x y q)
     (define cw (* (pict-width q) 0.6))
     (define ch (/ (pict-height q) 2))
     (define cx (+ x cw))
     (define cy (+ y ch))
     (define rx (/ (pict-width p) 2))
     (define ry (/ (pict-height p) 2))
     (define v (make-rectangular (- cx rx)
                                 (- ry cy)))
     (define v2 (make-polar (magnitude v)
                            (+ (angle v) (* n 2 pi))))
     (pin-over p
               (- (+ rx (real-part v2)) cw)
               (- ry (imag-part v2) ch)
               q))

   (define (spin n)
     (let* ([p implies]
            [p (rot n p tx ty2 typ)]
            [p (rot n p brx bry mod)]
            [p (rot n p blx bly ctc)])
       (leftward p)))

   (play-n
    #:title title
    #:skip-first? #t
    #:steps 25
    (lambda (n) (spin (* 1.5 (fast-middle n)))))

   (void)))

(module+ main
  (cycle-slides))

