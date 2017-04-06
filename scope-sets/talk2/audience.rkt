#lang slideshow
(require "desktop.rkt"
         "person-icon.rkt")

(provide audience-slides)

;; ----------------------------------------

(define (mt s)
  (define p (text s '(bold . "Menlo") (* 10 (current-font-size))))
  (inset p (* (pict-height p) -0.10)))
(define (ot s)
  (scale (t s) 2))
   
(define thought-color "lightgray")

(define (ht-append/layers sp . l)
  (define p (apply ht-append sp (for/list ([i (in-list l)])
                                  (if (box? i)
                                      (ghost (unbox i))
                                      i))))
  (for/fold ([p p]) ([i (in-list (reverse l))])
    (if (box? i)
        (pin-over p (unbox i) lt-find (unbox i))
        p)))

(define audience-icon
  (let* ([i (person-icon)]
         [p (scale (inset i 0 0 0 (* -0.3 (pict-height i))) 1.9)])
    (ht-append/layers
     (- gap-size)
     (box (colorize p "mediumblue"))
     (inset (colorize p "lightblue") 0 -10 0 0)
     (box (inset (colorize p "firebrick") 0 10 0 0))
     (box (colorize p "mediumblue"))
     (colorize p "pink"))))
  
(define (assumptions-slide #:parens? [parens? #f]
                           #:syntax? [syntax? parens?]
                           #:hygiene? [hygiene? syntax?]
                           #:macros? [macros? hygiene?])
  (define state-gap-size 0)
  (define assumptions
    (htl-append
     gap-size
     ((if parens? values ghost) (scale (t "(") 8))
     (hc-append (* 5 gap-size)
                ((if macros? values ghost)
                 (vc-append state-gap-size
                            (colorize (mt "✓") "forestgreen")
                            (ot "macros")))
                ((if hygiene? values ghost)
                 (vc-append state-gap-size
                            (colorize (mt "~") "DarkKhaki")
                            (ot "hygiene")))
                ((if syntax? values ghost)
                 (vc-append state-gap-size
                            (colorize (mt "?") "firebrick")
                            (let ([p (ot "syntax")])
                              (refocus (vc-append (current-line-sep)
                                                  p
                                                  (ot "objects"))
                                       p)))))
     ((if parens? values ghost) (scale (t ")") 8))))
  (slide
   (vc-append
    (* gap-size 8)
    (scale
     (refocus (cc-superimpose
               (let ([p (rotate (let ([c (cloud (* 1.2 (pict-height assumptions)) (* 1.4 (pict-width assumptions)) thought-color)])
                                  (hc-append (* -0.6 (pict-height assumptions))
                                             c
                                             c))
                                (/ pi 2))])
                 (refocus (vc-append
                           0
                           p
                           (inset
                            (scale
                             (colorize (let ([b (filled-ellipse (* client-w 1/4) (* client-h 1/5))])
                                         (vr-append
                                          b
                                          (scale b 0.5)
                                          (scale b 0.25)))
                                       thought-color)
                             0.6)
                            0 0 (* 12 gap-size) 0))
                          p))
               assumptions)
              assumptions)
     0.7)
    audience-icon)))

(define (audience-slides)
  (assumptions-slide)
  (assumptions-slide #:macros? #t)
  (assumptions-slide #:hygiene? #t)
  (assumptions-slide #:syntax? #t)
  #;(assumptions-slide #:parens? #t))

(module+ main
  (audience-slides))
