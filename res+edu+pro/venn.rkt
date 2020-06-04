#lang slideshow
(require racket/runtime-path
         "color.rkt"
         "logo.rkt"
         "gear.rkt"
         "paper.rkt"
         "person.rkt")

(provide make-venn
         venn-slides
         venn+prof)

(define (circle diameter label angle color #:flip? [flip? #f])
  (define radius (/ diameter 2))
  (define c (cellophane (colorize (filled-ellipse diameter diameter) color)
                        pale-alpha))
  (define cs ((if flip? reverse values)
              (for/list ([c (in-string label)])
                (colorize (t (string c)) color))))
  (define w (apply + (map pict-width cs)))
  (define circumference (* diameter pi))
  (define sweep (* 2 pi (/ w circumference)))
  (let loop ([c c] [angle (+ angle (/ sweep 2))] [cs cs])
    (cond
      [(null? cs) c]
      [else
       (define ac (car cs))
       (define rc (rotate (inset ac (* -1/2 (pict-width ac)) (* -1/2 (pict-height ac)))
                          (+ (- angle (* 1/2 pi))
                             (if flip? pi 0))))
       (define sweep (* 2 pi (/ (pict-width ac) circumference)))
       (define pos (make-polar (+ radius (/ (pict-height ac) 2))
                               (- angle (/ sweep 2))))
       (loop (pin-over c
                       (+ radius (real-part pos))
                       (- radius (imag-part pos))
                       rc)
             (- angle sweep)
             (cdr cs))])))

(define (make-venn #:icons? [icons? #t]
                   #:res-alpha [res-alpha 1]
                   #:edu-alpha [edu-alpha 1]
                   #:prod-alpha [prod-alpha 1])
  (let* ([p (scale
             (linewidth
              #f
              (vc-append
               -175
               (hc-append
                -150
                (cellophane (circle 300 "Research" (* 3/4 pi) res-color) res-alpha)
                (cellophane (circle 300 "Education" (* 1/4 pi) edu-color) edu-alpha))
               (cellophane (circle 300 "Production" (* 3/2 pi) #:flip? #t prod-color) prod-alpha)))
             1.5)]
         [p (if icons?
                (refocus (ht-append gap-size
                                    (let ([g (three-papers)])
                                      (inset g 0 (* 1/2 (pict-height g)) (* 1/2 (pict-width g)) 0))
                                    p)
                         p)
                p)]
         [p (if icons?
                (rb-superimpose p (inset (gear) 0 0 (* 2 gap-size) (* -1 gap-size)))
                p)]
         [p (if icons?
                (refocus (ht-append (* 2 gap-size)
                                    p
                                    (let ([g (scale (students) 0.6)])
                                      (inset g 0 (* 1 (pict-height g)) 0 0)))
                         p)
                p)])
    p))

(define-runtime-path haskell-logo-png "logos/haskell-logo.png")
(define haskell-logo (scale (bitmap haskell-logo-png) 1/2))

(define-runtime-path scala-logo-png "logos/scala-logo.png")
(define scala-logo (scale (bitmap scala-logo-png) 2/3))

(define (in-res label p)
  (lt-superimpose
   p
   (inset label 50 130 0 0)))

(define (in-pro label p)
   (cb-superimpose p
                   (inset label 0 0 0 100)))

(define (in-edu label p)
  (rt-superimpose
   p
   (inset label 0 130 (/ (- 250 (pict-width label)) 2) 0)))

(define (bts a b)
  (vc-append (current-line-sep) (bt a) (bt b)))

(define the-venn (delay (make-venn)))

(define (venn+prof prof)
  (define venn (force the-venn))
  (ct-superimpose venn
                  (inset (scale prof 0.8) 0 (* 0.15 (pict-height venn)) 0 0)))

(define (venn-slides)
  (define venn (force the-venn))

  (slide #:name "Venn" venn)

  (define (logo-slide logo)
    (slide
     (cc-superimpose venn
                     (inset (scale logo 0.3) 0 0 0 (* 2 gap-size)))))

  (logo-slide logo)
  (logo-slide haskell-logo)
  (logo-slide scala-logo)
  (logo-slide logo)

  (slide (in-pro (bt "stability") (in-res (bt "novelty") venn)))
  (slide (in-pro (bt "generality") (in-edu (bt "simplicity") venn)))
  (slide (in-edu (bt "established") (in-res (bts "cutting" "edge") venn)))
  (slide (in-edu (bts "most" "cases") (in-pro (bts "all" "cases") (in-res (bts "interesting" "cases") venn))))

  ;; Leave this one to transition in "prof-intro.rkt":
  (when #f
    (slide (venn+prof (professor))))

  (void))

(module+ main
  (venn-slides))
