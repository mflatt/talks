#lang slideshow
(require slideshow/play
         "bear.rkt"
         "syntax-parse.rkt"
         "logo.rkt"
         "plot.rkt"
         "contributor.rkt")

(provide end-slides)

(define (end-slides)
  (many-desks-slides (scale racket-logo 0.1))

  (define (pip at p)
    (define pip (blank))
    (refocus (at p pip) pip))
  
  (define (more-cowbell p)
   (lc-superimpose
    (rc-superimpose
     (rt-superimpose
      (lt-superimpose p
                      (pip rt-superimpose (inset (t "Term rewriting")
                                                 0 gap-size (- gap-size) 0)))
      (pip lt-superimpose (inset (vl-append (current-line-sep)
                                            (t "Lightweight modular")
                                            (t "staging"))
                                 0 gap-size 0 0)))
     (inset (pip lb-superimpose (t "Aspects")) 0 0 (* -2 gap-size) 0))
    (pip rb-superimpose (inset (t "Structured editors")
                               0 0 gap-size (* 2 gap-size)))) )
  
  (define loop
    (loop-content #:macros-t (lambda (s)
                               (vc-append (current-line-sep)
                                          (bt "Programming Language")
                                          (t s)))))
  
  (slide (pin-over (ghost (more-cowbell loop)) loop lt-find loop))
  (slide (more-cowbell loop))
  
  (overall-incidental+inevitable-slides)
  
  (contributors-slides))

(module+ main
  (end-slides))
