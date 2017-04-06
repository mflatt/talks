#lang racket/base
(require slideshow
         slideshow/balloon
         "bear.rkt")

(provide inevitable
         incidental)


(define progression-width (* client-w 1/3))

(define (progression icon label content
                     #:fill? [fill? #f]
                     #:width [width progression-width])
  (define p (ht-append
             20
             icon
             (if (null? content)
                 label
                 (vl-append
                  (current-line-sep)
                  label
                  (apply para #:fill? fill? #:width width content)))))
  (cc-superimpose (colorize (filled-rounded-rectangle (+ (pict-width p) gap-size)
                                                      (+ (pict-height p) gap-size)
                                                      16)
                            balloon-color)
                  p))

(define (inevitable #:width [width progression-width]
                    #:fill? [fill? #f]
                    . content)
  (progression (head #:bear? #f #:hair 'swoop #:hair-brush gold-brush)
               (colorize (bit "Inevitable") "forestgreen") content
               #:fill? fill?
               #:width width))

(define (incidental #:width [width progression-width]
                    #:fill? [fill? #f]
                    . content)
  (progression hot-papa-bowl
               (colorize (bit "Incidental") "orange") content
               #:fill? fill?
               #:width width))
