#lang slideshow
(require slideshow/code
         slideshow/balloon
         "contributor.rkt"
         "implementation.rkt")

(provide future-slides)

(define (future-slides)
  (define m-arrow (colorize (arrow (* 7 gap-size) (* 1/2 pi)) "gold"))
  
  (define (impl-slide lower?)
    (slide
     #:title "Macros and Lower Implementation Levels"
     (make-implementation #:top-name "Racket"
                          #:bottom-name "core implementation"
                          #:top (list
                                 (cc-superimpose m-arrow
                                                 (inset (colorize (bt "macros") "forestgreen")
                                                        0 gap-size 0 0)))
                          #:top-align cb-superimpose
                          #:bottom-scale 0.5
                          #:bottom (if lower?
                                       (list (inset (cc-superimpose (rotate m-arrow pi)
                                                                    (inset (colorize (bt "macros?") "forestgreen")
                                                                           0 0 0 (* 2 gap-size)))
                                                    0 0 0 (* -2 gap-size)))
                                       null)
                          #:bottom-align ct-superimpose)))
  (impl-slide #f)
  (impl-slide #t)
  
  (slide
   #:title "More Conclusion"
   (para "Something connecting to metaprogramming more broadly")))

(module+ main
  (future-slides))
