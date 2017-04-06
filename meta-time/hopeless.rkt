#lang slideshow

(provide hopeless-slides)

(define (hopeless-slides)
  (slide
   (colorize
    (scale
     (vc-append
      (current-line-sep)
      (bt "The top level")
      (hbl-append (bt "is ") (bt "hopeless")))
     3)
    "black")
   'next
   (blank)
   (scale
    (para #:align 'center "... as a way to construct large programs")
    1.25)
   (blank)
   (blank)
   'next
   (colorize
    (parameterize ([current-font-size (floor (* #e0.8 (current-font-size)))]
                   [current-para-width client-w])
      (vc-append
       (para #:align 'right "although it's adequate for interactive exploration")
       (para #:align 'right "(REPLs)")))
    "blue")))

(module+ main
  (hopeless-slides))

