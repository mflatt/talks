#lang slideshow
(require racket/runtime-path
         "history.rkt"
         "util.rkt")

(provide contributor-slides)

(define-runtime-path racket-mode-png "racket-mode.png")
(define racket-mode (scale (bitmap racket-mode-png) 0.5))

(define (contributor-slides)
  (as-history
   #:res 0
   #:edu 0
   (slide
    #:title (ca 2014 "Racket Mode" #:who "Hendershott")
    racket-mode)))

(module+ main
  (contributor-slides))
