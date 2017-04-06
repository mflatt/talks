#lang slideshow
(provide st)

(define (st s #:t [t t])
  (define a (t s))
  (define b (colorize a "black"))
  (refocus (lt-superimpose (inset b 1 1 0 0)
                           (inset b -1 -1 0 0)
                           (colorize a "gold"))
           a))

