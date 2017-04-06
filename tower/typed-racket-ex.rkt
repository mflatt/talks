#lang typed/racket

(: factorial (Number -> Number))
(define (factorial n)
  (cond
    [(zero? n) 1]
    [else (* n (factorial (sub1 n)))]))
