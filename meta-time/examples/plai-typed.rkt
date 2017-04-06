#lang plai-typed

(define-syntax-rule (case expr [(d ...) rhs] ...)
  (let ([v expr])
    (cond
      [(or (equal? v 'd) ...) rhs]
      ...)))

(define (decode [s : symbol]) : number
  (case s
    [(i) 1]
    [(v) 5]
    [(x) 10]))

(decode 'i)
