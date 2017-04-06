#lang scheme

(provide roman->number)

(define (roman->number str)
  (rx-case str
   ["([XV]*)(I{1,4})" (xv i) (+ (roman->number xv)
                                (string-length i))]
   ["(X*)IV" (x) (+ (roman->number x) 4)]
   ["(X*)V" (x) (+ (roman->number x) 5)]
   ["(X*)IX" (x) (+ (roman->number x) 9)]
   ["(X{1,4})" (x) (* 10 (string-length x))]
   [".*" () (error "bad number")]))

(define-syntax-rule (rx-case expr 
                     [pat (id ...) handle] 
                     ...)
  (let ([v expr])
    (cond
     [(regexp-match (pregexp (string-append "^" pat "$")) v)
      => (lambda (m)
           (apply (lambda (id ...) handle) (cdr m)))]
     ...)))

