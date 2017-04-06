#lang racket

(define-syntax define-syntax-rule
  (syntax-rules (...)
    [(define-syntax-rule (def-id (id a ...) expr)
       tmpl)
     (define-syntax def-id
       (syntax-rules ()
         [(def-id (id a ...) expr)
          (define-values (id) 
            (lambda (a ...) "fish"))]))]))

(define-syntax-rule (define (id a ...) expr)
  (define-values (id) 
    (lambda (a ...) (list a ...))))

(define (+ x y)
  (* x y))

(+ 1 2)






























