#lang racket
(require racket/stxparam)

(provide this
         obj block select assign
         (rename-out [plus +])
         (rename-out [module-begin #%module-begin])
         #%datum
         #%top-interaction)

;; ----------------------------------------
;; Opaque object representation:

(struct object (ht))

(define (obj-ht o)
  (if (object? o)
      (object-ht o)
      (error "not an object:" o)))

;; ----------------------------------------
;; Syntactic forms:

;; `this' gets a meaning from its use context:
(define-syntax-parameter this 'bad)

;; form for object creation:
(define-syntax-rule (obj [id rhs-expr] ...)
  (object (make-hash (list (cons 'id rhs-expr) ...))))

;; form for a member "method":
(define-syntax-rule (block expr ...)
  (lambda (self) 
    (syntax-parameterize ([this (make-rename-transformer #'self)])
      (begin expr ...))))

;; form to select from an object:
(define-syntax-rule (select obj-expr name)
  (let ()
    (define obj obj-expr)
    (define v (hash-ref (obj-ht obj) 'name
                        (lambda ()
                          (error "no such member:" 'name))))
    (if (procedure? v) 
        (v obj)
        v)))

;; for to set a member:
(define-syntax-rule (assign obj-expr name expr)
  (hash-set! (obj-ht obj-expr) 'name expr))

;; form for addition:
(define-syntax-rule (plus l-expr r-expr)
  (+ l-expr r-expr))

;; an `sobj' module has exactly one object,
;; an its `main' member is implicitly selected:
(define-syntax-rule (module-begin obj-expr)
  (#%module-begin (select obj-expr main)))
