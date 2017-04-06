#lang racket/base
(require racket/class)

(define door%
  (class object%
    (define open? #f)
    
    (define/public (enter)
      (if open?
          "ok"
          "it's closed!"))
    
    (define/public (open)
      (set! open? #t))
    
    (super-new)))

(define d (new door%))
(send d open)
(send d enter)
