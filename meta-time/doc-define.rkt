#lang racket/base
(require (for-syntax racket/base)
         scribble/srcdoc)

(provide (rename-out [new-define define])
         (all-from-out scribble/srcdoc))

(define-syntax (new-define stx)
  (syntax-case stx ()
    [(_ (id arg ...)
        #:contract ctc
        #:doc doc
        body ...)
     #'(begin
         (provide
          (proc-doc/names id
                          ctc
                          (arg ...)
                          doc))
         (define (id arg ...) body ...))]))
