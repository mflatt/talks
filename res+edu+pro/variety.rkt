#lang slideshow
(require slideshow/code
         racket/runtime-path
         "logo.rkt"
         "recycle.rkt"
         "util.rkt"
         "chain.rkt"
         "doc.rkt"
         "asm.rkt"
         "gui.rkt"
         "pkg.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide variety-slides)

(define lang-design
  (code #,(tt "#lang") racket))

(define pkg (scale (pkg-icon) 2))

(define-syntax-rule (define-also-code id e)
  (begin
    e
    (define id (code e))))

(define-also-code macros
  (define-syntax (slides stx)
    (syntax-parse stx
      [(_ id) #'(void)]
      [(_ id e es ...)
       #'(let ([id e])
           (slide id)
           (slides id es ...))])))

(define (variety-slides)
  (slides p
          (cc-superimpose titleless-page logo)
          (ct-superimpose p (inset lang-design
                                   (* -1 (pict-width lang-design)) 0 0 0))
          (rb-superimpose p asm-code)
          (rt-superimpose p (inset gui-image
                                   0 (* -2 gap-size) 0 0))
          (cb-superimpose p (inset (scale recycle-icon 3)
                                   0 0 0 0))
          (lb-superimpose p (inset doc
                                   gap-size 0 0 (* -4 gap-size)))
          (lt-superimpose p (inset chain1
                                   (* 3 gap-size) (* 1 gap-size) 0 0))
          (ct-superimpose p (inset pkg
                                   0 (* 3 gap-size) (* 20 gap-size) 0))
          (rc-superimpose p (scale macros 0.75))))

(module+ main
  (variety-slides))
